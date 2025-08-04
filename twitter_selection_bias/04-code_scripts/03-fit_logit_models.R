library(dplyr)
library(purrr)
library(data.table)
library(gtsummary)
library(gt)

#### ---- DIRECTORIES ---- ####
input_dir <- "00-raw_data/"
output_dir <- "01-processed_data/"
figures_dir <- "02-figures/"
code_dir <- "04-code_scripts/"

#### ---- LOAD FUNCTIONS ---- ####
source(paste0(code_dir,"00-functions.R"))

#### ---- LOAD MERGED SURVEY DATA ---- ####
load(paste0(output_dir, "survey_data_nations_merged.RData"))

#### ---- SUMMMARY STATISTICS ---- ####

# Change the survey names to more intuitive labels
survey_data_merged <- survey_data_merged |> 
  mutate(
    survey_name = recode(survey_name,
                         "FR_2022_survey_subset" = "FR:2022",
                         "GER_2021_survey_subset" = "GR:2021",
                         "POL_2023_survey_subset" = "PL:2023",
                         "UK_2024_survey_subset" = "UK:2024",
                         "US_2020_survey_subset" = "US:2020",
                         "US_2024_survey_subset" = "US:2024")
  )

# Correct the handful of cases where a respondent had a Twitter account but gave a missing
# response to data linkage (code these cases as no consent)
survey_data_merged <- survey_data_merged |> 
  mutate(linkage_consent = case_when(
    uses_twitter == "Yes" & is.na(linkage_consent) ~ "No",
    TRUE ~ as.character(linkage_consent)
  )) |> 
  mutate(linkage_consent = factor(linkage_consent))

# Generate summary table for the dependent variables (unweighted)
table1 <-
  tbl_summary(
    survey_data_merged,
    include = c(uses_twitter, linkage_consent),
    by = survey_name,
    missing = "always",
    label = list(
      uses_twitter ~ "Uses Twitter",
      linkage_consent ~ "Consented to Linkage"
    )
  ) |>
  add_overall(last = TRUE) |>
  add_n() |>
  modify_header(list(
    label ~ "**Variable**"
  )) |>
  bold_labels()  |>
  modify_table_body(
    ~ .x %>%
      mutate(label = ifelse(label == "Unknown", "Missing", label))
  ) |> 
  as_gt() |> 
  gtsave(filename = paste0(figures_dir,"summary_table_unweighted.png"))

# Generate a weighted table
weighted_data <- survey::svydesign(~1, 
                                   data = subset(survey_data_merged, !is.na(weight)),
                                   weights = ~weight)

table2 <-
  tbl_svysummary(
    weighted_data,
    include = c(uses_twitter, linkage_consent),
    by = survey_name,
    missing = "always",
    label = list(
      uses_twitter ~ "Uses Twitter",
      linkage_consent ~ "Consented to Linkage"
    )
  ) |>
  add_overall(last = TRUE) |>
  add_n() |>
  modify_header(list(
    label ~ "**Variable**"
  )) |>
  bold_labels()  |>
  modify_table_body(
    ~ .x %>%
      mutate(label = ifelse(label == "Unknown", "Missing", label))
  ) |> 
  as_gt() |> 
  gtsave(filename = paste0(figures_dir,"summary_table_weighted.png"))

#### ---- FIT LOGIT MODELS ---- ####

# 01: Overall Bias (Pooled Regression)

# Set national survey names to factor and set reference category to US:2020
survey_data_merged$survey_name <- factor(survey_data_merged$survey_name) |> 
  relevel(ref = "US:2020")

# Create dummy variable for repeated participants in US 2020 and 2024 surveys
survey_data_merged <- survey_data_merged |> 
  mutate(repeat_participant = case_when(
    !is.na(survey_data_merged$ID_2020) ~ 1,
    survey_data_merged$survey_name == "US:2020" & 
      survey_data_merged$ID %in% survey_data_merged$ID_2020 ~ 1,
    .default = 0
  ))

# Fit (unweighted) models
usage_model_pooled <- fit_logit(data = survey_data_merged, dep_var = "uses_twitter", pooled = TRUE, weighted = FALSE)
linkage_model_pooled <- fit_logit(data = survey_data_merged, dep_var = "linkage_consent", pooled = TRUE, weighted = FALSE) 

# Merge results
usage_model_pooled$dep_var <- "uses_twitter"
linkage_model_pooled$dep_var <- "linkage_consent"

pooled_results <- rbind(usage_model_pooled,linkage_model_pooled) |> 
  mutate(nation = "Pooled")

# 02: Between-Country Comparisons

# Define model inputs (nation + DV)
dep_vars <- c("uses_twitter","linkage_consent")
datasets <- split(survey_data_merged, survey_data_merged$survey_name)

inputs <- expand.grid(
  survey_name = names(datasets),
  dep_var = dep_vars,
  stringsAsFactors = FALSE
)

# Fit models
results <- pmap(inputs, function(survey_name, dep_var) {
  data <- datasets[[survey_name]]
  repeated <- ifelse(startsWith(survey_name, "US"), TRUE, FALSE)
  result <- fit_logit(data = data, dep_var = dep_var, longitudinal = repeated, weighted = FALSE) |> 
    mutate(nation = survey_name,
           dep_var = dep_var)
})

# Merge model results together
results_joined <- data.table::rbindlist(results) |> 
  rbind(pooled_results)

# Save as .RData file
save(results_joined, file = paste0(output_dir,"logit_model_coefficients.RData"))

#### --- FIT WEIGHTED MODELS FOR COMPARISON ---- ####

# Fit weighted models
usage_model_pooled_weighted <- fit_logit(data = survey_data_merged, dep_var = "uses_twitter", pooled = TRUE, weighted = TRUE)
linkage_model_pooled_weighted <- fit_logit(data = survey_data_merged, dep_var = "linkage_consent", pooled = TRUE, weighted = TRUE) 

# Merge results
usage_model_pooled_weighted$dep_var <- "uses_twitter"
linkage_model_pooled_weighted$dep_var <- "linkage_consent"

pooled_results_weighted <- rbind(usage_model_pooled_weighted,linkage_model_pooled_weighted) |> 
  mutate(nation = "Pooled")

# 02: Between-Country Comparisons

# Fit models
results_weighted <- pmap(inputs, function(survey_name, dep_var) {
  data <- datasets[[survey_name]]
  repeated <- ifelse(startsWith(survey_name, "US"), TRUE, FALSE)
  result <- fit_logit(data = data, dep_var = dep_var, longitudinal = repeated, weighted = TRUE) |> 
    mutate(nation = survey_name,
           dep_var = dep_var)
})

# Merge model results together
results_weighted_joined <- data.table::rbindlist(results_weighted) |> 
  rbind(pooled_results_weighted)

# Save as .RData file
save(results_weighted_joined, file = paste0(output_dir,"logit_model_coefficients_WEIGHTED.RData"))

#### ---- END ---- ####
