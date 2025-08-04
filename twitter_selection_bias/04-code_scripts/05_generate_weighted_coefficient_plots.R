library(dplyr)
library(ggplot2)
library(data.table)
library(viridis)

#### ---- DIRECTORIES ---- ####
input_dir <- "00-raw_data/"
output_dir <- "01-processed_data/"
figures_dir <- "02-figures/"
code_dir <- "04-code_scripts/"

#### ---- LOAD MODEL OUTPUTS ---- ####
load(paste0(output_dir, "logit_model_coefficients_WEIGHTED.RData"))

#### ---- PLOT MODEL COEFFICIENTS ---- ####

# First, coefficients are capped at 5 to avoid extreme values distorting the plot
cap_val <- 5
results_capped <- results_weighted_joined |> 
  mutate(
    estimate = ifelse(estimate > cap_val, cap_val, estimate),
    conf.high = ifelse(conf.high > cap_val, cap_val, conf.high)
  )

# Second, rename variable values for clarity 
results_capped <- results_capped |> 
  mutate(
    term = recode(term,
                  "age30-44" = "Age (30-44)",
                  "age45-64" = "Age (45-64)",
                  "age65+" = "Age (65+)", 
                  "education_levelHigh" = "Education Level (High)",
                  "education_levelMedium" = "Education Level (Medium)",
                  "employment_statusEmployed" = "Employment Status (Employed)",
                  "genderMale" = "Gender (Male)",
                  "general_trustHigh" = "General Trust (High)",
                  "general_trustMedium" = "General Trust (Medium)",
                  "household_incomeHigh" = "Household Income (High)",
                  "household_incomeMedium" = "Household Income (Medium)",
                  "ideologyConservative" = "Ideology (Conservative)",
                  "ideologyModerate" = "Ideology (Moderate)",
                  "political_attentionHigh" = "Political Attention (High)",
                  "political_attentionMedium" = "Political Attention (Medium)",
                  "privacy_concernHigh" = "Privacy Concern (High)",
                  "privacy_concernMedium" = "Privacy Concern (Medium)",
                  "trust_SM_companiesHigh" = "Trust in SM Companies (High)",
                  "trust_SM_companiesMedium" = "Trust in SM Companies (Medium)",
                  "post_twitterDaily" = "Posts Political Tweets (Daily)",
                  "post_twitterMonthly/Weekly" = "Posts Political Tweets (Monthly/Weekly)",
                  "post_twitterYearly" = "Posts Political Tweets (Yearly)",
                  "reply_twitterDaily" = "Replies Political Tweets (Daily)",
                  "reply_twitterMonthly/Weekly" = "Replies Political Tweets (Monthly/Weekly)",
                  "reply_twitterYearly" = "Replies Political Tweets (Yearly)",
                  "survey_nameFR:2022" = "Nation (FR:2022)",
                  "survey_nameGR:2021" = "Nation (GR:2021)",
                  "survey_namePL:2023" = "Nation (PL:2023)",
                  "survey_nameUK:2024" = "Nation (UK:2024)",
                  "survey_nameUS:2024" = "Nation (US:2024)",
                  "repeat_participant" = "Repeated (US)",
    ),
    dep_var = recode(dep_var,
                     "uses_twitter" = "Twitter Usage",
                     "linkage_consent" = "Linkage Consent"),
    nation = recode(nation,
                    "FR_2022_survey_subset" = "FR:2022",
                    "GER_2021_survey_subset" = "GR:2021",
                    "POL_2023_survey_subset" = "PL:2023",
                    "UK_2024_survey_subset" = "UK:2024",
                    "US_2020_survey_subset" = "US:2020",
                    "US_2024_survey_subset" = "US:2024")
  )

# Thirdly, re-level variables 
results_capped$dep_var <- factor(results_capped$dep_var, levels = c("Twitter Usage", "Linkage Consent"))
results_capped$term <- factor(results_capped$term, levels = rev(unique(results_capped$term)))

# PLOT 1: Pooled Regression Coefficients 
model_plot1 <- ggplot(results_capped |> filter(term != "(Intercept)" & nation == "Pooled"), 
                      aes(x = term, y = estimate)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  coord_flip() +
  labs(y = "Odds Ratio (95% CIs)", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 9, face = "bold")
  ) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ dep_var) +
  scale_color_viridis_d()

ggsave(paste0(figures_dir,"model_coefficient_plot_pooled_regression_WEIGHTED.png"),
       model_plot1,
       units="in", width=9, height=8, dpi=600,
       bg="white")

# PLOT 2: Cross-national Regression Coefficients (excludes US 2020)
model_plot2 <- ggplot(results_capped |> filter(term != "(Intercept)" & !(nation %in% c("US:2020","Pooled"))), 
                      aes(x = term, y = estimate, color = nation, shape = nation)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  coord_flip() +
  labs(y = "Odds Ratio (95% CIs)", x = "",
       color = "Nation/Year",
       shape = "Nation/Year") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 9, face = "bold")
  ) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ dep_var) +
  scale_color_viridis_d()

ggsave(paste0(figures_dir,"model_coefficient_plot_cross_national_comparison_WEIGHTED.png"),
       model_plot2,
       units="in", width=9, height=8, dpi=300,
       bg="white")

# PLOT 3: Longitudinal Regression Coefficients (US 2020 and 2024)
model_plot3 <- ggplot(results_capped |> filter(term != "(Intercept)" & nation %in% c("US:2020","US:2024")), 
                      aes(x = term, y = estimate, color = nation, shape = nation)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  coord_flip() +
  labs(y = "Odds Ratio (95% CIs)", x = "",
       color = "Nation/Year",
       shape = "Nation/Year") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 9, face = "bold")
  ) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ dep_var) +
  scale_color_viridis_d()

ggsave(paste0(figures_dir,"model_coefficient_plot_within_country_comparison_WEIGHTED.png"),
       model_plot3,
       units="in", width=9, height=8, dpi=300,
       bg="white")

#### ---- END ---- ####