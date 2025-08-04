library(foreign)
library(dplyr)

#### ---- DIRECTORIES ---- ####
input_dir <- "00-raw_data/"
output_dir <- "01-processed_data/"

#### ---- LOAD RAW SURVEY DATA ---- ####

# Get full paths and base file names
file_paths <- list.files(input_dir, full.names = TRUE)
file_names <- tools::file_path_sans_ext(basename(file_paths))

# Read the files and add a column with the file name
survey_data <- Map(function(path, name) {
  df <- read.spss(path, to.data.frame = TRUE)
  df$survey_name <- name
  return(df)
}, path = file_paths, name = file_names)

#### ---- SELECT AND STANDARDISE VARIABLE NAMES ---- ####

# Poland Election Survey 2023
pol_survey_2023_subset <- survey_data[["00-raw_data/POL_2023_survey.sav"]] |> 
  mutate(
    media1_num = as.numeric(as.character(uom_plpre_9b_1)),
    media2_num = as.numeric(as.character(uom_plpre_9c_1)),
    media1_num = ifelse(media1_num == 997, NA, media1_num),
    media2_num = ifelse(media2_num == 997, NA, media2_num),
    media_mean = round(rowMeans(across(c(media1_num, media2_num)), na.rm = TRUE))
  ) |> 
  mutate(ID_2020 = NA) |> # this additional column only exists in US 2024 but is needed for merging
  select(
    survey_name, 
    ID,
    ID_2020,
    weight = W8_1_prenatrep_incl,
    age,
    gender,
    education_level = education_PL,
    employment_status = empl_stat_EU,
    household_income = profile_gross_household_EU,
    political_attention = uom_frpre_3_1,
    ideology = uom_plpre_2_1,
    privacy_concern = uom_plpre_23,
    trust_gov = uom_plpre_9a_1,
    trust_media = media_mean,
    trust_SM_companies = uom_plpre_9d_1,
    trust_military = uom_plpre_9e_1,
    trust_parties = uom_plpre_9f_1,
    post_twitter = uom_plpre_24, 
    reply_twitter = uom_plpre_25,
    uses_twitter = uom_plpre_19_2,
    linkage_consent = twitterconsent
  ) |> 
  write.csv(file = paste0(output_dir,"POL_2023_survey_subset.csv"))

# Germany Election Survey 2021
ger_survey_2022_subset <- survey_data[["00-raw_data/GER_2021_survey.sav"]] |> 
  mutate(ID_2020 = NA) |> 
  select(
    survey_name, 
    ID,
    ID_2020,
    weight = W8_prenatrep,
    age,
    gender,
    education_level = voceduc_neu,
    employment_status = emps,
    household_income = hinc,
    political_attention = pol_interest,
    ideology = leftright_neu_shiftx_scale,
    privacy_concern = uom_depre_24,
    trust_gov = uom_depre_9b_1,
    trust_media = uom_depre_9f_1,
    trust_SM_companies = uom_depre_9g_1,
    trust_military = uom_depre_9c_1,
    trust_parties = uom_depre_9e_1,
    post_twitter = uom_depre_25, 
    reply_twitter = uom_depre_26,
    uses_twitter = uom_depre_20_2,
    linkage_consent = twitterconsent
  )  |> 
  write.csv(file = paste0(output_dir,"GER_2021_survey_subset.csv"))

# France Election Survey 2022
fr_survey_2022_subset <- survey_data[["00-raw_data/FR_2022_survey.sav"]] |> 
  mutate(ID_2020 = NA) |> 
  select(
    survey_name, 
    ID,
    ID_2020,
    weight = W8_W1_NatRep,
    age,
    gender,
    education_level = education,
    employment_status = work_status_v2,
    household_income = income,
    political_attention = uom_frpre_3_1_W1,
    ideology = uom_frpre_2_1_W1,
    privacy_concern = uom_frpre_25x_W1,
    trust_gov = uom_frpre_8a_1_W1,
    trust_media = uom_frpre_8c_1_W1,
    trust_SM_companies = uom_frpre_8d_1_W1,
    trust_military = uom_frpre_8e_1_W1,
    trust_parties = uom_frpre_8f_1_W1,
    post_twitter = uom_frpre_26_W1, 
    reply_twitter = uom_frpre_27_W1,
    uses_twitter = uom_frpre_21_2_W1,
    linkage_consent = twitterconsent_W1
  ) |> 
  write.csv(file = paste0(output_dir,"FR_2022_survey_subset.csv"))

# UK Election Survey 2024 
uk_survey_2024_subset <- survey_data[["00-raw_data/UK_2024_survey.sav"]] |> 
  mutate(ID_2020 = NA) |> 
  select(
    survey_name,
    ID,
    ID_2020,
    weight = W8_natreppre,
    age,
    gender = profile_gender,
    education_level = highest_education_simple,
    employment_status = profile_work_stat,
    household_income = profile_gross_household,
    political_attention = polAttentionpdl,
    ideology = uom_ukpre_2_1,
    privacy_concern = uom_ukpre_23,
    trust_gov = uom_ukpre_9a_1,
    trust_media = uom_ukpre_9e_1,
    trust_SM_companies = uom_ukpre_9f_1,
    trust_military = uom_ukpre_9g_1,
    trust_parties = uom_ukpre_9h_1,
    post_twitter = uom_ukpre_24, 
    reply_twitter = uom_ukpre_25,
    uses_twitter = uom_ukpre_19_2,
    linkage_consent = twitterconsent,
  ) |> 
  write.csv(file = paste0(output_dir,"UK_2024_survey_subset.csv"))

# US Election Survey 2020 
us_survey_2020_subset <- survey_data[["00-raw_data/US_2020_survey.sav"]] |> 
  mutate(ID_2020 = NA) |> 
  select(
    survey_name,
    ID,
    ID_2020,
    weight = weight_preminuspulse,
    age,
    gender,
    education_level = educ_w8,
    employment_status = empl_stat_2018,
    household_income = profile_gross_household,
    political_attention = uom_uspre_5_1,
    ideology = uom_uspre_2_1,
    privacy_concern = uom_uspre_25x,
    trust_gov = uom_uspre_6a_1,
    trust_media = uom_uspre_6c_1,
    trust_SM_companies = uom_uspre_6d_1,
    trust_military = uom_uspre_6e_1,
    trust_parties = uom_uspre_6f_1,
    post_twitter = uom_uspre_26, 
    reply_twitter = uom_uspre_27,
    uses_twitter = uom_uspre_21_2,
    linkage_consent = twitterconsent,
  ) |> 
  write.csv(file = paste0(output_dir,"US_2020_survey_subset.csv"))

# US Election Survey 2024 
us_survey_2024_subset <- survey_data[["00-raw_data/US_2024_survey.sav"]] |> 
  select(
    survey_name,
    ID,
    ID_2020, # to track within person change 
    weight = W8_natreppreeveryone,
    age,
    gender,
    education_level = educ_w8,
    employment_status = empl_stat_2018,
    household_income = profile_gross_household,
    political_attention = uom_uspre_4_1,
    ideology = uom_uspre_2_1,
    privacy_concern = uom_uspre_25,
    trust_gov = uom_uspre_9a_1,
    trust_media = uom_uspre_9c_1,
    trust_SM_companies = uom_uspre_9d_1,
    trust_military = uom_uspre_9e_1,
    trust_parties = uom_uspre_9f_1,
    post_twitter = uom_uspre_26,
    reply_twitter = uom_uspre_27,
    uses_twitter = uom_uspre_21_2,
    linkage_consent = twitterconsent
  ) |> 
  write.csv(file = paste0(output_dir,"US_2024_survey_subset.csv"))

#### ---- END ---- ####