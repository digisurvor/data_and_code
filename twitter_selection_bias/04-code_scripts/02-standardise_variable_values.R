library(dplyr)

#### ---- DIRECTORIES ---- ####
input_dir <- "00-raw_data/"
output_dir <- "01-processed_data/"
code_dir <- "04-code_scripts/"

#### ---- LOAD FUNCTIONS ---- ####
source(paste0(code_dir,"00-functions.R"))

#### ---- LOAD SURVEY DATA SUBSET ---- ####

# Get full paths and base file names
file_paths <- list.files(output_dir, full.names = TRUE)
file_names <- tools::file_path_sans_ext(basename(file_paths))

# Read the files and add a column with the file name
survey_data <- Map(function(path, name) {
  df <- read.csv(path)
  df$survey_name <- name
  return(df)
}, path = file_paths, name = file_names)

#### ---- STANDARDISE VARIABLE VALUES ---- ####

# US Election Survey 2020
us_election_2020_processed <- survey_data[["01-processed_data/US_2020_survey_subset.csv"]] |> 
  mutate(education_level = case_when(
    education_level == "No HS, High School Grad" ~ "Low",
    education_level == "Some College, 2-year" ~ "Medium",
    education_level == "4-year" | education_level == "Post-Grad" ~ "High"
  )) |> 
  mutate(employment_status = case_when(
    employment_status %in% c("Working full time","Working part time") ~ "Employed",
    employment_status == "Prefer not to say" ~ NA,
    .default = "Not Employed"
  )) |> 
  mutate(household_income = case_when(
    household_income %in% c("Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999",
                            "$30,000 - $39,999", "$40,000 - $49,999") ~ "Low",
    household_income %in% c("$50,000 - $59,999", "$60,000 - $69,999", "$70,000 - $79,999",
                            "$80,000 - $99,999") ~ "Medium",
    household_income %in% c("$100,000 - $119,999", "$120,000 - $149,999","$150,000 - $199,999", 
                            "$200,000 - $249,999", "$250,000 - $349,999",
                            "$350,000 - $499,999", "$500,000 or more") ~ "High"
  )) |> 
  recode_vars() |> 
  relevel_vars()
  
# US Election Survey 2024
us_election_2024_processed <- survey_data[["01-processed_data/US_2024_survey_subset.csv"]] |> 
  mutate(education_level = case_when(
    education_level == "No HS, High School Grad" ~ "Low",
    education_level == "Some College, 2-year" ~ "Medium",
    education_level == "4-year" | education_level == "Post-Grad" ~ "High"
  )) |> 
  mutate(employment_status = case_when(
    employment_status %in% c("Working full time","Working part time") ~ "Employed",
    employment_status == "Prefer not to say" ~ NA,
    .default = "Not Employed"
  )) |> 
  mutate(household_income = case_when(
    household_income %in% c("Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999",
                            "$30,000 - $39,999", "$40,000 - $49,999") ~ "Low",
    household_income %in% c("$50,000 - $59,999", "$60,000 - $69,999", "$70,000 - $79,999",
                            "$80,000 - $99,999") ~ "Medium",
    household_income %in% c("$100,000 - $119,999", "$120,000 - $149,999","$150,000 - $199,999", 
                            "$200,000 - $249,999", "$250,000 - $349,999",
                            "$350,000 - $499,999", "$500,000 or more") ~ "High"
  )) |> 
  recode_vars() |> 
  relevel_vars() 

# UK Election Survey 2024
uk_election_2024_processed <- survey_data[["01-processed_data/UK_2024_survey_subset.csv"]] |> 
  mutate(employment_status = case_when(
    employment_status %in% c("Working full time (30 or more hours per week)",
                             "Working part time (8-29 hours a week)",
                             "Working part time (Less than 8 hours a week)") ~ "Employed",
    .default = "Not Employed"
  )) |> 
  mutate(household_income = case_when(
    household_income %in% c("under £5,000 per year", "£5,000 to £9,999 per year", 
                            "£10,000 to £14,999 per year","£15,000 to £19,999 per year", 
                            "£20,000 to £24,999 per year") ~ "Low",
    household_income %in% c("£25,000 to £29,999 per year", "£30,000 to £34,999 per year", 
                            "£35,000 to £39,999 per year","£40,000 to £44,999 per year",
                            "£45,000 to £49,999 per year") ~ "Medium",
    household_income %in% c("£50,000 to £59,999 per year", "£60,000 to £69,999 per year",
                            "£70,000 to £99,999 per year", "£100,000 to £149,999 per year", 
                            "£150,000 and over") ~ "High"
  )) |> 
  recode_vars() |> 
  relevel_vars()
  
# Poland Election Survey 2023
pol_election_2023_processed <- survey_data[["01-processed_data/POL_2023_survey_subset.csv"]] |> 
  mutate(education_level = case_when(
    education_level %in% c("I did not complete any formal education",
                           "Early childhood education",
                           "Primary education",
                           "Lower secondary education (GCSEs or equivalent level)",
                           "Upper secondary education (A-Levels or baccalaureate)") ~ "Low",
    education_level %in% c("Post-secondary, non-tertiary education (generally vocational/ professional qualification of 1-2 years, e.g. college, tra",
                           "Short-cycle tertiary education (vocational education and training, studying towards a non-academic degree, e.g. nursing/") ~
      "Medium",
    education_level %in% c("Bachelors or equivalent level degree",
                           "Masters or equivalent level degree",
                           "Doctoral or equivalent level degree") ~ "High"
  )) |> 
  mutate(employment_status = case_when(
    employment_status %in% c("Working full time","Working part time") ~ "Employed",
    employment_status == "Prefer not to say" ~ NA,
    .default = "Not Employed"
  )) |> 
  mutate(household_income = case_when(
    household_income %in% c("under 5.000€ per year", "5.000€ to 9.999€ per year", 
                            "10.000€ to 14.999€ per year","15.000€ to 19.999€ per year", 
                            "20.000€ to 24.999€ per year") ~ "Low",
    household_income %in% c("25.000€ to 29.999€ per year", "30.000€ to 34.999€ per year", 
                            "35.000€ to 39.999€ per year","40.000€ to 44.999€ per year",
                            "45.000€ to 49.999€ per year") ~ "Medium",
    household_income %in% c("50.000€ to 59.999€ per year", "60.000€ to 69.999€ per year",
                            "70.000€ to 99.999€ per year", "100.000€ to 149.999€ per year", 
                            "150.000€ and over") ~ "High"
  )) |> 
  mutate(across(c(post_twitter,reply_twitter), ~ case_when(
                    . == "Nigdy" ~ "Never",
                    . == "Rzadziej niz kilka razy w roku" ~ "Less often than a few times a year",
                    . == "Kilka razy w roku" ~ "Few times a year",
                    . == "Kilka razy w miesiacu" ~ "Few times a month",
                    . == "Kilka razy w tygodniu" ~ "Few times a week",
                    . == "Raz dziennie" ~ "Once a day",
                    . == "Kilka razy dziennie" ~ "A few times a day",
                    . == "Wiele razy dziennie" ~ "Many times a day",
                    .default = .
  ))) |> 
  mutate(uses_twitter = case_when(
    uses_twitter == "Mam obecnie konto" ~ "I have an account currently",
    .default = uses_twitter
  )) |> 
  mutate(linkage_consent = case_when(
    linkage_consent == "Wyrazam zgode" ~ "I consent",
    .default = linkage_consent
  )) |> 
  recode_vars() |> 
  relevel_vars()

# France Election Survey 2022
fr_election_2022_processed <- survey_data[["01-processed_data/FR_2022_survey_subset.csv"]] |> 
  mutate(gender = case_when(
    gender == "Un homme" ~ "Male",
    gender == "Une femme" ~ "Female"
  )) |> 
  mutate(education_level = case_when(
    education_level %in% c("Aucun diplôme",
                           "Baccalauréat, brevet professionnel ou équivalent",
                           "Brevet des collèges",
                           "Certificat d'étude") ~ "Low",
    education_level %in% c("Diplôme de l'enseignement technique court (CAP, BEP ou équivalent)") ~
      "Medium",
    education_level %in% c("Diplôme du premier cycle universitaire ou technique (Bac +2, BTS, DEUG, License)",
                           "Diplôme du second/troisième cycle universitaire (Master, Grandes écoles, Doctorat)") ~ "High"
  )) |> 
  mutate(employment_status = case_when(
    employment_status %in% c("Travail à temps plein (30h par semaine ou plus)",
                             "Travail à temps partiel") ~ "Employed",
    employment_status %in% c("Je ne sais pas",
                             "Aucune des propositions ci-dessus") ~ NA,
    .default = "Not Employed"
  )) |> 
  mutate(household_income = case_when(
    household_income %in% c("Moins de 15 000€", "15 000€ - 19 999€", 
                            "20 000€ - 29 999€") ~ "Low",
    household_income %in% c("30 000€ - 39 999€","40 000€ - 49 999€",
                            "50 000€ - 59 999€") ~ "Medium",
    household_income %in% c("60 000€ - 69 999€","70 000€ - 79 999€",
                            "80 000€ - 89 999€","90 000€ - 99 999€",
                            "100 000€ - 124 999€", "125 000€ - 149,999€", 
                            "150 000€ - 199 999€", "200 000€ et plus") ~ "High"
  )) |> 
  mutate(across(c(post_twitter,reply_twitter), ~ case_when(
    . == "Jamais" ~ "Never",
    . == "Moins de quelques fois par an" ~ "Less often than a few times a year",
    . == "Quelques fois par an" ~ "Few times a year",
    . == "Quelques fois par mois" ~ "Few times a month",
    . == "Quelques fois par semaine" ~ "Few times a week",
    . == "Une fois par jour" ~ "Once a day",
    . == "Quelques fois par jour" ~ "A few times a day",
    . == "De nombreuses fois par jour" ~ "Many times a day",
    .default = .
  ))) |> 
  mutate(uses_twitter = case_when(
    uses_twitter == "Je possède actuellement un compte" ~ "I have an account currently",
    .default = uses_twitter
  )) |> 
  mutate(linkage_consent = case_when(
    linkage_consent == "J’accepte" ~ "I consent",
    .default = linkage_consent
  )) |> 
  recode_vars() |> 
  relevel_vars()

# Germany Election Survey 2021
ger_election_2021_processed <- survey_data[["01-processed_data/GER_2021_survey_subset.csv"]] |> 
  mutate(gender = case_when(
    gender == "männlich" ~ "Male",
    gender == "weiblich" ~ "Female"
  )) |> 
  mutate(political_attention = case_when(
    political_attention %in% c("Überhaupt nicht","Weniger stark") ~ "0",
    political_attention == "Mittelmäßig" ~ "5",
    political_attention %in% c("Ziemlich stark","Sehr stark") ~ "10"
  )) |> 
  mutate(education_level = case_when(
    education_level %in% c("Keinen Abschluss",
                           "Lehre oder vergleichbarer Abschluss") ~ "Low",
    education_level %in% c("Noch in Ausbildung",
                           "Noch im Studium") ~ "Medium",
    education_level %in% c("Universitäts- oder Fachhochschulabschluss") ~ "High"
  )) |> 
  mutate(employment_status = case_when(
    employment_status %in% c("Erwerbstätigkeit/Berufstätigkeit") ~ "Employed",
    employment_status %in% c("keine Angabe") ~ NA,
    .default = "Not Employed"
  )) |> 
  mutate(household_income = case_when(
    household_income %in% c("unter EUR 500", "EUR 500 bis unter EUR 1.000", 
                            "EUR 1.000 bis unter EUR 1.500","EUR 1.500 bis unter EUR 2.000") ~ "Low",
    household_income %in% c("EUR 2.000 bis unter EUR 2.500","EUR 2.500 bis unter EUR 3.000",
                            "EUR 3.000 bis unter EUR 3.500","EUR 3.500 bis unter EUR 4.000") ~ "Medium",
    household_income %in% c("EUR 4.000 bis unter EUR 4.500","EUR 4.500 bis unter EUR 5.000",
                            "EUR 5.000 bis unter EUR 10.000","EUR 10.000 und mehr") ~ "High"
  )) |> 
  mutate(across(c(post_twitter,reply_twitter), ~ case_when(
    . == "Nie" ~ "Never",
    . == "Seltener als ein paar Mal im Jahr" ~ "Less often than a few times a year",
    . == "Ein paar Mal im Jahr" ~ "Few times a year",
    . == "Ein paar Mal im Monat" ~ "Few times a month",
    . == "Ein paar Mal in der Woche" ~ "Few times a week",
    . == "Einmal täglich" ~ "Once a day",
    . == "Ein paar Mal täglich" ~ "A few times a day",
    . == "Mehrmals täglich" ~ "Many times a day",
    .default = .
  ))) |> 
  mutate(uses_twitter = case_when(
    uses_twitter == "Ich habe derzeit einen Account" ~ "I have an account currently",
    .default = uses_twitter
  )) |> 
  mutate(linkage_consent = case_when(
    linkage_consent == "Ich stimme zu" ~ "I consent",
    .default = linkage_consent
  )) |> 
  recode_vars() |> 
  relevel_vars()

#### ---- MERGE DATASETS TOGETHER AND SAVE AS RDATA FILE ---- ####

survey_data_merged <- rbind(fr_election_2022_processed,ger_election_2021_processed,pol_election_2023_processed,
                            uk_election_2024_processed,us_election_2020_processed,us_election_2024_processed)

save(survey_data_merged, file = paste0(output_dir,"survey_data_nations_merged.RData"))

#### ---- END ---- ####