library(dplyr)
library(stringr)
library(broom)

#### ---- RECODE VARIABLES ---- ####

recode_vars <- function(dataset) {
  
  # 00: Age 
  dataset$age <- as.numeric(as.character(dataset$age))
  
  dataset <- dataset |> 
    mutate(age = case_when(
      age <= 29 ~ "18-29",
      age >= 30 & age <= 44 ~ "30-44",
      age >= 45 & age <= 64 ~ "45-64",
      age >= 65 ~ "65+"
    ))
  
  # 01: Political Attention
  dataset <- dataset |> 
    mutate(political_attention = case_when(
      political_attention %in% c("0","1","2","3") ~ "Low",
      political_attention %in% c("4","5","6") ~ "Medium",
      political_attention %in% c("7","8","9","10") ~ "High",
      TRUE ~ NA_character_
    ))
  
  # 02: Left/Right Ideology
  dataset <- dataset |> 
    mutate(ideology = case_when(
      ideology %in% c("0","1","2","3") ~ "Liberal",
      ideology %in% c("4","5","6") ~ "Moderate",
      ideology %in% c("7","8","9","10") ~ "Conservative",
      TRUE ~ NA_character_
    ))
  
  # 03: Privacy Concern
  dataset <- dataset |> 
    mutate(privacy_concern = case_when(
      str_starts(privacy_concern, "0") | privacy_concern %in% c("1", "2", "3") ~ "High",
      privacy_concern %in% c("4", "5", "6") ~ "Medium",
      privacy_concern %in% c("7", "8", "9") | str_starts(privacy_concern, "10") ~ "Low",
      TRUE ~ NA_character_
    ))
  
  # 04: General Trust 
  dataset$general_trust <- dataset |> 
    
    # Select the five trust variables
    select(trust_gov,trust_media,trust_SM_companies,
           trust_military,trust_parties) |> 
    
    # Convert the variables to numeric and missing to NA
    mutate(across(everything(), ~ na_if(as.numeric(as.character(.)), 997))) |> 
    
    # Calculate the mean trust score for each respondent
    rowMeans(na.rm = T)
  
  # Recode the general trust variable into fewer levels
  dataset <- dataset |> 
    mutate(general_trust = case_when(
      general_trust <= 3 ~ "Low",
      general_trust > 3 & general_trust < 7 ~ "Medium",
      general_trust > 7 ~ "High",
      TRUE ~ NA_character_
    ))
  
  # 05: Trust in Social Media Companies
  dataset <- dataset |> 
    mutate(trust_SM_companies = case_when(
      trust_SM_companies %in% c("0","1","2","3") ~ "Low",
      trust_SM_companies %in% c("4","5","6") ~ "Medium",
      trust_SM_companies %in% c("7","8","9","10") ~ "High",
      TRUE ~ NA_character_
    ))
  
  # 06: Twitter Politics Posting
  dataset <- dataset |> 
    mutate(across(c(post_twitter,reply_twitter),~case_when(
      . == "Never" ~ "Never",
      . %in% c("Less often than a few times a year",
                          "Few times a year") ~ "Yearly",
      . %in% c("Few times a month",
                          "Few times a week") ~ "Monthly/Weekly",
      . %in% c("Once a day",
                          "A few times a day",
                          "Many times a day") ~ "Daily",
      TRUE ~ NA_character_
    )))
  
  # 08: Twitter Usage
  dataset$uses_twitter <- ifelse(dataset$uses_twitter == "I have an account currently", 
                                 "Yes", "No")
  
  # 09: Linkage Consent
  dataset$linkage_consent <- ifelse(dataset$linkage_consent == "I consent",
                                    "Yes", "No")

  # Drop additional trust variables
  data_subset <- dataset |> 
    select(-c(trust_gov,trust_media,
              trust_military,trust_parties))
  
  return(data_subset) 
}

#### ---- RELEVEL VARIABLES ---- ####

relevel_vars <- function(dataset) {
  
  # Convert empty strings to NA
  dataset <- dataset |> 
    mutate(across(everything(), ~ na_if(as.character(.), "")))
  
  # Weights
  dataset$weight <- as.numeric(as.character(dataset$weight))
  
  # Age
  dataset$age <- factor(dataset$age, levels = c("18-29","30-44","45-64","65+"))
  
  # Gender
  dataset$gender <- factor(dataset$gender, levels = c("Female","Male"))

  # Education
  dataset$education_level <- factor(dataset$education_level, levels = c("Low","Medium","High"))
  
  # Employment Status
  dataset$employment_status <- factor(dataset$employment_status, levels = c("Not Employed",
                                                                            "Employed"))
  
  # Household Income
  dataset$household_income <- factor(dataset$household_income, levels = c("Low","Medium","High"))
  
  # Political attention
  dataset$political_attention <- factor(dataset$political_attention, levels = c("Low","Medium","High"))

  # Political ideology
  dataset$ideology <- factor(dataset$ideology, levels = c("Liberal",
                                                          "Moderate",
                                                          "Conservative"))
  
  # Privacy concern
  dataset$privacy_concern <- factor(dataset$privacy_concern, levels = c("Low","Medium","High"))
  
  # General trust
  dataset$general_trust <- factor(dataset$general_trust, levels = c("Low","Medium","High"))
  
  # Twitter political posting
  dataset$post_twitter <- factor(dataset$post_twitter, levels = c("Never",
                                                                  "Yearly",
                                                                  "Monthly/Weekly",
                                                                  "Daily"))
  
  # Twitter political replies
  dataset$reply_twitter <- factor(dataset$reply_twitter, levels = c("Never",
                                                                    "Yearly",
                                                                    "Monthly/Weekly",
                                                                    "Daily"))
  
  # Trust in social media companies
  dataset$trust_SM_companies <- factor(dataset$trust_SM_companies, levels = c("Low","Medium","High"))

  # Twitter usage
  dataset$uses_twitter <- factor(dataset$uses_twitter, levels = c("No","Yes"))
  
  # Linkage consent
  dataset$linkage_consent <- factor(dataset$linkage_consent, levels = c("No","Yes"))
  
  return(dataset)
  
}

#### ---- FIT LOGIT MODEL ---- ####

fit_logit <- function(dataset, dep_var, weighted = TRUE, pooled = FALSE, longitudinal = FALSE) {
  
  # Define formula 
  formula <- as.formula(paste(dep_var, "~ age + gender + education_level + employment_status +
                                household_income + political_attention + ideology + privacy_concern +
                                general_trust + trust_SM_companies"))
  
  # When predicting linkage consent, subset by Twitter users only and update formula
  if (dep_var == "linkage_consent") {
    
   dataset <- subset(dataset, dataset$uses_twitter == "Yes")
   
   formula <- update(formula, reformulate(c(".", " + post_twitter + reply_twitter")))
    
  }
  
  # For datasets with repeated participants, add dummy variable
  if (longitudinal == TRUE) {
    
    formula <- update(formula, reformulate(c(".", " + repeat_participant")))
    
  }
  
  # If running a pooled regression, add survey nation and repeat participant to the formula as a fixed effect
  if (pooled == TRUE) {
    
    formula <- update(formula, reformulate(c(".", " + survey_name + repeat_participant")))
    
  }
  
  # If using weights, select weights from the dataset
  if (weighted == TRUE) {
    
    weights <- dataset$weight
    
  } else {
    
    weights <- NULL
    
  }
  
  # Fit logit model
  model <- glm(formula, data = dataset, family = binomial, weights = weights)
  
  # Create tidy model output
  tidy_model <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
  
  return(tidy_model)
  
}
