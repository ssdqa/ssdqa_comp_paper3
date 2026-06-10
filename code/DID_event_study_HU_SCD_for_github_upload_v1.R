######################################################################
# Event Study Analysis for Hydroxyurea Effects in Sickle Cell Disease
#
# This script conducts multiple event study analyses to examine the
# time-varying effect of hydroxyurea (HU) on several outcomes.
#
# Author: Paul George
# Date: 02 Feb 2025
######################################################################

###############################
# 1. Setup Environment
###############################

# Set working directory 

# Load required libraries
library(tidyverse)     # Data manipulation and plotting
library(here)          # File path management
library(did)           # Difference-in-differences (Callaway & Sant'Anna)
library(DRDID)         # Alternative DID methods
library(fixest)        # Fixed effects estimation
library(did2s)         # Alternative DID estimator
library(stringr)       # String manipulation
library(trend)         # Trend tests
library(DIDmultiplegt) # Multiplicity DID estimator

# Clear workspace
rm(list = ls())

###############################
# 2. Load and Inspect Data
###############################

# Load the pre-processed data
load('annual_dataframe.rda')

# Remove any grouping and assign to a new data frame
annual_HU_df <- annual_df_resub %>% ungroup()

# Quick checks on the data
range(annual_HU_df$calendar_year)
n_distinct(annual_HU_df$corp_id)  # Note: later we use corp_id_numeric

###############################
# 3. Data Preparation
###############################

# Create an empty data frame to store DID model outputs
did_output_df <- data.frame(
  model = character(),
  outcome = character(), 
  ATT = numeric(),
  standard_error = numeric(),
  CI_low = numeric(),
  CI_high = numeric(),
  stringsAsFactors = FALSE
)

# Select and prepare variables for analysis
df <- annual_HU_df %>% 
  ungroup() %>% 
  select(
    corp_id_numeric, age_whole_number, first_HU_age, year_on_HU,            # Grouping/time variables
    calendar_year, first_HU_calendar_year, HU_this_year, HU_this_year_80,   # Treatment indicators
    MCV_avg, Hgb_avg, ANC_avg,                                              # Continuous outcome variables
    ED_hosp_days_year_winsor, hosp_days_year_winsor, ED_days_per_year,      # Count outcome variables
    TCD_abnormal_conditional_binary,                                        # Binary outcome variable
    hospitalization_count_per_age_year, 
    baseline_SVI, baseline_insurance, baseline_Hgb_avg,                     # Baseline covariates
    Sex, calendar_year, MCV_baseline1,                                      # Additional covariates
    adherence1, adherence3                                               # Adherence variables
  ) %>% 
  mutate(
    birth_year = calendar_year - age_whole_number,
    birth_year_group = cut(birth_year, breaks = c(-Inf, 2004, 2015, Inf))
  )

# Summary tables to inspect key variables
table(df$age_whole_number, df$birth_year_group)
table(df$HU_this_year_80)
n_distinct(df$corp_id_numeric)
nrow(df)
table(df$first_HU_age)
table(df$age_whole_number)

# Create an example data frame with a subset of variables (if needed)
df_example <- df %>% select(
  corp_id_numeric, age_whole_number, first_HU_age, year_on_HU,
  HU_this_year,
  MCV_avg, Hgb_avg, ANC_avg,
  ED_days_per_year,
  TCD_abnormal_conditional_binary,
  baseline_SVI, baseline_insurance, baseline_Hgb_avg,
  Sex, calendar_year, MCV_baseline1,
  adherence1, adherence3
)

# A vector of key variable names for reference
variable_names <- c(
  "corp_id_numeric", "age_whole_number", "first_HU_age", "year_on_HU",
  "calendar_year", "first_HU_calendar_year",
  "MCV_avg", "Hgb_avg", "ANC_avg",
  "ED_hosp_days_year_winsor", "hosp_days_year_winsor", "ED_days_per_year",
  "TCD_abnormal_conditional_binary",
  "baseline_SVI", "baseline_insurance", "baseline_Hgb_avg",
  "Sex", "birth_year", "calendar_year", "MCV_baseline1",
  "adherence1", "adherence3"
)

###############################
# 4. Outcome Data Visualization
###############################

# Density plot for MCV_avg
ggplot(df, aes(x = MCV_avg)) +
  geom_density(fill = "blue", alpha = 0.5) +  
  labs(title = "Density Plot for MCV",
       x = "MCV",
       y = "Density") +
  theme_minimal()

# Density plot for Hgb_avg
ggplot(df, aes(x = Hgb_avg)) +
  geom_density(fill = "blue", alpha = 0.5) +  
  labs(title = "Density Plot for Hemoglobin",
       x = "Hgb",
       y = "Density") +
  theme_minimal()

# Density plot for ANC_avg
ggplot(df, aes(x = ANC_avg)) +
  geom_density(fill = "blue", alpha = 0.5) +  
  labs(title = "Density Plot for ANC",
       x = "ANC",
       y = "Density") +
  theme_minimal()

# Density plot for Winsorized ED + Hospital Days per Year
ggplot(df, aes(x = ED_hosp_days_year_winsor)) +
  geom_density(fill = "blue", alpha = 0.5) +  
  labs(title = "Density Plot of Winsorized ED + Hospital Days per Year",
       x = "Winsorized ED + Hospital Days per Year",
       y = "Density") +
  theme_minimal()

###############################
# 5. DID Analysis using Callaway & Sant'Anna (CS)
###############################

# Create a modified data frame with treatment variables for CS estimation
df_CS <- df %>% 
  mutate(
    first_HU_age_CS = if_else(is.na(first_HU_age), 0, first_HU_age), 
    first_HU_calendar_year_CS = if_else(is.na(first_HU_age), 0, first_HU_calendar_year)
  )

# Check distribution of treatment assignment
table(df_CS$first_HU_age_CS)
table(df_CS$year_on_HU, useNA = "always")

# Specify covariate formula (if you want to include additional controls)
xformula <- ~ baseline_SVI + baseline_insurance + Sex + birth_year

####-- Analysis for ED_days_per_year Outcome (All Patients) --####

# Run the Callaway & Sant'Anna DID estimator for ED_days_per_year
did1_attgt <- att_gt(
  yname = "ED_days_per_year",   
  tname = "age_whole_number", 
  idname = "corp_id_numeric", 
  gname = "first_HU_age_CS", 
  data = df_CS, 
  # xformla = xformula,  # Uncomment to include covariates
  est_method = "dr", 
  panel = TRUE, 
  allow_unbalanced_panel = TRUE, 
  control_group = "notyettreated"
)

# Summarize the DID estimates
summary(did1_attgt)

# Aggregate estimates with simple and dynamic specifications
agg.simple <- aggte(did1_attgt, type = "simple", na.rm = TRUE)
summary(agg.simple)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)
print(agg.es$overall.att)

# Append results to did_output_df
model_name <- "all patients"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome, 
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

# Plot dynamic event study estimates using built-in plotting function
ggdid(agg.es)

# Extract coefficients for custom plotting
coef.cs <- tidy(agg.es)
coef.cs$period <- ifelse(coef.cs$event.time < 0, "Pre-treatment", "Post-treatment")

# Custom event study plot (ED_days_per_year)
ggplot(coef.cs %>% filter(event.time > -6 & event.time < 16), 
       aes(x = event.time, y = estimate, color = period)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) + 
  labs(title = "Event Study Estimates", 
       subtitle = "All patients",
       x = "Treatment year (0 = hydroxyurea initiation)", 
       y = "Change in ED visits per year", 
       color = "Period") + 
  theme_classic()

###############################
# FIGURE 1 - RESUBMISSION (Commented in original code)
###############################
# [This section appears to be a placeholder. Adjust as needed.]

###############################
# 6. Hospital Count Analysis (Reviewer Request)
###############################

# Check treatment timing variables
table(df$year_on_HU, useNA = "always")

# Prepare data for hospitalization count analysis
df_CS <- df %>% 
  mutate(
    first_HU_age_CS = if_else(is.na(first_HU_age), 0, first_HU_age), 
    first_HU_calendar_year_CS = if_else(is.na(first_HU_age), 0, first_HU_calendar_year)
  )

table(df_CS$first_HU_age_CS)
table(df_CS$year_on_HU, useNA = "always")

# Define covariate formula
xformula <- ~ baseline_SVI + baseline_insurance + Sex + birth_year

# Run DID for hospitalization_count_per_age_year
did1_attgt <- att_gt(
  yname = "hospitalization_count_per_age_year",   
  tname = "age_whole_number", 
  idname = "corp_id_numeric", 
  gname = "first_HU_age_CS", 
  data = df_CS, 
  # xformla = xformula,  # Add covariates if desired
  est_method = "dr", 
  panel = TRUE, 
  allow_unbalanced_panel = TRUE, 
  control_group = "notyettreated"
)

# Summarize and aggregate estimates
summary(did1_attgt)
agg.simple <- aggte(did1_attgt, type = "simple", na.rm = TRUE)
summary(agg.simple)
agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)
print(agg.es$overall.att)

# Append results to did_output_df
model_name <- "all patients"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome, 
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

# Plot the event study for hospitalization counts
ggdid(agg.es)

coef.cs <- tidy(agg.es)
coef.cs$period <- ifelse(coef.cs$event.time < 0, "Pre-treatment", "Post-treatment")
str(coef.cs)

ggplot(coef.cs %>% filter(event.time > -6 & event.time < 16), 
       aes(x = event.time, y = estimate, color = period)) + 
  geom_point() +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  
  geom_hline(yintercept = 0, linetype = "dotted") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) + 
  labs(title = "Event Study Estimates", 
       subtitle = "All patients", 
       x = "Treatment year (0 = hydroxyurea initiation)", 
       y = "Change in number of hospitalizations per year", 
       color = "Period") +  
  theme_classic()

###############################
# 7. Trend Test Using Mann-Kendall & Simple Regression
###############################

# Extract post-treatment coefficients (event.time >= 0)
post_treatment_data <- coef.cs[coef.cs$event.time >= 0,  c("event.time", "estimate")]

# Perform Mann-Kendall trend test
mk_test_result <- mk.test(post_treatment_data$estimate)
print(mk_test_result)

# Simple regression of estimate on event time
fit <- lm(estimate ~ event.time, data = post_treatment_data)
summary(fit)

###############################
# 8. DID Analysis with Covariates and Sample Restrictions
###############################

# Create a version of the dataset with additional covariates:
# Recode baseline_insurance into two groups (Commercial vs. Medicaid/Uninsured)
df_CS <- df %>% 
  mutate(
    first_HU_age_CS = if_else(is.na(first_HU_age), 0, first_HU_age), 
    first_HU_calendar_year_CS = if_else(is.na(first_HU_age), 0, first_HU_calendar_year), 
    baseline_insurance_CS = if_else(baseline_insurance == "Commercial", "Commercial", "Medicaid/Uninsured")
  )

# Filter data for a restricted sample:
# - Either never treated (NA year_on_HU) or treated within a limited window
# - Only consider patients younger than 16 years old
df_CScov <- df_CS %>% 
  filter(is.na(year_on_HU) | (year_on_HU > -4 & year_on_HU < 11)) %>% 
  filter(age_whole_number < 16)

str(df_CScov)
table(df_CScov$age_whole_number, df_CScov$birth_year_group)
table(df_CScov$Sex)

# Define covariate formula (here, baseline_insurance_CS, Sex, and baseline_SVI)
xformula <- ~ baseline_insurance_CS + Sex + baseline_SVI

# Run DID for ED_days_per_year with covariates on the restricted sample
did1_attgt <- att_gt(
  yname = "ED_days_per_year",   
  tname = "age_whole_number", 
  idname = "corp_id_numeric", 
  gname = "first_HU_age_CS", 
  data = df_CScov, 
  xformla = xformula, 
  est_method = "dr", 
  panel = TRUE, 
  allow_unbalanced_panel = TRUE, 
  control_group = "notyettreated"
)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es)
print(agg.es$overall.att)

ggdid(agg.es)

# Append results to did_output_df
model_name <- "all patients + covariates"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome, 
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

# Custom plot for this analysis
ggdid(agg.es)

coef.cs <- tidy(agg.es)
coef.cs$period <- ifelse(coef.cs$event.time < 0, "Pre-treatment", "Post-treatment")
str(coef.cs)

ggplot(coef.cs %>% filter(event.time > -6 & event.time < 16), 
       aes(x = event.time, y = estimate, color = period)) + 
  geom_point() +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  
  geom_hline(yintercept = 0, linetype = "dotted") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) + 
  labs(title = "Event Study Estimates", 
       subtitle = "All patients, with covariates", 
       x = "Treatment year (0 = hydroxyurea initiation)", 
       y = "Change in ED visits per year", 
       color = "Period") +  
  theme_classic()

###############################
# 9. Analysis for Patients Who Start Treatment at 1 Year of Age
###############################

# Subset data: include patients who were treated at age <= 2 
# (Note: never-treated are coded as 0 so they are included as well)
df_CS1 <- df_CS %>% 
  group_by(corp_id_numeric) %>% 
  filter(first_HU_age_CS <= 2) %>%      
  slice_head(n = 10)  # Limit to first 10 observations per group (adjust if needed)

# Run DID for ED_days_per_year on the subsample
did1_attgt <- att_gt(
  yname = "ED_days_per_year",   
  tname = "age_whole_number", 
  idname = "corp_id_numeric", 
  gname = "first_HU_age_CS", 
  data = df_CS1, 
  # xformla = xformula, 
  est_method = "dr", 
  panel = TRUE, 
  allow_unbalanced_panel = TRUE, 
  control_group = "notyettreated"
)

summary(did1_attgt)
agg.simple <- aggte(did1_attgt, type = "simple", na.rm = TRUE)
summary(agg.simple)
agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)
print(agg.es)
summary(agg.es)

# Append results
model_name <- "treatment at 1 year only"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome, 
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

# Plot for this subsample
ggdid(agg.es)

coef.cs <- tidy(agg.es)
coef.cs$period <- ifelse(coef.cs$event.time < 0, "Pre-treatment", "Post-treatment")
str(coef.cs)

ggplot(coef.cs, 
       aes(x = event.time, y = estimate, color = period)) + 
  geom_point() +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  
  geom_hline(yintercept = 0, linetype = "dotted") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) +  
  labs(title = "Event Study Estimates", 
       subtitle = "Patients who started HU at 1 year of age", 
       x = "Treatment year (0 = hydroxyurea initiation)", 
       y = "Change in ED visits per year", 
       color = "Period") +  
  scale_x_continuous(breaks = c(0, 5, 10)) +  
  theme_classic()

###############################
# 10. Analysis for Patients with Good Adherence
###############################

# Check variable structures
str(df_CS$corp_id_numeric)
str(df_CS$year_on_HU)
str(df_CS$adherence1)
str(df_CS$adherence3)
str(df_CS$first_HU_age_CS)  # Note: 0 indicates never treated

# Create a good adherence indicator based on defined rules:
# - Never treated with HU (first_HU_age_CS == 0) are coded as good adherence
# - If adherence3 is "yes" then mark as good adherence
# - If year_on_HU is <= 1 then mark as good adherence
# - Otherwise, mark as 0
df_CS_adherence <- df_CS %>%
  mutate(
    good_adherence = case_when(
      first_HU_age_CS == 0 ~ 1,
      adherence3 == "yes" ~ 1,
      year_on_HU <= 1 ~ 1,
      TRUE ~ 0
    )
  )

# Check distribution of good adherence
table(df_CS_adherence$good_adherence, useNA = "always")
table(df_CS_adherence$good_adherence, df_CS_adherence$year_on_HU, useNA = "always")

# Optionally inspect a subset of variables
checkdf <- df_CS_adherence %>% 
  select(corp_id_numeric, age_whole_number, MCV_avg, MCV_baseline1, 
         first_HU_age_CS, adherence1, year_on_HU, good_adherence)

# Filter for good adherence observations
df_CS_adherence <- df_CS_adherence %>% filter(good_adherence == 1)

# Define a covariate formula
xformula <- ~ baseline_SVI + baseline_insurance + Sex + calendar_year

# Run DID for ED_days_per_year for adherent patients
did1_attgt <- att_gt(
  yname = "ED_days_per_year",   
  tname = "age_whole_number", 
  idname = "corp_id_numeric", 
  gname = "first_HU_age_CS", 
  data = df_CS_adherence, 
  # xformla = xformula, 
  est_method = "dr", 
  panel = TRUE, 
  allow_unbalanced_panel = TRUE, 
  control_group = "notyettreated"
)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)
print(agg.es)

# Append results for adherent sample
model_name <- "adherent_only df"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome, 
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

# Plot for adherent patients
ggdid(agg.es)

coef.cs <- tidy(agg.es)
coef.cs$period <- ifelse(coef.cs$event.time < 0, "Pre-treatment", "Post-treatment")
str(coef.cs)

ggplot(coef.cs %>% filter(event.time > -6 & event.time < 16), 
       aes(x = event.time, y = estimate, color = period)) + 
  geom_point() +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  
  geom_hline(yintercept = 0, linetype = "dotted") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) +  
  labs(title = "Event Study Estimates", 
       subtitle = "Patients with good adherence", 
       x = "Treatment year (0 = hydroxyurea initiation)", 
       y = "Change in ED visits per year", 
       color = "Period") +  
  theme_classic()

###############################
# 11. Analysis for Recently Treated Patients (Calendar Year > 2014)
###############################

table(df_CS$calendar_year)

df_CS_recent <- df_CS %>% 
  filter(calendar_year > 2014)

table(df_CS_recent$calendar_year)

# Define covariate formula (if needed)
xformula <- ~ baseline_SVI + baseline_insurance + Sex + calendar_year

# Run DID for ED_days_per_year on the recent sample
did1_attgt <- att_gt(
  yname = "ED_days_per_year",   
  tname = "age_whole_number", 
  idname = "corp_id_numeric", 
  gname = "first_HU_age_CS", 
  data = df_CS_recent, 
  # xformla = xformula, 
  est_method = "dr", 
  panel = TRUE, 
  allow_unbalanced_panel = TRUE, 
  control_group = "notyettreated"
)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)
print(agg.es)

# Append results for recent sample
model_name <- "recent df"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome, 
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

# Plot for recent sample
ggdid(agg.es)

coef.cs <- tidy(agg.es)
coef.cs$period <- ifelse(coef.cs$event.time < 0, "Pre-treatment", "Post-treatment")
str(coef.cs)

ggplot(coef.cs %>% filter(event.time > -6 & event.time < 16), 
       aes(x = event.time, y = estimate, color = period)) + 
  geom_point() +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  
  geom_hline(yintercept = 0, linetype = "dotted") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) +  
  labs(title = "Event Study Estimates", 
       subtitle = "Sample limited to calendar year 2015-2021", 
       x = "Treatment year (0 = hydroxyurea initiation)", 
       y = "Change in ED visits per year", 
       color = "Period") +  
  theme_classic()

###############################
# 12. Changing Outcome to Winsorized ED + Hospital Days per Year
###############################

# Print variable names for reference
variable_names

####-- Analysis for All Patients (Outcome: ED_hosp_days_year_winsor) --####

did1_attgt <- att_gt(
  yname = "ED_hosp_days_year_winsor",   
  tname = "age_whole_number", 
  idname = "corp_id_numeric", 
  gname = "first_HU_age_CS", 
  data = df_CS, 
  # xformla = xformula, 
  est_method = "dr", 
  panel = TRUE, 
  allow_unbalanced_panel = TRUE, 
  control_group = "notyettreated"
)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)

# Append results
model_name <- "all patients"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome,
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

# Plot the event study for winsorized ED+hospital days
coef.cs <- tidy(agg.es)
coef.cs$period <- ifelse(coef.cs$event.time < 0, "Pre-treatment", "Post-treatment")
str(coef.cs)

ggplot(coef.cs %>% filter(event.time > -6 & event.time < 16),
       aes(x = event.time, y = estimate, color = period)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) +
  labs(title = "Event Study Estimates",
       subtitle = "All patients",
       x = "Treatment year (0 = hydroxyurea initiation)",
       y = "Change in ED + hospital days per year",
       color = "Period") +
  theme_classic()

####-- Analysis for Patients Who Start Treatment at 1 Year (Outcome: ED_hosp_days_year_winsor) --####

did1_attgt <- att_gt(
  yname = "ED_hosp_days_year_winsor",
  tname = "age_whole_number",
  idname = "corp_id_numeric",
  gname = "first_HU_age_CS",
  data = df_CS1,
  # xformla = xformula,
  est_method = "dr",
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "notyettreated"
)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)

# Append results
model_name <- "treatment at 1 year only"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome,
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

# Plot for treatment-at-1-year subgroup
coef.cs <- tidy(agg.es)
coef.cs$period <- ifelse(coef.cs$event.time < 0, "Pre-treatment", "Post-treatment")
str(coef.cs)

ggplot(coef.cs %>% filter(event.time > -6 & event.time < 16),
       aes(x = event.time, y = estimate, color = period)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) +
  labs(title = "Event Study Estimates",
       subtitle = "All patients",
       x = "Treatment year (0 = hydroxyurea initiation)",
       y = "Change in ED + hospital days per year",
       color = "Period") +
  theme_classic()

####-- Analysis for Patients with Good Adherence (Outcome: ED_hosp_days_year_winsor) --####

did1_attgt <- att_gt(
  yname = "ED_hosp_days_year_winsor",
  tname = "age_whole_number",
  idname = "corp_id_numeric",
  gname = "first_HU_age_CS",
  data = df_CS_adherence,
  # xformla = xformula,
  est_method = "dr",
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "notyettreated"
)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)

# Append results for adherent sample
model_name <- "adherent_only df"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome,
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

# Plot for adherent patients (ED_hosp_days_year_winsor)
ggdid(agg.es)

coef.cs <- tidy(agg.es)
coef.cs$period <- ifelse(coef.cs$event.time < 0, "Pre-treatment", "Post-treatment")
str(coef.cs)

ggplot(coef.cs %>% filter(event.time > -6 & event.time < 16),
       aes(x = event.time, y = estimate, color = period)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) +
  labs(title = "Event Study Estimates",
       subtitle = "Patients with good adherence",
       x = "Treatment year (0 = hydroxyurea initiation)",
       y = "Change in ED + hospital days per year",
       color = "Period") +
  theme_classic()

###############################
# 13. Analysis for Outcome: Hgb_avg
###############################

####-- Analysis for All Patients (Outcome: Hgb_avg) --####

did1_attgt <- att_gt(
  yname = "Hgb_avg",
  tname = "age_whole_number",
  idname = "corp_id_numeric",
  gname = "first_HU_age_CS",
  data = df_CS,
  # xformla = xformula,
  est_method = "dr",
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "notyettreated"
)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)

# Append results for Hgb_avg (all patients)
model_name <- "all patients"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome,
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

# Plot for Hgb_avg (all patients)
coef.cs <- tidy(agg.es)
coef.cs$period <- ifelse(coef.cs$event.time < 0, "Pre-treatment", "Post-treatment")
str(coef.cs)

ggplot(coef.cs %>% filter(event.time > -6 & event.time < 14),
       aes(x = event.time, y = estimate, color = period)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) +
  labs(title = "Event Study Estimates",
       subtitle = "All patients",
       x = "Treatment year (0 = hydroxyurea initiation)",
       y = "Change in hemoglobin per year",
       color = "Period") +
  theme_classic()

###############################
# Trend Tests for Hgb_avg
###############################

post_treatment_data <- coef.cs[coef.cs$event.time >= 0, c("event.time", "estimate")]

# Mann-Kendall trend test
mk_test_result <- mk.test(post_treatment_data$estimate)
print(mk_test_result)

# Simple regression analysis for trend
fit <- lm(estimate ~ event.time, data = post_treatment_data)
summary(fit)

####-- Analysis for Patients Who Start Treatment at 1 Year (Outcome: Hgb_avg) --####

did1_attgt <- att_gt(
  yname = "Hgb_avg",
  tname = "age_whole_number",
  idname = "corp_id_numeric",
  gname = "first_HU_age_CS",
  data = df_CS1,
  # xformla = xformula,
  est_method = "dr",
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "notyettreated"
)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)

# Append results for treatment-at-1-year subgroup (Hgb_avg)
model_name <- "treatment at 1 year only"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome,
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

####-- Analysis for Patients with Good Adherence (Outcome: Hgb_avg) --####

did1_attgt <- att_gt(
  yname = "Hgb_avg",
  tname = "age_whole_number",
  idname = "corp_id_numeric",
  gname = "first_HU_age_CS",
  data = df_CS_adherence,
  # xformla = xformula,
  est_method = "dr",
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "notyettreated"
)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)

# Append results for adherent sample (Hgb_avg)
model_name <- "adherent_only df"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome,
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

# Create adherence plot for Hgb_avg
coef.cs <- tidy(agg.es)
coef.cs$period <- ifelse(coef.cs$event.time < 0, "Pre-treatment", "Post-treatment")
str(coef.cs)

hbg_adherence_plot <- ggplot(coef.cs %>% filter(event.time > -6 & event.time < 13),
                             aes(x = event.time, y = estimate, color = period)) + 
  geom_point() +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  
  geom_hline(yintercept = 0, linetype = "dotted") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) +  
  labs(title = "Event Study Estimates", 
       subtitle = "Patients with good adherence", 
       x = "Treatment year (0 = hydroxyurea initiation)", 
       y = "Change in hemoglobin per year", 
       color = "Period") +  
  theme_classic()

# Save the adherence plot in high resolution
save_path <- "C:/Users/georg/OneDrive/HU SCD Event Study/high_res_plots/"
ggsave(
  filename = file.path(save_path, "hbg_adherence_plot.png"),
  plot = hbg_adherence_plot,
  width = 8,
  height = 6,
  dpi = 600
)

###############################
# 14. Analysis for Outcome: MCV_avg
###############################

####-- Analysis for All Patients (Outcome: MCV_avg) --####

did1_attgt <- att_gt(
  yname = "MCV_avg",
  tname = "age_whole_number",
  idname = "corp_id_numeric",
  gname = "first_HU_age_CS",
  data = df_CS,
  # xformla = xformula,
  est_method = "dr",
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "notyettreated"
)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)

# Append results for MCV_avg (all patients)
model_name <- "all patients"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome,
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

# Plot for MCV_avg (all patients)
coef.cs <- tidy(agg.es)
coef.cs$period <- ifelse(coef.cs$event.time < 0, "Pre-treatment", "Post-treatment")
str(coef.cs)

ggplot(coef.cs %>% filter(event.time > -6 & event.time < 12),
       aes(x = event.time, y = estimate, color = period)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) +
  labs(title = "Event Study Estimates",
       subtitle = "All patients",
       x = "Treatment year (0 = hydroxyurea initiation)",
       y = "Change in MCV per year",
       color = "Period") +
  theme_classic()

####-- Analysis for Patients Who Start Treatment at 1 Year (Outcome: MCV_avg) --####

did1_attgt <- att_gt(
  yname = "MCV_avg",
  tname = "age_whole_number",
  idname = "corp_id_numeric",
  gname = "first_HU_age_CS",
  data = df_CS1,
  # xformla = xformula,
  est_method = "dr",
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "notyettreated"
)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)

# Append results for treatment-at-1-year subgroup (MCV_avg)
model_name <- "treatment at 1 year only"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome,
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

####-- Analysis for Patients with Good Adherence (Outcome: MCV_avg) --####

did1_attgt <- att_gt(
  yname = "MCV_avg",
  tname = "age_whole_number",
  idname = "corp_id_numeric",
  gname = "first_HU_age_CS",
  data = df_CS_adherence,
  # xformla = xformula,
  est_method = "dr",
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "notyettreated"
)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)

# Append results for adherent sample (MCV_avg)
model_name <- "adherent_only df"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome,
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

###############################
# 15. Analysis for Outcome: ANC_avg
###############################

####-- Analysis for All Patients (Outcome: ANC_avg) --####

did1_attgt <- att_gt(
  yname = "ANC_avg",
  tname = "age_whole_number",
  idname = "corp_id_numeric",
  gname = "first_HU_age_CS",
  data = df_CS,
  # xformla = xformula,
  est_method = "dr",
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "notyettreated"
)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)

# Append results for ANC_avg (all patients)
model_name <- "all patients"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome,
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

# Plot for ANC_avg (all patients)
coef.cs <- tidy(agg.es)
coef.cs$period <- ifelse(coef.cs$event.time < 0, "Pre-treatment", "Post-treatment")
str(coef.cs)

ggplot(coef.cs %>% filter(event.time > -5 & event.time < 12),
       aes(x = event.time, y = estimate, color = period)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) +
  labs(title = "Event Study Estimates",
       subtitle = "All patients",
       x = "Treatment year (0 = hydroxyurea initiation)",
       y = "Change in ANC per year",
       color = "Period") +
  theme_classic()

####-- Analysis for Patients Who Start Treatment at 1 Year (Outcome: ANC_avg) --####

did1_attgt <- att_gt(
  yname = "ANC_avg",
  tname = "age_whole_number",
  idname = "corp_id_numeric",
  gname = "first_HU_age_CS",
  data = df_CS1,
  # xformla = xformula,
  est_method = "dr",
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "notyettreated"
)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)

# Append results for treatment-at-1-year subgroup (ANC_avg)
model_name <- "treatment at 1 year only"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome,
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

####-- Analysis for Patients with Good Adherence (Outcome: ANC_avg) --####

did1_attgt <- att_gt(
  yname = "ANC_avg",
  tname = "age_whole_number",
  idname = "corp_id_numeric",
  gname = "first_HU_age_CS",
  data = df_CS_adherence,
  # xformla = xformula,
  est_method = "dr",
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "notyettreated"
)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)

# Append results for adherent sample (ANC_avg)
model_name <- "adherent_only df"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error

new_row <- data.frame(
  model = model_name,
  outcome = outcome,
  ATT = ATT,
  standard_error = standard_error,
  CI_low = CI_low,
  CI_high = CI_high
)
did_output_df <- rbind(did_output_df, new_row)

###############################
# 16. Alternative Estimation Approaches
###############################

#### A. Sun & Abraham Method ####

# Prepare data for Sun & Abraham: set never-treated units to a high value
df_SA <- df %>% 
  mutate(first_HU_age_SA = ifelse(is.na(first_HU_age), 200, first_HU_age)) %>%
  filter(year_on_HU > -6 | is.na(year_on_HU))

# Estimate using fixed effects with sunab() function
sa20 <- feols(
  ED_days_per_year ~ age_whole_number + sunab(first_HU_age_SA, age_whole_number) | 
    as.factor(corp_id_numeric) + as.factor(age_whole_number), 
  data = df_SA
)
summary(sa20)
summary(sa20, agg = "ATT")

# Process coefficients for plotting
coef.sa20 <- tidy(sa20) %>%
  mutate(event.time = as.numeric(sub(".*::(\\s*-?\\d+)", "\\1", term)))

# Append a reference row for event time -1
new_row <- tibble(
  term = "age_whole_number::-1",
  estimate = 0,
  std.error = 0,
  statistic = 0,
  p.value = 0,
  event.time = -1
)
coef.sa20 <- bind_rows(coef.sa20, new_row)

coef.sa20$period <- ifelse(coef.sa20$event.time < 0, "Pre-treatment", "Post-treatment")

# Calculate 95% confidence intervals
coef.sa20 <- coef.sa20 %>%
  mutate(conf.low = estimate - qt(0.975, df = length(estimate) - 1) * std.error,
         conf.high = estimate + qt(0.975, df = length(estimate) - 1) * std.error,
         model = "Sun and Abraham")

# Plot Sun & Abraham estimates
ggplot(coef.sa20 %>% filter(event.time > -4 & event.time < 16), 
       aes(x = event.time, y = estimate, color = period)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) + 
  labs(title = "Event Study Estimates",
       subtitle = "Sun and Abraham method", 
       x = "Treatment year (0 = hydroxyurea initiation)", 
       y = "Change Estimate", 
       color = "Period") +  
  theme_classic()

# iplot using fixest (interactive plot)
sa20 |>
  iplot(
    main     = "fixest::sunab",
    xlab     = "Time to treatment",
    ref.line = -1
  )

#### B. Two-Way Fixed Effects (TWFE) ####

# Prepare data for TWFE estimation
df_twfe <- df %>% 
  mutate(
    time_to_treat = if_else(is.na(year_on_HU), -1, year_on_HU),
    HU_ever = if_else(is.na(year_on_HU), 0, 1),
    HU_this_year = if_else(year_on_HU > 0, 1, 0)
  ) %>% 
  mutate(HU_this_year = ifelse(is.na(HU_this_year), 0, HU_this_year))

# TWFE model without event study specification
m.twfe <- feols(ED_days_per_year ~ HU_this_year | corp_id_numeric + age_whole_number, 
                cluster = ~corp_id_numeric, data = df_twfe)
summary(m.twfe)

# TWFE event study model
mod.twfe <- feols(ED_days_per_year ~ i(time_to_treat, HU_ever, ref = -1) | 
                    corp_id_numeric + age_whole_number, 
                  cluster = ~corp_id_numeric, data = df_twfe)
summary(mod.twfe)

# Process TWFE coefficients for plotting
coef.twfe <- tidy(mod.twfe) %>%
  mutate(event.time = as.numeric(str_extract(term, "(?<=::)-?\\d+(?=:)"))) %>%
  mutate(conf.low = estimate - qt(0.975, df = length(estimate) - 1) * std.error,
         conf.high = estimate + qt(0.975, df = length(estimate) - 1) * std.error)

# Append reference row
new_row <- tibble(
  term = "age_whole_number::-1",
  estimate = 0,
  std.error = 0,
  statistic = 0,
  p.value = 0,
  event.time = -1
)
coef.twfe <- bind_rows(coef.twfe, new_row)

coef.twfe$period <- ifelse(coef.twfe$event.time < 0, "Pre-treatment", "Post-treatment")
coef.twfe$model <- "Two way fixed effects"

# Plot TWFE event study estimates
ggplot(coef.twfe %>% filter(event.time > -4 & event.time < 16), 
       aes(x = event.time, y = estimate, color = period)) + 
  geom_point() +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  
  geom_hline(yintercept = 0, linetype = "dotted") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) + 
  labs(title = "Event Study Estimates",
       subtitle = "Two way fixed effects", 
       x = "Treatment year (0 = hydroxyurea initiation)", 
       y = "Change Estimate",
       color = "Period") + 
  theme_classic()

#### C. DID2s (Gardner Approach) ####

# Prepare data for did2s estimation by creating indicator variables
df_did2s <- df %>% 
  mutate(
    HU_this_year_TF = ifelse(HU_this_year == 1, TRUE, FALSE), 
    year_on_HU_2 = ifelse(is.na(year_on_HU), 100, year_on_HU)
  )

table(df$HU_this_year, useNA = "always")
table(df_did2s$HU_this_year_TF)
table(df$year_on_HU, useNA = "always")
table(df_did2s$year_on_HU_2, useNA = "always")

# Estimate using did2s
es_mod <- did2s(
  data = df_did2s, 
  yname = "ED_days_per_year", 
  first_stage = ~ 0 | corp_id_numeric + age_whole_number, 
  second_stage = ~ i(year_on_HU_2, ref = c(0, 100)), 
  treatment = "HU_this_year_TF", 
  cluster_var = "corp_id_numeric"
)

rm(coef.did2s)
coef.did2s <- tidy(es_mod)
head(coef.did2s)

# Create a reference row (for term 'year_on_HU_2::0')
new_row <- tibble(
  term = "year_on_HU_2::0",  
  estimate = 0,  
  std.error = 0.00001,
  statistic = NA,  
  p.value = NA
)
coef.did2s <- bind_rows(coef.did2s, new_row)

# Extract numeric event time from term and calculate CIs
coef.did2s <- coef.did2s %>%
  mutate(event.time = as.numeric(str_extract(term, "-?\\d+$")))  %>%
  mutate(conf.low = estimate - 1.96 * std.error,
         conf.high = estimate + 1.96 * std.error) %>% 
  mutate(period = ifelse(event.time < 0, "Pre-treatment", "Post-treatment"))

coef.did2s$model <- "Gardner"

# Plot Gardner estimates
ggplot(coef.did2s %>% filter(event.time > -4 & event.time < 16), 
       aes(x = event.time, y = estimate, color = period)) + 
  geom_point() +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  
  geom_hline(yintercept = 0, linetype = "dotted") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
  scale_color_manual(values = c("Pre-treatment" = "darkred", "Post-treatment" = "darkblue")) + 
  labs(title = "Event Study Estimates",
       subtitle = "Gardner", 
       x = "Treatment year (0 = hydroxyurea initiation)", 
       y = "Change Estimate",
       color = "Period") + 
  theme_classic()

###############################
# 17. DID Multiplegt (Optional)
###############################

df_multi <- df %>% 
  mutate(treat = if_else(HU_this_year == 1, TRUE, FALSE))
table(df_multi$treat)

# The following is an example call for did_multiplegt. This function outputs a plot
# but does not directly return coefficient estimates for saving.
# Uncomment and adjust as needed:
# A <- did_multiplegt(
#  df_multi, 
#  Y = "ED_days_per_year", 
#  G = "corp_id_numeric", 
#  T = "age_whole_number", 
#  D = "treat", 
#  dynamic = 10, 
#  placebo = 4, 
#  brep = 4, 
#  cluster = "corp_id_numeric"
# )

###############################
# 18. Combine Models for Robustness Comparison
###############################

# Combine coefficients from different models (Sun & Abraham, Gardner, TWFE, and CS)
# Note: Ensure that coef.cs1 is defined; here, we assume it is a copy of the CS estimates.
coef.cs1 <- coef.cs %>% select(-type)  # Remove column 'type' if it exists

combined_df <- bind_rows(coef.sa20, coef.did2s, coef.twfe, coef.cs1)

# Adjust event time slightly for each model to prevent overlap in the plot
combined_df <- combined_df %>%
  mutate(event.time.adjusted = case_when(
    model == "Sun and Abraham" ~ event.time + 0.1,
    model == "Gardner" ~ event.time,        
    model == "Two way fixed effects" ~ event.time - 0.1,  
    model == "Callaway and Sant`Anna" ~ event.time + 0.2,
    TRUE ~ event.time
  ))

# Create a combined plot for the various estimators
ggplot(combined_df %>% filter(event.time > -4 & event.time < 13), 
       aes(x = event.time.adjusted, y = estimate, color = model)) + 
  geom_point() +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 1) +  
  geom_hline(yintercept = 0, linetype = "dotted") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  
  scale_color_manual(values = c("Sun and Abraham" = "darkgreen", 
                                "Gardner" = "orange", 
                                "Two way fixed effects" = "darkblue", 
                                "Callaway and Sant`Anna" = "black")) +  
  labs(title = "Event Study Estimates",
       subtitle = "Comparison of Different Estimators", 
       x = "Treatment year (0 = hydroxyurea initiation)",
       y = "Change Estimate",
       color = "Model") + 
  theme_classic() + 
  coord_cartesian(ylim = c(-2, 1))
