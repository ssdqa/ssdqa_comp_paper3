# library(did)
# data("mpdta")
# 
# out <- att_gt(
#   yname = 'lemp',
#   gname = 'first.treat',
#   idname = 'countyreal',
#   tname = 'year',
#   xformla = ~1,
#   data = mpdta,
#   est_method = 'reg'
# )
# 
# summary(out)
# ggdid(out, ylim=c(-.25, .1))
# 
# es <- aggte(out, type = 'dynamic')
# summary(es)
# 
# group_effects <- aggte(out, type = 'group')
# summary(group_effects)
# 
# # set seed so everything is reproducible
# set.seed(1814)
# 
# # generate dataset with 4 time periods
# sp <- reset.sim()
# sp$te <- 0
# time.periods <- 4
# 
# # add dynamic effects
# sp$te.e <- 1:time.periods
# 
# # generate data set with these parameters
# # here, we dropped all units who are treated in time period 1 as they do not help us recover ATT(g,t)'s.
# dta <- build_sim_dataset(sp)
# 
# # estimate group-time average treatment effects using att_gt method
# example_attgt <- att_gt(yname = "Y",
#                         tname = "period",
#                         idname = "id",
#                         gname = "G",
#                         xformla = ~X,
#                         data = dta
# )
# 
# summary(example_attgt)
# 
# 
# agg.simple <- aggte(example_attgt, type = 'simple')
# summary(agg.simple)
# # dynamic
# agg.es <- aggte(example_attgt, type = 'dynamic')
# summary(agg.es)
# #group
# agg.gs <- aggte(example_attgt, type = 'group')
# summary(agg.gs)




#source('setup/setup.R')

library(did)
library(data.table)

df_CS <- results_tbl('did_primary_vars') %>% 
  collect() %>%
  group_by(site, person_id) %>%
  mutate(age_year_first_hu = case_when(any(treated_50 == 1) ~ age_year_first_hu,
                                       TRUE ~ NA_integer_)) %>%
  ungroup() %>%
  mutate(
    first_HU_age_CS = if_else(is.na(age_year_first_hu), 0, age_year_first_hu + 1), 
    first_HU_calendar_year_CS = if_else(is.na(age_year_first_hu), 0, calendar_year_first_hu)
  )

df_CS <- df_CS %>%
  mutate(ed_days_ageyear = ifelse(is.na(ed_days_ageyear), 0, ed_days_ageyear)) %>%
  select(person_id, age_year, first_HU_age_CS, first_HU_calendar_year_CS, ed_days_ageyear) %>%
  filter(age_year >= 0)

#df_CS_dt <- as.data.table(df_CS)

did1_attgt <- att_gt(
  yname = "ed_days_ageyear",   
  tname = "age_year", 
  idname = "person_id", 
  gname = "first_HU_age_CS", 
  data = df_CS, 
  # xformla = xformula,  # Uncomment to include covariates
  est_method = "reg", 
  faster_mode = FALSE,
  panel = TRUE, 
  allow_unbalanced_panel = TRUE, 
  control_group = "notyettreated"
)

summary(did1_attgt)

agg.simple <- aggte(did1_attgt, type = "simple", na.rm = TRUE)
summary(agg.simple)

agg.es <- aggte(did1_attgt, type = "dynamic", na.rm = TRUE)
print(agg.es$overall.att)

model_name <- "all patients"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error


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





df_CS <- results_tbl('did_primary_vars') %>% 
  collect() %>%
  group_by(site, person_id) %>%
  mutate(age_year_first_hu = case_when(any(treated_50 == 1) ~ age_year_first_hu,
                                       TRUE ~ NA_integer_)) %>%
  ungroup() %>%
  mutate(
    first_HU_age_CS = if_else(is.na(age_year_first_hu), 0, age_year_first_hu + 1), 
    first_HU_calendar_year_CS = if_else(is.na(age_year_first_hu), 0, calendar_year_first_hu)
  )

df_CS <- df_CS %>%
  mutate(hosp_days_ageyear = ifelse(is.na(hosp_days_ageyear), 0, hosp_days_ageyear)) %>%
  select(person_id, age_year, first_HU_age_CS, first_HU_calendar_year_CS, hosp_days_ageyear) %>%
  filter(age_year >= 0, first_HU_age_CS <= 2)


did2_attgt <- att_gt(
  yname = "hosp_days_ageyear",   
  tname = "age_year", 
  idname = "person_id", 
  gname = "first_HU_age_CS", 
  data = df_CS, 
  # xformla = xformula,  # Uncomment to include covariates
  est_method = "reg", 
  faster_mode = FALSE,
  panel = TRUE, 
  allow_unbalanced_panel = TRUE, 
  control_group = "notyettreated"
)

summary(did2_attgt)

agg.simple <- aggte(did2_attgt, type = "simple", na.rm = TRUE)
summary(agg.simple)

agg.es <- aggte(did2_attgt, type = "dynamic", na.rm = TRUE)
print(agg.es$overall.att)

model_name <- "all patients"
outcome <- agg.es$DIDparams$yname
ATT <- agg.es$overall.att
standard_error <- agg.es$overall.se
CI_low <- ATT - qt(0.975, df = Inf) * standard_error
CI_high <- ATT + qt(0.975, df = Inf) * standard_error


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
       y = "Change in Hospital days per year", 
       color = "Period") + 
  theme_classic()

