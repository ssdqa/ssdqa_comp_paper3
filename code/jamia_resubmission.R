
## HU assignments
cohort_hu_dfe <- results_tbl('did_primary_vars') %>%
  group_by(site, person_id, first_sca_dx, last_visit_date, end_date) %>%
  summarise(hu_treated = ifelse(any(treated_50 == 1), TRUE, FALSE))

cohort_hu_asis <- trino_session_nodq$results_tbl('did_primary_vars_nodq') %>%
  group_by(site, person_id, first_sca_dx, last_visit_date, end_date) %>%
  summarise(hu_treated = ifelse(any(treated_50 == 1), TRUE, FALSE))

# demographics: FU, Year of CE (pre-post 2017), Age at CE, Sex & Race/ethnicity (PEW style combo)
demos_dfe <- trino_session$cdm_tbl('person') %>%
  inner_join(cohort_hu_dfe) %>%
  # group_by(site, person_id) %>%
  mutate(fu = date_diff('day', first_sca_dx, end_date),
         fu = fu / 365.25,
         age_at_ce = date_diff('day', birth_date, first_sca_dx),
         age_at_ce = age_at_ce / 365.25,
         year_ce = ifelse(first_sca_dx >= as.Date('2017-01-01'), 'Post 2017', 'Pre 2017'),
         sex = ifelse(gender_concept_id %in% c(44814660, 44814650,
                                               44814653, 44814649), 'Other/Unknown', gender_concept_name),
         race_eth = case_when(ethnicity_concept_id == 38003563 ~ 'Hispanic or Latino',
                              race_concept_id %in% c(44814660, 44814650,
                                                     44814653, 44814649) ~ 'Other/Unknown',
                              TRUE ~ race_concept_name)) %>%
  distinct(site, person_id, hu_treated, fu, age_at_ce, year_ce, sex, race_eth) %>%
  filter(!is.na(fu)) %>% collect()
  
demos_asis <- trino_session_nodq$cdm_tbl('person') %>%
  inner_join(cohort_hu_asis) %>%
  # group_by(site, person_id) %>%
  mutate(fu = date_diff('day', first_sca_dx, end_date),
         fu = fu / 365.25,
         age_at_ce = date_diff('day', birth_date, first_sca_dx),
         age_at_ce = age_at_ce / 365.25,
         year_ce = ifelse(first_sca_dx >= as.Date('2017-01-01'), 'Post 2017', 'Pre 2017'),
         sex = ifelse(gender_concept_id %in% c(44814660, 44814650,
                                               44814653, 44814649), 'Other/Unknown', gender_concept_name),
         race_eth = case_when(ethnicity_concept_id == 38003563 ~ 'Hispanic or Latino',
                              race_concept_id %in% c(44814660, 44814650,
                                                     44814653, 44814649) ~ 'Other/Unknown',
                              TRUE ~ race_concept_name)) %>%
  distinct(site, person_id, hu_treated, fu, age_at_ce, year_ce, sex, race_eth) %>%
  filter(!is.na(fu)) %>% collect()
  
# Age at first HU exposure
hu_ever_dfe <- trino_session$cdm_tbl('drug_exposure') %>%
  inner_join(cohort_hu_dfe) %>%
  filter(drug_exposure_start_date >= first_sca_dx, 
         drug_exposure_start_date <= end_date) %>%
  inner_join(load_codeset("rx_hydroxyurea"), by = c('drug_concept_id' = 'concept_id')) %>%
  inner_join(trino_session$cdm_tbl('person') %>% select(person_id, birth_date)) %>%
  group_by(site, person_id, birth_date, hu_treated) %>%
  filter(drug_exposure_start_date == min(drug_exposure_start_date)) %>%
  mutate(age_at_first_hu_exposure_ever = date_diff('day', birth_date, drug_exposure_start_date),
         age_at_first_hu_exposure_ever = age_at_first_hu_exposure_ever / 365.25) %>%
  distinct(site, person_id, birth_date, hu_treated, age_at_first_hu_exposure_ever) %>% collect()

hu_ever_asis <- trino_session_nodq$cdm_tbl('drug_exposure') %>%
  inner_join(cohort_hu_asis) %>%
  filter(drug_exposure_start_date >= first_sca_dx, 
         drug_exposure_start_date <= end_date) %>%
  inner_join(trino_session_nodq$load_codeset("rx_hydroxyurea"), by = c('drug_concept_id' = 'concept_id')) %>%
  inner_join(trino_session_nodq$cdm_tbl('person') %>% select(person_id, birth_date)) %>%
  group_by(site, person_id, birth_date, hu_treated) %>%
  filter(drug_exposure_start_date == min(drug_exposure_start_date)) %>%
  mutate(age_at_first_hu_exposure_ever = date_diff('day', birth_date, drug_exposure_start_date),
         age_at_first_hu_exposure_ever = age_at_first_hu_exposure_ever / 365.25) %>%
  distinct(site, person_id, birth_date, hu_treated, age_at_first_hu_exposure_ever) %>% collect()

# Age-year at HU 50% criteria alignment
hu_fiftypct_dfe <- trino_session$results_tbl('did_primary_vars') %>%
  filter(treated_50 == 1) %>%
  group_by(site, person_id) %>%
  mutate(age_year = age_year + 1) %>%
  filter(age_year == min(age_year)) %>%
  distinct(site, person_id, age_year) %>%
  rename('age_year_meet_hu_crit' = age_year) %>% collect()

hu_fiftypct_asis <- trino_session_nodq$results_tbl('did_primary_vars_nodq') %>%
  filter(treated_50 == 1) %>%
  group_by(site, person_id) %>%
  mutate(age_year = age_year + 1) %>%
  filter(age_year == min(age_year)) %>%
  distinct(site, person_id, age_year) %>%
  rename('age_year_meet_hu_crit' = age_year) %>% collect()

# Duration of HU use
hu_duration_dfe <- trino_session$results_tbl('did_primary_vars') %>%
  filter(treated_50 == 1) %>%
  group_by(site, person_id) %>%
  summarise(n_age_year_hu_crit = n_distinct(age_year)) %>% collect()

hu_duration_asis <- trino_session_nodq$results_tbl('did_primary_vars_nodq') %>%
  filter(treated_50 == 1) %>%
  group_by(site, person_id) %>%
  summarise(n_age_year_hu_crit = n_distinct(age_year)) %>% collect()

# SCD subtype (SS / SB0)

sca_labelled <- read_codeset('dx_sca') %>%
  mutate(cluster = case_when(grepl('hemoglobin ss', tolower(concept_name)) ~ 'HbSS',
                             grepl('beta', tolower(concept_name)) ~ 'HbSB0',
                             TRUE ~ 'SCA Unspecified'))
readr::write_csv(sca_labelled, 'specs/dx_sca_label.csv')

sca_subtype_dfe <- trino_session$cdm_tbl('condition_occurrence') %>%
  inner_join(cohort_hu_dfe, by = c('site', 'person_id', 'condition_start_date' = 'first_sca_dx')) %>%
  inner_join(load_codeset("dx_sca_label"), by = c("condition_concept_id" = 'concept_id')) %>%
  collect() %>%
  #filter(person_id == 12119946) %>% select(condition_concept_name, condition_source_concept_name, condition_source_value, cluster)
  group_by(site, person_id, hu_treated) %>%
  mutate(sca_type = case_when(any(cluster == 'HbSB0') ~ 'HbSB0',
                              any(cluster == 'HbSS') ~ 'HbSS',
                              TRUE ~ cluster)) %>%
  distinct(site, person_id, hu_treated, sca_type)

sca_subtype_asis <- trino_session_nodq$cdm_tbl('condition_occurrence') %>%
  inner_join(cohort_hu_asis, by = c('site', 'person_id', 'condition_start_date' = 'first_sca_dx')) %>%
  inner_join(trino_session_nodq$load_codeset("dx_sca_label"), by = c("condition_concept_id" = 'concept_id')) %>%
  collect() %>%
  #filter(person_id == 12119946) %>% select(condition_concept_name, condition_source_concept_name, condition_source_value, cluster)
  group_by(site, person_id, hu_treated) %>%
  mutate(sca_type = case_when(any(cluster == 'HbSB0') ~ 'HbSB0',
                              any(cluster == 'HbSS') ~ 'HbSS',
                              TRUE ~ cluster)) %>%
  distinct(site, person_id, hu_treated, sca_type)

## DFE table stats
library(gtsummary)

table_one_dfe <- demos_dfe %>%
  left_join(hu_ever_dfe) %>%
  left_join(hu_fiftypct_dfe) %>%
  left_join(hu_duration_dfe) %>%
  left_join(sca_subtype_dfe)

tbl_summary(table_one_dfe %>% select(-c(person_id, birth_date, site)) %>%
              mutate(fu = as.numeric(fu),
                     age_at_ce = as.numeric(age_at_ce),
                     age_at_first_hu_exposure_ever = as.numeric(age_at_first_hu_exposure_ever),
                     age_year_meet_hu_crit = as.numeric(age_year_meet_hu_crit),
                     n_age_year_hu_crit = as.numeric(n_age_year_hu_crit)),
            statistic = list(all_continuous() ~ "{mean} ({median})", 
                             all_categorical() ~ "{n} ({p}%)"))

table_one_asis <- demos_asis %>%
  left_join(hu_ever_asis) %>%
  left_join(hu_fiftypct_asis) %>%
  left_join(hu_duration_asis) %>%
  left_join(sca_subtype_asis)

tbl_summary(table_one_asis %>% select(-c(person_id, birth_date, site)) %>%
              mutate(fu = as.numeric(fu),
                     age_at_ce = as.numeric(age_at_ce),
                     age_at_first_hu_exposure_ever = as.numeric(age_at_first_hu_exposure_ever),
                     age_year_meet_hu_crit = as.numeric(age_year_meet_hu_crit),
                     n_age_year_hu_crit = as.numeric(n_age_year_hu_crit)),
            statistic = list(all_continuous() ~ "{mean} ({median})", 
                             all_categorical() ~ "{n} ({p}%)"))
  