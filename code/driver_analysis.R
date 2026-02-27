
## things i need to compute

treated_props <- results_tbl('hu_visits_labelled') %>%
  filter(hu_label != 'censored',
         visit_concept_id %in% c(9201, 9202, 581399, 9203, 2000000048,
                                 2000000088)) %>%
  left_join(cdm_tbl('cdm_person') %>% select(person_id, birth_date)) %>%
  mutate(visit_age = date_diff('day', birth_date, visit_start_date),
         age_year = floor(visit_age / 365.25),
         age_year = as.numeric(age_year)) %>%
  group_by(site, person_id, age_year, hu_label) %>%
  summarise(nvis = n_distinct(visit_occurrence_id)) %>%
  group_by(site, person_id, age_year) %>%
  mutate(totvis = sum(nvis),
         propvis = as.numeric(nvis) / as.numeric(totvis))

### hu treatement labels
did_treatment_labels <- treated_props %>%
  mutate(treated_50 = ifelse(hu_label == 'hydroxyurea' & propvis >= 0.5, 1, 0),
         treated_80 = ifelse(hu_label == 'hydroxyurea' & propvis >= 0.8, 1, 0)) %>%
  group_by(site, person_id, age_year) %>%
  summarise(treated_50 = as.numeric(sum(treated_50)),
            treated_80 = as.numeric(sum(treated_80)))

### baseline values (from date of initial HU or a non-sick visit in 1 year lookback)
first_hu <- cdm_tbl('de_hydroxyurea_windows') %>%
  inner_join(did_treatment_labels) %>%
  group_by(site, person_id) %>%
  filter(drug_exposure_start_date == min(drug_exposure_start_date)) %>%
  left_join(cdm_tbl('person') %>% select(site, person_id, birth_date)) %>%
  mutate(drug_age = date_diff('day', birth_date, drug_exposure_start_date),
         age_year_first_HU = floor(drug_age / 365.25),
         age_year_first_HU = as.numeric(age_year_first_HU),
         calendar_year_first_HU = year(drug_exposure_start_date),
         first_HU_date = drug_exposure_start_date) %>%
  distinct(site, person_id, first_HU_date, age_year_first_HU, calendar_year_first_HU) %>%
  compute_new('first_hu_dat')

#### MCV
bl_mcv <- cdm_tbl('measurement_labs') %>%
  inner_join(load_codeset('lab_mcv'), 
             by = c('measurement_concept_id' = 'concept_id')) %>%
  inner_join(first_hu) %>%
  left_join(cdm_tbl('visit_occurrence') %>% select(visit_occurrence_id, visit_concept_id)) %>%
  mutate(yr_lookback = date_add('year', -1L, as.Date(first_HU_date))) %>%
  filter(measurement_date == first_HU_date | 
           (measurement_date >= yr_lookback & measurement_date <= first_HU_date)) %>%
  select(site, person_id, visit_concept_id, measurement_date, yr_lookback, first_HU_date, 
         value_as_number, unit_concept_id) %>%
  filter(!visit_concept_id %in% c(9201, 9203, 2000000048, 2000000088)) %>%
  filter(!is.na(value_as_number)) %>%
  group_by(site, person_id) %>%
  filter(measurement_date == min(measurement_date)) %>%
  summarise(value_as_number = mean(value_as_number)) %>%
  rename('baseline_mcv' = value_as_number)

##### ANC
#' Anything >= 30 we will assume is reported in a unit of N, and anything < 30 
#' we will assume is reported in thousands which we will then multiply by 1,000 to convert to N
#' ^^ this is what we did for COSMOS

bl_anc <- cdm_tbl('measurement_labs') %>%
  inner_join(load_codeset('lab_anc'), 
             by = c('measurement_concept_id' = 'concept_id')) %>%
  inner_join(first_hu) %>%
  left_join(cdm_tbl('visit_occurrence') %>% select(visit_occurrence_id, visit_concept_id)) %>%
  mutate(yr_lookback = date_add('year', -1L, as.Date(first_HU_date))) %>%
  filter(measurement_date == first_HU_date | 
           (measurement_date >= yr_lookback & measurement_date <= first_HU_date)) %>%
  select(site, person_id, visit_concept_id, measurement_date, yr_lookback, first_HU_date, 
         value_as_number, unit_concept_id) %>%
  filter(!visit_concept_id %in% c(9201, 9203, 2000000048, 2000000088)) %>%
  filter(!is.na(value_as_number)) %>%
  group_by(site, person_id) %>%
  filter(measurement_date == min(measurement_date)) %>%
  summarise(value_as_number = mean(value_as_number)) %>%
  mutate(value_as_number = ifelse(value_as_number < 30, value_as_number * 1000, value_as_number)) %>%
  rename('baseline_anc' = value_as_number)

##### HGB
bl_hgb <- cdm_tbl('measurement_labs') %>%
  inner_join(load_codeset('lab_serum_hemoglobin'), 
             by = c('measurement_concept_id' = 'concept_id')) %>%
  inner_join(first_hu) %>%
  left_join(cdm_tbl('visit_occurrence') %>% select(visit_occurrence_id, visit_concept_id)) %>%
  mutate(yr_lookback = date_add('year', -1L, as.Date(first_HU_date))) %>%
  filter(measurement_date == first_HU_date | 
           (measurement_date >= yr_lookback & measurement_date <= first_HU_date)) %>%
  select(site, person_id, visit_concept_id, measurement_date, yr_lookback, first_HU_date, 
         value_as_number, unit_concept_id) %>%
  filter(!visit_concept_id %in% c(9201, 9203, 2000000048, 2000000088)) %>%
  filter(!is.na(value_as_number)) %>%
  group_by(site, person_id) %>%
  filter(measurement_date == min(measurement_date)) %>%
  summarise(value_as_number = mean(value_as_number)) %>%
  rename('baseline_hgb' = value_as_number)

hu_w_baseline_labs <- first_hu %>%
  left_join(bl_hgb) %>%
  left_join(bl_anc) %>% ## what do we do about NAs?
  left_join(bl_mcv) %>%
  compute_new('bl_labs')

### ED days per year
ed_per_ageyear <- cdm_tbl('visit_occurrence') %>%
  inner_join(results_tbl('final_cohort_censored')) %>%
  filter(visit_start_date >= first_sca_dx,
         visit_start_date <= end_date) %>%
  filter(visit_concept_id == 9203) %>%
  mutate(ed_days = date_diff('day', visit_start_date, visit_end_date),
         ed_days = ed_days + 1L) %>%
  select(site, person_id, visit_start_date, visit_end_date, ed_days) %>%
  left_join(cdm_tbl('person') %>% select(person_id, birth_date)) %>%
  mutate(visit_age = date_diff('day', birth_date, visit_start_date),
         age_year = floor(visit_age / 365.25),
         age_year = as.numeric(age_year)) %>%
  group_by(site, person_id, age_year) %>%
  summarise(ed_days_ageyear = sum(ed_days))


### hospitalization days per year
hosp_per_ageyear <- cdm_tbl('visit_occurrence') %>%
  inner_join(results_tbl('final_cohort_censored')) %>%
  filter(visit_start_date >= first_sca_dx,
         visit_start_date <= end_date) %>%
  filter(visit_concept_id %in% c(9201, 2000000088)) %>%
  mutate(hosp_days = date_diff('day', visit_start_date, visit_end_date),
         hosp_days = hosp_days + 1L) %>%
  select(site, person_id, visit_start_date, visit_end_date, hosp_days) %>%
  left_join(cdm_tbl('person') %>% select(person_id, birth_date)) %>%
  mutate(visit_age = date_diff('day', birth_date, visit_start_date),
         age_year = floor(visit_age / 365.25),
         age_year = as.numeric(age_year)) %>%
  group_by(site, person_id, age_year) %>%
  summarise(hosp_days_ageyear = sum(hosp_days))

### average MCV (exclude IP & ED)
### average hemoglobin (exclude IP & ED)
### average ANC (exclude IP & ED)
