
set_argos_default(trino_session_nodq)

### censor og cohort
## apply censorship criteria ##

# start & last visit dates (default end)
cht_start <- results_tbl('step3_cohort') %>%
  inner_join(results_tbl('sca_attrition_cohort')) %>%
  select(site, person_id, first_sca_dx)

last_visit <- trino_session_nodq$cdm_tbl('visit_occurrence') %>%
  inner_join(cht_start) %>%
  filter(visit_start_date >= as.Date('2011-01-01') &
           visit_start_date <= as.Date('2024-12-31')) %>%
  group_by(site, person_id) %>%
  filter(visit_start_date == max(visit_start_date)) %>%
  rename('last_visit_date' = visit_start_date) %>%
  distinct(site, person_id, last_visit_date) 

# bone marrow transplant or gene therapy
bone_gene <- trino_session_nodq$cdm_tbl('procedure_occurrence') %>%
  inner_join(load_codeset('px_bonemarrow_stemcell'), 
             by = c('procedure_concept_id' = 'concept_id')) %>%
  inner_join(cht_start) %>%
  filter(procedure_date >= as.Date(first_sca_dx), 
         procedure_date <= as.Date('2024-12-31')) %>%
  collect() %>%
  group_by(site, person_id) %>%
  filter(procedure_date == min(procedure_date)) %>%
  reframe(bone_gene_date = procedure_date) %>%
  distinct() %>% ungroup()

# other drugs (ie voxelotor & crizanlizuma)
expand_drugs <- get_descendants(codeset = load_codeset('rx_altdrug_ing'),
                                table_name = 'altdrugs')

other_drugs <- trino_session_nodq$cdm_tbl('drug_exposure') %>%
  inner_join(expand_drugs, by = c('drug_concept_id' = 'concept_id')) %>%
  inner_join(cht_start) %>%
  filter(drug_exposure_start_date >= as.Date(first_sca_dx), 
         drug_exposure_start_date <= as.Date('2024-12-31')) %>%
  group_by(site, person_id) %>%
  filter(drug_exposure_start_date == min(drug_exposure_start_date)) %>%
  collect() %>%
  reframe(alt_drug_initiation = drug_exposure_start_date) %>%
  distinct()

# long term transfusion (2 transfusions w/n 3+ weeks & < 6 weeks)
transfuse_3_6_week <- trino_session_nodq$cdm_tbl('procedure_occurrence') %>%
  inner_join(cht_start) %>%
  inner_join(load_codeset('px_transfusion'), by = c('procedure_concept_id' = 'concept_id')) %>%
  filter(procedure_date >= as.Date(first_sca_dx), procedure_date <= as.Date('2024-12-31')) %>%
  filter(procedure_type_concept_id != 44786631) %>%
  arrange(site, person_id, procedure_date) %>%
  group_by(site, person_id) %>%
  mutate(proc_diff = date_diff('day', lag(procedure_date), procedure_date),
         in_window = ifelse(proc_diff >= 21 & proc_diff < 42, '3-6 weeks', 'outside')) %>% 
  collect()

transfuse_cht <- transfuse_3_6_week %>%
  group_by(site, person_id, in_window) %>% 
  summarise(ct = n()) %>% 
  filter(in_window == '3-6 weeks') %>% 
  ungroup() %>% 
  distinct(site, person_id)

transfusion_dates <- transfuse_3_6_week %>%
  inner_join(transfuse_cht) %>%
  filter(in_window == '3-6 weeks') %>%
  group_by(site, person_id) %>%
  filter(procedure_date == min(procedure_date)) %>%
  summarise(transfusion_initiation = procedure_date - proc_diff) %>%
  ungroup()


####### 2 year visit gap ############
biggap <- trino_session_nodq$cdm_tbl('visit_occurrence') %>%
  inner_join(cht_start) %>%
  filter(visit_start_date >= first_sca_dx &
           visit_start_date <= as.Date('2024-12-31')) %>%
  arrange(site, person_id, visit_start_date) %>%
  group_by(site, person_id) %>%
  mutate(visit_gap = date_diff('day', lag(visit_start_date), visit_end_date)) %>%
  filter(visit_gap > 730.5) %>% 
  collect() %>%
  group_by(site, person_id) %>%
  filter(visit_start_date == min(visit_start_date)) %>%
  mutate(continuous_care_stop = visit_start_date - visit_gap) %>%
  distinct(site, person_id, continuous_care_stop) %>% ungroup()


####### Censorship Dates ############

censored_cohort <- cht_start %>%
  collect() %>%
  left_join(last_visit %>% collect()) %>%
  left_join(transfusion_dates) %>%
  left_join(other_drugs) %>%
  left_join(bone_gene) %>%
  left_join(biggap) %>%
  group_by(site, person_id) %>%
  mutate(end_date = min(transfusion_initiation, alt_drug_initiation, bone_gene_date, last_visit_date,
                        continuous_care_stop,
                        na.rm = TRUE),
         censorship_reason = case_when(end_date == transfusion_initiation ~ 'Transfusion',
                                       end_date == alt_drug_initiation ~ 'Alternate Drug Therapy',
                                       end_date == bone_gene_date ~ 'Bone Marrow Transplant or Gene Therapy',
                                       end_date == continuous_care_stop ~ '> 2 Year Gap Between Encounters',
                                       TRUE ~ 'None / Last Visit'))

output_tbl(censored_cohort, 'nodq_cohort_censored')

#### Compute hydroxyurea windows

hu_rx_tbl <- trino_session_nodq$cdm_tbl("drug_exposure") %>%
  inner_join(load_codeset("rx_hydroxyurea"), by = c('drug_concept_id' = 'concept_id')) %>%
  inner_join(results_tbl('nodq_cohort_censored')) %>%
  filter(drug_type_concept_id %in% c(38000177)) %>%
  select(site, person_id, first_sca_dx, end_date, censorship_reason, visit_occurrence_id, 
         drug_exposure_start_date, drug_exposure_end_date,
         drug_concept_id, drug_type_concept_id, drug_concept_name, refills, 
         days_supply, quantity, frequency) %>%
  arrange(site, person_id, drug_exposure_start_date) %>%
  group_by(site, person_id) %>%
  mutate(drug_exposure_end_date = ifelse(is.na(drug_exposure_end_date),
                                         lead(drug_exposure_start_date),
                                         drug_exposure_end_date)) %>%
  mutate(length_rx = date_diff('day', drug_exposure_start_date, drug_exposure_end_date),
         time_bw_rx = date_diff('day', lag(drug_exposure_end_date), drug_exposure_start_date),
         drug_exposure_end_date = ifelse(is.na(drug_exposure_end_date), date_add('day', 120L, drug_exposure_start_date), drug_exposure_end_date)) %>%
  filter(length_rx > 0, time_bw_rx >= 0) %>% 
  mutate(days_supply = case_when(length_rx > 120 ~ 120L,
                                 length_rx <= 120 ~ length_rx,
                                 TRUE ~ NA),
         drug_exposure_end_date = case_when(length_rx > 120 ~ date_add('day', 120L, drug_exposure_start_date),
                                            length_rx <= 120 ~ drug_exposure_end_date,
                                            TRUE ~ NA)) %>%
  filter(drug_exposure_start_date >= first_sca_dx & drug_exposure_start_date < end_date) %>%
  collect()


hu_ip_tbl <- trino_session_nodq$cdm_tbl("drug_exposure") %>%
  inner_join(load_codeset("rx_hydroxyurea"), by = c('drug_concept_id' = 'concept_id')) %>%
  inner_join(results_tbl('nodq_cohort_censored')) %>%
  filter(drug_type_concept_id %in% c(38000180)) %>%
  select(site, person_id, first_sca_dx, end_date, censorship_reason, visit_occurrence_id, 
         drug_exposure_start_date, drug_exposure_end_date,
         drug_concept_id, drug_type_concept_id, drug_concept_name, refills, 
         days_supply, quantity, frequency) %>%
  arrange(site, person_id, drug_exposure_start_date) %>%
  group_by(site, person_id) %>%
  mutate(drug_exposure_end_date = ifelse(is.na(drug_exposure_end_date),
                                         drug_exposure_start_date,
                                         drug_exposure_end_date)) %>%
  mutate(length_rx = date_diff('day', drug_exposure_start_date, drug_exposure_end_date),
         time_bw_rx = date_diff('day', lag(drug_exposure_end_date), drug_exposure_start_date)) %>%
  filter(time_bw_rx >= 0) %>% 
  filter(drug_exposure_start_date >= first_sca_dx & drug_exposure_start_date < end_date) %>%
  collect()


combo <- hu_rx_tbl %>%
  union(hu_ip_tbl)

output_tbl(combo, 'de_hydroxyurea_windows_nodq', .chunk_size = 3000)


treated_visits <- trino_session_nodq$cdm_tbl('visit_occurrence') %>%
  select(site, person_id, visit_occurrence_id, visit_start_date, visit_end_date,
         visit_concept_id) %>%
  inner_join(results_tbl('nodq_cohort_censored')) %>%
  filter(visit_start_date >= first_sca_dx & visit_start_date <= end_date) %>%
  left_join(results_tbl('de_hydroxyurea_windows_nodq') %>% 
              select(site, person_id,
                     drug_exposure_start_date, drug_exposure_end_date)) %>%
  mutate(hu_treated = case_when(visit_start_date >= drug_exposure_start_date & 
                                  visit_start_date <= drug_exposure_end_date ~ TRUE,
                                TRUE ~ FALSE)) %>%
  filter(hu_treated) %>%
  distinct(site, person_id, visit_occurrence_id, hu_treated)

all_visits <- trino_session_nodq$cdm_tbl('visit_occurrence') %>%
  select(site, person_id, visit_occurrence_id, visit_start_date, visit_end_date,
         visit_concept_id) %>%
  inner_join(results_tbl('nodq_cohort_censored')) %>%
  filter(visit_start_date >= first_sca_dx) %>%
  left_join(treated_visits) %>%
  mutate(hu_label = case_when(visit_start_date > end_date ~ 'censored',
                              hu_treated == TRUE ~ 'hydroxyurea',
                              is.na(hu_treated) ~ 'no hydroxyurea',
                              TRUE ~ NA_character_))

output_tbl(all_visits, 'hu_visits_labelled_nodq')


### Compute inputs for DID analysis
treated_props <- results_tbl('hu_visits_labelled_nodq') %>%
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
# first_hu <- cdm_tbl('de_hydroxyurea_windows') %>%
#   inner_join(did_treatment_labels) %>%
#   group_by(site, person_id, treated_50) %>%
#   filter(drug_exposure_start_date == min(drug_exposure_start_date)) %>%
#   left_join(cdm_tbl('person') %>% select(site, person_id, birth_date)) %>%
#   mutate(drug_age = date_diff('day', birth_date, drug_exposure_start_date),
#          age_year_first_HU = floor(drug_age / 365.25),
#          age_year_first_HU = as.numeric(age_year_first_HU),
#          calendar_year_first_HU = year(drug_exposure_start_date),
#          first_HU_date = drug_exposure_start_date) %>%
#   filter(treated_50 == 1) %>%
#   ungroup() %>%
#   distinct(site, person_id, first_HU_date, age_year_first_HU, calendar_year_first_HU) %>%
#   compute_new('first_hu_dat')

first_hu_ay <- cdm_tbl('de_hydroxyurea_windows_nodq') %>%
  inner_join(did_treatment_labels) %>%
  filter(treated_50 == 1) %>%
  group_by(site, person_id) %>%
  filter(age_year == min(age_year)) %>%
  distinct(site, person_id, age_year) %>%
  rename('age_year_first_HU' = 'age_year')

first_hu <- cdm_tbl('de_hydroxyurea_windows_nodq') %>%
  inner_join(did_treatment_labels) %>%
  inner_join(first_hu_ay, by = c('site', 'person_id', 'age_year' = 'age_year_first_HU')) %>%
  group_by(site, person_id, age_year) %>%
  filter(drug_exposure_start_date == min(drug_exposure_start_date)) %>%
  mutate(first_HU_date = drug_exposure_start_date,
         calendar_year_first_HU = year(drug_exposure_start_date)) %>%
  ungroup() %>%
  distinct(site, person_id, first_HU_date, age_year, calendar_year_first_HU) %>%
  rename('age_year_first_HU' = 'age_year') %>%
  compute_new('first_hu_dat_nodq')

#### MCV
bl_mcv <- trino_session_nodq$cdm_tbl('measurement_labs') %>%
  inner_join(load_codeset('lab_mcv'), 
             by = c('measurement_concept_id' = 'concept_id')) %>%
  inner_join(first_hu) %>%
  left_join(trino_session_nodq$cdm_tbl('visit_occurrence') %>% select(visit_occurrence_id, visit_concept_id)) %>%
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

bl_anc <- trino_session_nodq$cdm_tbl('measurement_labs') %>%
  inner_join(load_codeset('lab_anc'), 
             by = c('measurement_concept_id' = 'concept_id')) %>%
  inner_join(first_hu) %>%
  left_join(trino_session_nodq$cdm_tbl('visit_occurrence') %>% select(visit_occurrence_id, visit_concept_id)) %>%
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
bl_hgb <- trino_session_nodq$cdm_tbl('measurement_labs') %>%
  inner_join(load_codeset('lab_serum_hemoglobin'), 
             by = c('measurement_concept_id' = 'concept_id')) %>%
  inner_join(first_hu) %>%
  left_join(trino_session_nodq$cdm_tbl('visit_occurrence') %>% select(visit_occurrence_id, visit_concept_id)) %>%
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
  compute_new('bl_labs_nodq')

### ED days per year
ed_per_ageyear <- trino_session_nodq$cdm_tbl('visit_occurrence') %>%
  inner_join(results_tbl('nodq_cohort_censored')) %>%
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

## ED Visits
ed_per_ageyear2 <- trino_session_nodq$cdm_tbl('visit_occurrence') %>%
  inner_join(results_tbl('nodq_cohort_censored')) %>%
  filter(visit_start_date >= first_sca_dx,
         visit_start_date <= end_date) %>%
  filter(visit_concept_id == 9203) %>%
  left_join(cdm_tbl('person') %>% select(person_id, birth_date)) %>%
  mutate(visit_age = date_diff('day', birth_date, visit_start_date),
         age_year = floor(visit_age / 365.25),
         age_year = as.numeric(age_year)) %>%
  group_by(site, person_id, age_year) %>%
  summarise(ed_vis_ageyear = n_distinct(visit_occurrence_id))


### hospitalization days per year
hosp_per_ageyear <- trino_session_nodq$cdm_tbl('visit_occurrence') %>%
  inner_join(results_tbl('nodq_cohort_censored')) %>%
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

### hospitalization days per year
hosp_per_ageyear2 <- trino_session_nodq$cdm_tbl('visit_occurrence') %>%
  inner_join(results_tbl('nodq_cohort_censored')) %>%
  filter(visit_start_date >= first_sca_dx,
         visit_start_date <= end_date) %>%
  filter(visit_concept_id %in% c(9201, 2000000088)) %>%
  left_join(cdm_tbl('person') %>% select(person_id, birth_date)) %>%
  mutate(visit_age = date_diff('day', birth_date, visit_start_date),
         age_year = floor(visit_age / 365.25),
         age_year = as.numeric(age_year)) %>%
  group_by(site, person_id, age_year) %>%
  summarise(hosp_vis_ageyear = n_distinct(visit_occurrence_id))

### average MCV (exclude IP & ED)
mean_mcv <- trino_session_nodq$cdm_tbl('measurement_labs') %>%
  inner_join(load_codeset('lab_mcv'), 
             by = c('measurement_concept_id' = 'concept_id')) %>%
  inner_join(results_tbl('nodq_cohort_censored')) %>%
  left_join(trino_session_nodq$cdm_tbl('person') %>% select(person_id, birth_date)) %>%
  left_join(trino_session_nodq$cdm_tbl('visit_occurrence') %>% select(visit_occurrence_id, visit_concept_id)) %>%
  mutate(meas_age = date_diff('day', birth_date, measurement_date),
         age_year = floor(meas_age / 365.25),
         age_year = as.numeric(age_year)) %>%
  filter(measurement_date >= first_sca_dx & measurement_date <= end_date,
         !visit_concept_id %in% c(9201, 9203, 2000000048, 2000000088)) %>%
  group_by(site, person_id, age_year) %>%
  summarise(avg_mcv = mean(value_as_number, na.rm = TRUE))

### average hemoglobin (exclude IP & ED)
mean_hgb <- trino_session_nodq$cdm_tbl('measurement_labs') %>%
  inner_join(load_codeset('lab_serum_hemoglobin'), 
             by = c('measurement_concept_id' = 'concept_id')) %>%
  inner_join(results_tbl('nodq_cohort_censored')) %>%
  left_join(trino_session_nodq$cdm_tbl('person') %>% select(person_id, birth_date)) %>%
  left_join(trino_session_nodq$cdm_tbl('visit_occurrence') %>% select(visit_occurrence_id, visit_concept_id)) %>%
  mutate(meas_age = date_diff('day', birth_date, measurement_date),
         age_year = floor(meas_age / 365.25),
         age_year = as.numeric(age_year)) %>%
  filter(measurement_date >= first_sca_dx & measurement_date <= end_date,
         !visit_concept_id %in% c(9201, 9203, 2000000048, 2000000088)) %>%
  group_by(site, person_id, age_year) %>%
  summarise(avg_hgb = mean(value_as_number, na.rm = TRUE))

### average ANC (exclude IP & ED)
mean_anc <- trino_session_nodq$cdm_tbl('measurement_labs') %>%
  inner_join(load_codeset('lab_anc'), 
             by = c('measurement_concept_id' = 'concept_id')) %>%
  inner_join(results_tbl('nodq_cohort_censored')) %>%
  left_join(trino_session_nodq$cdm_tbl('person') %>% select(person_id, birth_date)) %>%
  left_join(trino_session_nodq$cdm_tbl('visit_occurrence') %>% select(visit_occurrence_id, visit_concept_id)) %>%
  mutate(meas_age = date_diff('day', birth_date, measurement_date),
         age_year = floor(meas_age / 365.25),
         age_year = as.numeric(age_year),
         value_as_number = ifelse(value_as_number < 30, value_as_number * 1000, value_as_number)) %>%
  filter(measurement_date >= first_sca_dx & measurement_date <= end_date,
         !visit_concept_id %in% c(9201, 9203, 2000000048, 2000000088)) %>%
  group_by(site, person_id, age_year) %>%
  summarise(avg_anc = mean(value_as_number, na.rm = TRUE))

### combine
ay_avgs <- ed_per_ageyear %>%
  full_join(ed_per_ageyear2) %>%
  full_join(hosp_per_ageyear) %>%
  full_join(hosp_per_ageyear2) %>%
  full_join(mean_mcv) %>%
  full_join(mean_anc) %>%
  full_join(mean_hgb) %>%
  compute_new('ay_avgs_nodq')


## assemble DID table

did_primary_vars <- results_tbl('nodq_cohort_censored') %>%
  select(site, person_id, first_sca_dx, last_visit_date, censorship_reason, end_date) %>%
  left_join(did_treatment_labels) %>%
  full_join(results_tbl('ay_avgs_nodq')) %>%
  left_join(first_hu) %>%
  left_join(results_tbl("bl_labs_nodq") %>% 
              select(site, person_id, baseline_hgb, baseline_anc, baseline_mcv))

output_tbl(did_primary_vars %>% ungroup(), 'did_primary_vars_nodq')
