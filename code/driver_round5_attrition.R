
#' ** Attrition Cohort Construction**
attrition_counts_r5 <- list()

s3_ch <- results_tbl('round2_cohort') %>% distinct(site, person_id)

##' `Patients with presence of SCD lab`
##' No specific filtering for results

s4_ch <- cdm_tbl('measurement_labs') %>%
  filter(measurement_date >= as.Date('2011-01-01'),
         measurement_date <= as.Date('2024-12-31')) %>%
  inner_join(load_codeset('lab_scd'), by = c('measurement_concept_id' = 'concept_id')) %>%
  inner_join(s3_ch) %>%
  select(site, person_id)

attrition_counts_r5$step4 <- s4_ch %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 4L,
         attrition_step = 'Patients with at least 1 SCD lab, of any result, after remapping') %>%
  collect()

##' `Patients with at least 3 visits with a hematologist`

s5_ch <- find_specialty(visits = cdm_tbl("visit_occurrence") %>%
                          filter(visit_start_date >= as.Date('2011-01-01') &
                                   visit_start_date <= as.Date('2024-12-31')) %>%
                          inner_join(s4_ch),
                        specialty_conceptset = load_codeset('hematology_specialty'))

s5_3vis <- s5_ch %>%
  group_by(site, person_id) %>%
  summarise(n_heme = n_distinct(visit_occurrence_id)) %>%
  filter(n_heme >= 3)

attrition_counts_r5$step5 <- s5_3vis %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 5L,
         attrition_step = 'Patients with at least 3 visits with a hematologist, after remapping') %>%
  collect()

##' `Patients with at least 2 years of total follow-up`

s6_ch <- cdm_tbl('visit_occurrence') %>%
  inner_join(s5_3vis) %>%
  group_by(site, person_id) %>%
  summarise(minvis = min(visit_start_date),
            maxvis = max(visit_start_date)) %>%
  mutate(totalfu = date_diff('day', minvis, maxvis),
         fu_yrs = as.numeric(totalfu) / 365.25) %>%
  filter(fu_yrs >= 2)

attrition_counts_r5$step6 <- s6_ch %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 6L,
         attrition_step = 'Patients with at least 2 years of follow-up') %>%
  collect()

## Build final cohort w/ start + end dates
last_visit <- cdm_tbl('visit_occurrence') %>%
  inner_join(s6_ch) %>%
  filter(visit_start_date >= as.Date('2011-01-01') &
           visit_start_date <= as.Date('2024-12-31')) %>%
  group_by(site, person_id) %>%
  filter(visit_start_date == max(visit_start_date)) %>%
  rename('end_date' = visit_start_date) %>%
  distinct(site, person_id, end_date)

final_cohort <- s6_ch %>%
  select(site, person_id) %>%
  inner_join(results_tbl('round2_cohort') %>% select(-end_date)) %>%
  distinct() %>%
  left_join(last_visit)

trino_session$output_tbl(final_cohort, #%>% rename('start_date' = 'first_sca_dx'),
                         'sca_attrition_cohort_r5')
postgres_session$output_tbl(final_cohort, #%>% rename('start_date' = 'first_sca_dx'),
                            'sca_attrition_cohort_r5')

## Output attrition counts
steps1_3 <- postgres_session$results_tbl('attrition_counts') %>%
  filter(step_number < 4) %>% collect()

attrition_full <- purrr::reduce(.x = attrition_counts_r5,
                                .f = dplyr::union)

postgres_session$output_tbl(steps1_3 %>% union(attrition_full), 
                            'attrition_counts_r5')


#' ** Table 1 **

##' `Demographics`
demos <- cdm_tbl('person') %>%
  select(site, person_id, birth_date, gender_concept_name, 
         race_concept_name, ethnicity_concept_name) %>%
  inner_join(results_tbl('sca_attrition_cohort_r5')) %>%
  left_join(s6_ch %>% select(person_id, fu_yrs)) %>%
  mutate(age_at_dx = date_diff('day', birth_date, start_date),
         age_at_dx = as.numeric(age_at_dx) / 365.25) %>%
  mutate(race_concept_name = case_when(race_concept_name %in% c('Other', 'Unknown', 'No information',
                                                                'Refuse to answer', 'Multiple race') ~ 'Other/Unknown',
                                       TRUE ~ race_concept_name),
         ethnicity_concept_name = case_when(ethnicity_concept_name %in% c('Other', 'Unknown', 'No information',
                                                                          'Refuse to answer') ~ 'Other/Unknown',
                                            TRUE ~ ethnicity_concept_name)) %>%
  select(-c(start_date, end_date)) %>% 
  collect()

##' `Utilization`
gen_visits <- cdm_tbl('visit_occurrence') %>%
  filter(visit_start_date >= as.Date('2011-01-01') &
           visit_start_date <= as.Date('2024-12-31')) %>%
  inner_join(results_tbl('sca_attrition_cohort_r5')) %>%
  mutate(visit_grp = case_when(visit_concept_id %in% c(9203, 2000000048) ~ 'ED Visit',
                               visit_concept_id %in% c(9201, 2000001532) ~ 'Hospitalization')) %>%
  filter(!is.na(visit_grp)) %>%
  group_by(site, person_id, visit_grp) %>%
  summarise(n_visit = n_distinct(visit_occurrence_id)) %>% collect()

heme_visits <- find_specialty(visits = cdm_tbl("visit_occurrence") %>%
                                filter(visit_start_date >= as.Date('2011-01-01') &
                                         visit_start_date <= as.Date('2024-12-31')) %>%
                                inner_join(results_tbl('sca_attrition_cohort_r5')),
                              specialty_conceptset = load_codeset('hematology_specialty')) %>%
  mutate(visit_grp = 'Hematology Specialist Visit (Post-Remapping)') %>%
  group_by(site, person_id, visit_grp) %>%
  summarise(n_visit = n_distinct(visit_occurrence_id)) %>% collect()

visits <- gen_visits %>%
  union(heme_visits) %>%
  pivot_wider(names_from = visit_grp,
              values_from = n_visit)

##' `Hydroxyurea Exposure`
hdrxy <- cdm_tbl('drug_exposure') %>%
  filter(drug_exposure_start_date >= as.Date('2011-01-01') &
           drug_exposure_start_date <= as.Date('2024-12-31')) %>%
  inner_join(results_tbl('sca_attrition_cohort_r5')) %>%
  inner_join(load_codeset('rx_hydroxyurea'), by = c('drug_concept_id' = 'concept_id')) %>%
  group_by(site, person_id) %>%
  summarise(`Hydroxyurea Exposure` = 'Yes') %>% collect()

t1_input <- demos %>%
  left_join(visits) %>%
  left_join(hdrxy) %>%
  mutate(`Hydroxyurea Exposure` = ifelse(is.na(`Hydroxyurea Exposure`), 'No', 
                                         `Hydroxyurea Exposure`),
         gender_concept_name = str_to_title(gender_concept_name)) %>%
  mutate(across(where(is.numeric), .fns = ~replace_na(.,0))) %>%
  rename('Age at SCA Diagnosis' = 'age_at_dx',
         'Race' = 'race_concept_name',
         'Ethnicity' = 'ethnicity_concept_name',
         'Gender' = 'gender_concept_name',
         'Follow-Up (Years)' = 'fu_yrs')

postgres_session$output_tbl(t1_input, 'table1_input_r5')


#' ** SQUBA Cohort Attrition **

ca_ms_exp <- ca_process(attrition_tbl = postgres_session$results_tbl('attrition_counts_r5') %>%
                          collect(),
                        multi_or_single_site = 'multi',
                        anomaly_or_exploratory = 'exploratory',
                        start_step_num = 1)

postgres_session$output_tbl(ca_ms_exp, 'ca_ms_exp_cs_r5')

ca_ms_anom_prs <- ca_process(attrition_tbl = postgres_session$results_tbl('attrition_counts_r5') %>%
                               collect(),
                             multi_or_single_site = 'multi',
                             anomaly_or_exploratory = 'anomaly',
                             var_col = 'prop_retained_start',
                             start_step_num = 1)

postgres_session$output_tbl(ca_ms_anom_prs, 'ca_ms_anom_cs_prs_r5')

ca_ms_anom_num <- ca_process(attrition_tbl = postgres_session$results_tbl('attrition_counts_r5') %>%
                               collect(),
                             multi_or_single_site = 'multi',
                             anomaly_or_exploratory = 'anomaly',
                             var_col = 'num_pts',
                             start_step_num = 1)

postgres_session$output_tbl(ca_ms_anom_num, 'ca_ms_anom_cs_num_r5')

