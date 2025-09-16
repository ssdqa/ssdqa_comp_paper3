
## rerun attrition based on results of rounds 1-3 of dqa
## start at step 3 which is what the mini cdm is based on 
# config('cdm_schema', 'ssdqa_paper3')
# config('table_names', list(
#   'condition_occurrence' = 'cdm_condition_occurrence',
#   'procedure_occurrence' = 'cdm_procedure_occurrence',
#   'visit_occurrence' = 'cdm_visit_occurrence',
#   'provider' = 'cdm_provider_remap',
#   'care_site' = 'cdm_care_site_remap',
#   'measurement_labs' = 'cdm_measurement_labs_remap',
#   'measurement_vitals' = 'cdm_measurement_vitals',
#   'measurement_anthro' = 'cdm_measurement_anthro',
#   'drug_exposure' = 'cdm_drug_exposure',
#   'person' = 'cdm_person'
# ))

#' ** Attrition Cohort Construction**
attrition_counts_r4 <- list()

s3_ch <- results_tbl('round2_cohort') %>% distinct(site, person_id)

#' `Step 4: Patients with lab-confirmed diagnosis` 
#' (Hemoglobinopathy screening, Hemoglobin electrophoresis) 
#' in electropheresis, want to look for S/S, S/Beta, sickle cell?
s4_ch <- cdm_tbl('measurement_labs') %>%
  filter(measurement_date >= as.Date('2011-01-01'),
         measurement_date <= as.Date('2024-12-31')) %>%
  inner_join(load_codeset('lab_scd'), by = c('measurement_concept_id' = 'concept_id')) %>%
  inner_join(s3_ch) %>%
  select(site, person_id, measurement_concept_id, value_as_number, value_as_concept_id,
         value_as_concept_name, value_source_value, subtyping) %>% 
  collect()

hbs_parsing <- s4_ch %>% 
  filter(grepl('Hb S =|= Hb S|= HB S', value_source_value)) %>%
  collect() %>%
  mutate(value_as_number = as.numeric(value_as_number)) %>%
  mutate(value_as_number = ifelse(is.na(value_as_number),readr::parse_number(value_source_value),
                                  value_as_number)) %>%
  filter(value_as_number > 50) %>%
  distinct(site, person_id)

# hbs_parsing_db <- copy_to_new(df = hbs_parsing)

remaining_labs <- s4_ch %>%
  filter(!grepl('Hb S =|= Hb S|= HB S', value_source_value)) %>%
  mutate(value_as_number = as.numeric(value_as_number)) %>%
  mutate(keep = case_when(subtyping %in% c('quant') & value_as_number > 50 ~ TRUE,
                          subtyping %in% c('text') & 
                            grepl('fsa$|fs$| SAF | SA | SF | S | FS | FSA | SFA | SFA2 ', value_source_value) ~ TRUE,
                          measurement_concept_id == 2000000999 & 
                            grepl('HbS POSITIVE| S/S |Hgb S/S|HGB S/S|hgb s/s|S/beta| S/Beta | S/Beta| S-beta|HbSS|HgbSS|homozygous sickle cell', value_source_value) &
                            !grepl('A/S|nonspecific', value_source_value) ~ TRUE,
                          TRUE ~ FALSE)) %>%
  filter(keep) %>%
  distinct(site, person_id) %>%
  # compute_new() %>%
  union(hbs_parsing)

remaining_labs_db <- copy_to_new(df = remaining_labs)

attrition_counts_r4$step4 <- remaining_labs %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 4L,
         attrition_step = 'Patients with lab-confirmed diagnosis, after remapping') %>%
  collect()

#' `Step 5: Patients with > 3 visits`
s5_ch <- cdm_tbl('visit_occurrence') %>%
  filter(visit_start_date >= as.Date('2011-01-01') &
           visit_start_date <= as.Date('2024-12-31')) %>%
  inner_join(remaining_labs_db) %>%
  group_by(site, person_id) %>%
  summarise(n_visit = n_distinct(visit_occurrence_id)) %>%
  filter(n_visit > 3) #%>% compute_new()

attrition_counts_r4$step5 <- s5_ch %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 5L,
         attrition_step = 'Patients with > 3 visits') %>%
  collect()

#' `Step 6: At least 1 visit with Hematology specialist` (new criteria)
s6_ch <- find_specialty(visits = cdm_tbl("visit_occurrence") %>%
                          filter(visit_start_date >= as.Date('2011-01-01') &
                                   visit_start_date <= as.Date('2024-12-31')) %>%
                          inner_join(s5_ch),
                        specialty_conceptset = load_codeset('hematology_specialty'))

attrition_counts_r4$step6 <- s6_ch %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 6L,
         attrition_step = 'Patients at least 1 visit with a hematology specialist, after remapping') %>%
  collect()

#' `Step 7: > 50% of visits with Hematology specialist` (new criteria)
s7_ch <- s6_ch %>%
  group_by(site, person_id) %>%
  summarise(n_heme = n_distinct(visit_occurrence_id)) %>%
  inner_join(s5_ch) %>%
  mutate(prop_heme = as.numeric(n_heme) / as.numeric(n_visit)) %>%
  filter(prop_heme > 0.5) #%>%
  #compute_new()

attrition_counts_r4$step7 <- s7_ch %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 7L,
         attrition_step = 'Patients with > 50% of visits with a hematology specialist, after remapping') %>%
  collect()

## Build final cohort w/ start + end dates
last_visit <- cdm_tbl('visit_occurrence') %>%
  inner_join(s7_ch) %>%
  filter(visit_start_date >= as.Date('2011-01-01') &
           visit_start_date <= as.Date('2024-12-31')) %>%
  group_by(site, person_id) %>%
  filter(visit_start_date == max(visit_start_date)) %>%
  rename('end_date' = visit_start_date) %>%
  distinct(site, person_id, end_date)

final_cohort <- s7_ch %>%
  select(site, person_id) %>%
  inner_join(results_tbl('round2_cohort') %>% select(-end_date)) %>%
  distinct() %>%
  left_join(last_visit) #%>%
  #compute_new()

trino_session$output_tbl(final_cohort, #%>% rename('start_date' = 'first_sca_dx'),
                         'sca_attrition_cohort_r4')
postgres_session$output_tbl(final_cohort, #%>% rename('start_date' = 'first_sca_dx'),
                            'sca_attrition_cohort_r4')

## Output attrition counts
steps1_3 <- postgres_session$results_tbl('attrition_counts') %>%
  filter(step_number < 4) %>% collect()

attrition_full <- purrr::reduce(.x = attrition_counts_r4,
                                .f = dplyr::union)

postgres_session$output_tbl(steps1_3 %>% union(attrition_full), 'attrition_counts_r4')


#' ** Table 1 **

##' `Demographics`
demos <- cdm_tbl('person') %>%
  select(site, person_id, birth_date, gender_concept_name, 
         race_concept_name, ethnicity_concept_name) %>%
  inner_join(results_tbl('sca_attrition_cohort_r4')) %>%
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
  inner_join(results_tbl('sca_attrition_cohort_r4')) %>%
  mutate(visit_grp = case_when(visit_concept_id %in% c(9203, 2000000048) ~ 'ED Visit',
                               visit_concept_id %in% c(9201, 2000001532) ~ 'Hospitalization')) %>%
  filter(!is.na(visit_grp)) %>%
  group_by(site, person_id, visit_grp) %>%
  summarise(n_visit = n_distinct(visit_occurrence_id)) %>% collect()

heme_visits <- find_specialty(visits = cdm_tbl("visit_occurrence") %>%
                                filter(visit_start_date >= as.Date('2011-01-01') &
                                         visit_start_date <= as.Date('2024-12-31')) %>%
                                inner_join(results_tbl('sca_attrition_cohort_r4')),
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
  inner_join(results_tbl('sca_attrition_cohort_r4')) %>%
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
         'Gender' = 'gender_concept_name')

postgres_session$output_tbl(t1_input, 'table1_input')


#' ** SQUBA Cohort Attrition **

ca_ms_exp <- ca_process(attrition_tbl = postgres_session$results_tbl('attrition_counts_r4') %>%
                          collect(),
                        multi_or_single_site = 'multi',
                        anomaly_or_exploratory = 'exploratory',
                        start_step_num = 1)

postgres_session$output_tbl(ca_ms_exp, 'ca_ms_exp_cs_r4')

ca_ms_anom_prs <- ca_process(attrition_tbl = postgres_session$results_tbl('attrition_counts_r4') %>%
                               collect(),
                             multi_or_single_site = 'multi',
                             anomaly_or_exploratory = 'anomaly',
                             var_col = 'prop_retained_start',
                             start_step_num = 1)

postgres_session$output_tbl(ca_ms_anom_prs, 'ca_ms_anom_cs_prs_r4')

ca_ms_anom_num <- ca_process(attrition_tbl = postgres_session$results_tbl('attrition_counts_r4') %>%
                               collect(),
                             multi_or_single_site = 'multi',
                             anomaly_or_exploratory = 'anomaly',
                             var_col = 'num_pts',
                             start_step_num = 1)

postgres_session$output_tbl(ca_ms_anom_num, 'ca_ms_anom_cs_num_r4')
