
#' ** Attrition Cohort Construction**
attrition_counts <- list()

#' `Step 1: Patients with SCD diagnosis, including SCA` 
s1_ch <- cdm_tbl('condition_occurrence') %>%
  filter(condition_start_date >= '2011-01-01',
         condition_start_date <= '2024-12-31') %>%
  inner_join(load_codeset('dx_scd'), by = c('condition_concept_id' = 'concept_id'))

attrition_counts$step1 <- s1_ch %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 1,
         attrition_step = 'Patients with SCD diagnosis, including SCA') %>%
  collect()


#' `Step 2: Patients with SCA diagnosis (& first dx within study period)` 
s2_ch <- cdm_tbl('condition_occurrence') %>%
  inner_join(load_codeset('dx_sca'), by = c('condition_concept_id' = 'concept_id')) %>%
  inner_join(s1_ch %>% select(site, person_id)) %>%
  group_by(site, person_id) %>%
  filter(condition_start_date == min(condition_start_date)) %>%
  rename('first_sca_dx' = 'condition_start_date') %>%
  filter(first_sca_dx >= '2011-01-01' & first_sca_dx <= '2024-12-31') %>%
  select(site, person_id, first_sca_dx) %>% compute_new()

attrition_counts$step2 <- s2_ch %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 2,
         attrition_step = 'Patients with SCA diagnosis') %>%
  collect()

#' `Step 3: Patients with SCA < 18 years old` 
s3_ch <- cdm_tbl('person') %>%
  select(site, person_id, birth_date) %>%
  inner_join(s2_ch) %>%
  mutate(age_first_dx = first_sca_dx - birth_date,
         age_first_dx = as.numeric(age_first_dx) / 365.25) %>%
  filter(age_first_dx < 18)

output_tbl(s3_ch %>% distinct(site, person_id, first_sca_dx),
           'step3_cohort')

attrition_counts$step3 <- s3_ch %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 3,
         attrition_step = 'Patients with SCA < 18 years old at first diagnosis') %>%
  collect()

#' `Step 4: Patients with lab-confirmed diagnosis` 
#' (Hemoglobinopathy screening, Hemoglobin electrophoresis) 
s4_ch <- cdm_tbl('measurement_labs') %>%
  filter(measurement_date >= '2011-01-01',
         measurement_date <= '2024-12-31') %>%
  inner_join(load_codeset('lab_scd'), by = c('measurement_concept_id' = 'concept_id')) %>%
  inner_join(s3_ch) %>%
  select(site, person_id, measurement_concept_id, value_as_number, value_as_concept_id,
         value_as_concept_name, value_source_value, subtyping) 

hbs_parsing <- s4_ch %>% 
  filter(grepl('Hb S =|= Hb S|= HB S', value_source_value)) %>%
  collect() %>%
  mutate(value_as_number = ifelse(is.na(value_as_number),readr::parse_number(value_source_value),
                                  value_as_number)) %>%
  filter(value_as_number > 50) %>%
  distinct(site, person_id)

hbs_parsing_db <- copy_to_new(df = hbs_parsing)

remaining_labs <- s4_ch %>%
  filter(!grepl('Hb S =|= Hb S|= HB S', value_source_value)) %>%
  mutate(keep = case_when(subtyping %in% c('quant') & value_as_number > 50 ~ TRUE,
                          subtyping %in% c('text') & 
                            grepl('fsa$|fs$| SAF | SA | SF | S | FS | FSA | SFA | SFA2 ', value_source_value) ~ TRUE,
                          TRUE ~ FALSE)) %>%
  filter(keep) %>%
  distinct(site, person_id) %>%
  compute_new() %>%
  union(hbs_parsing_db)

attrition_counts$step4 <- remaining_labs %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 4,
         attrition_step = 'Patients with lab-confirmed diagnosis') %>%
  collect()

#' `Step 5: Patients with > 3 visits`
s5_ch <- cdm_tbl('visit_occurrence') %>%
  filter(visit_start_date >= '2011-01-01' &
           visit_start_date <= '2024-12-31') %>%
  inner_join(remaining_labs) %>%
  group_by(site, person_id) %>%
  summarise(n_visit = n_distinct(visit_occurrence_id)) %>%
  filter(n_visit > 3) %>% compute_new()

attrition_counts$step5 <- s5_ch %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 5,
         attrition_step = 'Patients with > 3 visits') %>%
  collect()

#' `Step 6: At least 1 visit with Hematology specialist` (new criteria)
s6_ch <- find_specialty(visits = cdm_tbl("visit_occurrence") %>%
                          filter(visit_start_date >= '2011-01-01' &
                                   visit_start_date <= '2024-12-31') %>%
                          inner_join(s5_ch),
                        specialty_conceptset = load_codeset('hematology_specialty'))

attrition_counts$step6 <- s6_ch %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 6,
         attrition_step = 'Patients at least 1 visit with a hematology specialist') %>%
  collect()

#' `Step 7: > 50% of visits with Hematology specialist` (new criteria)
s7_ch <- s6_ch %>%
  group_by(site, person_id) %>%
  summarise(n_heme = n_distinct(visit_occurrence_id)) %>%
  inner_join(s5_ch) %>%
  mutate(prop_heme = as.numeric(n_heme) / as.numeric(n_visit)) %>%
  filter(prop_heme > 0.5) %>%
  compute_new()

attrition_counts$step7 <- s7_ch %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 7,
         attrition_step = 'Patients with > 50% of visits with a hematology specialist') %>%
  collect()

## Build final cohort w/ start + end dates
last_visit <- cdm_tbl('visit_occurrence') %>%
  inner_join(s7_ch) %>%
  filter(visit_start_date >= '2011-01-01' &
           visit_start_date <= '2024-12-31') %>%
  group_by(site, person_id) %>%
  filter(visit_start_date == max(visit_start_date)) %>%
  rename('end_date' = visit_start_date) %>%
  distinct(site, person_id, end_date)

final_cohort <- s7_ch %>%
  select(site, person_id) %>%
  inner_join(s2_ch) %>%
  distinct() %>%
  left_join(last_visit) %>%
  compute_new()

output_tbl(final_cohort %>% rename('start_date' = 'first_sca_dx'),
           'sca_attrition_cohort')

## Output attrition counts
attrition_full <- purrr::reduce(.x = attrition_counts,
                                .f = dplyr::union)

output_tbl(attrition_full, 'attrition_counts')


#' ** Table 1 **

##' `Demographics`
demos <- cdm_tbl('person') %>%
  select(site, person_id, birth_date, gender_concept_name, 
         race_concept_name, ethnicity_concept_name) %>%
  inner_join(results_tbl('sca_attrition_cohort')) %>%
  mutate(age_at_dx = start_date - birth_date,
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
  filter(visit_start_date >= '2011-01-01' &
           visit_start_date <= '2024-12-31') %>%
  inner_join(results_tbl('sca_attrition_cohort')) %>%
  mutate(visit_grp = case_when(visit_concept_id %in% c(9203, 2000000048) ~ 'ED Visit',
                               visit_concept_id %in% c(9201, 2000001532) ~ 'Hospitalization')) %>%
  filter(!is.na(visit_grp)) %>%
  group_by(site, person_id, visit_grp) %>%
  summarise(n_visit = n_distinct(visit_occurrence_id)) %>% collect()

heme_visits <- find_specialty(visits = cdm_tbl("visit_occurrence") %>%
                                filter(visit_start_date >= '2011-01-01' &
                                         visit_start_date <= '2024-12-31') %>%
                                inner_join(results_tbl('sca_attrition_cohort')),
                              specialty_conceptset = load_codeset('hematology_specialty')) %>%
  mutate(visit_grp = 'Hematology Specialist Visit') %>%
  group_by(site, person_id, visit_grp) %>%
  summarise(n_visit = n_distinct(visit_occurrence_id)) %>% collect()

visits <- gen_visits %>%
  union(heme_visits) %>%
  pivot_wider(names_from = visit_grp,
              values_from = n_visit)

##' `Hydroxyurea Exposure`
hdrxy <- cdm_tbl('drug_exposure') %>%
  filter(drug_exposure_start_date >= '2011-01-01' &
           drug_exposure_start_date <= '2024-12-31') %>%
  inner_join(results_tbl('sca_attrition_cohort')) %>%
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

output_tbl(t1_input, 'table1_input')


#' ** SQUBA Cohort Attrition **

ca_ms_exp <- ca_process(attrition_tbl = results_tbl('attrition_counts') %>%
                          collect(),
                        multi_or_single_site = 'multi',
                        anomaly_or_exploratory = 'exploratory',
                        start_step_num = 1)

output_tbl(ca_ms_exp, 'ca_ms_exp_cs')

ca_ms_anom_prs <- ca_process(attrition_tbl = results_tbl('attrition_counts') %>%
                          collect(),
                        multi_or_single_site = 'multi',
                        anomaly_or_exploratory = 'anomaly',
                        var_col = 'prop_retained_start',
                        start_step_num = 1)

output_tbl(ca_ms_anom_prs, 'ca_ms_anom_cs_prs')

ca_ms_anom_num <- ca_process(attrition_tbl = results_tbl('attrition_counts') %>%
                               collect(),
                             multi_or_single_site = 'multi',
                             anomaly_or_exploratory = 'anomaly',
                             var_col = 'num_pts',
                             start_step_num = 1)

output_tbl(ca_ms_anom_num, 'ca_ms_anom_cs_num')
