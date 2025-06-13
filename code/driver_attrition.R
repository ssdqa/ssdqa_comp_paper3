
#' ** Attrition Cohort Construction**
attrition_counts <- list()

#' `Step 1: Patients with SCD diagnosis, including SCA` 
s1_ch <- cdm_tbl('condition_occurrence') %>%
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

attrition_counts$step3 <- s3_ch %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 3,
         attrition_step = 'Patients with SCA < 18 years old at first diagnosis') %>%
  collect()

#' `Step 4: Patients with lab-confirmed diagnosis` 
#' (Hemoglobinopathy screening, Hemoglobin electrophoresis) 
s4_ch <- cdm_tbl('measurement_labs') %>%
  inner_join(load_codeset('lab_scd'), by = c('measurement_concept_id' = 'concept_id')) %>%
  inner_join(s3_ch) %>%
  distinct(site, person_id) %>%
  compute_new()

attrition_counts$step4 <- s4_ch %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 4,
         attrition_step = 'Patients with lab-confirmed diagnosis') %>%
  collect()

#' `Step 5: Patients with > 3 visits`
s5_ch <- cdm_tbl('visit_occurrence') %>%
  inner_join(s4_ch) %>%
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

