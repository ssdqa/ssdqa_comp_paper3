
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

##' `Patients with presence of SCD lab`
##' No specific filtering for results

s4_ch <- cdm_tbl('measurement_labs') %>%
  filter(measurement_date >= as.Date('2011-01-01'),
         measurement_date <= as.Date('2024-12-31')) %>%
  inner_join(load_codeset('lab_scd'), by = c('measurement_concept_id' = 'concept_id')) %>%
  inner_join(s3_ch) %>%
  select(site, person_id)

attrition_counts$step4 <- s4_ch %>%
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

attrition_counts$step5 <- s5_3vis %>%
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

attrition_counts$step6 <- s6_ch %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 6L,
         attrition_step = 'Patients with at least 2 years of follow-up') %>%
  collect()

##' `Patients with at least 2 SCA Diagnoses, at least 30 days apart`
cht_sca_dx <- cdm_tbl('condition_occurrence') %>%
  filter(condition_start_date => '2011-01-01' & condition_start_date <= '2024-01-01') %>%
  inner_join(s6_ch) %>%
  inner_join(load_codeset('dx_sca'), by = c('condition_concept_id' = 'concept_id'))

s7_ch <- cht_sca_dx %>%
  group_by(site, person_id, start_date, end_date) %>%
  summarise(ndx = n(),
            min_dx = min(condition_start_date), 
            max_dx = max(condition_start_date)) %>%
  mutate(diff_dx = date_diff('day', min_dx, max_dx)) %>%
  filter(ndx > 1, diff_dx >= 30) %>%
  ungroup()

attrition_counts$step7 <- s7_ch %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 7L,
         attrition_step = 'Patients with at least 2 SCA diagnoses at least 30 days apart') %>%
  collect()

output_tbl(s7_ch, 'final_cohort_precensor')

#################################################################################
## apply censorship criteria ##

# start & last visit dates (default end)
cht_start <- s2_ch %>%
  inner_join(s7_ch) %>%
  select(site, person_id, first_sca_dx)

last_visit <- cdm_tbl('visit_occurrence') %>%
  inner_join(results_tbl('sca_round7_cohort')) %>%
  filter(visit_start_date >= as.Date('2011-01-01') &
           visit_start_date <= as.Date('2024-12-31')) %>%
  group_by(site, person_id) %>%
  filter(visit_start_date == max(visit_start_date)) %>%
  rename('last_visit_date' = visit_start_date) %>%
  distinct(site, person_id, last_visit_date) 

# bone marrow transplant or gene therapy
bone_gene <- cdm_tbl('procedure_occurrence') %>%
  inner_join(load_codeset('px_bonemarrow_stemcell'), 
             by = c('procedure_concept_id' = 'concept_id')) %>%
  inner_join(results_tbl('sca_round7_cohort')) %>%
  filter(procedure_date >= as.Date(start_date), 
         procedure_date <= as.Date('2024-01-01')) %>%
  collect() %>%
  group_by(site, person_id) %>%
  filter(procedure_date == min(procedure_date)) %>%
  summarise(bone_gene_date = procedure_date) %>%
  distinct() %>% ungroup()

# other drugs (ie voxelotor & crizanlizuma)
expand_drugs <- get_descendants(codeset = load_codeset('rx_altdrug_ing'),
                                table_name = 'altdrugs')

other_drugs <- cdm_tbl('drug_exposure') %>%
  inner_join(expand_drugs, by = c('drug_concept_id' = 'concept_id')) %>%
  inner_join(results_tbl('sca_round7_cohort')) %>%
  filter(drug_exposure_start_date >= as.Date(start_date), 
         drug_exposure_start_date <= as.Date('2024-01-01')) %>%
  group_by(site, person_id) %>%
  filter(drug_exposure_start_date == min(drug_exposure_start_date)) %>%
  collect() %>%
  summarise(alt_drug_initiation = drug_exposure_start_date) %>%
  distinct()

# long term transfusion (2 transfusions w/n 3+ weeks & < 6 weeks)

transfusion_concepts <- load_codeset("px_blood_prod_transf", 'ccc') %>%
  inner_join(vocabulary_tbl('concept'), by = c('code1' = 'concept_code')) %>%
  select(concept_id, 'concept_code' = 'code1', concept_name, vocabulary_id) %>%
  collect()

## Erythrocytapheresis (4233006)
## therapeutic apheresis, for red blood cells (2108151)
## therapeutic apheresis, for plasma pheresis (2108163)
## plasmapheresis? (4049372)
## Therapeutic plasmapheresis (2008373)
## Transfusion of blood product (4024656)
## Transfusion, blood or blood components (2108119)
## Packed blood cell transfusion (4125928)
## Platelet transfusion (4130829)
## Transfusion of packed red blood cells (4323715)

additional_transfusions <- vocabulary_tbl('concept') %>%
  filter(concept_id %in% c(4233006, 2108151, 2108163, 4049372, 2008373,
                           4024656, 2108119, 4125928, 4130829, 4323715)) %>%
  select(concept_id, concept_code, concept_name, vocabulary_id) %>%
  collect()

transfusion_concepts %>%
  union(additional_transfusions) %>%
  readr::write_csv('specs/px_transfusion.csv')

transfuse_3_6_week <- cdm_tbl('procedure_occurrence') %>%
  inner_join(results_tbl('sca_round7_cohort')) %>%
  inner_join(load_codeset('px_transfusion'), by = c('procedure_concept_id' = 'concept_id')) %>%
  filter(procedure_date >= as.Date(start_date), procedure_date <= as.Date('2024-01-01')) %>%
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


####### Censorship Dates ############

censored_cohort <- results_tbl('sca_round7_cohort') %>%
  collect() %>%
  left_join(last_visit %>% collect()) %>%
  left_join(transfusion_dates) %>%
  left_join(other_drugs) %>%
  left_join(bone_gene) %>%
  group_by(site, person_id) %>%
  mutate(end_date = min(transfusion_initiation, alt_drug_initiation, bone_gene_date, last_visit_date,
                        na.rm = TRUE),
         censorship_reason = case_when(end_date == transfusion_initiation ~ 'Transfusion',
                                       end_date == alt_drug_initiation ~ 'Alternate Drug Therapy',
                                       end_date == bone_gene_date ~ 'Bone Marrow Transplant or Gene Therapy',
                                       TRUE ~ 'None / Last Visit'))

output_tbl(censored_cohort, 'final_cohort_censored')
