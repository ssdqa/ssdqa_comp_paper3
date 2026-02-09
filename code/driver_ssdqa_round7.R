

cht_sca_dx <- cdm_tbl('condition_occurrence') %>%
  inner_join(results_tbl('sca_attrition_cohort_r5')) %>%
  inner_join(load_codeset('dx_sca'), by = c('condition_concept_id' = 'concept_id'))

twodx_30days <- cht_sca_dx %>%
  group_by(site, person_id, start_date, end_date) %>%
  summarise(ndx = n(),
            min_dx = min(condition_start_date), 
            max_dx = max(condition_start_date)) %>%
  mutate(diff_dx = date_diff('day', min_dx, max_dx)) %>%
  filter(ndx > 1, diff_dx >= 30) %>%
  ungroup()

output_tbl(twodx_30days %>% select(site, person_id, start_date, end_date), 
           'sca_round7_cohort')


step7 <- twodx_30days %>%
  group_by(site) %>%
  summarise(num_pts = n_distinct(person_id)) %>%
  mutate(step_number = 7L,
         attrition_step = 'Patients with > 1 SCA diagnoses at least 30 days apart') %>%
  collect()

postgres_session$results_tbl('attrition_counts_r5') %>%
  collect() %>%
  union(step7) %>%
  postgres_session$output_tbl('attrition_counts_r7')


## Cohort Attrition
ca_ms_exp <- ca_process(attrition_tbl = postgres_session$results_tbl('attrition_counts_r7') %>%
                          collect(),
                        multi_or_single_site = 'multi',
                        anomaly_or_exploratory = 'exploratory',
                        start_step_num = 1)

postgres_session$output_tbl(ca_ms_exp, 'ca_ms_exp_cs_r7')

## Sensitivity to Selection Criteria
ssc_ms_exp_cs_r7 <- ssc_process(base_cohort = results_tbl('sca_attrition_cohort_r5'),
                                alt_cohorts = list('1+ SCA Dx' = results_tbl('sca_round7_cohort')),
                                omop_or_pcornet = 'omop',
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                provider_tbl = cdm_tbl('provider'),
                                care_site_tbl = cdm_tbl('care_site'),
                                specialty_concepts = read_codeset('hematology_specialty'),
                                outcome_concepts = ssc_outcomes,
                                domain_tbl = ssc_domains,
                                domain_select = ssc_domains %>% pull(domain))

postgres_session$output_tbl(ssc_ms_exp_cs_r7$summary_values, 'ssc_ms_exp_cs_r7')
postgres_session$output_tbl(ssc_ms_exp_cs_r7$cohort_overlap, 'ssc_ms_exp_cs_overlap_r7')
