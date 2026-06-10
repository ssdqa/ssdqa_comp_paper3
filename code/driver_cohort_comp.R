
set_argos_default(trino_session)
ssc_ms_exp_cs_sq <- ssc_process(base_cohort = results_tbl('final_cohort_censored') %>%
                                  rename('start_date' = 'first_sca_dx') %>%
                                  select(site, person_id, start_date, end_date),
                                alt_cohorts = list('Same Drop' = results_tbl('final_cohort_censored') %>%
                                                     rename('start_date' = 'first_sca_dx') %>%
                                                     select(site, person_id, start_date, end_date)),
                                omop_or_pcornet = 'omop',
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                provider_tbl = cdm_tbl('provider'),
                                care_site_tbl = cdm_tbl('care_site'),
                                specialty_concepts = read_codeset('hematology_specialty'),
                                outcome_concepts = ssc_outcomes,
                                domain_tbl = ssc_domains,
                                domain_select = ssc_domains %>% pull(domain))

blah <- results_tbl('did_primary_vars') %>%
  group_by(site, person_id) %>%
  mutate(never_treated = ifelse(any(treated_50 == 1L), FALSE, TRUE),
         yrs_treated = sum(treated_50)) %>%
  #ungroup() %>%
  mutate(mcv_thresh = case_when(age_year < 1 ~ baseline_mcv * 1.1,
                                age_year >= 1 & age_year < 4 ~ 
                                  (baseline_mcv * 1.1) + 4,
                                age_year >= 4 ~ (baseline_mcv * 1.1) + 6)) %>%
  mutate(adherant = case_when(never_treated ~ TRUE,
                              avg_mcv >= mcv_thresh ~ TRUE,
                              yrs_treated == 1L ~ TRUE,
                              TRUE ~ FALSE)) %>%
  filter(adherant, !never_treated) %>% distinct(site, person_id)

ssc_ms_exp_cs_sq_adh <- ssc_process(base_cohort = results_tbl('final_cohort_censored') %>%
                                      inner_join(blah) %>%
                                  rename('start_date' = 'first_sca_dx') %>%
                                  select(site, person_id, start_date, end_date),
                                alt_cohorts = list('Same Drop' = results_tbl('final_cohort_censored') %>%
                                                     rename('start_date' = 'first_sca_dx') %>%
                                                     select(site, person_id, start_date, end_date)),
                                omop_or_pcornet = 'omop',
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                provider_tbl = cdm_tbl('provider'),
                                care_site_tbl = cdm_tbl('care_site'),
                                specialty_concepts = read_codeset('hematology_specialty'),
                                outcome_concepts = ssc_outcomes,
                                domain_tbl = ssc_domains,
                                domain_select = ssc_domains %>% pull(domain)) 

set_argos_default(trino_session_nodq)
ssc_ms_exp_cs_nosq <- ssc_process(base_cohort = results_tbl('nodq_cohort_censored') %>%
                                  rename('start_date' = 'first_sca_dx') %>%
                                  select(site, person_id, start_date, end_date),
                                alt_cohorts = list('Same Drop' = results_tbl('nodq_cohort_censored') %>%
                                                     rename('start_date' = 'first_sca_dx') %>%
                                                     select(site, person_id, start_date, end_date)),
                                omop_or_pcornet = 'omop',
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                provider_tbl = cdm_tbl('provider'),
                                care_site_tbl = cdm_tbl('care_site'),
                                specialty_concepts = read_codeset('hematology_specialty'),
                                outcome_concepts = ssc_outcomes,
                                domain_tbl = ssc_domains,
                                domain_select = ssc_domains %>% pull(domain))

ssc_ms_exp_cs_sq$summary_values %>% 
  filter(cohort_id != 'Same Drop') %>%
  union(ssc_ms_exp_cs_nosq$summary_values %>% filter(cohort_id != 'Same Drop') %>%
          mutate(cohort_id = 'Non-SQUBA')) %>% 
  union(ssc_ms_exp_cs_sq_adh$summary_values %>% filter(cohort_id != 'Same Drop') %>%
          mutate(cohort_id = 'SQUBA Treated Adherent')) %>% 
  output_tbl('ssc_ms_exp_cs_dqcomp')


