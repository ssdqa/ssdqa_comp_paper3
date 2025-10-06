
#' `Patient Facts`
#' Add hydroxyurea

#' `Multi Site, Exploratory, Cross-Sectional`
pf_ms_exp_cs1_r5 <- pf_process(cohort = results_tbl('sca_attrition_cohort_r5'),
                               study_name = 'ssdqa_paper3',
                               omop_or_pcornet = 'omop',
                               multi_or_single_site = 'multi',
                               anomaly_or_exploratory = 'exploratory',
                               time = FALSE,
                               visit_types = c('inpatient', 'emergency department',
                                               'all'),
                               domain_tbl = read_codeset("input_pf_domains_r5", 'ccc'),
                               visit_tbl = cdm_tbl('visit_occurrence'),
                               visit_type_table = read_codeset('input_pf_visits', 'ic') %>%
                                 mutate(visit_type = ifelse(visit_type == 'hematology specialists', 'all', visit_type)))

pf_ms_exp_cs2_r5 <- pf_process(cohort = results_tbl('sca_attrition_cohort_r5'),
                               study_name = 'ssdqa_paper3',
                               omop_or_pcornet = 'omop',
                               multi_or_single_site = 'multi',
                               anomaly_or_exploratory = 'exploratory',
                               time = FALSE,
                               visit_types = c('hematology specialists'),
                               domain_tbl = read_codeset("input_pf_domains_r5", 'ccc'),
                               visit_tbl = results_tbl('hematology_spec_visits_remap'),
                               visit_type_table = read_codeset('input_pf_visits', 'ic'))

pf_ms_exp_cs_final_r5 <- pf_ms_exp_cs1_r5 %>% union(pf_ms_exp_cs2_r5)
postgres_session$output_tbl(pf_ms_exp_cs_final_r5, 'pf_ms_exp_cs_r5')

#' `Multi Site, Anomaly Detection, Cross-Sectional`
pf_ms_anom_cs1_r5 <- pf_process(cohort = results_tbl('sca_attrition_cohort_r5'),
                                study_name = 'ssdqa_paper3',
                                omop_or_pcornet = 'omop',
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'anomaly',
                                time = FALSE,
                                visit_types = c('inpatient', 'emergency department',
                                                'all'),
                                domain_tbl = read_codeset("input_pf_domains_r5", 'ccc'),
                                visit_tbl = cdm_tbl('visit_occurrence'),
                                visit_type_table = read_codeset('input_pf_visits', 'ic') %>%
                                  mutate(visit_type = ifelse(visit_type == 'hematology specialists', 'all', visit_type)))

pf_ms_anom_cs2_r5 <- pf_process(cohort = results_tbl('sca_attrition_cohort_r5'),
                                study_name = 'ssdqa_paper3',
                                omop_or_pcornet = 'omop',
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'anomaly',
                                time = FALSE,
                                visit_types = c('hematology specialists'),
                                domain_tbl = read_codeset("input_pf_domains_r5", 'ccc'),
                                visit_tbl = results_tbl('hematology_spec_visits_remap'),
                                visit_type_table = read_codeset('input_pf_visits', 'ic'))

pf_ms_anom_cs_final_r5 <- pf_ms_anom_cs1_r5 %>% union(pf_ms_anom_cs2_r5)
postgres_session$output_tbl(pf_ms_anom_cs_final_r5, 'pf_ms_anom_cs_r5')

#' `Multi Site, Anomaly Detection, Longitudinal`
pf_ms_anom_la1_r5 <- pf_process(cohort = results_tbl('sca_attrition_cohort_r5'),
                                study_name = 'ssdqa_paper3',
                                omop_or_pcornet = 'omop',
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'anomaly',
                                time = TRUE,
                                time_span = c('2011-01-01', '2025-01-01'),
                                time_period = 'year',
                                visit_types = c('inpatient', 'emergency department',
                                                'all'),
                                domain_tbl = read_codeset("input_pf_domains_r5", 'ccc'),
                                visit_tbl = cdm_tbl('visit_occurrence'),
                                visit_type_table = read_codeset('input_pf_visits', 'ic') %>%
                                  mutate(visit_type = ifelse(visit_type == 'hematology specialists', 'all', visit_type)))

pf_ms_anom_la2_r5 <- pf_process(cohort = results_tbl('sca_attrition_cohort_r5'),
                                study_name = 'ssdqa_paper3',
                                omop_or_pcornet = 'omop',
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'anomaly',
                                time = TRUE,
                                time_span = c('2011-01-01', '2025-01-01'),
                                time_period = 'year',
                                visit_types = c('hematology specialists'),
                                domain_tbl = read_codeset("input_pf_domains_r5", 'ccc'),
                                visit_tbl = results_tbl('hematology_spec_visits_remap'),
                                visit_type_table = read_codeset('input_pf_visits', 'ic'))

pf_ms_anom_la_final_r5 <- pf_ms_anom_la1_r5 %>% union(pf_ms_anom_la2_r5)
postgres_session$output_tbl(pf_ms_anom_la_final_r5, 'pf_ms_anom_la_r5')

#' `Expected Variables Present`
#' focus on labs (heme, anc, mcv)

evp_ms_exp_cs_r5 <- evp_process(cohort = results_tbl('sca_attrition_cohort_r5'),
                                omop_or_pcornet = 'omop',
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                time = FALSE,
                                evp_variable_file = read_codeset('input_evp_r5', 'ccccc'))

postgres_session$output_tbl(evp_ms_exp_cs_r5, 'evp_ms_exp_cs_r5')

evp_ms_anom_cs_r5 <- evp_process(cohort = results_tbl('sca_attrition_cohort_r5'),
                                 omop_or_pcornet = 'omop',
                                 multi_or_single_site = 'multi',
                                 anomaly_or_exploratory = 'anomaly',
                                 time = FALSE,
                                 evp_variable_file = read_codeset('input_evp_r5', 'ccccc'))

postgres_session$output_tbl(evp_ms_anom_cs_r5, 'evp_ms_anom_cs_r5')

