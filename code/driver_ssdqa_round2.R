
# ## prep codesets
# read_codeset('dx_scd') %>%
#   anti_join(read_codeset('dx_sca')) %>%
#   readr::write_csv('specs/dx_scd_no_sca.csv')
# 
# ## prep cohort
# round2_cohort <- cdm_tbl('visit_occurrence') %>%
#   inner_join(results_tbl('step3_cohort')) %>%
#   group_by(site, person_id, first_sca_dx) %>%
#   filter(visit_start_date == max(visit_start_date)) %>%
#   distinct(site, person_id, first_sca_dx, visit_start_date) %>%
#   rename('start_date' = first_sca_dx,
#          'end_date' = visit_start_date)
# 
# output_tbl(round2_cohort, 'round2_cohort')


sites <- c('national', 'texas', 'chop', 'cchmc', 'nemours',
           'nationwide', 'stanford', 'seattle', 'lurie', 'colorado')

##' **Clinical Events and Specialties**

####' `Multi Site, Anomaly Detection, Cross-Sectional`

cnc_list <- list()
specs_list <- list()

for(i in sites){
  set_argos_default(trino_session)
  
  cnc_sp_site <- cnc_sp_process(cohort = results_tbl('round2_cohort') %>% ungroup() %>% 
                                  filter(site == i),
                                omop_or_pcornet = 'omop',
                                multi_or_single_site = 'single',
                                anomaly_or_exploratory = 'anomaly',
                                time = FALSE,
                                codeset_tbl = read_codeset('input_cnc_sp', 'ccccc'),
                                care_site = TRUE,
                                provider = TRUE,
                                vocab_tbl = NULL)
  
  cnc_list[[i]] <- cnc_sp_site[[2]]
  specs_list[[i]] <- cnc_sp_site[[1]]
}

cnc_sp_ms_anom_cs <- purrr::reduce(.x = cnc_list,
                                   .f = dplyr::union)

postgres_session$output_tbl(cnc_sp_ms_anom_cs %>% mutate(output_function = 'cnc_sp_ms_anom_cs'), 
                            'cnc_sp_ms_anom_cs')

cnc_sp_specialties <- purrr::reduce(.x = specs_list,
                                    .f = dplyr::union)
postgres_session$output_tbl(cnc_sp_ms_anom_cs %>% mutate(output_function = 'cnc_sp_ms_anom_cs'), 
                            'cnc_sp_specialty_names')

####' `Multi Site, Anomaly Detection, Longitudinal`
cnc_sp_ms_anom_la <- cnc_sp_process(cohort = results_tbl('round2_cohort') %>% ungroup(),
                                   omop_or_pcornet = 'omop',
                                   multi_or_single_site = 'multi',
                                   anomaly_or_exploratory = 'anomaly',
                                   time = TRUE,
                                   time_period = 'year',
                                   time_span = c('2009-01-01', '2025-01-01'),
                                   codeset_tbl = read_codeset('input_cnc_sp', 'ccccc'),
                                   care_site = TRUE,
                                   provider = TRUE,
                                   vocab_tbl = NULL)

postgres_session$output_tbl(cnc_sp_ms_anom_la[[2]], 'cnc_sp_ms_anom_la')

##' **Patient Facts**
heme_specialists <- find_specialty(visits = cdm_tbl('visit_occurrence'),
                                   specialty_conceptset = load_codeset("hematology_specialty")) %>%
  select(site, person_id, visit_occurrence_id, visit_concept_id, 
         visit_start_date, visit_end_date, provider_id, care_site_id)

output_tbl(heme_specialists, 'hematology_spec_visits')

####' `Multi Site, Exploratory, Cross-Sectional`

pf_ms_exp_cs1 <- pf_process(cohort = results_tbl('round2_cohort'),
                            study_name = 'ssdqa_paper3',
                            omop_or_pcornet = 'omop',
                            multi_or_single_site = 'multi',
                            anomaly_or_exploratory = 'exploratory',
                            time = FALSE,
                            visit_types = c('inpatient', 'outpatient', 
                                            'outpatient (non-9202)', 'emergency department'),
                            domain_tbl = read_codeset("input_pf_domains", 'ccc'),
                            visit_tbl = cdm_tbl('visit_occurrence'),
                            visit_type_table = read_codeset('input_pf_visits', 'ic'))

pf_ms_exp_cs2 <- pf_process(cohort = results_tbl('round2_cohort'),
                            study_name = 'ssdqa_paper3',
                            omop_or_pcornet = 'omop',
                            multi_or_single_site = 'multi',
                            anomaly_or_exploratory = 'exploratory',
                            time = FALSE,
                            visit_types = c('hematology specialists'),
                            domain_tbl = read_codeset("input_pf_domains", 'ccc'),
                            visit_tbl = results_tbl('hematology_spec_visits'),
                            visit_type_table = read_codeset('input_pf_visits', 'ic'))

pf_ms_exp_cs_final <- pf_ms_exp_cs1 %>% union(pf_ms_exp_cs2)
postgres_session$output_tbl(pf_ms_exp_cs_final, 'pf_ms_exp_cs')

##' **Source & Concept Vocabularies**
 
##' string search first, then use that as concept set input?
