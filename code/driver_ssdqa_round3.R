
##' **Source & Concept Vocabularies**

heme_strings <- cdm_tbl('measurement_labs') %>%
  filter(str_like(tolower(measurement_source_value), '%hemoglobin s%') |
           str_like(tolower(measurement_source_value), '%beta globin%') |
           str_like(tolower(measurement_source_value), '%hemoglobin identification%') |
           str_like(tolower(measurement_source_value), '%hemoglobin quant%') |
           str_like(tolower(measurement_source_value), '%hemoglobin elect%')) %>%
  distinct(measurement_source_value) %>%
  rename('concept_id' = 'measurement_source_value') %>%
  collect()

readr::write_csv(heme_strings, 'specs/heme_lab_source_values_r3.csv')

####' `Multi Site, Anomaly Detection, Cross-Sectional`

scv_ms_anom_cs_heme <- scv_process(cohort = results_tbl('round2_cohort'),
                              concept_set = read_codeset('heme_lab_source_values_r3', 'c'),
                              omop_or_pcornet = 'omop',
                              multi_or_single_site = 'multi',
                              anomaly_or_exploratory = 'anomaly',
                              time = FALSE,
                              domain_tbl = read_codeset('input_scv_domain', 'ccccc'),
                              code_type = 'source',
                              code_domain = 'measurement_labs')

postgres_session$output_tbl(scv_ms_anom_cs_heme, 'scv_ms_anom_cs_r3_heme')

db_remove_table(name = 'ssdqa_paper3.concept_set')

scv_ms_anom_cs_labs <- scv_process(cohort = results_tbl('round2_cohort'),
                                   concept_set = read_codeset('unmapped_labs', 'iccc'),
                                   omop_or_pcornet = 'omop',
                                   multi_or_single_site = 'multi',
                                   anomaly_or_exploratory = 'anomaly',
                                   time = FALSE,
                                   domain_tbl = read_codeset('input_scv_domain', 'ccccc'),
                                   code_type = 'cdm',
                                   code_domain = 'measurement_labs')

postgres_session$output_tbl(scv_ms_anom_cs_labs, 'scv_ms_anom_cs_r3_labs')

####' `Multi Site, Exploratory, Cross-Sectional`

scv_ms_exp_cs_heme <- scv_process(cohort = results_tbl('round2_cohort'),
                             concept_set = read_codeset('heme_lab_source_values_r3', 'c'),
                             omop_or_pcornet = 'omop',
                             multi_or_single_site = 'multi',
                             anomaly_or_exploratory = 'exploratory',
                             time = FALSE,
                             domain_tbl = read_codeset('input_scv_domain', 'ccccc'),
                             code_type = 'source',
                             code_domain = 'measurement_labs')

postgres_session$output_tbl(scv_ms_exp_cs_heme, 'scv_ms_exp_cs_r3_heme')

db_remove_table(name = 'ssdqa_paper3.concept_set')

scv_ms_exp_cs_labs <- scv_process(cohort = results_tbl('round2_cohort'),
                                  concept_set = read_codeset('unmapped_labs', 'icc'),
                                  omop_or_pcornet = 'omop',
                                  multi_or_single_site = 'multi',
                                  anomaly_or_exploratory = 'exploratory',
                                  time = FALSE,
                                  domain_tbl = read_codeset('input_scv_domain', 'ccccc'),
                                  code_type = 'cdm',
                                  code_domain = 'measurement_labs')

postgres_session$output_tbl(scv_ms_exp_cs_labs, 'scv_ms_exp_cs_r3_labs')


####### Specialties
pv_cs_w_date <- cdm_tbl("visit_occurrence") %>%
  inner_join(results_tbl('round2_cohort')) %>%
  select(site, person_id, visit_start_date, visit_occurrence_id,
         provider_id, care_site_id) %>%
  left_join(cdm_tbl('provider') %>% select(provider_id, specialty_concept_id,
                                           specialty_source_value) %>%
              rename('pv_spec_cid' = 'specialty_concept_id',
                     'pv_spec_srcval' = 'specialty_source_value')) %>%
  left_join(cdm_tbl('care_site') %>% select(care_site_id, specialty_concept_id,
                                           specialty_source_value) %>%
              rename('cs_spec_cid' = 'specialty_concept_id',
                     'cs_spec_srcval' = 'specialty_source_value'))

output_tbl(pv_cs_w_date, 'pv_cs_w_date')

config('cdm_schema', 'ssdqa_paper3')  

##### provider
provider_input <- tibble(domain = 'pv_cs_w_date',
                         concept_field = 'pv_spec_cid',
                         source_concept_field = 'pv_spec_srcval',
                         date_field = 'visit_start_date',
                         vocabulary_field = NA)

scv_ms_anom_cs_pv <- scv_process(cohort = results_tbl('round2_cohort'),
                                   concept_set = read_codeset('unmapped_specialty', 'iccc'),
                                   omop_or_pcornet = 'omop',
                                   multi_or_single_site = 'multi',
                                   anomaly_or_exploratory = 'anomaly',
                                   time = FALSE,
                                   domain_tbl = provider_input,
                                   code_type = 'cdm',
                                   code_domain = 'pv_cs_w_date')

postgres_session$output_tbl(scv_ms_anom_cs_pv %>% mutate(domain = 'provider'), 
                            'scv_ms_anom_cs_r3_pv')

scv_ms_exp_cs_pv <- scv_process(cohort = results_tbl('round2_cohort'),
                                  concept_set = read_codeset('unmapped_specialty', 'icc'),
                                  omop_or_pcornet = 'omop',
                                  multi_or_single_site = 'multi',
                                  anomaly_or_exploratory = 'exploratory',
                                  time = FALSE,
                                  domain_tbl = provider_input,
                                  code_type = 'cdm',
                                  code_domain = 'pv_cs_w_date')

postgres_session$output_tbl(scv_ms_exp_cs_pv %>% mutate(domain = 'provider'), 
                            'scv_ms_exp_cs_r3_pv')

###### care site
caresite_input <- tibble(domain = 'pv_cs_w_date',
                         concept_field = 'cs_spec_cid',
                         source_concept_field = 'cs_spec_srcval',
                         date_field = 'visit_start_date',
                         vocabulary_field = NA)

scv_ms_anom_cs_cs <- scv_process(cohort = results_tbl('round2_cohort'),
                                   concept_set = read_codeset('unmapped_specialty', 'iccc'),
                                   omop_or_pcornet = 'omop',
                                   multi_or_single_site = 'multi',
                                   anomaly_or_exploratory = 'anomaly',
                                   time = FALSE,
                                   domain_tbl = caresite_input,
                                   code_type = 'cdm',
                                   code_domain = 'pv_cs_w_date')

postgres_session$output_tbl(scv_ms_anom_cs_cs %>% mutate(domain = 'care_site'), 
                            'scv_ms_anom_cs_r3_cs')

scv_ms_exp_cs_cs <- scv_process(cohort = results_tbl('round2_cohort'),
                                concept_set = read_codeset('unmapped_specialty', 'icc'),
                                omop_or_pcornet = 'omop',
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                time = FALSE,
                                domain_tbl = caresite_input,
                                code_type = 'cdm',
                                code_domain = 'pv_cs_w_date')

postgres_session$output_tbl(scv_ms_exp_cs_cs %>% mutate(domain = 'care_site'), 
                            'scv_ms_exp_cs_r3_cs')
