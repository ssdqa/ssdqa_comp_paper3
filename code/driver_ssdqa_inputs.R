####' `Round 2 DQ`
##' **Clinical Events & Specialties**
cnc_sp_input <- tibble(domain = c('SCA diagnosis', 'Non-SCA diagnosis', 'Hydroxyurea prescription',
                                  'MCV labs', 'ANC labs'),
                       domain_tbl = c('condition_occurrence', 'condition_occurrence', 'drug_exposure',
                                      'measurement_labs', 'measurement_labs'),
                       concept_field = c('condition_concept_id', 'condition_concept_id',
                                         'drug_concept_id', 'measurement_concept_id', 'measurement_concept_id'),
                       date_field = c('condition_start_date', 'condition_start_date', 'drug_exposure_start_date',
                                      'measurement_date', 'measurement_date'),
                       vocabulary_field = c(NA, NA, NA, NA, NA),
                       codeset_name = c('dx_sca', 'dx_scd_no_sca', 'rx_hydroxyurea', 'lab_mcv', 'lab_anc'))

readr::write_csv(cnc_sp_input, 'specs/input_cnc_sp.csv')


##' **Patient Facts**

mcv_cids <- read_codeset('lab_mcv') %>% pull(concept_id) %>% paste(collapse = ', ')
heme_cids <- read_codeset('lab_serum_hemoglobin') %>% pull(concept_id) %>% paste(collapse = ', ')
anc_cids <- read_codeset('lab_anc') %>% pull(concept_id) %>% paste(collapse = ', ')

pf_domain_input <- tibble(domain = c('procedures', 'all drugs', 'prescription drugs',
                                     'diagnoses', 'all labs', 'mcv labs', 'hemoglobin labs',
                                     'anc labs'),
                          domain_tbl = c('procedure_occurrence', 'drug_exposure', 'drug_exposure',
                                         'condition_occurrence', 'measurement_labs', 'measurement_labs',
                                         'measurement_labs', 'measurement_labs'),
                          filter_logic = c(NA, NA, 'drug_type_concept_id == 38000177', NA, NA,
                                           paste0('measurement_concept_id %in% c(', mcv_cids, ')'),
                                           paste0('measurement_concept_id %in% c(', heme_cids, ')'),
                                           paste0('measurement_concept_id %in% c(', anc_cids, ')')))

readr::write_csv(pf_domain_input, 'specs/input_pf_domains.csv')

pf_visit_input <- tibble(visit_concept_id = c(9201, 9202, 2000000469, 581399, 44814711, 9203, 2000000048,
                                              9201, 9202, 2000000469, 581399, 44814711, 9203, 2000000048,
                                              2000000088,2000000104,44814710,2000001532,44814649),
                         visit_type = c('inpatient', 'outpatient', 'outpatient (non-9202)', 'outpatient (non-9202)',
                                        'outpatient (non-9202)', 'emergency department', 'emergency department',
                                        'hematology specialists','hematology specialists','hematology specialists',
                                        'hematology specialists','hematology specialists','hematology specialists',
                                        'hematology specialists','hematology specialists','hematology specialists',
                                        'hematology specialists','hematology specialists','hematology specialists'))

readr::write_csv(pf_visit_input, 'specs/input_pf_visits.csv')

##' **Source and Concept Vocabularies**

scv_domain_input <- tibble(domain = c('measurement_labs', 'provider', 'care_site'),
                           concept_field = c('measurement_concept_id', 'specialty_concept_id','specialty_concept_id'),
                           source_concept_field = c('measurement_source_value','specialty_source_value','specialty_source_value'),
                           date_field = c('measurement_date', 'visit_start_date', 'visit_start_date'),
                           vocabulary_field = c(NA, NA, NA))

readr::write_csv(scv_domain_input, 'specs/input_scv_domain.csv')


####' `Round 4 DQ`
##' **Sensitivity to Selection Criteria**
ssc_domains <- tibble(domain = c('conditions', 'procedures', 'drugs', 'labs', 'inpatient visits', 'ed visits', 'outpatient visits'),
                      domain_tbl = c('condition_occurrence', 'procedure_occurrence', 'drug_exposure', 'measurement_labs',
                                     'visit_occurrence', 'visit_occurrence', 'visit_occurrence'),
                      concept_field = c('condition_concept_id', 'procedure_concept_id', 'drug_concept_id', 'measurement_concept_id',
                                        'visit_concept_id', 'visit_concept_id', 'visit_concept_id'),
                      date_field = c('condition_start_date', 'procedure_date', 'drug_exposure_start_date', 'measurement_date',
                                     'visit_start_date', 'visit_start_date', 'visit_start_date'),
                      vocabulary_field = c(NA, NA, NA, NA, NA, NA, NA),
                      filter_logic = c(NA, NA, NA, NA, "visit_concept_id == 9201", "visit_concept_id %in% c(9203, 2000000048)", "visit_concept_id %in% c(9202, 581399)"))

readr::write_csv(ssc_domains, 'specs/ssc_domains.csv')

ssc_outcomes <- read_codeset('rx_hydroxyurea') %>%
  mutate(variable = 'Hydroxyurea',
         domain = 'drugs') %>%
  union(read_codeset('lab_anc') %>%
          mutate(variable = 'ANC labs',
                 domain = 'labs')) %>%
  union(read_codeset('lab_mcv') %>%
          select(-c(domain_name, concept_class)) %>%
          rename('vocabulary_id' = 'vocabulary_name') %>%
          mutate(variable = 'MCV labs',
                 domain = 'labs')) #%>%
  # union(read_codeset('rx_opioids') %>%
  #         rename('cluster' = 'ingredient') %>%
  #         mutate(variable = 'Opioids',
  #                domain = 'drugs'))

readr::write_csv(ssc_outcomes, 'specs/ssc_outcomes.csv')

#' **Clinical Events and Specialties**
#' Use input from R2 but add transcranial doppler procedures

cnc_sp_input_r4 <- read_codeset('input_cnc_sp', 'ccccc') %>%
  add_row(domain = 'Transcranial Doppler',
          domain_tbl = 'procedure_occurrence',
          concept_field = 'procedure_concept_id',
          date_field = 'procedure_date', 
          vocabulary_field = NA_character_,
          codeset_name = 'px_transcranial_doppler')

readr::write_csv(cnc_sp_input_r4, 'specs/input_cnc_sp_r4.csv')

#' **Quantitative Variable Distribution**

qvd_input_r4 <- tibble(value_name = c('ANC (per microliter)', 'ANC (thousand per microliter)',
                                      'ANC (no unit)', 'ANC (percent)', 'ANC (microliter)',
                                      'ANC (cells per microliter)', 'ANC (thousand per cubic millimeter)',
                                      'ANC (per cubic millimeter)', 'ANC (billion per liter)', 'MCV (femtoliter)',
                                      'MCV (no unit)', 'SCD Quant (percent)', 'SCD Quant (no unit)'),
                       domain_tbl = c('measurement_labs','measurement_labs','measurement_labs','measurement_labs','measurement_labs',
                                      'measurement_labs','measurement_labs','measurement_labs','measurement_labs','measurement_labs',
                                      'measurement_labs','measurement_labs','measurement_labs'),
                       value_field = c('value_as_number','value_as_number','value_as_number','value_as_number',
                                       'value_as_number','value_as_number','value_as_number','value_as_number',
                                       'value_as_number','value_as_number','value_as_number','value_as_number','value_as_number'),
                       date_field = c('measurement_date','measurement_date','measurement_date','measurement_date','measurement_date',
                                      'measurement_date','measurement_date','measurement_date','measurement_date','measurement_date',
                                      'measurement_date','measurement_date','measurement_date'),
                       concept_field = c('measurement_concept_id','measurement_concept_id','measurement_concept_id','measurement_concept_id',
                                         'measurement_concept_id','measurement_concept_id','measurement_concept_id','measurement_concept_id',
                                         'measurement_concept_id','measurement_concept_id','measurement_concept_id', 'measurement_concept_id',
                                         'measurement_concept_id'),
                       codeset_name = c('lab_anc','lab_anc','lab_anc','lab_anc','lab_anc','lab_anc','lab_anc','lab_anc',
                                        'lab_anc', 'lab_mcv', 'lab_mcv', 'lab_scd', 'lab_scd'),
                       filter_logic = c('unit_concept_id == 8647', 'unit_concept_id == 8848', 'unit_concept_id %in% c(0, 44814650)',
                                        'unit_concept_id == 8554', 'unit_concept_id == 9665', 'unit_concept_id == 8784',
                                        'unit_concept_id == 8961', 'unit_concept_id == 8785', 'unit_concept_id == 9444',
                                        'unit_concept_id == 8583', 'unit_concept_id %in% c(0, 44814650)', 'unit_concept_id == 8554',
                                        'unit_concept_id %in% c(0, 44814650)'))

readr::write_csv(qvd_input_r4, 'specs/input_qvd_r4.csv')

#' **Categorical Variable Distributions**

cvd_input_r4 <- tibble(domain = c('measurement_labs'),
                       concept_field = c('measurement_concept_id'),
                       vs_field = c('unit_concept_id'),
                       date_field = c('measurement_date')) 
readr::write_csv(cvd_input_r4, 'specs/input_cvd_r4.csv')


####' `Round 5 DQ`

#' **Patient Facts**
hydrx_codes <- read_codeset('rx_hydroxyurea') %>% pull(concept_id) %>% paste(collapse = ', ')

pf_input_r5 <- read_codeset('input_pf_domains', 'ccccc') %>%
  add_row(domain = 'hydroxyurea',
          domain_tbl = 'drug_exposure',
          filter_logic = paste0('drug_concept_id %in% c(', hydrx_codes, ')'))

readr::write_csv(pf_input_r5, 'specs/input_pf_domains_r5.csv')


#' **Expected Variables Present**

evp_input_r5 <- tibble(variable = c('Hemoglobin Labs', 'ANC Labs', 'MCV Labs', 'Hydroxyurea'),
                       domain_tbl = c('measurement_labs', 'measurement_labs', 'measurement_labs', 'drug_exposure'),
                       concept_field = c('measurement_concept_id', 'measurement_concept_id', 'measurement_concept_id',
                                         'drug_concept_id'),
                       date_field = c('measurement_date', 'measurement_date', 'measurement_date', 'drug_exposure_start_date'),
                       codeset_name = c('lab_serum_hemoglobin', 'lab_anc', 'lab_mcv', 'rx_hydroxyurea'),
                       filter_logic = c(NA, NA, NA, NA))

readr::write_csv(evp_input_r5, 'specs/input_evp_r5.csv')
