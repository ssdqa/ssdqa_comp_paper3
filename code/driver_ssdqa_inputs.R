
##' **Clinical Events & Specialties**
####' `Round 2 DQ`
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

scv_domain_input <- tibble(domain = 'measurement_labs',
                           concept_field = 'measurement_concept_id',
                           source_concept_field = 'measurement_source_value',
                           date_field = 'measurement_date',
                           vocabulary_field = NA)

readr::write_csv(scv_domain_input, 'specs/input_scv_domain.csv')
