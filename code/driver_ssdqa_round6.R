
## identify source values for labs / drug of interest for SCV analysis

### ANC
anc_test <- cdm_tbl('measurement_labs') %>%
  inner_join(load_codeset('lab_anc'), by = c('measurement_concept_id' = 'concept_id')) %>%
  distinct(site, measurement_concept_id, measurement_source_value) %>%
  collect()

anc_svs <- cdm_tbl('measurement_labs') %>%
  filter((str_like(tolower(measurement_source_value), '%neutrophil%') | 
           str_like(tolower(measurement_source_value), '%neutrophils%') |
           str_like(tolower(measurement_source_value), '%absolute neut%')) &
           !str_like(tolower(measurement_source_value), '%anti-neutrophil%') &
           !str_like(tolower(measurement_source_value), '%segmented neutrophil%'))

anc_svs %>% distinct(measurement_source_value) %>% collect() %>%
  readr::write_csv('specs/anc_lab_source_values_r6.csv')

### MCV
mcv_test <- cdm_tbl('measurement_labs') %>%
  inner_join(load_codeset('lab_mcv'), by = c('measurement_concept_id' = 'concept_id')) %>%
  distinct(site, measurement_concept_id, measurement_source_value) %>%
  collect()

mcv_svs <- cdm_tbl('measurement_labs') %>%
  filter(str_like(tolower(measurement_source_value), '%mcv%') | 
           str_like(tolower(measurement_source_value), '%mean corpuscular volume%') |
           str_like(tolower(measurement_source_value), '%mean cell volume%'))

mcv_svs %>% distinct(measurement_source_value) %>% collect() %>%
  readr::write_csv('specs/mcv_lab_source_values_r6.csv')

### hemoglobin
heme_test <- cdm_tbl('measurement_labs') %>%
  inner_join(load_codeset('lab_serum_hemoglobin'), by = c('measurement_concept_id' = 'concept_id')) %>%
  distinct(site, measurement_concept_id, measurement_source_value) %>%
  collect()

heme_svs <- cdm_tbl('measurement_labs') %>%
  filter(str_like(tolower(measurement_source_value), '%718-7%') |
           str_like(tolower(measurement_source_value), 'hemoglobin') | 
           str_like(tolower(measurement_source_value), '%hemoglobin\\|%') |
           str_like(tolower(measurement_source_value), '%total hemoglobin%') |
           (str_like(tolower(measurement_source_value), '%cbc%') & str_like(tolower(measurement_source_value), '%hemoglobin%')) |
           (str_like(tolower(measurement_source_value), '%cbc%') & str_like(tolower(measurement_source_value), '%hgb%')) |
           str_like(tolower(measurement_source_value), '%hb tot%'))

heme_svs %>% distinct(measurement_source_value) %>% collect() %>%
  readr::write_csv('specs/heme_lab_source_values_r6.csv')

### hydroxyurea
hdrx_test <- cdm_tbl('drug_exposure') %>%
  inner_join(load_codeset('rx_hydroxyurea'), by = c('drug_concept_id' = 'concept_id')) %>%
  distinct(site, drug_concept_id, drug_source_value) %>%
  collect()

hdrx_svs <- cdm_tbl('drug_exposure') %>%
  filter(str_like(tolower(drug_source_value), '%hydroxyurea%') | 
           str_like(tolower(drug_source_value), '%droxia%') |
           str_like(tolower(drug_source_value), '%siklos%') |
           str_like(tolower(drug_source_value), '%hydrea%') |
           str_like(tolower(drug_source_value), '%hydroxyur%'))

hdrx_svs %>% distinct(drug_source_value) %>% collect() %>%
  readr::write_csv('specs/hydroxyurea_source_values_r6.csv')


##' `Source & Concept Vocabularies`
#### Hemoglobin
scv_ms_exp_cs_heme <- scv_process(cohort = results_tbl('sca_attrition_cohort_r5'),
                                  concept_set = read_codeset('heme_lab_source_values_r6', 'c') %>%
                                    rename('concept_id' = 'measurement_source_value'),
                                  omop_or_pcornet = 'omop',
                                  multi_or_single_site = 'multi',
                                  anomaly_or_exploratory = 'exploratory',
                                  time = FALSE,
                                  domain_tbl = read_codeset('input_scv_domains_r6', 'ccccc'),
                                  code_type = 'source',
                                  code_domain = 'measurement_labs')

heme_cs_flag <- scv_ms_exp_cs_heme %>%
  left_join(read_codeset('lab_serum_hemoglobin') %>%
              select(concept_id) %>% mutate(in_concept_set = TRUE))
  

postgres_session$output_tbl(heme_cs_flag, 'scv_ms_exp_cs_r6_heme')

#### ANC
scv_ms_exp_cs_anc <- scv_process(cohort = results_tbl('sca_attrition_cohort_r5'),
                                  concept_set = read_codeset('anc_lab_source_values_r6', 'c') %>%
                                    rename('concept_id' = 'measurement_source_value'),
                                  omop_or_pcornet = 'omop',
                                  multi_or_single_site = 'multi',
                                  anomaly_or_exploratory = 'exploratory',
                                  time = FALSE,
                                  domain_tbl = read_codeset('input_scv_domains_r6', 'ccccc'),
                                  code_type = 'source',
                                  code_domain = 'measurement_labs')

anc_cs_flag <- scv_ms_exp_cs_anc %>%
  left_join(read_codeset('lab_anc') %>%
              select(concept_id) %>% mutate(in_concept_set = TRUE))


postgres_session$output_tbl(anc_cs_flag, 'scv_ms_exp_cs_r6_anc')

#### MCV
scv_ms_exp_cs_mcv <- scv_process(cohort = results_tbl('sca_attrition_cohort_r5'),
                                 concept_set = read_codeset('mcv_lab_source_values_r6', 'c') %>%
                                   rename('concept_id' = 'measurement_source_value'),
                                 omop_or_pcornet = 'omop',
                                 multi_or_single_site = 'multi',
                                 anomaly_or_exploratory = 'exploratory',
                                 time = FALSE,
                                 domain_tbl = read_codeset('input_scv_domains_r6', 'ccccc'),
                                 code_type = 'source',
                                 code_domain = 'measurement_labs')

mcv_cs_flag <- scv_ms_exp_cs_mcv %>%
  left_join(read_codeset('lab_mcv') %>%
              select(concept_id) %>% mutate(in_concept_set = TRUE))


postgres_session$output_tbl(mcv_cs_flag, 'scv_ms_exp_cs_r6_mcv')

#### Hydroxyurea
scv_ms_exp_cs_hdrx <- scv_process(cohort = results_tbl('sca_attrition_cohort_r5'),
                                 concept_set = readr::read_csv('specs/hydroxyurea_source_values_r6.csv', trim_ws = FALSE) %>%
                                   rename('concept_id' = 'drug_source_value'),
                                 omop_or_pcornet = 'omop',
                                 multi_or_single_site = 'multi',
                                 anomaly_or_exploratory = 'exploratory',
                                 time = FALSE,
                                 domain_tbl = read_codeset('input_scv_domains_r6', 'ccccc'),
                                 code_type = 'source',
                                 code_domain = 'drug_exposure')

hdrx_cs_flag <- scv_ms_exp_cs_hdrx %>%
  left_join(read_codeset('rx_hydroxyurea') %>%
              select(concept_id) %>% mutate(in_concept_set = TRUE))


postgres_session$output_tbl(hdrx_cs_flag, 'scv_ms_exp_cs_r6_hdrx')
