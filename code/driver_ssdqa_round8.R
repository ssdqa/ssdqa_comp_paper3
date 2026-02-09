
## pull drug metadata "codesets"
refills <- cdm_tbl('drug_exposure') %>%
  filter(!is.na(refills)) %>%
  distinct(refills) %>%
  rename('concept_id' = 'refills') %>%
  collect()
readr::write_csv(refills, 'specs/meta_refills.csv')

freq <- cdm_tbl('drug_exposure') %>%
  filter(!is.na(frequency)) %>%
  distinct(frequency) %>%
  rename('concept_id' = 'frequency') %>%
  collect()
readr::write_csv(freq, 'specs/meta_frequency.csv')

quant <- cdm_tbl('drug_exposure') %>%
  filter(!is.na(quantity)) %>%
  distinct(quantity) %>%
  mutate(quantity = as.numeric(quantity)) %>%
  rename('concept_id' = 'quantity') %>%
  collect()
readr::write_csv(quant, 'specs/meta_quantity.csv')

days <- cdm_tbl('drug_exposure') %>%
  filter(!is.na(days_supply)) %>%
  distinct(days_supply) %>%
  #mutate(quantity = as.numeric(quantity)) %>%
  rename('concept_id' = 'days_supply') %>%
  collect()
readr::write_csv(days, 'specs/meta_days_supply.csv')

## EVP

hu_drug_tbl <- cdm_tbl('drug_exposure') %>% 
  inner_join(load_codeset('rx_hydroxyurea'), 
             by = c('drug_concept_id' = 'concept_id'))

output_tbl(hu_drug_tbl, 'de_hydroxyurea')

###' `Multi-Site, Exploratory, Cross-Sectional`

evp_ms_exp_cs_r8 <- evp_process(cohort = results_tbl("sca_round7_cohort"),
                                omop_or_pcornet = 'omop',
                                evp_variable_file = read_codeset('input_evp_r8', 'cccc'),
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                time = FALSE)

postgres_session$output_tbl(evp_ms_exp_cs_r8, 'evp_ms_exp_cs_r8')

evp_ms_exp_la_r8 <- evp_process(cohort = results_tbl("sca_round7_cohort"),
                                omop_or_pcornet = 'omop',
                                evp_variable_file = read_codeset('input_evp_r8', 'cccc'),
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                time = TRUE,
                                time_period = 'year',
                                time_span = c('2011-01-01', '2025-01-01'))

postgres_session$output_tbl(evp_ms_exp_la_r8, 'evp_ms_exp_la_r8')

## QVD
#' `Multi Site, Exploratory, Cross-Sectional`
qvd_ms_exp_cs_r8 <- qvd_process(cohort = results_tbl('sca_round7_cohort'),
                                omop_or_pcornet = 'omop',
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                qvd_value_file = read_codeset('input_qvd_r8', 'cccc'))

postgres_session$output_tbl(qvd_ms_exp_cs_r8, 'qvd_ms_exp_cs_r8')


#' `Multi Site, Exploratory, Longitudinal`
qvd_ms_exp_la_r8 <- qvd_process(cohort = results_tbl('sca_round7_cohort'),
                                omop_or_pcornet = 'omop',
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                time = TRUE,
                                time_period = 'year',
                                time_span = c('2011-01-01', '2025-01-01'),
                                qvd_value_file = read_codeset('input_qvd_r8', 'cccc'))

postgres_session$output_tbl(qvd_ms_exp_la_r8, 'qvd_ms_exp_la_r8')


## PRC
#' `Multi Site, Exploratory, Cross-Sectional`

prc_ms_exp_cs_r8 <- prc_process(cohort = results_tbl('sca_round7_cohort'),
                                omop_or_pcornet = 'omop',
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                time = FALSE,
                                prc_event_file = read_codeset('input_prc_r8', 'cccc'))

postgres_session$output_tbl(prc_ms_exp_cs_r8, 'prc_ms_exp_cs_r8')
