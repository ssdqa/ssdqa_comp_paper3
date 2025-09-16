
##' **SSC**
### Comparator cohort: attrition the same up to step 3, then require 6 visits with hematologist
### and at least 2 years of fu

##' **comparison cohort**
heme_visits <- find_specialty(visits = cdm_tbl("visit_occurrence") %>%
                          filter(visit_start_date >= as.Date('2011-01-01') &
                                   visit_start_date <= as.Date('2024-12-31')) %>%
                          inner_join(results_tbl('round2_cohort')),
                        specialty_conceptset = load_codeset('hematology_specialty')) 

req6_heme <- heme_visits %>%
  group_by(site, person_id, start_date, end_date) %>%
  summarise(n_heme = n_distinct(visit_occurrence_id)) %>%
  filter(n_heme >= 6)

req_2yr_fu <- cdm_tbl('visit_occurrence') %>%
  inner_join(req6_heme) %>%
  group_by(site, person_id, start_date, end_date) %>%
  summarise(minvis = min(visit_start_date),
            maxvis = max(visit_start_date)) %>%
  mutate(fu = date_diff('day', minvis, maxvis)) %>%
  filter(fu >= 730)

output_tbl(req_2yr_fu %>% select(site, person_id, start_date, end_date), 'ssc_comparison_cohort_r4')

#' `Multi Site, Exploratory, Cross-Sectional`
ssc_ms_exp_cs_r4 <- ssc_process(base_cohort = results_tbl('sca_attrition_cohort_r4'),
                                alt_cohorts = list('6 Heme Visits + 2 Yrs FU' = results_tbl('ssc_comparison_cohort_r4')),
                                omop_or_pcornet = 'omop',
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                provider_tbl = cdm_tbl('provider'),
                                care_site_tbl = cdm_tbl('care_site'),
                                specialty_concepts = read_codeset('hematology_specialty'),
                                outcome_concepts = ssc_outcomes,
                                domain_tbl = ssc_domains,
                                domain_select = ssc_domains %>% pull(domain)
                                )

postgres_session$output_tbl(ssc_ms_exp_cs_r4$summary_values, 'ssc_ms_exp_cs_r4')
postgres_session$output_tbl(ssc_ms_exp_cs_r4$cohort_overlap, 'ssc_ms_exp_cs_overlap_r4')

##' **Categorical Distributions**
##' need to get this code from kim

#' `Single Site, Exploratory Cross-Sectional`

##' **CNC-SP**
##' rerun with remapped specialties
##' scd diagnoses, hydroxyurea rx, transcranial doppler, mcv, anc

#' `Multi Site, Exploratory, Cross-Sectional`
### provider specialties
cnc_sp_ms_exp_cs_r4_pv <- cnc_sp_process(cohort = results_tbl('sca_attrition_cohort_r4'),
                                         omop_or_pcornet = 'omop',
                                         multi_or_single_site = 'multi',
                                         anomaly_or_exploratory = 'exploratory',
                                         codeset_tbl = read_codeset('input_cnc_sp_r4', 'cccc'),
                                         care_site = FALSE,
                                         provider = TRUE)

postgres_session$output_tbl(cnc_sp_ms_exp_cs_r4_pv$cnc_sp_process_output, 
                            'cnc_sp_ms_exp_cs_r4_pv')

### care site specialties
cnc_sp_ms_exp_cs_r4_cs <- cnc_sp_process(cohort = results_tbl('sca_attrition_cohort_r4'),
                                         omop_or_pcornet = 'omop',
                                         multi_or_single_site = 'multi',
                                         anomaly_or_exploratory = 'exploratory',
                                         codeset_tbl = read_codeset('input_cnc_sp_r4', 'cccc'),
                                         care_site = TRUE,
                                         provider = FALSE)

postgres_session$output_tbl(cnc_sp_ms_exp_cs_r4_cs$cnc_sp_process_output, 
                            'cnc_sp_ms_exp_cs_r4_cs')

## specialty names (combined)
spec_names <- cnc_sp_ms_exp_cs_r4_pv$cnc_sp_process_names %>%
  union(cnc_sp_ms_exp_cs_r4_pv$cnc_sp_process_names) 
postgres_session$output_tbl(spec_names, 'cnc_sp_specialty_names_r4')

# postgres_session$results_tbl('cnc_sp_specialty_names_r4') %>%
#   select(-specialty_concept_name) %>%
#   inner_join(postgres_session$vocabulary_tbl('concept') %>% select(concept_id, concept_name),
#              by = c('specialty_concept_id' = 'concept_id')) %>%
#   rename('specialty_concept_name' = 'concept_name') %>%
#   mutate(specialty_concept_name = ifelse(specialty_concept_id == 2000000789, 'Remapped Hematology Specialty', 
#                                          specialty_concept_name)) %>%
#   collect() %>%
#   readr::write_csv('specs/cnc_sp_specialty_names_r4.csv')

## output again after naming
## the remapped hematology code is labelled separately
## should look at both separate and included in broader heme category
read_codeset('cnc_sp_specialty_names_r4') %>%
  postgres_session$output_tbl('cnc_sp_specialty_names_r4')

##' **QVD**
##' distribution of quantitative labs for each unit
##' use unit as site, loop through institutions? or just do combined?

#' `Multi Site, Exploratory, Cross-Sectional`
#' `Multi Site, Exploratory, Longitudinal`


##' **PF**
##' Run for ALL visits, heme visits, ED/IP visits
##' rebuild heme visits table with remapped specialties
##' same input otherwise from round2

#' `Multi Site, Exploratory, Cross-Sectional`
#' `Multi Site, Anomaly Detection, Cross-Sectional`
#' `Multi Site, Anomaly Detection, Longitudinal`