
source('code/addon_pedsnet_minicdm.R')

cdm_cohort <- results_tbl("round2_cohort")

personal_tbls <- personal_tables() %>% filter(output_name %in% c('condition_occurrence',
                                                                 'drug_exposure',
                                                                 'measurement_labs',
                                                                 'measurement_vitals',
                                                                 'measurement_anthro',
                                                                 'visit_occurrence',
                                                                 'person',
                                                                 'procedure_occurrence'))

impersonal_tbls <- impersonal_tables() %>% filter(output_name %in% c('care_site',
                                                                     'provider'))

mini_cdm <- build_mini_cdm(cohort=cdm_cohort,
                           clean=FALSE,
                           personal=personal_tbls,
                           impersonal=impersonal_tbls,
                           materialize=TRUE)

#############################################################################
config('cdm_schema', 'ssdqa_paper3')
config('table_names', list(
  'provider' = 'cdm_provider',
  'care_site' = 'cdm_care_site',
  'measurement_labs' = 'cdm_measurement_labs'
))

## Remap specialties based on DQ results
unmapped_specs <- read_codeset('unmapped_specialty') %>%
  pull(concept_id)

provider_remap <- cdm_tbl('provider') %>%
  rename('og_specialty_concept_id' = 'specialty_concept_id') %>%
  mutate(eval_name = tolower(specialty_source_value),
         specialty_concept_id = case_when(str_like(eval_name, '%haem%') & 
                                            og_specialty_concept_id %in% unmapped_specs ~ 2000000789L,
                                          str_like(eval_name, '%hema%') & 
                                            og_specialty_concept_id %in% unmapped_specs ~ 2000000789L,
                                          str_like(eval_name, '%heme%') & 
                                            og_specialty_concept_id %in% unmapped_specs ~ 2000000789L,
                                          TRUE ~ og_specialty_concept_id)) %>%
  select(-eval_name)
output_tbl(provider_remap, 'cdm_provider_remap')
  
caresite_remap <- cdm_tbl('care_site') %>%
  rename('og_specialty_concept_id' = 'specialty_concept_id') %>%
  mutate(eval_name = tolower(specialty_source_value),
         specialty_concept_id = case_when(str_like(eval_name, '%haem%') & 
                                            og_specialty_concept_id %in% unmapped_specs ~ 2000000789L,
                                          str_like(eval_name, '%hema%') & 
                                            og_specialty_concept_id %in% unmapped_specs ~ 2000000789L,
                                          str_like(eval_name, '%heme%') & 
                                            og_specialty_concept_id %in% unmapped_specs ~ 2000000789L,
                                          TRUE ~ og_specialty_concept_id)) %>%
  select(-eval_name)
output_tbl(caresite_remap, 'cdm_care_site_remap')

## remap labs based on dq results
unmapped_labs <- read_codeset('unmapped_labs') %>%
  pull(concept_id)

labs_remap <- cdm_tbl('measurement_labs') %>%
  rename('og_measurement_concept_id' = 'measurement_concept_id') %>%
  mutate(eval_name = tolower(measurement_source_value),
         measurement_concept_id = case_when(str_like(eval_name, '%hemoglobin quant%') & 
                                              og_measurement_concept_id %in% unmapped_labs ~ 2000000999L,
                                            str_like(eval_name, '%hb s|%') & 
                                              og_measurement_concept_id %in% unmapped_labs ~ 2000000999L,
                                            str_like(eval_name, '%sickle cell%') & 
                                              og_measurement_concept_id %in% unmapped_labs ~ 2000000999L,
                                            str_like(eval_name, '%hemoglobin s %') & 
                                              og_measurement_concept_id %in% unmapped_labs ~ 2000000999L,
                                            str_like(eval_name, '%electropheresis%') & 
                                              og_measurement_concept_id %in% unmapped_labs ~ 2000000999L,
                                            str_like(eval_name, '%electrophoresis%') & 
                                              og_measurement_concept_id %in% unmapped_labs ~ 2000000999L,
                                            TRUE ~ og_measurement_concept_id)) %>%
  select(-eval_name) %>%
  mutate(value_as_number = as.numeric(value_as_number))
output_tbl(labs_remap, 'cdm_measurement_labs_remap')

## remap drugs based on dq results
unmapped_drugs <- read_codeset('unmapped_labs') %>%
  pull(concept_id)

drugs_remap <- cdm_tbl('drug_exposure') %>%
  rename('og_drug_concept_id' = 'drug_concept_id') %>%
  mutate(eval_name = tolower(drug_source_value),
         drug_concept_id = case_when(str_like(eval_name, '%hydroxyurea%') &
                                      og_drug_concept_id %in% unmapped_drugs ~ 2000000998L,
                                      str_like(eval_name, '%droxia%') &
                                        og_drug_concept_id %in% unmapped_drugs ~ 2000000998L,
                                      str_like(eval_name, '%siklos%') &
                                        og_drug_concept_id %in% unmapped_drugs ~ 2000000998L,
                                      str_like(eval_name, '%hydrea%') &
                                        og_drug_concept_id %in% unmapped_drugs ~ 2000000998L,
                                      str_like(eval_name, '%hydroxyur%') &
                                        og_drug_concept_id %in% unmapped_drugs ~ 2000000998L,
                                      og_drug_concept_id == 19010309 &
                                       str_like(eval_name, '%hydroxyurea%') ~ 2000000998L,
                                     TRUE ~ og_drug_concept_id)) %>%
  select(-eval_name) #%>%
  #mutate(value_as_number = as.numeric(value_as_number))
output_tbl(drugs_remap %>% mutate(quantity = as.numeric(quantity)), 
           'cdm_drug_exposure_remap')

## update concept sets with remap concepts

# heme_spec <- read_codeset('hematology_specialty') %>%
#   add_row('concept_id' = 2000000789L,
#           'concept_code' = 'PEDSnet Remap',
#           'concept_name' = 'Remapped Hematology Specialty',
#           'vocabulary_id' = 'PEDSnet')
# readr::write_csv(heme_spec, 'specs/hematology_specialty.csv')

heme_labs <- read_codeset('lab_scd') %>%
  add_row('concept_id' = 2000000999L,
          'concept_code' = 'PEDSnet Remap',
          'concept_name' = 'Remapped SCD Labs',
          'vocabulary_id' = 'PEDSnet')
readr::write_csv(heme_labs, 'specs/lab_scd.csv')

hydrox <- read_codeset('rx_hydroxyurea') %>%
  add_row('concept_id' = 2000000998L,
          'concept_code' = 'PEDSnet Remap',
          'concept_name' = 'Remapped Hydroxyurea',
          'vocabulary_id' = 'PEDSnet')
readr::write_csv(hydrox, 'specs/rx_hydroxyurea.csv')
