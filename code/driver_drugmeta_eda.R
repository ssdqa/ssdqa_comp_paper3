
## fill in default days supply
## if days supply is NULL & there is > 120 day gap between rx: 120
## if days supply is NULL & there is < 120 day gap between rx: diff
## if days supply is not NULL: days_supply

hu_rx_tbl <- cdm_tbl("de_hydroxyurea") %>%
  inner_join(results_tbl('sca_round7_cohort')) %>%
  filter(drug_type_concept_id %in% c(38000177)) %>%
  select(site, person_id, visit_occurrence_id, drug_exposure_start_date, drug_exposure_end_date,
         drug_concept_id, drug_type_concept_id, drug_concept_name, refills, 
         days_supply, quantity, frequency) %>%
  arrange(site, person_id, drug_exposure_start_date) %>%
  group_by(site, person_id) %>%
  # mutate(drug_exposure_end_date = ifelse(is.na(drug_exposure_end_date),
  #                                        lead(drug_exposure_start_date),
  #                                        drug_exposure_end_date)) %>%
  mutate(length_rx = date_diff('day', drug_exposure_start_date, drug_exposure_end_date),
         time_bw_rx = date_diff('day', lag(drug_exposure_end_date), drug_exposure_start_date))



days_supply_update = case_when(!is.na(days_supply) ~ days_supply,
                               is.na(days_supply) & length_rx == 0 ~ 0,
                               is.na(days_supply) & length_rx > 120 ~ 120)


hu_ip_tbl <- cdm_tbl("de_hydroxyurea") %>%
  inner_join(results_tbl('sca_round7_cohort')) %>%
  filter(drug_type_concept_id %in% c(38000180)) %>%
  select(site, person_id, visit_occurrence_id, drug_exposure_start_date, drug_exposure_end_date,
         drug_concept_id, drug_type_concept_id, drug_concept_name, refills, 
         days_supply, quantity, frequency) %>%
  arrange(site, person_id, drug_exposure_start_date) %>%
  group_by(site, person_id) %>%
  # mutate(drug_exposure_end_date = ifelse(is.na(drug_exposure_end_date),
  #                                        lead(drug_exposure_start_date),
  #                                        drug_exposure_end_date)) %>%
  mutate(length_rx = date_diff('day', drug_exposure_start_date, drug_exposure_end_date),
         time_bw_rx = date_diff('day', lag(drug_exposure_end_date), drug_exposure_start_date)) %>%
  collect()

## default to date-based length of rx via end date
## if end date is null, use start of next rx; if > 120, default to 120; if <= 120, use


## incorporating ip admins
### 


### censor patients with 2 transfusions w/n 3+ weeks & < 6 weeks, bone marrow transplant,
### gene therapy (stem cell treatment), voxelotor & crizanlizumab exposure


