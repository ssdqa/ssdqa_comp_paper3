
## fill in default days supply
## if days supply is NULL & there is > 120 day gap between rx: 120
## if days supply is NULL & there is < 120 day gap between rx: diff
## if days supply is not NULL: days_supply

hu_rx_tbl <- cdm_tbl("de_hydroxyurea") %>%
  inner_join(results_tbl('final_cohort_censored')) %>%
  filter(drug_type_concept_id %in% c(38000177)) %>%
  select(site, person_id, first_sca_dx, end_date, censorship_reason, visit_occurrence_id, 
         drug_exposure_start_date, drug_exposure_end_date,
         drug_concept_id, drug_type_concept_id, drug_concept_name, refills, 
         days_supply, quantity, frequency) %>%
  arrange(site, person_id, drug_exposure_start_date) %>%
  group_by(site, person_id) %>%
  mutate(drug_exposure_end_date = ifelse(is.na(drug_exposure_end_date),
                                         lead(drug_exposure_start_date),
                                         drug_exposure_end_date)) %>%
  mutate(length_rx = date_diff('day', drug_exposure_start_date, drug_exposure_end_date),
         time_bw_rx = date_diff('day', lag(drug_exposure_end_date), drug_exposure_start_date),
         drug_exposure_end_date = ifelse(is.na(drug_exposure_end_date), date_add('day', 120L, drug_exposure_start_date), drug_exposure_end_date)) %>%
  filter(length_rx > 0, time_bw_rx >= 0) %>% 
  mutate(days_supply = case_when(length_rx > 120 ~ 120L,
                                 length_rx <= 120 ~ length_rx,
                                 TRUE ~ NA),
         drug_exposure_end_date = case_when(length_rx > 120 ~ date_add('day', 120L, drug_exposure_start_date),
                                            length_rx <= 120 ~ drug_exposure_end_date,
                                            TRUE ~ NA)) %>%
  filter(drug_exposure_start_date >= first_sca_dx & drug_exposure_start_date < end_date) %>%
  collect()


hu_ip_tbl <- cdm_tbl("de_hydroxyurea") %>%
  inner_join(results_tbl('final_cohort_censored')) %>%
  filter(drug_type_concept_id %in% c(38000180)) %>%
  select(site, person_id, first_sca_dx, end_date, censorship_reason, visit_occurrence_id, 
         drug_exposure_start_date, drug_exposure_end_date,
         drug_concept_id, drug_type_concept_id, drug_concept_name, refills, 
         days_supply, quantity, frequency) %>%
  arrange(site, person_id, drug_exposure_start_date) %>%
  group_by(site, person_id) %>%
  mutate(drug_exposure_end_date = ifelse(is.na(drug_exposure_end_date),
                                         drug_exposure_start_date,
                                         drug_exposure_end_date)) %>%
  mutate(length_rx = date_diff('day', drug_exposure_start_date, drug_exposure_end_date),
         time_bw_rx = date_diff('day', lag(drug_exposure_end_date), drug_exposure_start_date)) %>%
  filter(time_bw_rx >= 0) %>% 
  filter(drug_exposure_start_date >= first_sca_dx & drug_exposure_start_date < end_date) %>%
  collect()


combo <- hu_rx_tbl %>%
  union(hu_ip_tbl)

output_tbl(combo, 'de_hydroxyurea_windows', .chunk_size = 3000)


treated_visits <- cdm_tbl('visit_occurrence') %>%
  select(site, person_id, visit_occurrence_id, visit_start_date, visit_end_date,
         visit_concept_id) %>%
  inner_join(results_tbl('final_cohort_censored')) %>%
  filter(visit_start_date >= first_sca_dx & visit_start_date <= end_date) %>%
  left_join(results_tbl('de_hydroxyurea_windows') %>% 
              select(site, person_id,
                     drug_exposure_start_date, drug_exposure_end_date)) %>%
  mutate(hu_treated = case_when(visit_start_date >= drug_exposure_start_date & 
                                  visit_start_date <= drug_exposure_end_date ~ TRUE,
                                TRUE ~ FALSE)) %>%
  filter(hu_treated) %>%
  distinct(site, person_id, visit_occurrence_id, hu_treated)

all_visits <- cdm_tbl('visit_occurrence') %>%
  select(site, person_id, visit_occurrence_id, visit_start_date, visit_end_date,
         visit_concept_id) %>%
  inner_join(results_tbl('final_cohort_censored')) %>%
  filter(visit_start_date >= first_sca_dx) %>%
  left_join(treated_visits) %>%
  mutate(hu_label = case_when(visit_start_date > end_date ~ 'censored',
                              hu_treated == TRUE ~ 'hydroxyurea',
                              is.na(hu_treated) ~ 'no hydroxyurea',
                              TRUE ~ NA_character_))

output_tbl(all_visits, 'hu_visits_labelled')

## 4766530 (chop pt with a lot of back & forth)
## 15859837 (natl pt who stops HU without being censored)
## 

test_case <- all_visits %>% filter(person_id == 4766530) %>% collect()


View(all_visits %>% left_join(results_tbl('final_cohort_censored_v60')) %>% 
  filter(censorship_reason == "> 2 Year Gap Between Encounters") %>% 
  collect())

test_case %>% distinct(site, person_id, visit_start_date, hu_label) %>% 
  arrange(visit_start_date) %>% 
  ggplot(aes(x = visit_start_date, y = hu_label, color = hu_label)) + 
  geom_point(stat = 'identity')

