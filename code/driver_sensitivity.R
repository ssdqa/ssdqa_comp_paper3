
## never treated

blah <- results_tbl('did_primary_vars') %>%
  group_by(site, person_id) %>%
  mutate(never_treated = ifelse(any(treated_50 == 1L), FALSE, TRUE),
         yrs_treated = sum(treated_50)) %>%
  #ungroup() %>%
  mutate(mcv_thresh = case_when(age_year < 1 ~ baseline_mcv * 1.1,
                             age_year >= 1 & age_year < 4 ~ 
                               (baseline_mcv * 1.1) + 4,
                             age_year >= 4 ~ (baseline_mcv * 1.1) + 6)) %>%
  mutate(adherant = case_when(never_treated ~ TRUE,
                              avg_mcv >= mcv_thresh ~ TRUE,
                              yrs_treated == 1L ~ TRUE,
                              TRUE ~ FALSE)) %>%
  select(site, person_id, age_year, treated_50, never_treated, yrs_treated,
         baseline_mcv, avg_mcv, mcv_thresh, adherant) %>% collect()
  


