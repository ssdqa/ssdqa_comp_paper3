
tbls <- dbGetQuery(conn = trino_session$config('db_src'), "SELECT table_name 
           FROM information_schema.tables WHERE table_schema = 'ssdqa_paper3'") %>%
  pull(table_name)


tbls_drop <- tbls[!tbls %in% c('dx_sca', 'dx_scd_no_sca', 'rx_hydroxyurea', 'lab_mcv',
                               'lab_anc', 'round2_cohort', 'hematology_specialty', 'hematology_spec_visits',
                               'hematology_spec_visits_remap',
                               'pv_cs_w_date', 'sca_attrition_cohort_r4', 'ssc_comparison_cohort_r4')]
tbls_drop <- tbls_drop[!grepl('^cdm_', tbls_drop)]

for(i in tbls_drop){
  
  db_remove_table(db = trino_session$config('db_src'),
                  name = paste0('ssdqa_paper3.', i))
  
}
