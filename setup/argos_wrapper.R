
#' Initialize argos session
#'
#' @param session_name an arbitrary string label to identify your session
#' @param db_conn the database connection information; can either be a connection
#' object like those created by DBI::dbConnect OR the path to a JSON file containing
#' your connection information
#' @param is_json a boolean indicating whether db_conn is a file path pointing to a JSON
#' file or not
#' @param base_directory the base or working directory; in a project-oriented workflow, this 
#' will be the working directory established when opening the project.
#' @param specs_subdirectory the subdirectory within the base directory where any files to be used in the analysis
#' (i.e. concept sets) will be stored; defaults to `specs`
#' @param results_subdirectory the subdirectory within the base directory where results should be output; 
#' defaults to `results`
#' @param default_file_output a boolean indicating whether output_tbl should output a file by default or
#' if it should just output the results to the database; defaults to FALSE (i.e. no file output)
#' @param cdm_schema the schema on the database where the data in a CDM format is stored
#' @param results_schema the schema on the database where any results should be output
#' @param vocabulary_schema the schema on the database where vocabulary reference tables 
#' (i.e. the OHDSI vocabulary concept tables) are stored
#' @param results_tag if desired, a suffix to be appended onto results tables to help organize
#' project-specific output
#' @param cache_enabled a boolean value indicating whether repeated attempts to load the same
#' codeset (via load_codeset) should use a cached value rather than reloading; defaults to TRUE
#' @param retain_intermediates a boolean indicating whether intermediate/temporary tables should be
#' manifested and retained; defaults to FALSE
#' @param db_trace a boolean indicating whether the query log should include
#' detailed information about execution of SQL queries in the database 
#' (essentially a "verbose" argument); defaults to TRUE
#'
#' @returns will quietly load all exported argos functions into the environment and establish
#' the necessary configurations to allow them to operate; note that the argos session itself
#' will NOT appear in the global environment pane in the RStudio IDE
#' 
#' the connection information will print after this function is run to confirm connection to the database of choice
#' 
initialize_session <- function(session_name,
                               db_conn,
                               is_json = FALSE,
                               base_directory = getwd(),
                               specs_subdirectory = 'specs',
                               results_subdirectory = 'results',
                               default_file_output = FALSE,
                               cdm_schema = 'dcc_pedsnet',
                               results_schema,
                               vocabulary_schema = 'vocabulary',
                               results_tag = NULL,
                               cache_enabled = FALSE,
                               retain_intermediates = FALSE,
                               db_trace = TRUE){
  
  argos$public_methods$load_codeset <- function(name, col_types = NULL, table_name = name,
                                                indexes = list('concept_id'), full_path = FALSE,
                                                db = self$config('db_src'),
                                                .chunk_size = 5000) {
    
    if (self$config('cache_enabled')) {
      if (is.null(self$config('_codesets'))) self$config('_codesets', list())
      cache <- self$config('_codesets')
      if (! is.null(cache[[name]])) return(cache[[name]])
    }
    codes <-
      self$copy_to_new(db,
                       self$read_codeset(name, col_types = NULL,
                                         full_path = full_path),
                       name = table_name,
                       overwrite = TRUE,
                       indexes = indexes,
                       .chunk_size = .chunk_size)
    
    if (self$config('cache_enabled')) {
      cache[[name]] <- codes
      self$config('_codesets', cache)
    }
    
    codes
  }
  
  argos$public_methods$copy_to_new <- function (dest = config("db_src"), df, name = deparse(substitute(df)), 
                                                overwrite = TRUE, temporary = !config("retain_intermediates"), 
                                                ..., .chunk_size = 5000) 
  {
    name <- self$intermed_name(name, temporary = temporary)
    if (self$config("db_trace")) {
      message(" -> copy_to")
      start <- Sys.time()
      message(start)
      message("Data: ", deparse(substitute(df)))
      message("Table name: ", base::ifelse(packageVersion("dbplyr") < 
                                             "2.0.0", dbplyr::as.sql(name), dbplyr::as.sql(name, 
                                                                                           dbi_con(dest))), " (temp: ", temporary, ")")
      message("Data elements: ", paste(tbl_vars(df), collapse = ","))
      message("Rows: ", NROW(df))
    }
    if (overwrite && self$db_exists_table(dest, name)) {
      self$db_remove_table(dest, name)
    }
    dfsize <- tally(ungroup(df)) %>% pull(n)
    if (is.na(.chunk_size)) 
      .chunk_size <- dfsize
    cstart <- 1
    if (.chunk_size < dfsize) 
      cli::cli_progress_bar("Writing data", total = 100, format = "Writing data {cli::pb_bar} {cli::pb_percent}")
    while (cstart < dfsize || cstart == 1) {
      cend <- min(cstart + .chunk_size, dfsize)
      rslt <- dplyr::copy_to(dest = dest, df = slice(ungroup(df), 
                                                     cstart:cend), name = name, append = TRUE, overwrite = FALSE, 
                             temporary = temporary, ...)
      if (.chunk_size < dfsize) 
        cli::cli_progress_update(set = 100L * cend/dfsize)
      cstart <- cend + 1L
    }
    if (self$config("db_trace")) {
      end <- Sys.time()
      message(end, " ==> ", format(end - start))
    }
    rslt
  }
  
  assignInNamespace('find_fact_spec_conc_omop',
                    value = find_fact_spec_conc_omop <- function(cohort,
                                                                 visit_id,
                                                                 fact_codes,
                                                                 fact_tbl,
                                                                 care_site,
                                                                 provider,
                                                                 time=FALSE){
                      
                      if(!'cluster'%in%colnames(fact_codes)){fact_codes<-fact_codes%>%mutate(cluster=concept_name)}
                      if(!'category'%in%colnames(fact_codes)){fact_codes<-fact_codes%>%mutate(category='all')}
                      
                      message('Finding code occurrences')
                      fact_occurrences <-
                        fact_tbl %>% select(person_id, !!sym(visit_id), concept_id)%>%
                        inner_join(cohort) %>%
                        # select(person_id, concept_id,
                        #        visit_occurrence_id, site) %>%
                        inner_join(select(fact_codes,
                                          concept_id, concept_name,
                                          category, cluster)) 
                      
                      message('Finding specialties')
                      if(time){
                        if(visit_id == 'visit_occurrence_id'){
                          visits <- cdm_tbl('visit_occurrence') %>%
                            inner_join(cohort)%>%
                            filter(visit_start_date >= start_date,
                                   visit_start_date <= end_date) %>%
                            filter(visit_start_date>=time_start,
                                   visit_start_date<=time_end) %>%
                            select(visit_occurrence_id, visit_concept_id,
                                   visit_start_date, provider_id, care_site_id)
                        }else{
                          visits <- cdm_tbl('visit_detail') %>%
                            inner_join(cohort)%>%
                            filter(visit_detail_start_date >= start_date,
                                   visit_detail_start_date <= end_date) %>%
                            filter(visit_detail_start_date>=time_start,
                                   visit_detail_start_date<=time_end) %>%
                            select(visit_detail_id, visit_detail_concept_id,
                                   visit_detail_start_date, provider_id, care_site_id)
                        }
                      }else{
                        if(visit_id == 'visit_occurrence_id'){
                          visits <- cdm_tbl('visit_occurrence') %>%
                            inner_join(cohort)%>%
                            filter(visit_start_date >= start_date,
                                   visit_start_date <= end_date) %>%
                            select(visit_occurrence_id, visit_concept_id,
                                   visit_start_date, provider_id, care_site_id)
                        }else{
                          visits <- cdm_tbl('visit_detail') %>%
                            inner_join(cohort)%>%
                            filter(visit_detail_start_date >= start_date,
                                   visit_detail_start_date <= end_date) %>%
                            select(visit_detail_id, visit_detail_concept_id,
                                   visit_detail_start_date, provider_id, care_site_id)
                        }
                      }
                      
                      
                      if(care_site&provider){
                        pv_spec <- visits %>%
                          select(-care_site_id) %>%
                          inner_join(select(fact_occurrences, !!sym(visit_id)))%>%
                          left_join(select(cdm_tbl('provider'),c(provider_id, specialty_concept_id)),
                                    by = 'provider_id')%>%
                          rename(specialty_concept_id_pv=specialty_concept_id) 
                        
                        cs_spec <- visits %>%
                          select(-provider_id) %>%
                          inner_join(select(fact_occurrences,!!sym(visit_id)))%>%
                          left_join(select(cdm_tbl('care_site'),c(care_site_id, specialty_concept_id)),
                                    by = 'care_site_id')%>%
                          rename(specialty_concept_id_cs=specialty_concept_id) 
                        
                        spec_full <-
                          visits %>%
                          left_join(pv_spec) %>%
                          left_join(cs_spec) %>%
                          mutate(specialty_concept_id=case_when(!is.na(specialty_concept_id_pv)~specialty_concept_id_pv,
                                                                !is.na(specialty_concept_id_cs)~specialty_concept_id_cs,
                                                                TRUE~NA_integer_))%>%
                          select(-c(specialty_concept_id_pv,specialty_concept_id_cs, provider_id, care_site_id)) %>%
                          distinct() 
                        
                      }else if(provider&!care_site){
                        spec_full <- visits %>%
                          select(-care_site_id)%>%
                          inner_join(select(fact_occurrences, !!sym(visit_id)))%>%
                          left_join(select(cdm_tbl('provider'),c(provider_id, specialty_concept_id)),
                                    by = 'provider_id') %>%
                          distinct() %>% compute_new()
                      }else if(care_site&!provider){
                        spec_full <- visits %>%
                          select(-provider_id)%>%
                          inner_join(select(fact_occurrences, !!sym(visit_id)))%>%
                          left_join(select(cdm_tbl('care_site'),c(care_site_id, specialty_concept_id)),
                                    by = 'care_site_id') %>%
                          distinct() %>% compute_new()
                      }
                      
                      spec_final <-
                        inner_join(
                          spec_full,
                          fact_occurrences
                        ) 
                      
                      
                      return(spec_final)
                      
                    },
                    ns = 'clinicalevents.specialties')
  
  
  assignInNamespace(x = 'compute_pf_omop',
                    value = compute_pf_omop <- function(cohort,
                                                        pf_input_tbl,
                                                        grouped_list,
                                                        domain_tbl) {
                      
                      domain_results <- list()
                      domain_list <- split(domain_tbl, seq(nrow(domain_tbl)))
                      
                      
                      for (i in 1:length(domain_list)) {
                        
                        domain_name = domain_list[[i]]$domain
                        message(paste0('Starting domain ', domain_list[[i]]$domain))
                        
                        ## checks to see if the table needs to be filtered in any way;
                        ## allow for one filtering operation
                        if(! is.na(domain_list[[i]]$filter_logic)) {
                          domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]]$domain_tbl)) %>%
                            filter(!! rlang::parse_expr(domain_list[[i]]$filter_logic))
                        } else {domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]]$domain_tbl))}
                        
                        ## computes facts per patient by a named list of grouped variables
                        ## assumes person_id is part of named list
                        pf <-
                          pf_input_tbl %>%
                          inner_join(select(domain_tbl_use,
                                            visit_occurrence_id)) %>%
                          group_by(
                            !!! syms(grouped_list)
                          ) %>% summarise(total_strat_ct=n()) %>%
                          ungroup() %>%
                          mutate(domain=domain_name) %>%
                          mutate(k_mult = case_when(fu < 0.1 ~ 100,
                                                    fu >= 0.1 & fu < 1 ~ 10,
                                                    TRUE ~ 1),
                                 fact_ct_strat=ifelse(fu != 0,round(total_strat_ct/(fu * k_mult),2),0)) %>%
                          #select(-c(total_strat_ct, k_mult)) %>%
                          select(person_id,
                                 domain,
                                 fact_ct_strat) %>%
                          pivot_wider(names_from=domain,
                                      values_from=fact_ct_strat) %>%
                          right_join(cohort) %>%
                          relocate(person_id) %>%
                          collect()
                        
                        if(!any(colnames(pf) %in% domain_name)){
                          pf <- pf %>%
                            mutate(!!sym(domain_name) := NA_integer_)
                        }
                        
                        domain_results[[domain_name]] <- pf
                      }
                      
                      domain_results_left_join <-
                        reduce(.x=domain_results,
                               .f=left_join)
                    },
                    ns = 'patientfacts')
  
  assignInNamespace(x = 'compute_pf_for_fot_omop',
                    value = compute_pf_for_fot_omop <- function(cohort, pf_input_tbl,
                                                                grouped_list,
                                                                domain_tbl) {
                      
                      domain_results <- list()
                      domain_list <- split(domain_tbl, seq(nrow(domain_tbl)))
                      
                      
                      for (i in 1:length(domain_list)) {
                        
                        domain_name = domain_list[[i]]$domain
                        message(paste0('Starting domain ', domain_list[[i]]$domain))
                        
                        ## checks to see if the table needs to be filtered in any way;
                        ## allow for one filtering operation
                        if(! is.na(domain_list[[i]]$filter_logic)) {
                          domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]]$domain_tbl)) %>%
                            filter(!! rlang::parse_expr(domain_list[[i]]$filter_logic))
                        } else {domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]]$domain_tbl))}
                        
                        ## computes facts per patient by a named list of grouped variables
                        ## assumes person_id is part of named list
                        pf <-
                          pf_input_tbl %>%
                          inner_join(select(domain_tbl_use,
                                            visit_occurrence_id)) %>%
                          group_by(
                            !!! syms(grouped_list)
                          ) %>% summarise(total_strat_ct=n()) %>%
                          mutate(domain=domain_name) %>% ungroup()
                        
                        new_group <- grouped_list[! grouped_list %in% c('person_id')]
                        
                        pf_cohort_final <-
                          pf %>% right_join(select(cohort,
                                                   person_id)) %>%
                          distinct(person_id) %>% summarise(ct=n()) %>% pull()
                        
                        site_visit_ct_num <-
                          pf_input_tbl %>% summarise(ct=n_distinct(person_id)) %>%
                          pull()
                        
                        pf_final <-
                          pf %>% collect() %>% group_by(
                            !!! syms(new_group)
                          ) %>% group_by(domain, .add = TRUE) %>%
                          summarise(pts_w_fact=n(),
                                    sum_fact_ct=sum(total_strat_ct),
                                    median_fact_ct=median(total_strat_ct)) %>%
                          ungroup()
                        
                        
                        finalized <-
                          pf_final %>%
                          mutate(pt_ct_denom=pf_cohort_final,
                                 pts_w_visit=site_visit_ct_num) %>% collect()
                        
                        
                        domain_results[[domain_name]] <- finalized
                      }
                      
                      
                      reduce(.x=domain_results,
                             .f=dplyr::union)
                    },
                    ns = 'patientfacts')
  
  assignInNamespace('compute_demographic_summary_omop',
                    compute_demographic_summary_omop <- function (cohort_tbl, site_col, person_tbl = cdm_tbl("person"), 
                                                                  visit_tbl = cdm_tbl("visit_occurrence"), 
                                                                  demographic_mappings = sensitivityselectioncriteria::ssc_omop_demographics) 
                    {
                      demo_list <- split(demographic_mappings, seq(nrow(demographic_mappings)))
                      demo_rslt <- list()
                      for (i in 1:length(demo_list)) {
                        vals <- demo_list[[i]]$field_values %>% str_replace_all(" ", 
                                                                                "") %>% str_split(",") %>% unlist()
                        vals <- as.numeric(vals)
                        demographic <- person_tbl %>% inner_join(cohort_tbl) %>% 
                          mutate(demo_col = ifelse(!!sym(demo_list[[i]]$concept_field) %in% 
                                                     vals, TRUE, FALSE)) %>% select(!!sym(site_col), 
                                                                                    person_id, start_date, end_date, fu, cohort_id, 
                                                                                    demo_col) %>% rename(`:=`(!!sym(demo_list[[i]]$demographic), 
                                                                                                              demo_col)) %>% collect()
                        demo_rslt[[i]] <- demographic
                      }
                      demo_final <- purrr::reduce(.x = demo_rslt, .f = left_join)
                      new_person <- build_birth_date(cohort = cohort_tbl, person_tbl = person_tbl)
                      age_ced <- new_person %>% mutate(age_cohort_entry = as.numeric(as.Date(start_date) - 
                                                                                       birth_date), age_cohort_entry = round(age_cohort_entry/365.25, 
                                                                                                                             2)) %>% distinct(!!sym(site_col), person_id, age_cohort_entry)
                      if ("visit_start_date" %in% colnames(visit_tbl)) {
                        date_col <- "visit_start_date"
                      }
                      else {
                        date_col <- "visit_detail_start_date"
                      }
                      age_first_visit <- visit_tbl %>% select(person_id, !!sym(date_col)) %>% 
                        inner_join(cohort_tbl) %>% group_by(!!sym(site_col), 
                                                            person_id, cohort_id) %>% summarise(min_visit = min(!!sym(date_col))) %>% 
                        collect() %>% left_join(new_person) %>% mutate(age_first_visit = as.numeric(as.Date(min_visit) - 
                                                                                                      birth_date), age_first_visit = round(age_first_visit/365.25, 
                                                                                                                                           2)) %>% distinct(!!sym(site_col), person_id, age_first_visit)
                      summ_tbl <- demo_final %>% left_join(age_first_visit) %>% 
                        left_join(age_ced)
                    },
                    ns = 'sensitivityselectioncriteria')
  
  
  # Establish session
  argos_session <- argos$new(session_name)
  
  # set_argos_default(argos_session)
  
  # Set db_src
  if(!is_json){
    argos_session$config('db_src', db_conn)
  }else{
    argos_session$config('db_src', srcr(db_conn))
  }
  
  # Set misc configs
  argos_session$config('cdm_schema', cdm_schema)
  argos_session$config('results_schema', results_schema)
  argos_session$config('vocabulary_schema', vocabulary_schema)
  argos_session$config('cache_enabled', cache_enabled)
  argos_session$config('retain_intermediates', retain_intermediates)
  argos_session$config('db_trace', db_trace)
  argos_session$config('can_explain', !is.na(tryCatch(db_explain(config('db_src'), 'select 1 = 1'),
                                                            error = function(e) NA)))
  argos_session$config('results_target', ifelse(default_file_output, 'file', TRUE))
  
  if(is.null(results_tag)){
    argos_session$config('results_name_tag', '')
  }else{
    argos_session$config('results_name_tag', results_tag)
  }
  
  # Set working directory
  argos_session$config('base_dir', base_directory)
  
  # Set specs & results directories
  ## Drop path to base directory if present
  specs_drop_wd <- str_remove(specs_subdirectory, base_directory)
  results_drop_wd <- str_remove(results_subdirectory, base_directory)
  argos_session$config('subdirs', list(spec_dir = specs_drop_wd,
                                             result_dir = results_drop_wd))
  
  # Print session information
  db_str <- DBI::dbGetInfo(config('db_src'))
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
  cli::cli_inform(paste0('Connected to: ', db_str$dbname, '@', db_str$host))
  # cli::cli_inform('To see environment settings, run {.code get_argos_default()}')
  
  argos_session
}