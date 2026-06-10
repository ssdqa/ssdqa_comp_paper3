# cvd_process -----
#'
#' Valueset conformance
#'
#' This is a valueset conformance module that will compute frequency distributions of a column's contents when a set of criteria is satisfied. The user will provide the domain in which to search (`domain_tbl`) and criteria that must be satisfied, in the form of a concept set (`concept_set`). This function is compatible with both the OMOP and PCORnet CDMs based on the user's selection.
#'
#' @param cohort A dataframe with the cohort of patients for your study. Should include the columns:
#' - person_id
#' - start_date
#' - end_date
#' - site
#' @param domain_tbl a tibble that defines the domain and columns where facts should be identified. Must include the columns:
#' - domain: name of table
#' - concept_field: column in which to search for the concepts in `concept_set`
#' - vs_field: column to be summarized in valueset check
#' - date_field: column in the CDM table that should be used as the default date field for longitudinal analysis
#' @param concept_set a tibble containing the codes for which to limit analyses to in the `concept_field` defined in the `domain_tbl`. For OMOP CDM, should contain a column named `concept_id` with OMOP concept_ids.
#' @param omop_or_pcornet Option to run the function using the OMOP or PCORnet CDM as the default CDM
#' - omop: run the function against an OMOP CDM instance
#' - pcornet: run the function against a PCORnet CDM instance
#' @param multi_or_single_site Option to fun the function on a single vs multiple sites contained within the same data source
#' - single: run the function for a single site
#' - multi: run the function for multiple sites, which should be differentiated by a `site` column in the cohort table
#' @param anomaly_or_exploratory Option to conduct an `exploratory` or `anomaly` detection analysis. Exploratory analyses give a high level summary of the data to examine the fact representation within the cohort. Anomaly detection analyses are specialized to identify outliers within the cohort.
#' @param p_value **For multi_or_single_site='multi', time=FALSE, anomaly_or_exploratory='anomaly' only** the p value to be used as a threshold in the multi-site anomaly detection analysis for the `hotspots::outliers` function
#' @param time a logical that tells the function whether you would like to look at the output over time
#' @param time_span when time = TRUE, this argument defines the start and end dates for the time period of interest. Should be formatted as c(start date, end date) in 'yyy-mm-dd' date format
#' @param time_period when time = TRUE, this argument defines the distance between dates within the specified time period. Defaults to `year`, but other time periods such as `month` or `week` are also acceptable
#'
#' @return a dataframe with counts and proportions of each of the valueset contents
cvd_process<-function(cohort,
                         domain_tbl,
                         concept_set,
                         omop_or_pcornet,
                         multi_or_single_site = 'single',
                         anomaly_or_exploratory='exploratory',
                         p_value = 0.9,
                         time = TRUE,
                         time_span = c('2012-01-01', '2020-01-01'),
                         time_period = 'year',
                         vocab_tbl=NULL){
  ## Check proper arguments
  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(!multi_or_single_site %in% c('single', 'multi')){cli::cli_abort('Invalid argument for {.code multi_or_single_site}: please enter either {.code multi} or {.code single}')}
  if(!anomaly_or_exploratory %in% c('anomaly', 'exploratory')){cli::cli_abort('Invalid argument for {.code anomaly_or_exploratory}: please enter either {.code anomaly} or {.code exploratory}')}
  if(multi_or_single_site=='single'&anomaly_or_exploratory=='anomaly'&!time){cli::cli_abort('Check not relevant for cross-sectional single site anomaly detection : please enter a different value for {.code multi_or_single_site}, {.code anomaly_or_exploratory}, or {.code time}')}

  ## parameter summary output
  output_type <- suppressWarnings(param_summ(check_string='cvd',
                                             as.list(environment())))

  # set output grouping based on input parameters
  if(time){
    output_group_list<-c('site','time_start','time_increment')
  }else{
    output_group_list<-c('site')
  }

  if(tolower(omop_or_pcornet) == 'omop'){
    cvd_tbl<-cvd_process_omop(cohort = cohort,
                               domain_tbl = domain_tbl,
                               concept_set = concept_set,
                               multi_or_single_site = multi_or_single_site,
                               anomaly_or_exploratory=anomaly_or_exploratory,
                               p_value = p_value,
                               time = time,
                               time_span = time_span,
                               time_period = time_period,
                               vocab_tbl = vocab_tbl)
  }else if(tolower(omop_or_pcornet) == 'pcornet'){
    cvd_tbl<-cvd_process_pcornet(cohort = cohort,
                                  domain_tbl = domain_tbl,
                                  concept_set = concept_set,
                                  multi_or_single_site = multi_or_single_site,
                                  anomaly_or_exploratory=anomaly_or_exploratory,
                                  p_value = p_value,
                                  time = time,
                                  time_span = time_span,
                                  time_period = time_period,
                                  vocab_tbl = vocab_tbl)
  }else{cli::cli_abort('Invalid argument for {.code omop_or_pcornet}: this function is only compatible with {.code omop} or {.code pcornet}')}

  message('Finding valueset item proportions')
  # compute proportions based on input parameters
  cvd_tbl_prop<-cvd_tbl%>%
    ungroup()%>%
    group_by(!!!syms(output_group_list))%>%
    mutate(ct_denom=sum(ct_concept,na.rm=TRUE))%>%
    ungroup()%>%
    mutate(prop_concept=as.numeric(ct_concept)/as.numeric(ct_denom))%>%
    collect()%>%
    mutate(concept_id=as.character(concept_id))

  # apply anomaly detection, if requested
  if(anomaly_or_exploratory=='anomaly'){
    message('Applying anomaly detection')
    if(multi_or_single_site=='single'){
      cvd_tbl_fn<-anomalize_ss_anom_la(fot_input_tbl=cvd_tbl_prop,
                                       grp_vars='concept_id',
                                       time_var='time_start',
                                       var_col='prop_concept')
    }else if(multi_or_single_site=='multi'&!time){
      # add in a line here to condense NA and concept_id=0
      cvd_tbl_an<-compute_dist_anomalies(df_tbl = cvd_tbl_prop,
                                         grp_vars='concept_id',
                                         var_col='prop_concept',
                                         denom_cols=c('concept_id','ct_denom'))
      cvd_tbl_fn <- detect_outliers(df_tbl = cvd_tbl_an,
                                    tail_input = 'both',
                                    p_input = p_value,
                                    column_analysis = 'prop_concept',
                                    column_variable = 'concept_id')
    }else if(multi_or_single_site=='multi'&time){
      lookup <- cvd_tbl_prop %>% ungroup() %>% distinct(concept_id)
      cvd_tbl_an <- ms_anom_euclidean(fot_input_tbl = cvd_tbl_prop,
                                      grp_vars = c('site','concept_id'),
                                      var_col = 'prop_concept')
      cvd_tbl_fn <- cvd_tbl_an %>% left_join(lookup)
    }
  }else{
    cvd_tbl_fn<-cvd_tbl_prop
  }

  message('Finding concept names')
  cvd_rslt<-join_to_vocabulary(tbl=cvd_tbl_fn%>%mutate(concept_id=as.integer(concept_id)),
                                    vocab_tbl=vocab_tbl,
                                    col='concept_id',
                                    vocab_col='concept_id')


  if('list' %in% class(cvd_rslt)){
    cvd_rslt[[1]] <- cvd_rslt[[1]] %>% mutate(output_function = output_type$string)
  }else{
    cvd_rslt <- cvd_rslt %>% mutate(output_function = output_type$string)
  }

  print(cli::boxx(c('You can optionally use this dataframe in the accompanying',
                    '`cvd_output` function. Here are the parameters you will need:', '', output_type$vector, '',
                    'See ?cvd_output for more details.'), padding = c(0,1,0,1),
                  header = cli::col_cyan('Output Function Details')))

  return(cvd_rslt)

}

# cvd_process_omop ----
#' @param cohort the

cvd_process_omop<-function(cohort,
                              domain_tbl,
                              concept_set,
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory='exploratory',
                              p_value = 0.9,
                              time = TRUE,
                              time_span = c('2012-01-01', '2020-01-01'),
                              time_period = 'year',
                              vocab_tbl=NULL){

  # Add site check
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj
  vs_col<-domain_tbl$vs_field[[1]]

  site_output <- list()

  # Prep cohort

  cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter,
                                age_groups = NULL, #codeset = NULL,
                                omop_or_pcornet = 'omop') %>%
    group_by(!!! syms(grouped_list))

  # Execute function
  if(!time){
  for(k in 1:length(site_list_adj)){

    site_list_thisrnd<-site_list_adj[[k]]

    # filters by site
    cohort_site<-cohort_prep%>%filter(!!sym(site_col)%in%c(site_list_thisrnd))
    ct_compute<-check_vs_dist(cohort=cohort_site,
                                concept_set=concept_set,
                                domain_tbl=domain_tbl,
                                time=time,
                                grp=grouped_list,
                              omop_or_pcornet='omop')

    site_output[[k]] <- ct_compute%>%mutate(site=site_list_thisrnd)
  }

    cvd_tbl<-reduce(.x=site_output,
                       .f=dplyr::union)

  } else {
    cvd_tbl <- compute_fot(cohort=cohort_prep,
                              site_list=site_list_adj,
                              site_col=site_col,
                              time_span=time_span,
                              time_period = time_period,
                              reduce_id=NULL,
                              check_func=function(dat){
                                check_vs_dist(cohort=dat,
                                                concept_set=concept_set,
                                                domain_tbl=domain_tbl,
                                                time=TRUE,
                                                grp=grouped_list,
                                              omop_or_pcornet='omop')
                              })%>%replace_site_col()
  }

  return(cvd_tbl)

}

cvd_process_pcornet<-function(cohort,
                              domain_tbl,
                              concept_set,
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory='exploratory',
                              p_value = 0.9,
                              time = TRUE,
                              time_span = c('2012-01-01', '2020-01-01'),
                              time_period = 'year',
                              vocab_tbl=NULL){
  # Add site check
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj
  vs_col<-domain_tbl$vs_field[[1]]

  site_output <- list()

  # Prep cohort

  cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter,
                                age_groups = NULL, codeset = NULL,
                                omop_or_pcornet = 'pcornet') %>%
    group_by(!!! syms(grouped_list))

  # Execute function
  if(!time){
    for(k in 1:length(site_list_adj)){

      site_list_thisrnd<-site_list_adj[[k]]

      # filters by site
      cohort_site<-cohort_prep%>%filter(!!sym(site_col)%in%c(site_list_thisrnd))
      ct_compute<-check_vs_dist(cohort=cohort_site,
                                concept_set=concept_set,
                                domain_tbl=domain_tbl,
                                time=time,
                                grp=grouped_list,
                                omop_or_pcornet='pcornet')

      site_output[[k]] <- ct_compute%>%mutate(site=site_list_thisrnd)
    }

    cvd_tbl<-reduce(.x=site_output,
                    .f=dplyr::union)

  } else {
    cvd_tbl <- compute_fot(cohort=cohort_prep,
                           site_list=site_list_adj,
                           site_col=site_col,
                           time_span=time_span,
                           time_period = time_period,
                           reduce_id=NULL,
                           check_func=function(dat){
                             check_vs_dist(cohort=dat,
                                           concept_set=concept_set,
                                           domain_tbl=domain_tbl,
                                           time=TRUE,
                                           grp=grouped_list,
                                           omop_or_pcornet='pcornet')
                           })%>%replace_site_col()
  }

  return(cvd_tbl)


}

check_vs_dist<-function(cohort,
                        concept_set,
                        domain_tbl,
                        time,
                        grp,
                        omop_or_pcornet){

  i<-1
  # expect only one row in domain input file
  domain_tbl_name<-domain_tbl$domain[[i]]
  final_col<-domain_tbl$concept_field[[i]]
  vs_col<-domain_tbl$vs_field[[i]]
  date_col<-domain_tbl$date_field[[i]]
  domain_tbl_cdm<-cdm_tbl(domain_tbl_name)%>%
    inner_join(cohort)%>%
    filter(!!sym(date_col)>=start_date,
           !!sym(date_col)<=end_date)


  if(time){
    facts_inwindow<-domain_tbl_cdm%>%
      filter(!!sym(date_col)>=time_start,
             !!sym(date_col)<=time_end)%>%
      group_by(!!!syms(grp))%>%
      group_by(time_start, time_increment,.add=TRUE)
  }else{
    facts_inwindow<-domain_tbl_cdm
  }
  # group and count facts per valueset item
  if(omop_or_pcornet=='pcornet'){
  concept_counts<-facts_inwindow%>%
    inner_join(concept_set,
               by=setNames('concept_code',final_col))%>%
    group_by(!!sym(vs_col), .add=TRUE)%>%
    summarise(ct_concept=n())%>%
    rename('concept_id'=vs_col)
  }else{
    concept_counts<-facts_inwindow%>%
      inner_join(concept_set,
                 by=setNames('concept_id',final_col))%>%
      group_by(!!sym(vs_col), .add=TRUE)%>%
      summarise(ct_concept=n())%>%
      rename('concept_id'=vs_col)
  }
  return(concept_counts)
}

## visualizations ---
# filter_concept needed for ss_anom_la and ms_anom_la only, the code to focus on
cvd_output<-function(process_output,
                        output_function,
                        filter_concept){
  if(output_function=='cvd_ss_exp_cs'){
    cvd_output<-cvd_ss_exp_cs(process_output=process_output)
  }else if(output_function=='cvd_ss_exp_la'){
    cvd_output<-cvd_ss_exp_la(process_output=process_output)
  }else if(output_function=='cvd_ss_anom_la'){
    cvd_output<-cvd_ss_anom_la(process_output=process_output,
                                     filt=filter_concept)
  }else if(output_function=='cvd_ms_exp_cs'){
    cvd_output<-cvd_ms_exp_cs(process_output=process_output)
  }else if(output_function=='cvd_ms_exp_la'){
    cvd_output<-cvd_ms_exp_la(process_output=process_output)
  }else if(output_function=='cvd_ms_anom_cs'){
    cvd_output<-cvd_ms_anom_cs(process_output=process_output)
  }else if(output_function=='cvd_ms_anom_la'){
    cvd_output<-cvd_ms_anom_la(process_output=process_output,
                                     filt=filter_concept)
  }else(cli::cli_abort('Please enter a valid output_function for this check'))
}
cvd_ss_exp_cs<-function(process_output){
  # check to see if all of the values of concept_name are vocabulary table not found?
  concept_col<-ifelse('concept_name'%in%colnames(process_output), 'concept_name','concept_id')

  data_tbl<-process_output%>%
    mutate(text=paste0("Count: ", format(ct_concept,big.mark=","),
                       "\nProportion: ",round(prop_concept,2)))

  plt<-ggplot(data_tbl,
                 aes(x=as.character(!!sym(concept_col)),
                     y=prop_concept,
                     fill=as.character(!!sym(concept_col)),
                     text=text))+
    geom_bar(stat='identity', show.legend=FALSE)+
    theme_minimal()+
    coord_flip()+
    scale_fill_squba()+
    labs(x="Value",
         y="Proportion",
         title="Distribution of Categorical Variable Values")

  plt[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                              'tooltip' = TRUE)

  return(plt)
}

cvd_ss_exp_la<-function(process_output){
  concept_col<-ifelse('concept_name'%in%colnames(process_output), 'concept_name','concept_id')

  data_tbl<-process_output%>%
    mutate(text=paste0("Time: ",time_start,
                       "\nCount: ",format(ct_concept,big.mark=","),
                       "\nProportion: ",round(prop_concept,2)))

  plt<-ggplot(data_tbl,
         aes(x=time_start,
             y=prop_concept,
             color=!!sym(concept_col),
             group=!!sym(concept_col),
             text=text))+
    geom_line()+
    scale_color_squba()+
    theme_minimal()+
    labs(x="Time",
         y="Proportion with Value")
  plt[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                              'tooltip' = TRUE)

  return(plt)
}

cvd_ss_anom_la<-function(process_output,
                            filt){
  concept_col<-ifelse('concept_name'%in%colnames(process_output), 'concept_name','concept_id')

  time_inc <- process_output %>% filter(!is.na(time_increment)) %>% distinct(time_increment) %>% pull()
  c_final <- process_output %>% filter(concept_id==filt)

  if(time_inc == 'year'){

    c_plot <- qicharts2::qic(data = c_final, x = time_start, y = ct_concept, chart = 'pp', n = ct_denom)
    op_dat <- c_plot$data

    new_pp <- ggplot(op_dat, aes(x,y))+
      geom_ribbon(aes(ymin=lcl,max=ucl), fill="lightgray",alpha=0.4) +
      geom_line(colour=squba_colors_standard[[12]], size=0.5) +
      geom_line(aes(x,cl)) +
      geom_point(colour=squba_colors_standard[[6]], fill = squba_colors_standard[[6]], size = 1) +
      geom_point(data=subset(op_dat, y>=ucl), color=squba_colors_standard[[3]], size=2) +
      geom_point(data=subset(op_dat, y<=lcl), color=squba_colors_standard[[3]], size=2) +
      ggtitle(label=paste0("Control Chart: Proportion of Valueset Item ", filt, " over Time")) +
      labs(x = "Time",
           y = "Proportion")+
      theme_minimal()


    new_pp[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                   'tooltip' = FALSE)
    output<-new_pp

  }else{
    anomalies<-timetk::plot_anomalies(.data=c_final,
                                      .date_var=time_start,
                                      .interactive=FALSE,
                                      .title=paste0("Anomalies for Valueset Item ", filt, " over Time"))
    decomp<-timetk::plot_anomalies_decomp(.data=c_final,
                                          .date_var=time_start,
                                          .interactive=FALSE,
                                          .title=paste0("Anomalies for Valueset Item ", filt, " over Time"))
    anomalies[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                      'tooltip' = FALSE)
    decomp[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                   'tooltip' = FALSE)
    output<-list(anomalies, decomp)
  }

}
cvd_ms_exp_cs<-function(process_output){

  concept_col<-ifelse('concept_name'%in%colnames(process_output), 'concept_name','concept_id')
  dat_to_plot<-process_output%>%
    mutate(tooltip=paste0("Proportion: ",round(prop_concept,2),
                          "\nCount: ",format(ct_concept, big.mark=",")))
  plt <- ggplot(dat_to_plot, aes(x=site,
                                 y=!!sym(concept_col),
                                 fill=prop_concept))+
    ggiraph::geom_tile_interactive(aes(tooltip=tooltip))+
    geom_text(aes(label=round(prop_concept,2)),size=3,color='black')+
    scale_fill_squba(palette='diverging',discrete=FALSE)+
    theme_minimal()+
    theme(axis.text.x = element_text(angle=30, vjust=1, hjust=1))+
    labs(y=concept_col,
         title="Distribution of Categorical Variable Values across Sites",
         fill="Proportion")

  plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                              'tooltip' = TRUE)
  return(plt)

}

cvd_ms_exp_la<-function(process_output){
  concept_col<-ifelse('concept_name'%in%colnames(process_output), 'concept_name','concept_id')

  dat_toplot<-process_output%>%
    mutate(text=paste0("Site: ",site,
                       "\nProportion: ",round(prop_concept,2),
                       "\nCount: ",ct_concept))
  plt<-ggplot(dat_toplot,
         aes(x=time_start,
             y=prop_concept,
             color=site,
             group=site,
             text=text))+
    geom_line()+
    facet_wrap((concept_col))+
    scale_color_squba()+
    theme_minimal()+
    labs(x='Time',
         y='Proportion',
         color='Site')

  plt[['metadata']] <- tibble('pkg_backend' = 'plotly',
                              'tooltip' = TRUE)

  return(plt)

}

cvd_ms_anom_cs<-function(process_output){

  concept_col<-ifelse('concept_name'%in%colnames(process_output), 'concept_name','concept_id')

  # https://github.com/squba/sourceconceptvocabularies/blob/main/R/scv_output_subfuncs.R#L374
  dat_to_plot <- process_output%>%
    mutate(text=paste0("Valueset Item: ",!!sym(concept_col),
                       "\nSite: ",site,
                       "\nProportion: ",round(prop_concept,2),
                       "\nMean proportion: ",round(mean_val,2),
                       "\nSD: ",round(sd_val,2),
                       "\nMedian proportion: ",round(median_val,2),
                       "\nMAD: ",round(mad_val, 2)),
           anomaly_yn = ifelse(anomaly_yn == 'no outlier in group', 'not outlier', anomaly_yn))

  plt<-ggplot(dat_to_plot,
              aes(x=site,
                  y=!!sym(concept_col),
                  text=text,
                  color=prop_concept))+
    ggiraph::geom_point_interactive(aes(size=mean_val,shape=anomaly_yn, tooltip=text))+
    ggiraph::geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier'),
                           aes(size=mean_val,shape=anomaly_yn, tooltip = text), shape = 1, color = 'black')+
    scale_color_squba(palette = 'diverging', discrete = FALSE) +
    scale_shape_manual(values=c(19,8))+
    scale_y_discrete(labels = function(x) str_wrap(x, width = 60)) +
    theme_minimal()+
    theme(axis.text.x = element_text(angle=30, vjust=1, hjust=1))+
    labs(size="",
         title="Anomalous Proportion of Valueset Item",
         subtitle='Dot size is the mean proportion of the given valueset item across sites')+
    guides(color=guide_colorbar(title="Proportion"),
           shape=guide_legend(title="Anomaly"),
           size="none")

  plt[["metadata"]]<-tibble('pkg_backend' = 'ggiraph',
                            'tooltip'=TRUE)

  return(plt)
}

cvd_ms_anom_la<-function(process_output,
                            filt){
  filt_op<-process_output%>%
    filter(concept_id==filt)

  allsites<-
    filt_op %>%
    select(time_start, concept_id, mean_allsiteprop) %>%
    distinct()%>%
    rename(prop_concept=mean_allsiteprop)%>%
    mutate(site= 'all site average',
           text_smooth=paste0("Site: ", site,
                              "\nProportion: ",round(prop_concept,2)),

           text_raw=paste0("Site: ", site,
                           "\n","Proportion: ",round(prop_concept,2)))

  dat_to_plot<-filt_op%>%
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean),
           text_raw=paste0("Site: ", site,
                           "\n","Site Proportion: ",round(prop_concept,2),
                           "\n","Site Smoothed Proportion: ",site_loess,
                           "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean))

  p<-dat_to_plot %>%
    ggplot(aes(y = prop_concept,
               x = time_start,
               color = site,
               group = site,
               text = text_smooth)) +
    geom_line(data=allsites, linewidth=1.1) +
    geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5, formula = y ~ x) +
    scale_color_squba() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(y = 'Proportion (Loess)',
         x = 'Time',
         title = paste0('Smoothed Proportion of Valueset Item ', filt, ' Across Time'))

  q <- dat_to_plot %>%
    ggplot(aes(y = prop_concept, x = time_start, color = site,
               group=site, text=text_raw)) +
    geom_line(data=allsites,linewidth=1.1) +
    geom_line(linewidth=0.2) +
    scale_color_squba() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(x = 'Time',
         y = 'Proportion',
         title = paste0('Proportion of Valueset Item', filt, ' Across Time'))

  t <- dat_to_plot %>%
    distinct(site, dist_eucl_mean, site_loess) %>%
    group_by(site, dist_eucl_mean) %>%
    summarise(mean_site_loess = mean(site_loess)) %>%
    mutate(tooltip = paste0('Site: ', site,
                            '\nEuclidean Distance: ', dist_eucl_mean,
                            '\nAverage Loess Proportion: ', mean_site_loess)) %>%
    ggplot(aes(x = site, y = dist_eucl_mean, fill = mean_site_loess, tooltip = tooltip)) +
    ggiraph::geom_col_interactive() +
    coord_radial(r.axis.inside = FALSE, rotate.angle = TRUE) +
    guides(theta = guide_axis_theta(angle = 0)) +
    theme_minimal() +
    scale_fill_squba(palette = 'diverging', discrete = FALSE) +
    labs(fill = 'Avg. Proportion \n(Loess)',
         y ='Euclidean Distance',
         x = '',
         title = paste0('Euclidean Distance for Valueset Item ', filt))

  p[['metadata']] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = TRUE)

  q[['metadata']] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = TRUE)

  t[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                            'tooltip' = TRUE)

  output <- list(p, q, t)

  return(output)
}
