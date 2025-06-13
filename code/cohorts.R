
#' Identify specialties for relevant couplets
#'
#' @param visits CDM visit occurrence table
#' @param specialty_conceptset concept set with relevant specialties
#'
#' @return table of visits with their associated specialties, prioritizing provider
#'         specialty and using care site specialty where provider specialty is not available
#'         or not informative
#'
find_specialty <- function(visits,
                           specialty_conceptset) {
  prov_informative <- cdm_tbl('provider') %>%
    inner_join(specialty_conceptset, by = c('specialty_concept_id' = 'concept_id')) %>%
    select(provider_id, prov_specialty = specialty_concept_id)
  cs_informative <- cdm_tbl('care_site') %>%
    inner_join(specialty_conceptset, by = c('specialty_concept_id' = 'concept_id')) %>%
    select(care_site_id, cs_specialty = specialty_concept_id)
  
  visits %>%
    left_join(prov_informative, by = 'provider_id') %>%
    left_join(cs_informative, by = 'care_site_id') %>%
    filter(!is.na(prov_specialty)| !is.na(cs_specialty)) %>%
    mutate(visit_specialty_concept_id =
             case_when(prov_specialty != 38004477L ~ prov_specialty,
                       cs_specialty != 38004477L ~ cs_specialty,
                       prov_specialty == 38004477L ~ 38004477L,
                       cs_specialty == 38004477L ~ 38004477L,
                       TRUE ~ 0L)) %>%
  select(-prov_specialty, -cs_specialty)
}