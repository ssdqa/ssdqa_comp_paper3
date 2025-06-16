
# Load required packages
# devtools::install_github('PEDSnet/argos')
# devtools::install_github('ssdqa/squba.gen')
library(argos)
library(srcr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(DBI)
library(dbplyr)
library(lubridate)
library(squba.gen)
library(RPostgres)
library(squba)

# Source file with wrapper function
source(file.path('setup', 'argos_wrapper.R'))

# Establish connection to database
initialize_session(session_name = 'ssdqa_paper3',
                   db_conn = Sys.getenv('PEDSNET_BASE_CONFIG'),
                   is_json = TRUE,
                   cdm_schema = 'dcc_pedsnet',
                   results_schema = 'ssdqa_paper3', 
                   vocabulary_schema = 'vocabulary',
                   retain_intermediates = FALSE,
                   db_trace = TRUE,
                   results_tag = '')

# Source cohort_* files
for (fn in list.files('code', 'cohort_.+\\.R', full.names = TRUE)){
  source(fn)
  }
rm(fn)

