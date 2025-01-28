

# function to select population. This is the lowest level of the workflow. 
# It loads necessary data from the database and 

library(tidyverse)
library(lubridate)
library(forecast)
library(runner)
library(colorspace)
# library(zorgmijding)

# libraries to fetch data from the RG database
library(DBI)
library(RPostgreSQL)

# start here
setwd(here::here())

# .-----
# _____________________________________________________________________________________________
# Load data -----------------------------------------------------------------------------------
# _____________________________________________________________________________________________

# load data into a separate environment
dat <- new.env(parent = globalenv())

local(
  {
    # open connection with the db
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = "11 Q-Bert",
                     host = "localhost",
                     port = "5432",
                     user = "postgres",
                     password = rstudioapi::askForPassword("Database password")
    )
    
    cont_type <- dbQuoteIdentifier(con, "ContactType")
    
    gp_contacts_query <- dbSendQuery(con, 
                                     paste0("SELECT 
                                           exppatidx AS pat_id,
                                           contactd AS contact_date,
                                           cod AS icpc, 
                                           age,
                                           disease,"
                                           , cont_type, "AS contact_type 
                                           FROM care_avoidance.results_gp_contacts
                                           WHERE cod IN ('T90.0', 'T90.02', 'A91.05', 'K49.01');")
    )
    
    gp_contacts <- dbFetch(gp_contacts_query)
    dbClearResult(gp_contacts_query)
    
    
    icpc_col <- dbQuoteIdentifier(con, "Linked_ICPC")
    sys_col <- dbQuoteIdentifier(con, "Systolic")
    dia_col <- dbQuoteIdentifier(con, "Diastolic")
    
    # blood pressure
    bp_query <- dbSendQuery(con, 
                            paste0("SELECT 
                                  exppatidx AS pat_id,
                                  contactd AS contact_date,"
                                  , icpc_col, "AS icpc," 
                                  , sys_col,  "AS systolic,"
                                  , dia_col, "AS diastolic,
                                  age,
                                  Measurement_detail AS measure,
                                  unique_record_id AS record_id,
                                  prakid AS prak_id
                                  FROM care_avoidance.results_blood_pressure;")
    )
    
    bp <- dbFetch(bp_query)
    dbClearResult(bp_query)
    
    # cholesterol
    chol_query <- dbSendQuery(con, 
                              paste0("SELECT
                                  exppatidx AS pat_id,
                                  contactd AS contact_date,"
                                  , icpc_col, "AS icpc, 
                                  age,
                                  value,
                                  Measurement_detail AS measure,
                                  unique_record_id AS record_id,
                                  prakid AS prak_id
                                  FROM care_avoidance.results_cholesterol_levels;")
    )
    
    chol <- dbFetch(chol_query)
    dbClearResult(chol_query)
    
    # glucose
    glu_query <- dbSendQuery(con, 
                             paste0("SELECT
                                  exppatidx AS pat_id,
                                  contactd AS contact_date,"
                                  , icpc_col, "AS icpc, 
                                  age,
                                  value,
                                  unique_record_id AS record_id,
                                  prakid AS prak_id
                                  FROM care_avoidance.results_glucose_levels;")
    )
    
    glu <- dbFetch(glu_query)
    dbClearResult(glu_query)
    
    # medication
    med_query <- dbSendQuery(con, 
                             paste0("SELECT
                                  exppatidx AS pat_id,
                                  contactd AS contact_date,
                                  age,
                                  category,
                                  atc,
                                  atc_text,
                                  unique_record_id AS record_id,
                                  prakid AS prak_id
                                  FROM care_avoidance.results_medication;")
    )
    
    med <- dbFetch(med_query)
    dbClearResult(med_query)
    
    
    # incident medication
    inc_med_query <- dbSendQuery(con, 
                             paste0("SELECT
                                  exppatidx AS pat_id,
                                  contactd AS contact_date,
                                  age,
                                  category,
                                  atc,
                                  atc_text,
                                  unique_record_id AS record_id,
                                  prakid AS prak_id
                                  FROM care_avoidance.results_medication_incidence;")
    )
    
    inc_med <- dbFetch(inc_med_query)
    dbClearResult(inc_med_query)
    
    
    patients_query <- dbSendQuery(con, 
                                  "SELECT 
                                  exppatidx AS pat_id, 
                                  patbird AS pat_dob, 
                                  patdead AS pat_dom,
                                  gender AS sex, 
                                  prakid AS prak_id,
                                  patid AS patid,
                                  livenv, 
                                  socdep56, 
                                  ipcistd AS fu_start, 
                                  ipciendd AS fu_end
                                  FROM public.patient p1 
                                  WHERE ipcistd is not null;
                                  ")
    patients <- dbFetch(patients_query)
    dbClearResult(patients_query)
    
    
    prevalent_cases_query <- dbSendQuery(con,
                                         "SELECT
                                  exppatidx AS pat_id, 
                                  disease,
                                  first_contactd AS diagnosis_date,
                                  ipcistd AS fu_start, 
                                  ipciendd AS fu_end,
                                  cod AS icpc
                                  FROM care_avoidance.results_prevalent_cases;
                                  " )
    
    prevalent_cases <- dbFetch(prevalent_cases_query)
    dbClearResult(prevalent_cases_query)
    
    
    specialist_care_query <- dbSendQuery(con,
                                         "SELECT
                                          exppatidx AS pat_id, 
                                          contactd AS sc_entry_date,
                                          cod,
                                          valtxt AS value
                                          FROM public.measurement
                                          WHERE cod IN ('DMHBTZ', 'DMKZTZ') AND
                                                valtxt IN ('ja', 'specialist');
                                          " )
    
    specialist_care <- dbFetch(specialist_care_query)
    dbClearResult(specialist_care_query)
    
    
    comorbidities_query <- dbSendQuery(con,
                                       "SELECT
                                          exppatidx AS pat_id, 
                                          contactd_com AS contact_date,
                                          cod_com AS icpc
                                          FROM grip3.comorbidity;
                                          " )
    
    comorbidities <- dbFetch(comorbidities_query)
    dbClearResult(comorbidities_query)
    
    
    
    smoking_col <- dbQuoteIdentifier(con, "SmokingStatus")
    
    # blood pressure
    smoking_query <- dbSendQuery(con, 
                            paste0("SELECT 
                                  exppatidx AS pat_id,"
                                  , smoking_col, "AS smoking
                                  FROM care_avoidance.results_smoking_status;")
    )
    
    smoking <- dbFetch(smoking_query)
    dbClearResult(smoking_query)
    
    # close connection with the db
    dbDisconnect(con)
    
    rm(gp_contacts_query)
    rm(icpc_col)
    rm(sys_col)
    rm(dia_col)
    rm(smoking_col)
    rm(bp_query)
    rm(chol_query)
    rm(glu_query)
    rm(med_query)
    rm(patients_query)
    rm(prevalent_cases_query)
    rm(specialist_care_query)
    rm(comorbidities_query)
    rm(smoking_query)
    rm(con)
    rm(cont_type)
    rm(inc_med_query)
    
    patients$pat_id <- as.factor(patients$pat_id)
    med$pat_id <- as.factor(med$pat_id) 
    bp$pat_id <- as.factor(bp$pat_id)
    glu$pat_id <- as.factor(glu$pat_id)
    chol$pat_id <- as.factor(chol$pat_id)
    inc_med$pat_id <- as.factor(inc_med$pat_id)
    prevalent_cases$pat_id <- as.factor(prevalent_cases$pat_id)
    specialist_care$pat_id <- as.factor(specialist_care$pat_id)
    gp_contacts$pat_id <- as.factor(gp_contacts$pat_id)
    smoking$pat_id <- as.factor(smoking$pat_id)
    
    # remove irrelevant contacts such as postverwerking or prescriptions
    gp_contacts <- gp_contacts |>
      dplyr::filter(!(contact_type %in% c("Posthandling", "Presription")))
    
    # re-label sex
    patients$sex <- factor(patients$sex, levels = c(1, 2), labels = c("male", "female"))
    
  },
  
  
  env = dat
) 



# cvd_contact_df <- dat_contact$gp_contacts
# sample_2020_c <- get_sample_contact(baseline = "2017-01-01", 
#                           censor = "2020-12-31", 
#                           prevalence = prevalence_df,
#                           remove_cvd_events = TRUE, 
#                           intervention_period = c("2020"))
# 
# 
# sample_2018_c <- get_sample_contact(baseline = "2015-01-01", 
#                                     censor = "2018-12-31", 
#                                     prevalence = prevalence_df,
#                                     remove_cvd_events = TRUE, 
#                                     intervention_period = c("2018"))

# negative_control_2019 <- get_sample(baseline = "2016-01-01", 
#                                censor = "2019-12-31", 
#                                intervention_period = c("2019"))
# 
# 
# negative_control_2018 <- get_sample(baseline = "2015-01-01", 
#                                censor = "2018-12-31", 
#                                intervention_period = c("2018"))
# 
# negative_control_2018a <- get_sample(baseline = "2015-01-01", 
#                                     censor = "2019-12-31", 
#                                     intervention_period = c("2018", "2019"))
# 
# 
# 
# sample_2020 <- get_sample(baseline = "2017-01-01", 
#                           censor = "2020-12-31", 
#                           intervention_period = c("2020"))
# 
# 
# negative_control_2018 <- get_sample(baseline = "2015-01-01", 
#                                     censor = "2018-12-31", 
#                                     intervention_period = c("2018"))
# 
# sample_2021 <- get_sample(baseline = "2017-01-01", 
#                           censor = "2021-12-31", 
#                           intervention_period = c("2020", "2021"))
# 
# 
# 
# 
# # now if we change the definition of health care avoidance
# # negative_control_2019a <- get_sample(baseline = "2016-01-01", 
# #                                     censor = "2019-12-31", 
# #                                     intervention_period = c("2019"),
# #                                     
# #                                     avoidance_definition = list("min_measures" = 1, 
# #                                                                 "in_how_many_years" = 2))
# # 
# # 
# # negative_control_2018a <- get_sample(baseline = "2015-01-01", 
# #                                     censor = "2018-12-31", 
# #                                     intervention_period = c("2018"),
# #                                     
# #                                     avoidance_definition = list("min_measures" = 1, 
# #                                                                 "in_how_many_years" = 2))
# # 
# # negative_control_2018aa <- get_sample(baseline = "2015-01-01", 
# #                                      censor = "2019-12-31", 
# #                                      intervention_period = c("2018", "2019"),
# #                                      
# #                                      avoidance_definition = list("min_measures" = 1, 
# #                                                                  "in_how_many_years" = 2))
# 
# # # and here for the sample as well
# # sample_2020a <- get_sample(baseline = "2017-01-01", 
# #                           censor = "2020-12-31", 
# #                           intervention_period = c("2020"),
# #                           
# #                           avoidance_definition = list("min_measures" = 1, 
# #                                                       "in_how_many_years" = 2))
# # 
# # sample_2021a <- get_sample(baseline = "2017-01-01", 
# #                           censor = "2021-12-31", 
# #                           intervention_period = c("2020", "2021"),
# #                           
# #                           avoidance_definition = list("min_measures" = 1, 
# #                                                       "in_how_many_years" = 2))
# 
# sample_2020 <- get_sample(baseline = "2017-01-01", 
#                           censor = "2020-12-31", 
#                           remove_cvd_events = FALSE, 
#                           intervention_period = c("2020"))
# 
# 
# negative_control_2018 <- get_sample(baseline = "2015-01-01", 
#                                     censor = "2018-12-31", 
#                                     remove_cvd_events = FALSE, 
#                                     intervention_period = c("2018"))
# 
# 
# 
# sample_2020 <- get_sample(baseline = "2017-01-01", 
#                           censor = "2020-12-31", 
#                           remove_cvd_events = TRUE, 
#                           intervention_period = c("2020"))
# 
# 
# negative_control_2018 <- get_sample(baseline = "2015-01-01", 
#                                     censor = "2018-12-31", 
#                                     remove_cvd_events = TRUE, 
#                                     intervention_period = c("2018"))
# 
# 
# table(sample_2020$health_avoider)
# table(negative_control_2018$health_avoider)
# 
# 
# t <- table(sample_2020$health_avoider)
# tt <- table(negative_control_2019$health_avoider)
# t
# tt
# 
# prop.table(t)
# prop.table(tt)
# 
# 
# 
# table(sample_2020$health_avoider)
# table(sample_2020$n[sample_2020$health_avoider & sample_2020$fu_end > ymd("2021-12-01")])
# 118 / sum(table(sample_2020$n[sample_2020$health_avoider]))
# 
# table(negative_control_2018$n[negative_control_2018$health_avoider & negative_control_2018$fu_end > ymd("2019-12-01")])
# 44 / sum(table(negative_control_2018$n[negative_control_2018$health_avoider]))
# 
# x <- sample_2020[sample_2020$fu_end > ymd("2021-12-01"), ]
# round(prop.table(table(x$health_avoider, x$n == 0)), 3)
# table(x$health_avoider, x$n)
# 
# y <- negative_control_2018[negative_control_2018$fu_end > ymd("2019-12-01"), ]
# round(prop.table(table(y$health_avoider, y$n == 0)), 3)
# table(y$health_avoider, y$n)
# 
# sampple_2020 <- dplyr::left_join(sample_2020, specialist, by = "pat_id") 
# 
# negc <- dplyr::left_join(negative_control_2018, specialist, by = "pat_id") 
# 
# round(prop.table(table(sampple_2020$health_avoider, sampple_2020$cod, useNA = "ifany")), 3)
