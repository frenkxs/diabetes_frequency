dat <- readr::read_rds(file = here::here("datasets", "data.RData"))
source(here::here("R", "H_get_sample.R")) 

reformat_df <- . %>%
  dplyr::mutate(year = year(contact_date),
                month_year = lubridate::floor_date(contact_date, unit = "month"),
                week_year = lubridate::floor_date(contact_date, unit = "week", 
                                                  week_start = getOption("lubridate.week.start", 1)),
                pat_id = as.factor(pat_id)) %>%
  dplyr::distinct(pat_id, week_year, month_year, year)


# only select office blood pressure measurement
bp_df <- dat$bp %>%
  dplyr::filter(measure == "Office blood pressure measurement (OBPM)") %>%
  reformat_df 

# |>
#   dplyr::mutate(year = year(week_year))

# patients
patients_df <- dat$patients

# all prevalent data (including diseases other than DM2 !!!)
prevalence_df <- dat$prevalent_cases

# who entered specialist care for DM2
specialist_df <- dat$specialist_care

# prescriptions
medication_df <- dat$med %>% reformat_df |>
  dplyr::mutate(year = year(week_year))

inc_medication_df <-  dat$inc_med

# . ------------------------------------------------------
# ----------  Sample selection --------------------------

# . ------------------------------------------------------
# ----------- outcomes: MACE + heart failure removed -------------

# this is my sample
sample_2020 <- get_sample(dm_contacts = bp_df,
                          baseline = "2017-01-01", 
                          censor = "2021-01-01", 
                          remove_hf = TRUE,
                          intervention_period = c("2020"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 3, 
                                                      "in_how_many_years" = 3, 
                                                      "cutoff" = 3))

# negative control sample
sample_2018 <- get_sample(dm_contacts = bp_df,
                          baseline = "2015-01-01", 
                          censor = "2019-01-01", 
                          remove_hf = TRUE, 
                          intervention_period = c("2018"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 3, 
                                                      "in_how_many_years" = 3,
                                                      "cutoff" = 3))


save(sample_2018, sample_2020,
     file = here::here("datasets", "raw", "m3_nc_raw_nohf.RData"))

# this is my sample
sample_2020 <- get_sample(dm_contacts = bp_df,
                          baseline = "2017-01-01", 
                          censor = "2021-01-01", 
                          remove_hf = TRUE, 
                          intervention_period = c("2020"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 2, 
                                                      "in_how_many_years" = 3, 
                                                      "cutoff" = 3))

# negative control sample
sample_2018 <- get_sample(dm_contacts = bp_df,
                          baseline = "2015-01-01", 
                          censor = "2019-01-01", 
                          remove_hf = TRUE, 
                          intervention_period = c("2018"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 2, 
                                                      "in_how_many_years" = 3,
                                                      "cutoff" = 3))


save(sample_2018, sample_2020,
     file = here::here("datasets", "raw", "m2_nc_raw_nohf.RData"))


# this is my sample
sample_2020 <- get_sample(dm_contacts = bp_df,
                          baseline = "2017-01-01", 
                          censor = "2021-01-01", 
                          remove_hf = TRUE,
                          intervention_period = c("2020"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 1, 
                                                      "in_how_many_years" = 3, 
                                                      "cutoff" = 3))

# negative control sample
sample_2018 <- get_sample(dm_contacts = bp_df,
                          baseline = "2015-01-01", 
                          censor = "2019-01-01", 
                          remove_hf = TRUE,
                          intervention_period = c("2018"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 1, 
                                                      "in_how_many_years" = 3,
                                                      "cutoff" = 3))

save(sample_2018, sample_2020,
     file = here::here("datasets", "raw", "m1_nc_raw_nohf.RData"))

# ------- Outcomes: MACE, hf kept--------------------------------
# .


# this is my sample
sample_2020 <- get_sample(dm_contacts = bp_df,
                          baseline = "2017-01-01", 
                          censor = "2021-01-01", 
                          remove_hf = FALSE, 
                          intervention_period = c("2020"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 3, 
                                                      "in_how_many_years" = 3, 
                                                      "cutoff" = 3))

# negative control sample
sample_2018 <- get_sample(dm_contacts = bp_df,
                          baseline = "2015-01-01", 
                          censor = "2019-01-01", 
                          remove_hf = FALSE, 
                          intervention_period = c("2018"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 3, 
                                                      "in_how_many_years" = 3,
                                                      "cutoff" = 3))


save(sample_2018, sample_2020,
     file = here::here("datasets", "raw", "m3_nc_raw_whf.RData"))

# this is my sample
sample_2020 <- get_sample(dm_contacts = bp_df,
                          baseline = "2017-01-01", 
                          censor = "2021-01-01", 
                          remove_hf = FALSE, 
                          intervention_period = c("2020"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 2, 
                                                      "in_how_many_years" = 3, 
                                                      "cutoff" = 3))

# negative control sample
sample_2018 <- get_sample(dm_contacts = bp_df,
                          baseline = "2015-01-01", 
                          censor = "2019-01-01", 
                          remove_hf = FALSE,
                          intervention_period = c("2018"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 2, 
                                                      "in_how_many_years" = 3,
                                                      "cutoff" = 3))


save(sample_2018, sample_2020,
     file = here::here("datasets", "raw", "m2_nc_raw_whf.RData"))


# this is my sample
sample_2020 <- get_sample(dm_contacts = bp_df,
                          baseline = "2017-01-01", 
                          censor = "2021-01-01", 
                          remove_hf = FALSE,
                          intervention_period = c("2020"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 1, 
                                                      "in_how_many_years" = 3, 
                                                      "cutoff" = 3))

# negative control sample
sample_2018 <- get_sample(dm_contacts = bp_df,
                          baseline = "2015-01-01", 
                          censor = "2019-01-01", 
                          remove_hf = FALSE, 
                          intervention_period = c("2018"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 1, 
                                                      "in_how_many_years" = 3,
                                                      "cutoff" = 3))

save(sample_2018, sample_2020,
     file = here::here("datasets", "raw", "m1_nc_raw_whf.RData"))


# ------- Outcomes: MACE, hf kept in --------------------------------
# . -----------------------------------------------------------------  


# this is my sample
sample_2020 <- get_sample(dm_contacts = bp_df,
                          baseline = "2017-01-01", 
                          censor = "2021-01-01", 
                          remove_cvd_events = TRUE, 
                          intervention_period = c("2020"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 3, 
                                                      "in_how_many_years" = 3, 
                                                      "cutoff" = 3))

# negative control sample
sample_2018 <- get_sample(dm_contacts = bp_df,
                          baseline = "2015-01-01", 
                          censor = "2019-01-01", 
                          remove_cvd_events = TRUE, 
                          intervention_period = c("2018"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 3, 
                                                      "in_how_many_years" = 3,
                                                      "cutoff" = 3))


save(sample_2018, sample_2020,
     file = here::here("datasets", "raw", "m3_nc_raw_mace_hf.RData"))

# this is my sample
sample_2020 <- get_sample(dm_contacts = bp_df,
                          baseline = "2017-01-01", 
                          censor = "2021-01-01", 
                          remove_cvd_events = TRUE, 
                          intervention_period = c("2020"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 2, 
                                                      "in_how_many_years" = 3, 
                                                      "cutoff" = 3))

# negative control sample
sample_2018 <- get_sample(dm_contacts = bp_df,
                          baseline = "2015-01-01", 
                          censor = "2019-01-01", 
                          remove_cvd_events = TRUE, 
                          intervention_period = c("2018"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 2, 
                                                      "in_how_many_years" = 3,
                                                      "cutoff" = 3))


save(sample_2018, sample_2020,
     file = here::here("datasets", "raw", "m2_nc_raw_mace_hf.RData"))


# this is my sample
sample_2020 <- get_sample(dm_contacts = bp_df,
                          baseline = "2017-01-01", 
                          censor = "2021-01-01", 
                          remove_cvd_events = TRUE, 
                          intervention_period = c("2020"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 1, 
                                                      "in_how_many_years" = 3, 
                                                      "cutoff" = 3))

# negative control sample
sample_2018 <- get_sample(dm_contacts = bp_df,
                          baseline = "2015-01-01", 
                          censor = "2019-01-01", 
                          remove_cvd_events = TRUE, 
                          intervention_period = c("2018"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 1, 
                                                      "in_how_many_years" = 3,
                                                      "cutoff" = 3))

save(sample_2018, sample_2020,
     file = here::here("datasets", "raw", "m1_nc_raw_mace_hf.RData"))





# . -----------------------------------------------------------------
#   ------- # Add covariates ------------------------------------------
# . -----------------------------------------------------------------
dat <- readr::read_rds(file = here::here("datasets", "data.RData"))


# MACE w/o heart failure  ----------

## min 1 measurement a year pre-pandemic -------
load(here::here("datasets", "raw", "m1_nc_raw_nohf.RData"))
source(here::here("R", "03_add_covariates.R"))
source(here::here("R", "04_convert_to_long.R"))

# outcome is mace, mortality as competing event is non cvd death
temp <-  convert_to_long(sample_2020, sample_2018, 
                         mace, died_non_cvd)

sample_2020_long <- temp[[1]]
sample_2018_long <- temp[[2]]
sample_2020 <- temp[[3]]
sample_2018 <- temp[[4]]

save(sample_2020, sample_2018, sample_2020_long, sample_2018_long,
     file = here::here("datasets", "covar", "nohf", "m1_nc_covar_nohf_mace.RData"))


## min 2 measurements a year pre-pandemic ------
load(here::here("datasets", "raw", "m2_nc_raw_nohf.RData"))
source(here::here("R", "03_add_covariates.R"))
source(here::here("R", "04_convert_to_long.R"))

# outcome is mace, mortality as competing event is non cvd death
temp <-  convert_to_long(sample_2020, sample_2018, 
                         mace, died_non_cvd)

sample_2020_long <- temp[[1]]
sample_2018_long <- temp[[2]]
sample_2020 <- temp[[3]]
sample_2018 <- temp[[4]]

save(sample_2020, sample_2018, sample_2020_long, sample_2018_long,
     file = here::here("datasets", "covar", "nohf", "m2_nc_covar_nohf_mace.RData"))


## min 3 measurements a year pre-pandemic ------
load(here::here("datasets", "raw", "m3_nc_raw_nohf.RData"))
source(here::here("R", "03_add_covariates.R"))
source(here::here("R", "04_convert_to_long.R"))

# outcome is mace, mortality as competing event is non cvd death
temp <-  convert_to_long(sample_2020, sample_2018, 
                         mace, died_non_cvd)

sample_2020_long <- temp[[1]]
sample_2018_long <- temp[[2]]
sample_2020 <- temp[[3]]
sample_2018 <- temp[[4]]

save(sample_2020, sample_2018, sample_2020_long, sample_2018_long,
     file = here::here("datasets", "covar", "nohf", "m3_nc_covar_nohf_mace.RData"))



# . -----------
# MACE w/ heart failure  ----------

## min 1 measurement a year pre-pandemic -------
load(here::here("datasets", "raw", "m1_nc_raw_whf.RData"))
source(here::here("R", "03_add_covariates.R"))
source(here::here("R", "04_convert_to_long.R"))

# outcome is mace, mortality as competing event is non cvd death
temp <-  convert_to_long(sample_2020, sample_2018, 
                         mace, died_non_cvd)

sample_2020_long <- temp[[1]]
sample_2018_long <- temp[[2]]
sample_2020 <- temp[[3]]
sample_2018 <- temp[[4]]

save(sample_2020, sample_2018, sample_2020_long, sample_2018_long,
     file = here::here("datasets", "covar", "whf", "m1_nc_covar_whf_mace.RData"))


## min 2 measurements a year pre-pandemic ------
load(here::here("datasets", "raw", "m2_nc_raw_whf.RData"))
source(here::here("R", "03_add_covariates.R"))
source(here::here("R", "04_convert_to_long.R"))

# outcome is mace, mortality as competing event is non cvd death
temp <-  convert_to_long(sample_2020, sample_2018, 
                         mace, died_non_cvd)

sample_2020_long <- temp[[1]]
sample_2018_long <- temp[[2]]
sample_2020 <- temp[[3]]
sample_2018 <- temp[[4]]

save(sample_2020, sample_2018, sample_2020_long, sample_2018_long,
     file = here::here("datasets", "covar", "whf", "m2_nc_covar_whf_mace.RData"))


## min 3 measurements a year pre-pandemic ------
load(here::here("datasets", "raw", "m3_nc_raw_whf.RData"))
source(here::here("R", "03_add_covariates.R"))
source(here::here("R", "04_convert_to_long.R"))

# outcome is mace, mortality as competing event is non cvd death
temp <-  convert_to_long(sample_2020, sample_2018, 
                         mace, died_non_cvd)

sample_2020_long <- temp[[1]]
sample_2018_long <- temp[[2]]
sample_2020 <- temp[[3]]
sample_2018 <- temp[[4]]

save(sample_2020, sample_2018, sample_2020_long, sample_2018_long,
     file = here::here("datasets", "covar", "whf", "m3_nc_covar_whf_mace.RData"))


# .-------------------
# CVD w/o heart failure  ----------

## min 1 measurement a year pre-pandemic -------
load(here::here("datasets", "raw", "m1_nc_raw_nohf.RData"))
source(here::here("R", "03_add_covariates.R"))
source(here::here("R", "04_convert_to_long.R"))

# outcome is mace, mortality as competing event is non cvd death
temp <-  convert_to_long(sample_2020, sample_2018, 
                         cvd, died_all)

sample_2020_long <- temp[[1]]
sample_2018_long <- temp[[2]]
sample_2020 <- temp[[3]]
sample_2018 <- temp[[4]]

save(sample_2020, sample_2018, sample_2020_long, sample_2018_long,
     file = here::here("datasets", "covar", "nohf", "m1_nc_covar_nohf_cvd.RData"))


## min 2 measurements a year pre-pandemic ------
load(here::here("datasets", "raw", "m2_nc_raw_nohf.RData"))
source(here::here("R", "03_add_covariates.R"))
source(here::here("R", "04_convert_to_long.R"))

# outcome is mace, mortality as competing event is non cvd death
temp <-  convert_to_long(sample_2020, sample_2018, 
                         cvd, died_all)

sample_2020_long <- temp[[1]]
sample_2018_long <- temp[[2]]
sample_2020 <- temp[[3]]
sample_2018 <- temp[[4]]

save(sample_2020, sample_2018, sample_2020_long, sample_2018_long,
     file = here::here("datasets", "covar", "nohf", "m2_nc_covar_nohf_cvd.RData"))


## min 3 measurements a year pre-pandemic ------
load(here::here("datasets", "raw", "m3_nc_raw_nohf.RData"))
source(here::here("R", "03_add_covariates.R"))
source(here::here("R", "04_convert_to_long.R"))

# outcome is mace, mortality as competing event is non cvd death
temp <-  convert_to_long(sample_2020, sample_2018, 
                         cvd, died_all)

sample_2020_long <- temp[[1]]
sample_2018_long <- temp[[2]]
sample_2020 <- temp[[3]]
sample_2018 <- temp[[4]]

save(sample_2020, sample_2018, sample_2020_long, sample_2018_long,
     file = here::here("datasets", "covar", "nohf", "m3_nc_covar_nohf_macehf.RData"))


# .-------------------
# MACE + heart failure as outcome  ----------

## min 1 measurement a year pre-pandemic -------
load(here::here("datasets", "raw", "m1_nc_raw_nohf.RData"))
source(here::here("R", "03_add_covariates.R"))
source(here::here("R", "04_convert_to_long.R"))

# outcome is mace + heart failure, mortality as competing event is non-cvd death
temp <-  convert_to_long(sample_2020, sample_2018, 
                         cvd, died_all)

sample_2020_long <- temp[[1]]
sample_2018_long <- temp[[2]]
sample_2020 <- temp[[3]]
sample_2018 <- temp[[4]]

save(sample_2020, sample_2018, sample_2020_long, sample_2018_long,
     file = here::here("datasets", "covar", "macehf", "m1_nc_covar_nohf_macehf.RData"))


## min 2 measurements a year pre-pandemic ------
load(here::here("datasets", "raw", "m2_nc_raw_nohf.RData"))
source(here::here("R", "03_add_covariates.R"))
source(here::here("R", "04_convert_to_long.R"))

# outcome is mace, mortality as competing event is non cvd death
temp <-  convert_to_long(sample_2020, sample_2018, 
                         cvd, died_all)

sample_2020_long <- temp[[1]]
sample_2018_long <- temp[[2]]
sample_2020 <- temp[[3]]
sample_2018 <- temp[[4]]

save(sample_2020, sample_2018, sample_2020_long, sample_2018_long,
     file = here::here("datasets", "covar", "macehf", "m2_nc_covar_nohf_macehf.RData"))


## min 3 measurements a year pre-pandemic ------
load(here::here("datasets", "raw", "m3_nc_raw_nohf.RData"))
source(here::here("R", "03_add_covariates.R"))
source(here::here("R", "04_convert_to_long.R"))

# outcome is mace, mortality as competing event is non cvd death
temp <-  convert_to_long(sample_2020, sample_2018, 
                         cvd, died_all)

sample_2020_long <- temp[[1]]
sample_2018_long <- temp[[2]]
sample_2020 <- temp[[3]]
sample_2018 <- temp[[4]]

save(sample_2020, sample_2018, sample_2020_long, sample_2018_long,
     file = here::here("datasets", "covar", "macehf", "m3_nc_covar_nohf_cvd.RData"))

  


# . ---------
# CVD w/ heart failure  ----------

## min 1 measurement a year pre-pandemic -------
load(here::here("datasets", "raw", "m1_nc_raw_whf.RData"))
source(here::here("R", "03_add_covariates.R"))

sample_2020 <- sample_2020 |>
  dplyr::filter(diagnosis_date > ymd("2021-01-01") | is.na(diagnosis_date))
sample_2018 <- sample_2018 |>
  dplyr::filter(diagnosis_date > ymd("2019-01-01") | is.na(diagnosis_date))

source(here::here("R", "04_convert_to_long.R"))

# outcome is mace, mortality as competing event is non cvd death
temp <-  convert_to_long(sample_2020, sample_2018, 
                         cvd, died_all)

sample_2020_long <- temp[[1]]
sample_2018_long <- temp[[2]]
sample_2020 <- temp[[3]]
sample_2018 <- temp[[4]]

save(sample_2020, sample_2018, sample_2020_long, sample_2018_long,
     file = here::here("datasets", "covar", "whf", "m1_nc_covar_whf_cvd.RData"))


## min 2 measurements a year pre-pandemic ------
load(here::here("datasets", "raw", "m2_nc_raw_whf.RData"))
source(here::here("R", "03_add_covariates.R"))
sample_2020 <- sample_2020 |>
  dplyr::filter(diagnosis_date > ymd("2021-01-01") | is.na(diagnosis_date))
sample_2018 <- sample_2018 |>
  dplyr::filter(diagnosis_date > ymd("2019-01-01") | is.na(diagnosis_date))

source(here::here("R", "04_convert_to_long.R"))

# outcome is mace, mortality as competing event is non cvd death
temp <-  convert_to_long(sample_2020, sample_2018, 
                         cvd, died_all)

sample_2020_long <- temp[[1]]
sample_2018_long <- temp[[2]]
sample_2020 <- temp[[3]]
sample_2018 <- temp[[4]]

save(sample_2020, sample_2018, sample_2020_long, sample_2018_long,
     file = here::here("datasets", "covar", "whf", "m2_nc_covar_whf_cvd.RData"))


## min 3 measurements a year pre-pandemic ------
load(here::here("datasets", "raw", "m3_nc_raw_whf.RData"))
source(here::here("R", "03_add_covariates.R"))
sample_2020 <- sample_2020 |>
  dplyr::filter(diagnosis_date > ymd("2021-01-01") | is.na(diagnosis_date))
sample_2018 <- sample_2018 |>
  dplyr::filter(diagnosis_date > ymd("2019-01-01") | is.na(diagnosis_date))

source(here::here("R", "04_convert_to_long.R"))

# outcome is mace, mortality as competing event is non cvd death
temp <-  convert_to_long(sample_2020, sample_2018, 
                         cvd, died_all)

sample_2020_long <- temp[[1]]
sample_2018_long <- temp[[2]]
sample_2020 <- temp[[3]]
sample_2018 <- temp[[4]]

save(sample_2020, sample_2018, sample_2020_long, sample_2018_long,
     file = here::here("datasets", "covar", "whf", "m3_nc_covar_whf_cvd.RData"))