## .------------------------------------------
##
## Project: Zorgmijding 2
##
## .------------------------------------------
## Name: get_sample
## .------------------------------------------
## Type: helper
## .------------------------------------------
## Author: Premysl Velek
## .------------------------------------------
## Email: p.velek@erasmusmc.nl
## .------------------------------------------
## Date created: 2024-05-31
## .------------------------------------------
## Description:
## 
## This is a helper function that takes all the data and select the population
## sample according to give criteria
## 
## .------------------------------------------
##
## date created: 2024-05-31
## 
## date last edited: Fri May 31 16:00:43 2024 
##
## .-----------------------------------------
##
## notes
##
## .------------------------------------------


## set working directory
setwd(here::here())


## .------------------------------------------

options(scipen = 6, digits = 4)


## .-------------------------------------------

## load packages
require(tidyverse)
require(lubridate)
require(colorspace)
library(runner)

##. -------------------------------------------



# ------------------------------



# 
# reformat_df <- . %>%
#   dplyr::mutate(year = year(contact_date),
#                 month_year = lubridate::floor_date(contact_date, unit = "month"),
#                 week_year = lubridate::floor_date(contact_date, unit = "week",
#                                                   week_start = getOption("lubridate.week.start", 1)),
#                 pat_id = as.factor(pat_id)) %>%
#   dplyr::distinct(pat_id, week_year, month_year, year)
# 
# 
# # only select office blood pressure measurement
# bp_df <- dat$bp |>
#   dplyr::filter(measure == "Office blood pressure measurement (OBPM)") %>%
#   reformat_df
# 
# # patients
# patients_df <- dat$patients
# 
# # all prevalent data (including diseases other than DM2 !!!)
# prevalence_df <- dat$prevalent_cases
# 
# # who entered specialist care for DM2
# specialist_df <- dat$specialist_care
# 
# inc_medication_df <-  dat$inc_med
# 
# medication_df <- dat$med %>% reformat_df |>
#   dplyr::mutate(year = year(week_year))
# 
# dm_contacts = bp_df
# 
# patients = patients_df
# dm_contacts = bp_df
# prevalence = prevalence_df
# specialist = specialist_df
# medication = medication_df
# inc_medication <-  inc_medication_df
# baseline = "2018-01-01"
# censor = "2021-01-01"
# intervention_period = c("2020")
# remove_cvd_events = TRUE
# use_visit_cap = TRUE
# by_month = FALSE
# remove_cvd_events = TRUE
# avoidance_definition = list("min_measures" = 3,
#                             "in_how_many_years" = 2,
#                             "cutoff" = 3)


get_sample <- function(patients = patients_df, dm_contacts = dm_contacts_df,
                       prevalence = prevalence_df,
                       specialist = specialist_df,
                       medication = medication_df,
                       inc_medication = inc_medication_df,
                       baseline = "2017-01-01", censor = "2021-01-01",
                       remove_hf = TRUE, 
                       intervention_period,
                       use_visit_cap = TRUE,
                       by_month = FALSE,
                       avoidance_definition = list("min_measures" = 1, 
                                                   "in_how_many_years" = 3, 
                                                   "cutoff" = 3)){
  
  # format input data
  baseline <- lubridate::ymd(baseline)
  censor <- lubridate::ymd(censor)
  patients$pat_id <- as.factor(patients$pat_id)
  dm_contacts$pat_id <- as.factor(dm_contacts$pat_id)
  prevalence$pat_id <- as.factor(prevalence$pat_id)
  specialist$pat_id <- as.factor(specialist$pat_id)
  medication$pat_id <- as.factor(medication$pat_id)
  
  # only look at those practices that contributed enough follow-up time
  practices <- patients |>
    group_by(prak_id) |>
    summarise(prak_fu_start = min(fu_start),
              prak_fu_end = max(fu_end))
  
  # select patients from practices that contributed enough time before and after the pandemic
  # select those with start fu lower than baseline and fu end later than censor date
  practices_include <- practices[practices$prak_fu_start <= baseline & 
                                   practices$prak_fu_end > censor, ]
  
  # here are all patients from practices with enough follow-up time
  patients_include <- patients[patients$prak_id %in% practices_include$prak_id, ] 
  
  # remove patients who died during the baseline or intervention period
  patients_include <- patients_include |>
    dplyr::filter(pat_dom > censor | is.na(pat_dom)) |>
    
    # remove patients with end of follow up before the end of the intervention period
    dplyr::filter(fu_end > censor,
                  fu_start <= baseline) 
  
  
  # now select those with prevalent diabetes out of the patients above
  patients_DM <- prevalence |>
    # we need to select the earlier case if there are two codes (T90.02 and T90.00)
    dplyr::filter(icpc %in% c("T90.02", "T90.00")) |>
    group_by(pat_id) |>
    slice_min(diagnosis_date, with_ties = FALSE) |>
    dplyr::filter(diagnosis_date <= baseline) |>
    
    # now we're inner joining - only rows in patients include that have a match in DM prevalence 
    # will be included
    dplyr::inner_join(x = _, y = patients_include, by = "pat_id") 
  
  # remove patients who entered specialist care during baseline or intervention period
  patients_DM <- dplyr::left_join(patients_DM, specialist, by = "pat_id") |>
    dplyr::group_by(pat_id) |>
    dplyr::slice_min(sc_entry_date) |>
    dplyr::filter((value != "specialist") | (value == "specialist" & sc_entry_date > censor)) |>
    dplyr::select(-c(cod, value)) |>
    dplyr::distinct()
  
  # ketenzorg included
  # patients_DM <- dplyr::left_join(patients_DM, specialist, by = "pat_id") |>
  #   dplyr::group_by(pat_id) |>
  #   dplyr::slice_min(sc_entry_date) |>
  #   dplyr::filter(is.na(sc_entry_date) | sc_entry_date > censor) |>
  #   dplyr::select(-c(cod, value)) |>
  #   dplyr::distinct()
  
  
  # remove all with prevalent cvd event by the end of the intervention period
  # we're removing heart attack, stroke/TIA
  cvd_events <- c('K90.00', 'K90.01', 'K90.02', 'K90.03', 'K89.00', 'K75.00'
                  # now adding peripheral artery disease 
                  # , 'K91.00', 'K92.00', 'K92.01', 'K92.02', 'K92.03'
                  )
  
  # now we're removing heart failure
  if (remove_hf) {
    cvd_events <- c(cvd_events, 'K77.00' , 'K77.01', 'K77.03', 'K77.04')
  }
  
  prev_cvd <- prevalence |>
      dplyr::filter((icpc %in% cvd_events) & (diagnosis_date <= censor))
    
  patients_DM <- patients_DM |> 
      dplyr::filter(!(pat_id %in% prev_cvd$pat_id))

  
  # now finally select measurements done for the above group
  dm_contacts_DM <- dm_contacts |>
    dplyr::filter(pat_id %in% patients_DM$pat_id) |>
    dplyr::mutate(pat_id = droplevels(pat_id),
                  year = as.factor(year))
  
  # Number of measurements and how they are counted: if by month, multiple measurements
  # within the same month will be counted as one
  if(by_month){
    dm_contacts_DM <- dm_contacts_DM |>
      dplyr::ungroup() |>
    # group_by(pat_id, week_year) |>
    # dplyr::mutate(systolic_median = median(systolic, na.rm = TRUE),
    #               diastolic_median = median(diastolic, na.rm = TRUE),
    # ) |>
      dplyr::distinct(pat_id, month_year, .keep_all = TRUE) |>
      dplyr::ungroup()
  
  # if by week, we'll do the same but within week  
  } else {
    dm_contacts_DM <- dm_contacts_DM |>
      dplyr::ungroup() |>
      # group_by(pat_id, week_year) |>
      # dplyr::mutate(systolic_median = median(systolic, na.rm = TRUE),
      #               diastolic_median = median(diastolic, na.rm = TRUE),
      # ) |>
      dplyr::distinct(pat_id, week_year, .keep_all = TRUE) |>
      dplyr::ungroup()
  }
  
  
  # Now count the number of measurements per year for each patient
  no_measurement <- dm_contacts_DM |>
    dplyr::count(pat_id, year, .drop = FALSE)
  
  
  # do the same with medication and incident medication
  no_medication <-  medication |>
    # dplyr::filter(pat_id %in% patients_DM$pat_id) |>
    dplyr::mutate(pat_id = droplevels(pat_id),
                  year = as.factor(year)) |>
    dplyr::count(pat_id, year, .drop = FALSE)
  
  
  no_inc_medication <-  inc_medication |>
    dplyr::mutate(year = lubridate::year(contact_date)) |>
    # dplyr::filter(pat_id %in% patients_DM$pat_id) |>
    dplyr::mutate(pat_id = droplevels(pat_id),
                  year = as.factor(year)) |>
    dplyr::count(pat_id, year, .drop = FALSE)
  
 
  # get the years in the baseline period
  # the number of years in the background period will be the difference between the
  # baseline and the censor, less one (the exposure year)
  background_years <- (lubridate::year(censor) - lubridate::year(baseline)) - 1
  background <- seq(from = lubridate::year(baseline), length.out = background_years)
  
  # convert this into a wide format with one row per patient. It will later be 
  # joined with the final dataset
  study_per <- background_years + 1 + 3
  
  
  no_measurement_long <- no_measurement |>
    dplyr::filter(as.numeric(as.character(year)) >= year(baseline) &
                    as.numeric(as.character(year)) < year(baseline) + study_per) |>
    tidyr::pivot_wider(values_from = n, names_from = year)
  
    # and the same with medication
  no_medication_long <- no_medication |>
    dplyr::filter(as.numeric(as.character(year)) >= year(baseline) &
                    as.numeric(as.character(year)) < year(baseline) + study_per) |>
    tidyr::pivot_wider(values_from = n, names_from = year) 

  # and the same with medication
  no_inc_medication_long <- no_inc_medication |>
    dplyr::filter(as.numeric(as.character(year)) >= year(baseline) &
                    as.numeric(as.character(year)) < year(baseline) + study_per) |>
    tidyr::pivot_wider(values_from = n, names_from = year) 
  
  

 
  
  # rename the year column to avoid any confusion and formatting headaches
  names(no_measurement_long) <- names(no_medication_long) <- 
    c("pat_id", paste0("y", seq(1:study_per)))
  
  names(no_inc_medication_long) <- 
    c("pat_id", paste0("y", seq(1:study_per), "_inc_prescriptions"))
                                     
  # this will get used later on line 407 and 414 when replacing NA with zeros
  pres_years_name <- paste0("y", seq(1:study_per), "_prescriptions")
  inc_pres_years_name <- paste0("y", seq(1:study_per), "_inc_prescriptions")
  
  no_measurement_repeat <- no_measurement |>
    dplyr::ungroup() |>
    dplyr::filter(year %in% background) |>
    dplyr::group_by(pat_id) |>
    dplyr::summarise(years_w_measurement = sum(!(n < avoidance_definition$min_measures)))
  
  pat_enough_measurement <- no_measurement_repeat |>
    dplyr::filter(years_w_measurement >= avoidance_definition$in_how_many_years) |>
    dplyr::pull(pat_id)
  
  
  # now get the average number of measurements per year
  mean_no_measurement <- no_measurement |>
    
    # select only the three pre-pandemic years to calculate the mean number of measurements
    # only select patients with regular attendance to the GP before the pandemic
    dplyr::filter(year %in% background,
                  pat_id %in% pat_enough_measurement) |>
    dplyr::group_by(pat_id) |>
    dplyr::summarise(mean_measurement = mean(n), 
                     median_measurement = median(n)) |>
    
    # now add the actual number of measurements during the pandemic to those patients
    dplyr::left_join(x = _, y = no_measurement |> dplyr::filter(year %in% intervention_period), by = "pat_id") |>
    
    # calculate the change in number of measurements during the pandemic
    dplyr::mutate(delta = mean_measurement - n, 
                  delta_median = median_measurement - n,
                  year = droplevels(year)) 
  
  mean_delta <- mean_no_measurement |>
    dplyr::ungroup() |>
    dplyr::group_by(year) |>
    dplyr::summarise(m = mean(delta))
  
  message(paste0("Mean change in the number of measurements baseline vs. interversion period: "))
  print(mean_delta)
  
  p <- ggplot2::ggplot(mean_no_measurement, aes(x = delta, group = year, colour = year, fill = year)) +
    geom_histogram() +
    stat_summary(aes(xintercept = after_stat(x), y = 0), fun = mean, geom = "vline", orientation = "y",
                 colour = "black") +
    facet_wrap(~ year) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "none") 
  
  print(p)
  
  
  # now categorise patients:
  
  # health avoiders either had zero measurements only in 2020 or both 2020 and 2021
  # they also had to have a mean number of measurements greater than 1
  
  health_avoiders <- mean_no_measurement |>
    dplyr::group_by(pat_id, .drop = FALSE) |>
    # dplyr::summarise(n = sum(n)) |>
    dplyr::mutate(health_avoider_v2 =  if_else(delta >= avoidance_definition$cutoff, TRUE, FALSE),
                  
                  # if you have fewer median visits than the cutoff, then you'll be health avoider
                  # when zero visits in intervention year
                  # health_avoider_v2 =  if_else(median_measurement < avoidance_definition$cutoff &
                  #                                sum(n) == 0, TRUE, health_avoider_v2),
                  num_intervention = n)
  
  # this is user defined option
  if(use_visit_cap){
    health_avoiders <- health_avoiders |>
      # if you have 4 or more measurements in the intervention year, you will not be
      # classified as a health avoider, not matter how much your number of measurements
      # dropped
      dplyr::mutate(health_avoider_v2 =  if_else(num_intervention >= 4, FALSE, health_avoider_v2))
  }
                  
      
  health_avoiders <- health_avoiders |>
    
    dplyr::select(pat_id, health_avoider_v2, num_intervention) |>
    dplyr::distinct(pat_id, .keep_all = TRUE)
  
  # sample size, sample size for each group
  message(paste0("Number of healh avoiders vs non-health avoiders: "))
  print(addmargins(table("Health avoiders" = health_avoiders$health_avoider_v2)))
  print(round(prop.table(table("Health avoiders" = health_avoiders$health_avoider_v2)), 3))
  
  health_avoiders <- dplyr::left_join(health_avoiders, y = patients, by = "pat_id") 
  health_avoiders$age <- (health_avoiders$pat_dob %--% (baseline + years(3)) /  years(1))
  
  # mean age 
  message(paste0("Mean age by health avoidance statut: "))
  print(tapply(health_avoiders$age, health_avoiders$health_avoider_v2, mean))
  
  # sex ratio
  message(paste0("Sex ratio by health avoidance statut: "))
  print(prop.table(table("sex" = health_avoiders$sex, "health avoider" = health_avoiders$health_avoider_v2), 2))
  
  
  # now add to the data also measurements in the year after the intervention
  after_intervention <- as.character(as.numeric(intervention_period) + 1)
  health_avoiders <- dplyr::left_join(health_avoiders, 
                                      no_measurement[no_measurement$year %in% after_intervention, ],
                                      by = "pat_id")
  
  health_avoiders <- dplyr::left_join(health_avoiders, 
                                      mean_no_measurement |> dplyr::select(delta, delta_median, pat_id, 
                                                                           median_measurement, mean_measurement),
                                      by = "pat_id")
  
  
  # and finally, add annual data from all measurements from baseline onwards
  health_avoiders <- dplyr::left_join(health_avoiders, 
                                      no_measurement_long,
                                      by = "pat_id")
  
  
  # and finally, add prescription data from baseline onwards
  health_avoiders <- dplyr::left_join(health_avoiders, 
                                      no_medication_long,
                                      by = "pat_id", suffix = c("_contacts", "_prescriptions")) |>
    
    # replace NA with 0
    dplyr::mutate(across(!!pres_years_name[1]:!!pres_years_name[length(pres_years_name)], 
                         ~ tidyr::replace_na(., 0)))
  
  
  health_avoiders <- dplyr::left_join(health_avoiders, 
                                      no_inc_medication_long,
                                      by = "pat_id") |>
    
    # replace NA with 0
    dplyr::mutate(across(!!inc_pres_years_name[1]:!!inc_pres_years_name[length(inc_pres_years_name)], 
                         ~ tidyr::replace_na(., 0)))
  
  # get the column names of the background prescription years. To calculate the change 
  # we need to know the number of background years and map them onto the relevant variables
  pres_name <- paste0("y", seq(1, length.out = background_years + 1), "_prescriptions")
  inc_pres_name <- paste0("y", seq(1, length.out = background_years + 1), "_inc_prescriptions")
  
  # and finally, calculate the change in prescriptions
  health_avoiders <- health_avoiders |>
    dplyr::rowwise() |>
    dplyr::mutate(
      mean_prescription =         mean(c_across(y1_prescriptions:!!pres_name[length(pres_name) - 1]), na.rm = TRUE),
      median_prescription =     median(c_across(y1_prescriptions:!!pres_name[length(pres_name) - 1]), na.rm = TRUE),
      
      mean_inc_prescription =     mean(c_across(y1_inc_prescriptions:!!inc_pres_name[length(inc_pres_name) - 1]), na.rm = TRUE),
      median_inc_prescription = median(c_across(y1_inc_prescriptions:!!inc_pres_name[length(inc_pres_name) - 1]), na.rm = TRUE)
    ) |>
    dplyr::ungroup() 
  
  # This doesn't work anymore, need to be calculated manually after!!!
    # dplyr::mutate(delta_prescription_median = median_prescription - !!pres_name[length(pres_name)],
    #               delta_prescription_mean = mean_prescription - !!pres_name[length(pres_name)],
    #               
    #               delta_inc_prescription_median = median_inc_prescription - !!inc_pres_name[length(inc_pres_name)],
    #               delta_inc_prescription_mean =     mean_inc_prescription - !!inc_pres_name[length(inc_pres_name)]) 
  
  
  # add the percent change to the main dataset
  health_avoiders <-  health_avoiders |>
    dplyr::mutate(delta_perc_median = 1 - (num_intervention / median_measurement),
                  delta_perc_mean = 1 - (num_intervention / mean_measurement)) 
  
  health_avoiders

}


