

# convert the dataset to long format 
convert_to_long <- function(dat_2020, dat_2018, outcome, mortality) {
  
  
  # round all incident dates to 15th day of the month
  dat_2020$diagnosis_date <- lubridate::floor_date(dat_2020$diagnosis_date, 
                                                   unit = "months") + days(14)
  
  dat_2018$diagnosis_date <- lubridate::floor_date(dat_2018$diagnosis_date, 
                                                   unit = "months") + days(14)
  
  # round all deaths to the 15th day in the previous month
  dat_2020$pat_dom <- lubridate::floor_date(dat_2020$pat_dom - days(5), 
                                                   unit = "months") + days(14)
  
  dat_2018$pat_dom <- lubridate::floor_date(dat_2018$pat_dom -  days(5), 
                                                   unit = "months") + days(14)  
  
  dat_2020$date_of_death <- dat_2020$pat_dom
  dat_2018$date_of_death <- dat_2018$pat_dom
  
  # now we must also change the fu_end as mortality and events are the fu_end
  dat_2020 <- dat_2020 |>
    dplyr::mutate(fu_end = if_else(pat_dom < fu_end, pat_dom, fu_end, missing = fu_end)) |>
    dplyr::mutate(fu_end = if_else(lubridate::floor_date(diagnosis_date, unit = "months") ==
                                   lubridate::floor_date(fu_end, unit = "months"), diagnosis_date, fu_end, missing = fu_end)) 
    
  
  dat_2018 <- dat_2018 |>
    dplyr::mutate(fu_end = if_else(pat_dom < fu_end, pat_dom, fu_end, missing = fu_end)) |>
    dplyr::mutate(fu_end = if_else(lubridate::floor_date(diagnosis_date, unit = "months") ==
                                     lubridate::floor_date(fu_end, unit = "months"), diagnosis_date, fu_end, missing = fu_end)) 
  
  
  # now for those with no event, we move the end date to the previous month due IPCI practice of
  # setting the end day in the following month of the actual end date
  dat_2020 <- dat_2020 |>
    # we only need to select those without event, hence we need to filter out those with cvd mortality and those with cvd event
    dplyr::mutate(fu_end = if_else(day(fu_end) == 1 & is.na(diagnosis_date) & is.na(cause), lubridate::floor_date(fu_end - days(5), unit = "months") + days(14), fu_end))

  dat_2018 <- dat_2018 |>
    dplyr::mutate(fu_end = if_else(day(fu_end) == 1  & is.na(diagnosis_date) & is.na(cause), lubridate::floor_date(fu_end - days(5), unit = "months") + days(14), fu_end))
  
  
  # time in months
  dat_2020$time <- lubridate::ymd("2021-01-01") %--% dat_2020$fu_end / months(1) + 1
  dat_2018$time <- lubridate::ymd("2019-01-01") %--% dat_2018$fu_end / months(1) + 1
  
  
  # now round the time so it is always an integer. 
  dat_2020$time <- floor(dat_2020$time) 
  dat_2018$time <- floor(dat_2018$time) 
  
  
  dat_2020_long <- dat_2020 |>
    uncount(time, .id = "month", .remove = FALSE)
  
  dat_2018_long <- dat_2018 |>
    uncount(time, .id = "month", .remove = FALSE)
    
  # # number of months in study for all patients 
  # dat_2020 <- dat_2020 |>
  #   dplyr::mutate(n_months = floor((ymd("2021-01-01") %--% fu_end) / months(1))  + 1)
  # 
  # # number of months in the follow up.This will be the same for the experimental
  # # and the negative control sample
  # fu_time <- max(dat_2020$n_months)
  # 
  # dat_2018 <- dat_2018 |>
  #   dplyr::mutate(n_months = floor((ymd("2019-01-01") %--% fu_end) / months(1)) + 1,
  #                 # if the number of months exceeds the fu_time derived from the 
  #                 # experimental sample, it will be capped
  #                 n_months = if_else(n_months > fu_time, fu_time, n_months))
  # 
  # # copy each row in the dataset as many times as the number of months each patient stays on study
  # dat_2020_long <- dat_2020[rep(seq_len(nrow(dat_2020)), times = dat_2020$n_months), ] 
  # dat_2018_long <- dat_2018[rep(seq_len(nrow(dat_2018)), times = dat_2018$n_months), ] 
  # 
  # # now, create vector with sequences of months according to how many months each patients stayed
  # # on study
  # s_2020 <- c()
  # s_2018 <- c()
  # 
  # 
  # 
  # for (i in 1:length(dat_2020$n_months)) {
  #   s_2020 <- c(s_2020, rep(1:fu_time, length.out = dat_2020$n_months[i]))
  # }
  # for (i in 1:length(dat_2018$n_months)) {
  #   s_2018 <- c(s_2018, rep(1:fu_time, length.out = dat_2018$n_months[i]))
  # }
  # 
  # # copy this to the data frame
  # dat_2020_long$month <- s_2020
  # dat_2018_long$month <- s_2018
  
  # update the event and mortality indicator to only be true at the last month of the follow up
  o <- dplyr::enquo(outcome)
  m <- dplyr::enquo(mortality)
  
  
  update_events <- . %>%
    group_by(pat_id) %>%
    dplyr::mutate(mort = if_else(!!m == TRUE & month == time, TRUE, FALSE),
                  out = if_else(!!o == TRUE & month == time, TRUE, FALSE), 
                  
                  # died2 = if_else(died2 == TRUE & month == n_months, TRUE, FALSE),
                  # event2 = if_else(event2 == TRUE & month == n_months, TRUE, FALSE)
    )
  
  dat_2020_long <- dat_2020_long %>% update_events
  dat_2018_long <- dat_2018_long %>% update_events
  
  
  
  list(sample_2020_long = dat_2020_long, sample_2018_long = dat_2018_long,
       sample_2020 = dat_2020, sample_2018 = sample_2018)
  
}


# this is for the bootstrapping where we need to generate the long format 
# for each boostrap sample
convert_to_long_single <- function(dat, outcome, mortality) {
  
  dat_long <- dat |>
    uncount(time, .id = "month", .remove = FALSE)
  
  # # number of months in study for all patients 
  # dat_2020 <- dat_2020 |>
  #   dplyr::mutate(n_months = floor((ymd("2021-01-01") %--% fu_end) / months(1))  + 1)
  # 
  # # number of months in the follow up.This will be the same for the experimental
  # # and the negative control sample
  # fu_time <- max(dat_2020$n_months)
  # 
  # dat_2018 <- dat_2018 |>
  #   dplyr::mutate(n_months = floor((ymd("2019-01-01") %--% fu_end) / months(1)) + 1,
  #                 # if the number of months exceeds the fu_time derived from the 
  #                 # experimental sample, it will be capped
  #                 n_months = if_else(n_months > fu_time, fu_time, n_months))
  # 
  # # copy each row in the dataset as many times as the number of months each patient stays on study
  # dat_2020_long <- dat_2020[rep(seq_len(nrow(dat_2020)), times = dat_2020$n_months), ] 
  # dat_2018_long <- dat_2018[rep(seq_len(nrow(dat_2018)), times = dat_2018$n_months), ] 
  # 
  # # now, create vector with sequences of months according to how many months each patients stayed
  # # on study
  # s_2020 <- c()
  # s_2018 <- c()
  # 
  # 
  # 
  # for (i in 1:length(dat_2020$n_months)) {
  #   s_2020 <- c(s_2020, rep(1:fu_time, length.out = dat_2020$n_months[i]))
  # }
  # for (i in 1:length(dat_2018$n_months)) {
  #   s_2018 <- c(s_2018, rep(1:fu_time, length.out = dat_2018$n_months[i]))
  # }
  # 
  # # copy this to the data frame
  # dat_2020_long$month <- s_2020
  # dat_2018_long$month <- s_2018
  
  # update the event and mortality indicator to only be true at the last month of the follow up
  o <- dplyr::enquo(outcome)
  m <- dplyr::enquo(mortality)
  
  
  dat_long <- dat_long %>%
    group_by(pat_id) %>%
    dplyr::mutate(mort = if_else(!!m == TRUE & month == time, TRUE, FALSE),
                  out = if_else(!!o == TRUE & month == time, TRUE, FALSE), 
                  
                  # died2 = if_else(died2 == TRUE & month == n_months, TRUE, FALSE),
                  # event2 = if_else(event2 == TRUE & month == n_months, TRUE, FALSE)
    )
  
  dat_long
  
}





