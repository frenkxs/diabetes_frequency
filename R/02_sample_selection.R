library(adjustedCurves)
library(survival)
library(WeightIt)
library(cobalt)
library(patchwork)
library(table1)
library(smd)
 

#. ------------
# ------------ Sample selection ----------
#. ------------

# Build sample population 
# source(here::here("R", "01_load_data.R")) 
# dat <- readr::write_rds(dat, file = here::here("datasets", "data.RData"))

dat <- readr::read_rds(file = here::here("datasets", "data.RData"))

rm(list = setdiff(ls(), c("dat")))
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

# this is my sample
sample_2020 <- get_sample(dm_contacts = bp_df,
                          baseline = "2017-01-01", 
                          censor = "2021-01-01", 
                          remove_cvd_events = TRUE, 
                          intervention_period = c("2020"), 
                          use_visit_cap = FALSE,
                          by_month = TRUE,
                          avoidance_definition = list("min_measures" = 3, 
                                                      "in_how_many_years" = 4, 
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


temp <- bind_rows(sample_2020 |> add_column(sample = "2020"), 
                  sample_2018 |> add_column(sample = "2018"))

ggplot(temp, aes(x = delta, fill = sample, colour = sample, group = sample)) +
  geom_histogram(position = "dodge", alpha = 0.5, bins = 20) +
  scale_x_continuous(breaks = seq(-20, 20, by = 1), limits = c(-10, 10)) +
  # facet_wrap(~ sample) +
  theme_minimal()

ggplot(temp, aes(x = delta_median, fill = sample, colour = sample, group = sample)) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_x_continuous(breaks = seq(-20, 20, by = 1), limits = c(-10, 10)) +
  # facet_wrap(~ sample) +
  theme_minimal()


ggplot(temp, aes(x = delta_prescription_mean, fill = sample, colour = sample, group = sample)) +
  geom_histogram(position = "dodge", alpha = 0.5, bins = 30) +
  scale_x_continuous(breaks = seq(-20, 20, by = 1), limits = c(-10, 10)) +
  # facet_wrap(~ sample) +
  theme_minimal()

ggplot(temp, aes(x = delta_prescription_median, fill = sample, colour = sample, group = sample)) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_x_continuous(breaks = seq(-20, 20, by = 1), limits = c(-10, 10)) +
  # facet_wrap(~ sample) +
  theme_minimal()

ggplot(sample_2020, aes(x = delta_prescription_mean, fill =  health_avoider_v2, 
                        group = health_avoider_v2)) +
  geom_histogram(bins = 50) +
  stat_summary(aes(xintercept = after_stat(x), y = 0, colour = health_avoider_v2), fun = mean, 
               geom = "vline", orientation = "y")

# standardised mean difference between the change in the number of prescriptions
m_ha <- mean(sample_2020$delta_prescription_median[sample_2020$health_avoider_v2 == TRUE])
m_nha <- mean(sample_2020$delta_prescription_median[sample_2020$health_avoider_v2 == FALSE])
p_sd <- sqrt((var(sample_2020$delta_prescription_median[sample_2020$health_avoider_v2 == FALSE]) + 
                var(sample_2020$delta_prescription_median[sample_2020$health_avoider_v2 == TRUE])) / 2)

(m_ha - m_nha) / p_sd




median(sample_2020$delta_prescription_mean[sample_2020$health_avoider_v2 == FALSE])

d_m <- . %>%
  select(y1_contacts:y6_contacts, health_avoider_v2, pat_id) %>%
  pivot_longer(cols = c(y1_contacts:y6_contacts)) 

frmt <- list(
  geom_line(alpha = 0.1),
  geom_jitter(height = 0.3, width = 0, aes(alpha = health_avoider_v2)),
  scale_alpha_discrete(range = c(0.01, 0.5)),
  scale_y_continuous(limits = c(0, 13))
  )


sam <- sample_2020 %>% d_m %>%
  # dplyr::filter(health_avoider_v2 == TRUE) |>
  ggplot(aes(x = name, y = value, group = pat_id, colour = health_avoider_v2)) + frmt +
  ggtitle("sample")

neg_sam <- sample_2018 %>% d_m %>% 
  # dplyr::filter(health_avoider_v2 == TRUE) |>
  ggplot(aes(x = name, y = value, group = pat_id, colour = health_avoider_v2)) + frmt +
  ggtitle("neg sample")

wrap_elements(sam) / wrap_elements(neg_sam)



ggplot(temp, aes(x = delta, fill = sample, colour = sample, group = sample)) +
  geom_histogram(position = "dodge", alpha = 0.5, bins = 30) +
  scale_x_continuous(breaks = seq(-20, 20, by = 1), limits = c(-10, 10)) +
  # facet_wrap(~ sample) +
  theme_minimal()

ggplot(temp, aes(x = age, fill = health_avoider_v2, group = health_avoider_v2)) +
  geom_histogram(position = "dodge", alpha = 0.5, bins = 30) +
  # scale_x_continuous(breaks = seq(-20, 20, by = 1), limits = c(-10, 10)) +
  facet_wrap(~ sample) +
  theme_minimal()


