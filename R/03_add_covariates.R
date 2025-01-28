require(survival)
require(patchwork)
require(smd)
require(table1)

#. ------------
# ------------ Sample selection ----------
#. ------------

# Build sample population 
# source(here::here("R", "02_sample_selection.R"))


# --------- Covariates -----------------------------
#. ------------


# Comorbidities 
add_comorbidities <- . %>%
  dplyr::mutate(
    pat_id = factor(pat_id),
    icpc = substring(icpc, 1, 3),
    disease = case_when(
      icpc == "P70" ~ "Dementia",
      icpc == "R96" ~ "Asthma",
      icpc == "R95" ~ "COPD",
      icpc == "N87" ~ "Parkinsonism",
      icpc == "P76" ~ "Depression",
      icpc %in% c(
        "A79", "B72", "B73", "B74", "D74", "D75", "D76", "D77", "F74",
        "H75", "K72", "L71", "N74", "R84", "R85", "S77", "T71", "U75",
        "U76", "U77", "W72", "X75", "X76", "X77", "Y77", "Y78"
      ) ~ "Cancer"
    )) %>%
  na.omit() %>%
  dplyr::select(-c(icpc, contact_date)) %>%
  dplyr::distinct(pat_id, disease) %>%
  dplyr::mutate(value = TRUE)

comorbidities_df_2020 <- dat$comorbidities %>%
  dplyr::filter(icpc != "T90.02",
                contact_date < lubridate::ymd("2020-01-01")) %>%
  add_comorbidities |>
  pivot_wider(names_from = disease, values_from = value) %>%
  replace(is.na(.), FALSE)

comorbidities_df_2018 <- dat$comorbidities %>%
  dplyr::filter(icpc != "T90.02",
                contact_date < lubridate::ymd("2018-01-01")) %>%
  add_comorbidities |>
  pivot_wider(names_from = disease, values_from = value) %>%
  replace(is.na(.), FALSE)



# retrieve blood pressure, median systolic pressure over the year 2019 and 
# year 2017 for the negative control sample
bp_df_2019 <- dat$bp |>
  dplyr::filter(year(contact_date) == 2019) |>
  # remove outliers
  dplyr::filter(systolic < 240,
                systolic >= 75) |>
  group_by(pat_id) |>
  summarise(sys_bp_exp = median(systolic))

bp_df_2017 <- dat$bp |>
  dplyr::filter(year(contact_date) == 2017) |>
  # remove outliers
  dplyr::filter(systolic < 240,
                systolic >= 75) |>
  group_by(pat_id) |>
  summarise(sys_bp_exp = median(systolic))

bp_df_2020 <- dat$bp |>
  dplyr::filter(year(contact_date) == 2020) |>
  # remove outliers
  dplyr::filter(systolic < 240,
                systolic >= 75) |>
  group_by(pat_id) |>
  summarise(sys_bp_fu = median(systolic))

bp_df_2018 <- dat$bp |>
  dplyr::filter(year(contact_date) == 2018) |>
  # remove outliers
  dplyr::filter(systolic < 240,
                systolic >= 75) |>
  group_by(pat_id) |>
  summarise(sys_bp_fu = median(systolic))

# retrieve glucose levels, mean over the year 2019 and 2017
glu_df_2019 <- dat$glu |>
  dplyr::filter(year(contact_date) %in% c(2019, 2018)) |>
  group_by(pat_id) |>
  summarise(hba1c_exp = mean(value))

glu_df_2017 <- dat$glu |>
  dplyr::filter(year(contact_date) %in% c(2017, 2016)) |>
  group_by(pat_id) |>
  summarise(hba1c_exp = mean(value))


glu_df_2020 <- dat$glu |>
  dplyr::filter(year(contact_date) %in% c(2020, 2019)) |>
  group_by(pat_id) |>
  summarise(hba1c_fu = mean(value))

glu_df_2018 <- dat$glu |>
  dplyr::filter(year(contact_date) %in% c(2018, 2017)) |>
  group_by(pat_id) |>
  summarise(hba1c_fu = mean(value))

# retrieve cholesterol levels, mean over the year 2019
# add_chol <- . %>%
#   dplyr::filter(measure %in% c("HDL-cholesterol", "Cholesterol totaal", "LDL-cholesterol")) %>%
#   tidyr::pivot_wider(names_from = measure, values_from = value) %>%
#   dplyr::mutate(non_hdl_ch = `Cholesterol totaal` - `HDL-cholesterol`) %>%
#   group_by(pat_id) %>%
#   summarise(ldl_ch = mean(`LDL-cholesterol`, na.rm = TRUE))

# In case we need other values
#             non_hdl_ch = mean(non_hdl_ch, na.rm = TRUE),
#             
#             hdl_ch = mean(`Cholesterol totaal`, na.rm = TRUE),
#             tot_ch = mean(`HDL-cholesterol`,na.rm = TRUE))
  
# add_chol <- . %>%
#   dplyr::filter(measure == "LDL-cholesterol") %>%
#   # remove outliers
#   dplyr::filter(value >= 0.7) %>%
#   dplyr::mutate(value = case_when(value > 100 ~ value / 100,
#                                   value > 10 ~ value / 10,
#                                   .default = value)) %>%
#   group_by(pat_id)
                
add_chol <- dat$chol %>%
  dplyr::filter(measure %in% c("LDL-cholesterol", "Cholesterol totaal")) %>%
  tidyr::pivot_wider(names_from = measure, values_from = value) |>
  dplyr::filter(`Cholesterol totaal` > 0,
                `LDL-cholesterol` >= 0.7) %>%
  
  dplyr::mutate(`Cholesterol totaal` = case_when(
    `Cholesterol totaal` > 100 ~ `Cholesterol totaal` / 100,
    `Cholesterol totaal` > 10 ~ `Cholesterol totaal` / 10,
    .default = `Cholesterol totaal`),
    
    `LDL-cholesterol` = case_when(
      `LDL-cholesterol` > 100 ~ `LDL-cholesterol` / 100,
      `LDL-cholesterol` > 10 ~ `LDL-cholesterol` / 10,
      .default = `LDL-cholesterol`)
  )

# we're extracting both HLD and LDL cholesterol
add_chol <- . %>%
  dplyr::filter(measure %in% c("LDL-cholesterol", "Cholesterol totaal")) %>%
  tidyr::pivot_wider(names_from = measure, values_from = value) %>%
  # remove outliers
  dplyr::filter(`Cholesterol totaal` > 0,
                `LDL-cholesterol` >= 0.7) %>%
  
  dplyr::mutate(`Cholesterol totaal` = case_when(
    `Cholesterol totaal` > 100 ~ `Cholesterol totaal` / 100,
    `Cholesterol totaal` > 10 ~ `Cholesterol totaal` / 10,
    .default = `Cholesterol totaal`),
    
    `LDL-cholesterol` = case_when(
      `LDL-cholesterol` > 100 ~ `LDL-cholesterol` / 100,
      `LDL-cholesterol` > 10 ~ `LDL-cholesterol` / 10,
      .default = `LDL-cholesterol`)
  ) %>%
  group_by(pat_id)


chol_df_2019 <- dat$chol |>
  dplyr::filter(year(contact_date) %in% c(2019, 2018)) %>%
  add_chol  %>%
  dplyr::summarise(ldl_ch_exp = mean(`LDL-cholesterol`, na.rm = TRUE),
                   tot_ch_exp = mean(`Cholesterol totaal`, na.rm = TRUE))

chol_df_2017 <- dat$chol |>
  dplyr::filter(year(contact_date) %in% c(2017, 2016)) %>%
  add_chol  %>%
  dplyr::summarise(ldl_ch_exp = mean(`LDL-cholesterol`, na.rm = TRUE),
                   tot_ch_exp = mean(`Cholesterol totaal`, na.rm = TRUE))

chol_df_2020 <- dat$chol |>
  dplyr::filter(year(contact_date) %in% c(2020, 2019)) %>%
  add_chol  %>%
  dplyr::summarise(ldl_ch_fu = mean(`LDL-cholesterol`, na.rm = TRUE),
                   tot_ch_fu = mean(`Cholesterol totaal`, na.rm = TRUE))

chol_df_2018 <- dat$chol |>
  dplyr::filter(year(contact_date) %in% c(2018, 2017)) %>%
  add_chol  %>%
  dplyr::summarise(ldl_ch_fu = mean(`LDL-cholesterol`, na.rm = TRUE),
                   tot_ch_fu = mean(`Cholesterol totaal`, na.rm = TRUE))

# 
# # add angina as covariate
# ang_df <- dat$prevalent_cases[dat$prevalent_cases$disease == "Angina", ]
# 
# ang_df$pat_id <- factor(ang_df$pat_id)
# 
# # get the date of the earliest event for each patient
# ang_df <-  ang_df |>
#   group_by(pat_id) |>
#   slice_min(diagnosis_date, with_ties = FALSE) |>
#   dplyr::select(-c(fu_end, fu_start, icpc)) |>
#   dplyr::rename(angina = disease) |>
#   dplyr::mutate(angina = TRUE)
# 
# 
# # now only cases prior to the exposure will be considered 
# ang_df_2020 <- ang_df |>
#   dplyr::filter(diagnosis_date < lubridate::ymd("2020-01-01")) |>
#   dplyr::select(-diagnosis_date)
# 
# ang_df_2018 <- ang_df |>
#   dplyr::filter(diagnosis_date < lubridate::ymd("2018-01-01")) |>
#   dplyr::select(-diagnosis_date)
# 
# sample_2020 <- left_join(sample_2020, ang_df_2020,  by = "pat_id") |>
#   dplyr::mutate(angina = replace_na(angina, FALSE))
# 
# sample_2018 <- left_join(sample_2018, ang_df_2018,  by = "pat_id") |>
#   dplyr::mutate(angina = replace_na(angina, FALSE))


# add number of different drugs prescribed over the three years
med_2020 <- dat$med |>
  dplyr::filter(contact_date > ymd("2017-01-01"),
                contact_date < ymd("2019-12-31")) |>
  distinct(pat_id, atc) |>
  count(pat_id) |>
  dplyr::rename(n_drugs = n)

med_2018 <- dat$med |>
  dplyr::filter(contact_date > ymd("2015-01-01"),
                contact_date < ymd("2017-12-31")) |>
  distinct(pat_id, atc) |>
  count(pat_id) |>
  dplyr::rename(n_drugs = n)

count_drugs <- . %>%
  dplyr::distinct(pat_id, category) %>%
  dplyr::mutate(class = case_when(
    category %in% c("Calcium channel blockers", 
                    "ACE inhibitors",
                    "Betablockers",
                    "Diuretics",  
                    "ARBs") ~ "antihypertensives",
    category %in% c("Metformine", "Drugs used in diabetes") ~ "diabetes_drugs",
    category == "Lipid modifying agents" ~ "cholesterol_drugs"
  )
  ) %>%
  count(pat_id, class) %>%
  tidyr::pivot_wider(names_from = class, values_from = n) %>%
  replace(is.na(.), 0)

# number of drugs in each of the category: 
# antihypertensives, lipid modifying agents and diabetes drugs
med_class_2020 <- dat$med |>
  dplyr::filter(contact_date > ymd("2017-01-01"),
                contact_date < ymd("2019-12-31")) %>%
  count_drugs
  
med_class_2018 <- dat$med |>
  dplyr::filter(contact_date > ymd("2015-01-01"),
                contact_date < ymd("2017-12-31")) %>%
  count_drugs

# ----------------- 

# add covariates in
sample_2020 <- dplyr::left_join(sample_2020, bp_df_2019, by = "pat_id")
sample_2020 <- dplyr::left_join(sample_2020, chol_df_2019, by = "pat_id")
sample_2020 <- dplyr::left_join(sample_2020, glu_df_2019, by = "pat_id")

sample_2020 <- dplyr::left_join(sample_2020, bp_df_2020, by = "pat_id")
sample_2020 <- dplyr::left_join(sample_2020, chol_df_2020, by = "pat_id")
sample_2020 <- dplyr::left_join(sample_2020, glu_df_2020, by = "pat_id")

sample_2018 <- dplyr::left_join(sample_2018,   bp_df_2017, by = "pat_id")
sample_2018 <- dplyr::left_join(sample_2018, chol_df_2017, by = "pat_id")
sample_2018 <- dplyr::left_join(sample_2018,  glu_df_2017, by = "pat_id")

sample_2018 <- dplyr::left_join(sample_2018,   bp_df_2018, by = "pat_id")
sample_2018 <- dplyr::left_join(sample_2018, chol_df_2018, by = "pat_id")
sample_2018 <- dplyr::left_join(sample_2018,  glu_df_2018, by = "pat_id")

sample_2020 <- dplyr::left_join(sample_2020,  med_2020, by = "pat_id") |>
  dplyr::mutate(n_drugs = tidyr::replace_na(n_drugs, 0))
sample_2018 <- dplyr::left_join(sample_2018,  med_2018, by = "pat_id") |>
  dplyr::mutate(n_drugs = tidyr::replace_na(n_drugs, 0))

sample_2020 <- dplyr::left_join(sample_2020,  med_class_2020, by = "pat_id") 
sample_2018 <- dplyr::left_join(sample_2018,  med_class_2018, by = "pat_id") 

sample_2020 <- left_join(sample_2020, comorbidities_df_2020,  by = "pat_id") 
sample_2018 <- left_join(sample_2018, comorbidities_df_2018,  by = "pat_id") 
 
# clean the list and add count of chronic diseases
add_count <- . %>%
rowwise() %>%
  dplyr::mutate(d_count = sum(c_across(c(Depression, Parkinsonism, Dementia, COPD, 
                                       Cancer, Asthma)), na.rm = TRUE)) %>%
  replace_na(list(Depression = FALSE,
                  Cancer = FALSE,
                  Parkinsonism = FALSE,
                  Asthma = FALSE,
                  COPD = FALSE,
                  Dementia = FALSE, 
                  Hypertension = FALSE,
                  Heart_failure = FALSE, 
                  
                  antihypertensives = 0, 
                  diabetes_drugs = 0, 
                  cholesterol_drugs = 0)
             )


sample_2020 <- sample_2020 %>% add_count
sample_2018 <- sample_2018 %>% add_count  


# add smoking status
smok <- dat$smoking |> 
  mutate(pat_id = factor(pat_id),
         smoking = if_else((smoking == "Unknown"), NA_character_, smoking),
         
         # for now set all missing as non-smokers
         smoking = if_else(is.na(smoking), "Never smoker", smoking)
         ) 

sample_2020 <- left_join(sample_2020, smok,  by = "pat_id") 
sample_2018 <- left_join(sample_2018, smok,  by = "pat_id") 


# add cardiometabolic status
# controlled cardiometabolic status is defined according to weterling 2006 as:

tar_bp <-145
tar_hba1c <- 58
tar_chol <- 5.2

sample_2020 <- sample_2020 |>
  dplyr::mutate(target_bp = if_else(sys_bp_exp <= tar_bp, TRUE, FALSE),
                target_hba1c = if_else(hba1c_exp <= tar_hba1c, TRUE, FALSE),
                target_chol = if_else(tot_ch_exp <= tar_chol, TRUE, FALSE)
                ) |>
  
  dplyr::mutate(cardiomet_stat = factor(if_else(target_bp & target_hba1c & target_chol, 
                                         "controlled", "uncontrolled")))

sample_2018 <- sample_2018 |>
  dplyr::mutate(target_bp = if_else(sys_bp_exp <= tar_bp, TRUE, FALSE),
                target_hba1c = if_else(hba1c_exp <= tar_hba1c, TRUE, FALSE),
                target_chol = if_else(tot_ch_exp <= tar_chol, TRUE, FALSE)
  ) |>
  
  dplyr::mutate(cardiomet_stat = factor(if_else(target_bp & target_hba1c & target_chol, 
                                                "controlled", "uncontrolled")))


# summary(sample_2020)

# sample_2020 <- sample_2020 |>
#   mutate(delta_bp = sys_bp_2022 - sys_bp_2019,
#          delta_chol = non_hdl_ch_2022 - non_hdl_ch_2019,
#          delta_hba1c = hba1c_2022 - hba1c_2019)

# ggplot(sample_2020, aes(x = health_avoider, y = delta_bp)) +
#   geom_boxplot()
# 
# ggplot(sample_2020, aes(x = health_avoider, y = delta_chol)) +
#   geom_boxplot() +
#   scale_y_continuous(limits = c(0, 4))
# 
# ggplot(sample_2020, aes(x = health_avoider, y = delta_hba1c)) +
#   geom_boxplot() +
#   scale_y_continuous(limits = c(0, 4))



#  --------------- Outcome -------------
#. ------------

# retrieve outcome: incidence of cvd events after the intervention period
cvd_prevalence_df <- dat$prevalent_cases[dat$prevalent_cases$disease != "Diabetes", ]

# only consider MI and stroke, cvd mortality will be added later
mace <- c('K90.00', 'K90.01', 'K90.02', 'K90.03', 'K89.00', 'K75.00'
          #, 'K77.00' , 'K77.01', 'K77.03', 'K77.04' # here we're considering heart failure as well
          )

cvd_prevalence_df <- cvd_prevalence_df |>
  dplyr::filter(icpc %in% mace)

cvd_prevalence_df$pat_id <- factor(cvd_prevalence_df$pat_id)

# get the date of the earliest event for each patient
cvd_prevalence_df <-  cvd_prevalence_df |>
  group_by(pat_id) |>
  slice_min(diagnosis_date, with_ties = FALSE) |>
  dplyr::select(-c(fu_end, fu_start))


sample_2020 <- left_join(sample_2020, cvd_prevalence_df,  by = "pat_id")
sample_2018 <- left_join(sample_2018, cvd_prevalence_df,  by = "pat_id")



# remove the diagnosis date if it is later than the fu_end
sample_2020$diagnosis_date <- if_else(sample_2020$diagnosis_date > sample_2020$fu_end, NA, sample_2020$diagnosis_date)
sample_2018$diagnosis_date <- if_else(sample_2018$diagnosis_date > sample_2018$fu_end, NA, sample_2018$diagnosis_date)


# create an event variable
# event is only valid is it occurs before the follow-up end
sample_2020$event <- if_else(!is.na(sample_2020$diagnosis_date), TRUE, FALSE)
sample_2018$event <- if_else(!is.na(sample_2018$diagnosis_date), TRUE, FALSE)

sample_2018$disease <- if_else(!sample_2018$event, NA, sample_2018$disease)
sample_2020$disease <- if_else(!sample_2020$event, NA, sample_2020$disease)



# here we need to adjust the follow-up end so the follow-up time match that of the
# experimental sample. it will simply be 3 years from the baseline, i.e. 2022-01-01
sample_2018$fu_end <- if_else(sample_2018$fu_end > lubridate::ymd("2022-01-01"), 
                              lubridate::ymd("2022-01-01"), sample_2018$fu_end)

sample_2018$event <- if_else((!is.na(sample_2018$disease) & 
                                sample_2018$diagnosis_date <= sample_2018$fu_end), TRUE, FALSE)

# change the follow up end depending on the outcome. if event, follow up ends with it
# otherwise end of study/data
sample_2020$fu_end <- if_else(sample_2020$event == TRUE, sample_2020$diagnosis_date,
                              sample_2020$fu_end)

sample_2018$fu_end <- if_else(sample_2018$event == TRUE, sample_2018$diagnosis_date,
                              sample_2018$fu_end)


# now add the cardiovascular death form Vincent's file
cvd_mortality <- readr::read_csv(here::here("datasets", "cause_death_v2.txt")) |>
  dplyr::mutate(id = paste0(prakid, "/", patid)) |>
  dplyr::rename(cause = Overlijdensreden,
                date_of_death = `Date of death`) |>
  dplyr::select(id, cause, date_of_death) |>
  dplyr::mutate(date_of_death = dmy(date_of_death))

# need to add the patid and prak_id as we need to join on this combination rather than exppatidx
pats <- dat$patients |>
  dplyr::select(pat_id, patid, prak_id) |>
  dplyr::mutate(id = paste0(prak_id, "/", patid)) |>
  dplyr::select(pat_id, id)

# add id to the df
sample_2020 <- left_join(sample_2020, pats, by = "pat_id")
sample_2018 <- left_join(sample_2018, pats, by = "pat_id")

# join on id
sample_2020 <- dplyr::left_join(sample_2020, cvd_mortality, by = "id")
sample_2018 <- dplyr::left_join(sample_2018, cvd_mortality, by = "id")



# add mortality data
# death only counts if it is a censoring event, ie. it happened before the end of follow-up
sample_2020$died <- if_else(sample_2020$pat_dom <= sample_2020$fu_end, TRUE, FALSE, 
                            missing = FALSE)

sample_2018$died <- if_else(sample_2018$pat_dom <= sample_2018$fu_end, TRUE, FALSE, 
                            missing = FALSE)

# recode mortality and event data. Mortality now only decodes non-cardiovascular deaths
# event2 codes only cvd events, not cvd-mortality
# sample_2020 <- sample_2020 |>
#   dplyr::mutate(event2 = event,
#                 event = if_else(is.na(cause), event, TRUE),
#                 disease = if_else(is.na(cause), disease, "Cardiovascular death"),
#                 died2 = died,
#                 died = if_else(is.na(cause), died, FALSE))
# 
# sample_2018 <- sample_2018 |>
#   dplyr::mutate(event2 = event,
#                 event = if_else(is.na(cause), event, TRUE),
#                 disease = if_else(is.na(cause), disease, "Cardiovascular death"),
#                 died2 = died, 
#                 died = if_else(is.na(cause), died, FALSE))

# replace NaN with NA everywhere
sample_2020 <- sample_2020 |> dplyr::mutate(across(everything(), ~ if_else(is.nan(.), NA, .)))
sample_2018 <- sample_2018 |> dplyr::mutate(across(everything(), ~ if_else(is.nan(.), NA, .)))


add_outcome <- . %>%
  dplyr::mutate(
    cvd = if_else(!is.na(disease) & (diagnosis_date <= fu_end), TRUE, FALSE),
    cvd_mort = if_else(!is.na(cause) & (date_of_death <= fu_end), TRUE, FALSE),
    mace = if_else(cvd | cvd_mort, TRUE, FALSE),
    disease = if_else(cvd_mort, "Cardiovascular death", disease),
    disease = if_else(mace, disease, NA_character_),
    
    died_all = died,
    died_non_cvd = if_else(died & !cvd_mort, TRUE, FALSE)
  ) %>%
  dplyr::select(-died)

sample_2020 <- sample_2020 %>% add_outcome
sample_2018 <- sample_2018 %>% add_outcome





# #  --------------- Table 1 ------------
# #  . --------------
# 
# rndr <- function(x, ...) {
#   y = table1::render.default(x, ...)
#   if (is.logical(x)) y[2] else y
# }
# 
# 
# # function to calculate standardized mean difference
# smd_t1 <- function(data, ...){
#   y <- unlist(data)
#   group <- factor(rep(1:length(data), times = sapply(data, length)))
#   
#   round(smd::smd(y, group, na.rm = TRUE)[2], 3)
# }
# 
# 
# datt <- sample_2020
# # tlabels <- list(
# #   variables = list(age = render.varlabel(datt$age),
# #                    age = render.varlabel(datt$sex),
# #                    `social deprivation` = render.varlabel(datt$socdep56),
# #                    `living environment` = render.varlabel(datt$livenv),
# #                    `systolic blood pressure (mmHg)` = render.varlabel(datt$sys_bp_exp),
# #                    `LDL cholesterol (mmol/L)` = render.varlabel(datt$ldl_ch_exp),
# #                    `hbA1c (mmol/mol)` = render.varlabel(datt$hba1c_exp),
# #                    `Number of comorbidities` = render.varlabel(datt$d_count),
# #                    `Depression` = render.varlabel(datt$Depression),
# #                    `COPD` = render.varlabel(datt$COPD),
# #                    `Parkinsonism` = render.varlabel(datt$Parkinsonism),
# #                    `Dementia` = render.varlabel(datt$Dementia),
# #                    `Cancer` = render.varlabel(datt$Cancer),
# #                    `Smoking status` = render.varlabel(datt$smoking),
# #                    `Major Cardiovascular Adverse Event (MACE)` = render.varlabel(datt$event),
# #                    `MACE type` = render.varlabel(datt$disease),
# #                    `Non-cardiovascular mortality` = render.varlabel(datt$died),
# #                    `All-cause mortality` = render.varlabel(datt$died2)
# #   ),
# #   groups = list("", "Exposed", "")
# # )
# 
# 
# t1_2020 <- table1::table1(~ age + sex + socdep56 + livenv + sys_bp_exp + ldl_ch_exp + 
#                             hba1c_exp + d_count + Depression + Asthma + COPD + Parkinsonism +
#                             Dementia + Cancer + smoking + event + disease + 
#                             died_all | factor(health_avoider_v2), 
#                           data = datt,
#                           # labels = tlabels, 
#                           render = rndr,
#                           render.continuous = c(.="Mean (SD)")
#                           # extra.col = list(`SMD` = smd_t1) 
#                           )
# datt <- sample_2018
# t1_2018 <- table1::table1(~ age + sex + socdep56 + livenv + sys_bp_exp + ldl_ch_exp + 
#                             hba1c_exp + d_count + Depression + Asthma + COPD + Parkinsonism +
#                             Dementia + Cancer + smoking + event + disease + 
#                             died_all | factor(health_avoider_v2), 
#                           # labels = labels,
#                           data = datt,
#                           render = rndr,
#                           render.continuous = c(.="Mean (SD)")
#                           # extra.col = list(`SMD` = smd_t1) 
#                           )
# 
# 
# # # get exposure
# # sample_2020 <- sample_2020 |>
# #   dplyr::mutate(health_avoider_v3 = if_else(delta_perc_median > 0.69, TRUE, FALSE))
# # prop.table(table(sample_2020$health_avoider_v3))
# # 
# # 
# # sample_2018 <- sample_2018 |>
# #   dplyr::mutate(health_avoider_v3 = if_else(delta_perc_median > 0.69, TRUE, FALSE))
# # prop.table(table(sample_2018$health_avoider_v3))
# # 



# inpute median values for now
# input_miss <- . %>%
#   group_by(health_avoider_v2) %>%
#   mutate(across(livenv:socdep56, ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
#   mutate(across(sys_bp_exp:hba1c_fu, ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
#   ungroup()
# 
# sample_2020 <- sample_2020 %>% input_miss
# sample_2018 <- sample_2018 %>% input_miss




# only keep the object you need
rm(list = setdiff(ls(), c("sample_2020", "sample_2018", "dat")))







