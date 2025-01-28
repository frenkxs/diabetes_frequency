library(MatchThem)
# impute missing datasest
library(mice)
library(survey)
library(cobalt)
library(tidyverse)

source(here::here("R", "H_get_censoring_w.R"))


load(here::here("datasets", "covar", "nohf", "m2_nc_covar_nohf_mace.RData"))



sample_2020_long <- sample_2020_long |>
  dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.65, TRUE, FALSE))

sample_2020 <- sample_2020 |>
  dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.65, TRUE, FALSE))

# . -----------------------
#  ------- functions to reproduce the analysis from the manuscript ----
get_rd <- function(sample_2020_long, ni = 20) {

  # convert to a factor
  sample_2020_long$cause <- if_else(is.na(sample_2020_long$cause), "non_cvd", sample_2020_long$cause)
  sample_2020_long$cause <- factor(sample_2020_long$cause)
  sample_2020_long$smoking <- factor(sample_2020_long$smoking)
  sample_2020_long$disease <- factor(sample_2020_long$disease)
  
  # remove all variables that are not relevant for imputation
  dat <- sample_2020_long[, -c(3:4, 7:8, 14:15, 20:40, 70, 72:78, 83)]
  
  init <- mice::mice(dat, maxit = 0)
  init$loggedEvents
  meth <- init$method
  predM <- init$predictorMatrix
  
  # only these variables will be imputed
  imputed_var <- c("livenv", "smoking", "sys_bp_exp", "sys_bp_fu", "ldl_ch_fu", 
                   "hba1c_fu", "ldl_ch_exp", "tot_ch_exp", "hba1c_exp", "livenv", 
                   "socdep56")
  
  meth[!names(meth) %in% imputed_var] <- ""
  predM[, !colnames(predM) %in% imputed_var] <- 0
  
  # number of imputation iteration
  imputed <- mice::mice(dat, method = meth, 
                        predictorMatrix = predM,
                        m = ni)
  
  # Add cardiometabolic status 
  
  # controlled cardiometabolic status is defined according to Wermeling 2014 as:
  # blood pressure below or at 145
  # hba1c below  or at 58
  # total cholesterol below or at 5.2
  
  tar_bp <-145
  tar_hba1c <- 58
  tar_chol <- 5.2
  
  # extract the imputed data
  imp <- complete(imputed, action = "long", include = TRUE)
  
  # add the cardiometabolic status
  imp <- imp |>
    dplyr::mutate(target_bp = if_else(sys_bp_exp <= tar_bp, TRUE, FALSE),
                  target_hba1c = if_else(hba1c_exp <= tar_hba1c, TRUE, FALSE),
                  target_chol = if_else(tot_ch_exp <= tar_chol, TRUE, FALSE)
    ) |>
    
    dplyr::mutate(cardiomet_stat = factor(if_else(target_bp & target_hba1c & target_chol, 
                                                  "controlled", "uncontrolled")))
  # convert back to mids
  imputed <- as.mids(imp)
  
  # --
  
  # weighting the imputed datasets
  formul_weight <- formula(health_avoider_v2 ~ age + sex + socdep56 + livenv 
                            + sys_bp_exp + tot_ch_exp + hba1c_exp + Depression + Dementia 
                            + Asthma + COPD + Cancer + d_count + smoking + n_drugs
                            + antihypertensives + diabetes_drugs + cholesterol_drugs 
                            + cardiomet_stat)
  
  
  weighted <-  weightthem(formul_weight, datasets = imputed, 
                          approach = "within",
                          method = "glm", 
                          estimand = "ATE")
  
  # assess balance
  bal.tab(weighted, stats = c("m", "ks"), imp.fun = "max")
  
  
  # now, we need to compute the censoring weights and add them to the 
  # weighted data
  
  
  # analyse the imputed datasets
  formul_fit <- formula(out ~ health_avoider_v2 + month + month2 + ha_m
                        + age + sex + socdep56 + sys_bp_exp + tot_ch_exp 
                        + hba1c_exp + Cancer + Asthma + COPD 
                        + smoking + cardiomet_stat
                        + antihypertensives + diabetes_drugs + cholesterol_drugs)
  analysed <- list()
  
  # here we extract
  # 1. the imputed and weighted data one by one, 
  # 2. calculate censoring weights
  # 3. fit model with the final weights 
  for(i in 1:ni){
    
    # extract data from mice 
    temp <- MatchThem::complete(weighted, action = i)
    
    # add pat_id
    temp$pat_id <- sample_2020_long$pat_id
    
    # get the censoring weights as vector
    w <- get_censoring_w(temp, mort)
    
    # multiply the treatment weights calculated by weighit by the censoring weights
    temp$weights <- weighted$models[[i]]$weights * w
    
    # create interaction terms
    temp$month2 <- temp$month^2
    temp$ha_m <- temp$health_avoider_v2 * temp$month
    temp$ha_m2 <- temp$ha_m^2
    
    # fit model with robust SE
    analysed[[i]] <- svyglm(formul_fit, family = quasibinomial(),
                            design = survey::svydesign(id = ~ pat_id,
                                                       weights = ~ weights,
                                                       data = temp)
    )
    
  }
  
  
  
  # We will now use those five imputed datasets to construct a counterfactual data
  # where everybody is set to exposed and unexposed
  # function to construct counterfactual data
  construct_cf <- function(df){
    baseline_only <- df[df$month == 1, ]
    fu <- max(df$month)
    sample_size <- nrow(baseline_only)
    
    exposed <- baseline_only[rep(1:sample_size, each = fu), ]
    control <- baseline_only[rep(1:sample_size, each = fu), ]
    
    # recreate time
    exposed$month <- control$month <- rep(1:fu, times = sample_size)
    exposed$month2 <- control$month2 <- exposed$month * exposed$month 
    
    # set everyone to exposed and unexposed
    exposed$health_avoider_v2 <- TRUE
    control$health_avoider_v2 <- FALSE
    
    exposed$ha_m <- exposed$health_avoider_v2 * exposed$month
    control$ha_m <- control$health_avoider_v2 * control$month
    
    exposed$ha_m2 <- exposed$health_avoider_v2 * exposed$month2
    control$ha_m2 <- control$health_avoider_v2 * control$month2
    
    bind_rows(exposed, control)
  }
  
  # we'll store the data in a list
  cf_data_imp <- list()
  
  for (i in 1:ni){
    df <- MatchThem::complete(weighted, action = i)
    cf_data_imp[[i]] <- construct_cf(df)
  }
  
  # now we can go back to our analyses of the imputed data
  # we will fit each of the five models to new counterfactual 
  # data constructed out of the five imputed data
  
  fitted_prob <- 
    tibble(.imp = 1:ni) %>%
    mutate(p = map(.imp, ~ predict(analysed[[.]],
                                   newdata = cf_data_imp[[.]], 
                                   se.fit = TRUE) %>%
                     data.frame()) 
    ) |>
    
    # unnest the data
    unnest(p) |>
    # add to the fitted data
    bind_cols(
      bind_rows(cf_data_imp)
    ) |>
    # only select relevant columns, at this point we don't need the covariates anymore
    dplyr::select(pat_id, month, health_avoider_v2, link, SE, .imp)
  
  # finally, we can pool the resulting probabilities across the imputed datasets
  fitted_prob <- fitted_prob |>
    
    # we need to group it bu pat_id, exposure status and month to ge to the 
    # risk of event for each month and each patient (both as exposed and unexposed)
    group_by(pat_id, health_avoider_v2, month) |>
    summarise(p_bar = mean(link), 
              
              # within imputation variance
              v_w = mean(SE^2),
              
              # between imputation variance
              v_b = sum((link - p_bar)^2) / (ni - 1),
              
              # total variance
              v_t = v_w + (v_b * (1 + (1 / ni))),
              
              # and finally the standard error
              se_p = sqrt(v_t)
    ) |>
    
    # and now we can compute the confidence intervals
    mutate(lwr = p_bar - se_p * 1.96,
           upr = p_bar + se_p * 1.96) |>
    
    # and transform it to probability
    mutate(p_bar = plogis(p_bar),
           lwr = plogis(lwr),
           upr = plogis(upr),
           
           # and finally transform this into survival
           s_bar = 1 - p_bar,
           s_lwr = 1 - upr,
           s_upr = 1 - lwr)
  
  # this should result into one dataset with all participants and all months in two
  # versions, one exposed, one unexposed. Each month and each patient should have it's 
  # own probability
  
  # we can now summarise as we'd normally do with complete data
  # survival at each month and for each patient
  surv_m_pat <- fitted_prob |>
    dplyr::arrange(pat_id, month, health_avoider_v2) |>
    dplyr::ungroup() |>
    dplyr::group_by(pat_id, health_avoider_v2) |>
    dplyr::mutate(s = cumprod(s_bar),
                  s_lwr = cumprod(s_lwr),
                  s_upr = cumprod(s_upr))
  
  # Standardized survival at each month for the two groups
  surv_m <- surv_m_pat |>
    dplyr::ungroup() |>
    dplyr::select(s, health_avoider_v2, month, s_lwr, s_upr) |>
    dplyr::group_by(month, health_avoider_v2) |>
    dplyr::summarise(mean_s = mean(s),
                     s_lwr = mean(s_lwr),
                     s_upr = mean(s_upr))
  
  res <- dplyr::bind_rows(
    data.frame(month = c(0, 0),
               health_avoider_v2 = c(TRUE, FALSE),
               mean_s = c(1, 1),
               s_lwr = c(1, 1),
               s_upr = c(1, 1)),
    surv_m
  )
  
  res$health_avoider <- factor(res$health_avoider_v2, 
                               labels = c("Stable", "Reduced"))
  
  
  contrast <- tidyr::pivot_wider(res |> dplyr::select(-health_avoider_v2), 
                                 names_from = health_avoider, 
                                 values_from = c(mean_s, s_lwr, s_upr))
  
  contrast$RD <-     (1 - contrast$mean_s_Reduced) - (1 - contrast$mean_s_Stable)
  # contrast$RD_upr <- (1 -  contrast$s_upr_Reduced) - (1 -  contrast$s_upr_Stable)
  # contrast$RD_lwr <- (1 -  contrast$s_lwr_Reduced) - (1 -  contrast$s_lwr_Stable)
  
  # contrast$RR <- (1 - contrast$mean_s_Reduced) / (1 - contrast$mean_s_Stable)
  
  
  # Risk difference at the end of three years
  RD <- round(contrast$RD[contrast$month == 36], 4)
  
  list(full_data = res, contrast = contrast, RD = RD)
  
}  

get_bal <- function(sample_2020, sample_2018){

  formul <- formula(health_avoider_v2 ~ age + sex + socdep56 + livenv + sys_bp_exp +
                    tot_ch_exp + hba1c_exp + Depression + Asthma +
                    COPD + Cancer + smoking + antihypertensives +
                    diabetes_drugs + cholesterol_drugs)


  # remove NAs
  # inpute median values for now
  input_miss <- . %>%
    group_by(health_avoider_v2) %>%
    mutate(across(livenv:socdep56, ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    mutate(across(sys_bp_exp:hba1c_exp, ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    ungroup()
  
  dat_2020 <- sample_2020 %>% input_miss
  dat_2018 <- sample_2018 %>% input_miss
  
  
  ipw_2020 <- WeightIt::weightit(formul, data = dat_2020,
                                 method = "glm", 
                                 estimand = "ATE",
                                 stabilize = TRUE)
  
  ipw_2018 <- WeightIt::weightit(formul, data =  dat_2018,
                                 method = "glm", 
                                 estimand = "ATE",
                                 stabilize = TRUE)
  
  pretty_names <- data.frame(
    old = c("age",
            "sex",
            "socdep56",
            "livenv",
            "sys_bp_exp",
            "tot_ch_exp",
            "hba1c_exp",
            "Depression",
            
            "Asthma",
            "COPD",
            "Cancer",
            
            "smoking",
            "n_drugs",
            "antihypertensives",
            "diabetes_drugs",
            "cholesterol_drugs",
            "distance"),
    
    new = c("Age",
            "Sex",
            "Social deprivation indicator",
            "Living environment (urban vs. rural)",
            "Systolic blood pressure",
            "Total cholesterol",
            "Hemoglobin A1c",
            "Comorbid depression",
            
            "Comorbid asthma",
            "Comorbid COPD",
            "Comorbid cancer",
            
            "Smoking status",
            "Number of drugs prescribed",
            "Number of antihypertensives prescribed",
            "Number of diabetes drugs prescribed",
            "Number of cholesterol drugs prescribed",
            "distance")
  )
  
  clr <- c("deepskyblue3", "chocolate")
  a <- love.plot(ipw_2020, binary = "std", thresholds = c(m = 0.1),
                 line = TRUE,
                 var.order = "unadjusted",
                 abs = FALSE,
                 drop.distance = TRUE, 
                 sample.names = c("Original", "Weighted"), 
                 colours = clr,
                 title = "",
                 var.names = pretty_names) +
    scale_x_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.4, 0.4, length.out = 9)) +
    theme_minimal(base_size = 22)
  
  
  b <- love.plot(ipw_2018, binary = "std", thresholds = c(m = 0.1),
                 line = TRUE,
                 var.order = "unadjusted",
                 abs = FALSE,
                 drop.distance = TRUE, 
                 var.names = pretty_names) +
    scale_x_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.35, 0.35, length.out = 7)) +
    theme_minimal(base_size = 14)
  
  
  aa <- a$data
  bb <- b$data
  
  p <- a + 
    geom_point(data = b$data |> dplyr::filter(Sample == "Unadjusted"), shape = 18,
               size = 5, colour = "azure4") +
    geom_point(aes(colour = Sample, size = Sample)) +
    scale_size_discrete(range = c(5, 2))
  
  list(or = aa, ncs = bb, plot = p)
}

get_hist <- function(sample_2020, sample_2018){
  
  sample_2020 <- sample_2020 |>
    dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.65, TRUE, FALSE)) |>
    dplyr::mutate(health_avoider_v2 = factor(health_avoider_v2)) |>
    dplyr::mutate(health_avoider_v2 = recode(health_avoider_v2,
                                             "TRUE" = "Reduced",
                                             "FALSE"=  "Same"))
  
  sample_2018 <- sample_2018 |>
    dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.65, TRUE, FALSE)) |>
    dplyr::mutate(health_avoider_v2 = factor(health_avoider_v2)) |>
    dplyr::mutate(health_avoider_v2 = recode(health_avoider_v2,
                                             "TRUE" = "Reduced",
                                             "FALSE"=  "Same"))
  
  temp <- bind_rows(sample_2020 |> add_column(sample = "2020"),
                    sample_2018 |> add_column(sample = "2018"))
  
  ggplot(temp, aes(x = delta, fill = sample, colour = sample, group = sample)) +
    geom_histogram(position = "dodge", alpha = 0.5, bins = 25) +
    scale_x_continuous(breaks = seq(-7, 7, by = 1), limits = c(-9, 9),
                       name = "Change in number of blood pressure measurements") +
    scale_y_continuous(name = "") +
    scale_fill_discrete(name = "Intervention year") +
    scale_colour_discrete(guide = "none") +
    theme_minimal(base_size = 16)
}


# . -----------------------
# data for sensitivity 1 and 2 and 3 ---------
sample_2020_ha60 <- sample_2020 |>
  dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.60, TRUE, FALSE))
sample_2020_ha70 <- sample_2020 |>
  dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.70, TRUE, FALSE))
sample_2020_UCS <- sample_2020 |>
  dplyr::filter(cardiomet_stat == "uncontrolled") |>
  dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.65, TRUE, FALSE))

sample_2018_ha60 <- sample_2018 |>
  dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.60, TRUE, FALSE))
sample_2018_ha70 <- sample_2018 |>
  dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.70, TRUE, FALSE))
sample_2018_UCS <- sample_2018 |>
  dplyr::filter(cardiomet_stat == "uncontrolled") |>
  dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.65, TRUE, FALSE))



# . -----------------------
#  Analysis -------------
# sensitivity analysis 1: lower the threshold for exposure to 60%

sample_2020_long_ha60 <- sample_2020_long |>
  dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.6, TRUE, FALSE))

sens1 <- get_rd(sample_2020_long_ha60)


# sensitivity analysis 2: increase the threshold for exposure to 70%

sample_2020_long_ha70 <- sample_2020_long |>
  dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.7, TRUE, FALSE))

sens2 <- get_rd(sample_2020_long_ha70)


# sensitivity analysis 3: only include patients with uncontrolled cardiometabolic status
# be careful here, the cardiometabolic status has to be removed from all the 
# formulas as it is a constant in this case
sample_2020_long_UCS <- sample_2020_long |>
  dplyr::filter(cardiomet_stat == "uncontrolled") |>
  dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.65, TRUE, FALSE))

sens3 <- get_rd(sample_2020_long_UCS)

save(sens1, sens2, sens3, file = "sensitivity_rd.RData")

# . -----------------------
# ------- Plots -----------
#  ------survival curves --------
sens1$RD
sens2$RD
sens3$RD
 df <- sens3$full_data
# sensitivity 1
main_plot <- df |>
  ggplot(aes(x = month, y = mean_s, 
                 group = health_avoider, colour = health_avoider)) +
  geom_point(size = 6) +
  geom_line(alpha = 0.5, linewidth = 3) +
  # geom_vline(xintercept = 0) +
  # geom_hline(yintercept = 0.5) +
  scale_y_continuous(limits = c(0.5, 1), name = "Mean MACE-free survival") +
  scale_x_continuous(breaks = seq(0, 36, by = 4)) +
  scale_colour_discrete(name = "Monitoring frequency") +
  scale_fill_discrete(guide = "none") +
  theme_minimal(base_size = 22) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

inset_plot <- main_plot +
  scale_y_continuous(limits = c(0.9, 1), name = NULL) +
  # geom_hline(yintercept = 0.9) +
  scale_colour_discrete(guide = "none") +
  scale_x_continuous(name = NULL, breaks = seq(0, 36, by = 4)) +
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks.x = element_line(linewidth = 1)) 

main_plot +
  annotation_custom(
    ggplotGrob(inset_plot),
    ymin = 0.53, ymax = 0.85, xmin = 0.5, xmax = 34
  )


# #  ------ histograms --------

get_hist(sample_2020_UCS, sample_2018_UCS)


table(sample_2020_ha60$health_avoider_v2)
table(sample_2020_ha70$health_avoider_v2)
table(sample_2020_UCS$health_avoider_v2)
table(sample_2020$health_avoider_v2)

# ----- love plots -------

# Balance with different cutoff values


# main result
sample_2020 <- sample_2020 |>
  dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.65, TRUE, FALSE))
sample_2020 <- sample_2020 |>
  dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.65, TRUE, FALSE))


get_bal(sample_2020_ha60, sample_2018_ha60)
get_bal(sample_2020_ha70, sample_2018_ha70)
get_bal(sample_2020_UCS, sample_2018_UCS)
main_p <- get_bal(sample_2020, sample_2018)
