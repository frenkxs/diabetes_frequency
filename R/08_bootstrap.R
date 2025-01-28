library(MatchThem)
library(mice)
library(survey)
library(tidyverse)
source(here::here("R", "H_get_censoring_w.R"))
source(here::here("R", "04_convert_to_long.R"))

load(here::here("datasets", "covar", "nohf", "m2_nc_covar_nohf_mace.RData"))

# ni: number of imputations
# d: index for the boot function
# data: data (sample_2020 normally) with all dates and diagnoses already cleaned
get_contrast <- function(data, d, ni = 20){
  data <- data[d, ]
  
  data <- data |>
    dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.65, TRUE, FALSE))
  
  # construct a long dataset
  data_long <- convert_to_long_single(data, mace, died_non_cvd) 
  
  
  # convert to a factor
  data_long$cause <- if_else(is.na(data_long$cause), "non_cvd", data_long$cause)
  data_long$cause <- factor(data_long$cause)
  data_long$smoking <- factor(data_long$smoking)
  data_long$disease <- factor(data_long$disease)
  
  # remove all variables that are not relevant for imputation
  dat <- data_long[, -c(3:4, 7:8, 14:15, 20:40, 70, 72:78, 83)]
  
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
  
  
  imputed <- mice::mice(dat, method = meth, 
                        predictorMatrix = predM,
                        m = ni)
  
  # Add cardiometabolic status -------------
  
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
  
  #  --------------------------
  
  # weighting the imputed datasets
  formul_weight <- formula(health_avoider_v2 ~ age + sex + socdep56 + livenv + 
                             sys_bp_exp + tot_ch_exp + hba1c_exp + Depression + Dementia + 
                             Asthma + COPD + Cancer + d_count + smoking + n_drugs + 
                             antihypertensives + diabetes_drugs + cholesterol_drugs +
                             cardiomet_stat)
  
  
  weighted <-  weightthem(formul_weight, datasets = imputed, 
                          approach = "within",
                          method = "glm", 
                          estimand = "ATE")

  
  
  # now, we need to compute the censoring weights and add them to the 
  # weighted data
  
  
  # analyse the imputed datasets
  formul_fit <- formula(out ~ health_avoider_v2 + month + month2 + 
                          age + sex + socdep56 + 
                          + sys_bp_exp + tot_ch_exp + hba1c_exp + Cancer + Asthma + COPD + 
                          smoking + cardiomet_stat +
                          antihypertensives + diabetes_drugs + cholesterol_drugs)
  analysed <- list()
  
  # here we extract
  # 1. the imputed and weighted data one by one, 
  # 2. calculate censoring weights
  # 3. fit model with the final weights 
  for(i in 1:ni){
    
    # extract data from mice 
    temp <- MatchThem::complete(weighted, action = i)
    
    # add pat_id
    temp$pat_id <- data_long$pat_id
    
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
  
  # risk difference
  contrast$RD <- (1 - contrast$mean_s_Reduced) - (1 - contrast$mean_s_Stable)

  # hazard ratio
  contrast$log_ratio <- log(contrast$mean_s_Reduced) / log(contrast$mean_s_Stable)

  
  contrast$log_ratio[1] <- NA

  # mean hazard ratio up until each month
  contrast$cHR <- sapply(0:36, 
                         FUN = function(x) {mean(contrast$log_ratio[contrast$month <= x], 
                                                 na.rm = TRUE)})
  
  c(contrast$month, contrast$mean_s_Reduced, contrast$mean_s_Stable, contrast$cHR, contrast$RD)
  
  
  # overall mean hazard ratio
  # cHR <- round(contrast$cHR[contrast$month == 36], 3)
  # 
  # # Risk difference at the end of three years
  # RD <- round(contrast$RD[contrast$month == 36], 4)
  # 
  # c(cHR, RD)
}



# get bootstrap samples
out1 <- boot::boot(sample_2020, get_contrast, R = 40, ni = 10)
out2 <- boot::boot(sample_2020, get_contrast, R = 40, ni = 10)
out3 <- boot::boot(sample_2020, get_contrast, R = 40, ni = 10)
out4 <- boot::boot(sample_2020, get_contrast, R = 40, ni = 10)
out5 <- boot::boot(sample_2020, get_contrast, R = 40, ni = 10)
out6 <- boot::boot(sample_2020, get_contrast, R = 40, ni = 10)
out7 <- boot::boot(sample_2020, get_contrast, R = 40, ni = 10)
out8 <- boot::boot(sample_2020, get_contrast, R = 40, ni = 10)
out9 <- boot::boot(sample_2020, get_contrast, R = 40, ni = 10)
out10 <- boot::boot(sample_2020, get_contrast, R = 40, ni = 10)
 
save(out1, out2, out3, out4, out5, 
     out6, out7, out8, out9, out10, file = here::here("bootstrap_samples.RData"))


load(here::here("bootstrap_samples.RData"))
 
# convert samples dataframes where each row is a month with particular survival probability
convert_sam <- function(df){  
  df <- df[["t"]]
  for (i in 1:20) {
    temp <- matrix(df[i, ], nrow = 37, ncol = 5, dimnames =list(c(), 
                                                                c("month", 
                                                                  "mean_s_Reduced", 
                                                                  "mean_s_Stable", 
                                                                  "cHR", 
                                                                  "RD")))
    temp <- as.data.frame(temp)
    temp <- temp |> mutate(across(everything(), ~if_else(is.nan(.), NA, .)))
    temp$RR <- (1 - temp$mean_s_Reduced) / (1 - temp$mean_s_Stable)
    
    res_boot <- bind_rows(res_boot, temp)
  }
  res_boot
}


# initialise data frame to store the results
res_boot <-  data.frame(month = integer(),
                        mean_s_Reduced = numeric(),
                        mean_s_Stable = numeric(),
                        cHR = numeric(),
                        RD = numeric(),
                        RR = numeric())

# merge all samples together
res_boot <- convert_sam(out1)
res_boot <- convert_sam(out2)
res_boot <- convert_sam(out3)
res_boot <- convert_sam(out4)
res_boot <- convert_sam(out5)
res_boot <- convert_sam(out6)
res_boot <- convert_sam(out7)
res_boot <- convert_sam(out8)
res_boot <- convert_sam(out9)
res_boot <- convert_sam(out10)


# summarise into confidence intervals
res_boot <- res_boot |>
  group_by(month) |>
  summarise(s_ci_stable_lw = quantile(mean_s_Stable, probs = c(0.025), na.rm = TRUE),
            s_ci_stable_up = quantile(mean_s_Stable, probs = c(0.975), na.rm = TRUE),
            s_ci_reduced_lw = quantile(mean_s_Reduced, probs = c(0.025), na.rm = TRUE),
            s_ci_reduced_up = quantile(mean_s_Reduced, probs = c(0.975), na.rm = TRUE),
            s_ci_RD_lw = quantile(RD, probs = c(0.025), na.rm = TRUE),
            s_ci_RD_up = quantile(RD, probs = c(0.975), na.rm = TRUE),
            s_ci_RR_lw = quantile(RR, probs = c(0.025), na.rm = TRUE),
            s_ci_RR_up = quantile(RR, probs = c(0.975), na.rm = TRUE)) |>
  dplyr::select(month, s_ci_stable_lw, s_ci_stable_up, 
                s_ci_reduced_lw, s_ci_reduced_up, s_ci_RD_lw, s_ci_RD_up, 
                s_ci_RR_lw, s_ci_RR_up) |>
  dplyr::select(c(month, s_ci_stable_lw, s_ci_reduced_lw, s_ci_stable_up, s_ci_reduced_up)) |>
  pivot_longer(cols = -1, 
               names_pattern = "(..*)(..)$", names_to = c("status", "bound")) |>
  mutate(health_avoider = if_else(status == "s_ci_stable_", "Stable", "Reduced")) |>
  dplyr::select(-status) |>
  pivot_wider(names_from = bound, values_from = value) 


ares <- dplyr::left_join(res, res_boot, by = c("health_avoider", "month"))




main_plot <- ggplot(ares, aes(x = month, y = mean_s, 
                group = health_avoider, colour = health_avoider)) +
  geom_ribbon(aes(ymin = lw, ymax = up,
                  colour = health_avoider,
                  fill = health_avoider),
              alpha = 0.08, linetype = "dotted") +
  
  # geom_line(aes(y = s_upr, colour = health_avoider_v2f), linetype = "dotted", linewidth = 1) +
  # geom_line(aes(y = s_lwr, colour = health_avoider_v2f), linetype = "dotted", linewidth = 1) +
  geom_point(size = 16) +
  geom_line(alpha = 0.8, linewidth = 10) +
  # geom_vline(xintercept = 0) +
  # geom_hline(yintercept = 0.5) +
  scale_y_continuous(limits = c(0.5, 1), name = "\nMean MACE-free survival\n") +
  scale_x_continuous(breaks = seq(0, 36, by = 4), name = "\nMonth") +
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
        axis.ticks.x = element_line(linewidth = 1),
  axis.title = element_text(size = 17 * 2.5),
  axis.text  = element_text(size = 12 * 2.5),
  legend.text = element_text(size = 12 * 2.5),
  legend.title = element_text(size = 17 * 2.5),
  legend.key.size = unit(5, "line"))

main_plot +
  annotation_custom(
    ggplotGrob(inset_plot),
    ymin = 0.53, ymax = 0.85, xmin = 0.5, xmax = 34
  ) +
  theme(axis.title = element_text(size = 17 * 3),
        axis.text  = element_text(size = 12 * 3),
        legend.text = element_text(size = 12 * 3),
        legend.title = element_text(size = 17 * 3),
        legend.key.size = unit(5, "line"))


# save to png
ggsave(here::here("figures", "final", "fig5.png"), scale = 3, bg = "white", 
       width = 5000, height = 3455, units = "px", dpi = 350)
ggsave(here::here("figures", "final", "fig5.tiff"), scale = 3, bg = "white", 
       width = 5000, height = 3455, units = "px", dpi = 350)
ggsave(here::here("figures", "final", "fig5.svg"), scale = 3, bg = "white", 
       width = 5000, height = 3455, units = "px", dpi = 350)


# lower resolution
main_plot <- ggplot(ares, aes(x = month, y = mean_s, 
                              group = health_avoider, colour = health_avoider)) +
  geom_ribbon(aes(ymin = lw, ymax = up,
                  colour = health_avoider,
                  fill = health_avoider),
              alpha = 0.08, linetype = "dotted") +
  
  # geom_line(aes(y = s_upr, colour = health_avoider_v2f), linetype = "dotted", linewidth = 1) +
  # geom_line(aes(y = s_lwr, colour = health_avoider_v2f), linetype = "dotted", linewidth = 1) +
  geom_point(size = 10) +
  geom_line(alpha = 0.8, linewidth = 5) +
  # geom_vline(xintercept = 0) +
  # geom_hline(yintercept = 0.5) +
  scale_y_continuous(limits = c(0.5, 1), name = "\nMean MACE-free survival\n") +
  scale_x_continuous(breaks = seq(0, 36, by = 4), name = "\nMonth") +
  scale_colour_discrete(name = "Monitoring frequency") +
  scale_fill_discrete(guide = "none") +
  theme_minimal(base_size = 17) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())


inset_plot <- main_plot +
  scale_y_continuous(limits = c(0.9, 1), name = NULL) +
  # geom_hline(yintercept = 0.9) +
  scale_colour_discrete(guide = "none") +
  scale_x_continuous(name = NULL, breaks = seq(0, 36, by = 4)) +
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks.x = element_line(linewidth = 1),
        axis.title = element_text(size = 17 * 1.5),
        axis.text  = element_text(size = 12 * 1.5),
        legend.text = element_text(size = 12 * 1.5),
        legend.title = element_text(size = 17 * 1.5))

main_plot +
  annotation_custom(
    ggplotGrob(inset_plot),
    ymin = 0.53, ymax = 0.85, xmin = 0.5, xmax = 34
  ) +
  theme_minimal(base_size = 17) +
  theme(axis.title = element_text(size = 17 * 2),
        axis.text  = element_text(size = 12 * 2),
        legend.text = element_text(size = 12 * 2),
        legend.title = element_text(size = 17 * 2),
        legend.key.size = unit(5, "line"))


ggsave(here::here("figures", "final", "fig5_small.png"), scale = 2, bg = "white", 
       width = 5000, height = 1728, units = "px")
