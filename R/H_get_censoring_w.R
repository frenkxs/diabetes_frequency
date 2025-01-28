#. ------------------------------------------------------------
# .-------------------------------------------------------------
# -------- Probability weights for the data -------------------
#. ------------------------------------------------------------
# .-------------------------------------------------------------
library(WeightIt)
library(cobalt)


# ---------- Baseline weights -----------------
#  . --------------

# sample_2020 <- v1w[[3]]
# sample_2020_long <- v1w[[2]]
# 
# sample_2020 <- sample_2020 |>
#   dplyr::mutate(delta_median_perc = delta_median / median_measurement,
#               ha_v2 = if_else(delta_median_perc > 0.6, TRUE, FALSE))
# 
# sample_2020_long <- sample_2020_long |>
#   dplyr::mutate(delta_median_perc = delta_median / median_measurement,
#                 ha_v2 = if_else(delta_median_perc > 0.6, TRUE, FALSE))
# 
# 
# table(sample_2020$ha_v2, sample_2020$event)
# table(sample_2020$health_avoider_v2, sample_2020$event)
# 
# sample_2020$health_avoider_v2 <- sample_2020$ha_v2
# sample_2020_long$health_avoider_v2 <- sample_2020_long$ha_v2
# 
# dd <- sample_2020[, c(1:20, 50:58)]
# table(dd$health_avoider_v2, dd$ha_v2)

# IPW denominator: probability of health avoidance given age, sex, spb, chol, hba1c, 
# livenv, socdep, commorbidities, number of visits in prepandemic period

get_weights <- function(df, df_long, censor){

    
    # ---------- Censoring weights -----------------
    
    # censoring event here is mortality if it happens before our event could have occurred
    # cens here is the competing event (either all-cause death or non-cardiovascular death) 
    cens <- dplyr::enquo(censor)
  
    # we model probability of staying alive
    df_long <- df_long |>
      dplyr::mutate(alive = if_else(!!cens == TRUE, FALSE, TRUE))
    
    
    # add polynomial terms to both age and months
    formul_censoring <- formula(alive ~ poly(age, 2) + poly(month, 2) + 
                                  health_avoider_v2 + Depression + Cancer + 
                                  Dementia + Parkinsonism + Asthma + COPD + 
                                  factor(sex) + d_count + socdep56 + sys_bp_exp + sys_bp_fu + 
                                  ldl_ch_fu + hba1c_fu + n_drugs + 
                                  smoking + mean_inc_prescription)
    
    # denominator for weights
    fit_ipcw_den <- glm(formul, family = "binomial", data = df_long)

    ipcw <- data.frame(pat_id = df_long$pat_id, 
                            month = df_long$month,
                            ipcw_den = predict(fit_ipcw_den, type = "response"))
    
    
    # numerator for weights
    fit_ipcw_num <- glm(alive ~ 1, family = "binomial", data = df_long)
    ipcw$icpw_num <- predict(fit_ipcw_num, type = "response")
    
    ipcw$numcum <- ave(ipcw$icpw_num, ipcw$pat_id, FUN = cumprod)
    ipcw$dencum <- ave(ipcw$ipcw_den, ipcw$pat_id, FUN = cumprod)
    
    ipcw$ipw_censoring <- ipcw$numcum / ipcw$dencum
    
    
    df_long$ipw_censoring <- ipcw$ipw_censoring 
    
    
    
    df_long <- dplyr::left_join(df_long, df |> dplyr::select(pat_id, ipw_treatment),
                                         by = "pat_id")
    
    
    df_long$ipw <- df_long$ipw_censoring * df_long$ipw_treatment
    
    df_long
}


get_censoring_w <- function(df_long, censor){
  
  
  # ---------- Censoring weights -----------------
  
  # censoring event here is mortality if it happens before our event could have occurred
  # cens here is the competing event (either all-cause death or non-cardiovascular death) 
  cens <- dplyr::enquo(censor)
  
  # we model probability of staying alive
  df_long <- df_long |>
    dplyr::mutate(alive = if_else(!!cens == TRUE, FALSE, TRUE))
  
  
  # add polynomial terms to both age and months
  formul_censoring <- formula(alive ~ poly(age, 2) + poly(month, 2) + 
                                health_avoider_v2 + Depression + Cancer + 
                                Dementia + Parkinsonism + Asthma + COPD + 
                                factor(sex) + d_count +socdep56 + sys_bp_fu + 
                                ldl_ch_fu + hba1c_fu + n_drugs + mean_measurement + 
                                smoking + mean_inc_prescription)
  
  # denominator for weights
  fit_ipcw_den <- glm(formul_censoring, family = "binomial", data = df_long)
  
  ipcw <- data.frame(pat_id = df_long$pat_id, 
                     month = df_long$month,
                     ipcw_den = predict(fit_ipcw_den, type = "response"))
  
  
  # numerator for weights
  fit_ipcw_num <- glm(alive ~ 1, family = "binomial", data = df_long)
  ipcw$icpw_num <- predict(fit_ipcw_num, type = "response")
  
  ipcw$numcum <- ave(ipcw$icpw_num, ipcw$pat_id, FUN = cumprod)
  ipcw$dencum <- ave(ipcw$ipcw_den, ipcw$pat_id, FUN = cumprod)
  
  ipcw$numcum / ipcw$dencum
}


