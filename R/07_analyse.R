library(MatchThem)
# impute missing datasest
library(mice)
library(survey)
source(here::here("R", "H_get_censoring_w.R"))


load(here::here("datasets", "covar", "nohf", "m2_nc_covar_nohf_mace.RData"))

sample_2020_long <- sample_2020_long |>
  dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.65, TRUE, FALSE))

sample_2020 <- sample_2020 |>
  dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= 0.65, TRUE, FALSE))

table(sample_2020$y4_contacts, sample_2020$health_avoider_v2)
mean(sample_2020$mean_measurement[sample_2020$health_avoider_v2 == TRUE])

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
ni <- 20
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

# assess balance
bal.tab(weighted, stats = c("m", "ks"), imp.fun = "max")


# now, we need to compute the censoring weights and add them to the 
# weighted data


# analyse the imputed datasets
formul_fit <- formula(out ~ health_avoider_v2 + month + month2 + ha_m +
                        age + sex + socdep56 + sys_bp_exp + tot_ch_exp + 
                        hba1c_exp + Cancer + Asthma + COPD + 
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

contrast$RR <- (1 - contrast$mean_s_Reduced) / (1 - contrast$mean_s_Stable)


# Risk difference at the end of three years
RD <- round(contrast$RD[contrast$month == 36], 4)


