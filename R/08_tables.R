library(table1)

load(here::here("datasets", "covar", "nohf", "m2_nc_covar_nohf_mace.RData"))

datt <- sample_2020
datt <- sample_2018

get_t1 <- function(datt, threshold = 0.65){

    datt <- datt |>
      dplyr::mutate(health_avoider_v2 = if_else(delta_perc_mean >= threshold, TRUE, FALSE)) |>
      dplyr::mutate(health_avoider_v2 = factor(health_avoider_v2)) |>
      dplyr::mutate(health_avoider_v2 = recode(health_avoider_v2,
                                                   "TRUE" = "Reduced",
                                                   "FALSE"=  "Same")) 
    
    
    lab <- list(
      variables = list(
        age = "Age (years)",
        sex = "Sex",
        socdep56 = "Social deprivation" ,
        livenv = "Living environment",
        sys_bp_exp = "Systolic blood pressure (mmHg)",
        tot_ch_exp = "Total cholesterol (mmol/L)",
        hba1c_exp = "Hemoglobin A1c (mmol/mol)" ,
        cardiomet_stat = "Cardiometabolic status",
        smoking = "Smoking status",
        d_count = "Number of comorbidities",
        Depression = "Comorbid depression",
        COPD = "Comorbid COPD",
        Cancer =  "Comorbid cancer",
        Asthma = "Comorbid asthma",
        n_drugs = "Total number of different drugs presribed",
        mean_inc_prescription = "Total number of new drugs presribed",
        antihypertensives = "Number of antihypertensive drugs prescribed",
        cholesterol_drugs = "Number of cholesterol drugs prescribed" ,
        diabetes_drugs = "Number of diabetes drugs prescribed" ,
        cvd = "Non-fatal cardiovascular events",
        cvd_mort = "Cardiovascular mortality",
        mace = "Major Cardiovascular Adverse Events (MACE)",
        died_non_cvd = "Non-cardiovascular mortality",
        died_all = "All-cause mortality"),
    
      groups = list("Diabetes monitoring frequency", ""))
    
    strata <- c(split(datt, datt$health_avoider_v2), list(Total = datt))
    
    rndr <- function(x, ...) {
      y = table1::render.default(x, ...)
      if (is.logical(x)) y[2] else y
    }
    
    t1_2020 <- table1::table1(strata, 
                              lab,   
                              groupspan = c(2,1),
                              render = rndr,
                              render.continuous = c(.="Mean (SD)"))
    t1_2020
    
    # t1_2020 <- table1::table1(strata, 
    #                           data = datt,
    #                           lab,                          
    #                           render = rndr,
    #                           render.continuous = c(.="Mean (SD)"))
    #                           
    # t1_2020
    
    # datt <- sample_2018
    # t1_2018 <- table1::table1(~ age + sex + socdep56 + livenv + cvd + mace + died_non_cvd + died_all
    #                           | factor(health_avoider_v2), 
    #                           data = datt,
    #                           # labels = lab, 
    #                           render = rndr
    #                           # render.continuous = c(.="Mean (SD)")
    #                           # extra.col = list(`SMD` = smd_t1)  
    # )
}

# tables for sensitivity analyses
get_t1(sample_2018, threshold = 0.65)
