library(WeightIt)
library(cobalt)
library(patchwork)


get_treatment_w <- function(dat_2020, dat_2018){

  formul <- formula(health_avoider_v2 ~ age + sex + socdep56 + livenv + sys_bp_exp +
                    ldl_ch_exp + hba1c_exp + Depression + Dementia + Asthma +
                    COPD + Cancer + d_count + mean_measurement + smoking +
                    n_drugs)


  ipw_2020 <- WeightIt::weightit(formul, data = dat_2020,
                method = "glm", 
                estimand = "ATE",
                stabilize = TRUE)

  ipw_2018 <- WeightIt::weightit(formul, data = dat_2018,
                               method = "glm", 
                               estimand = "ATE",
                               stabilize = TRUE)


# a <- cobalt::bal.plot(ipw_2020, var.name = "age", which = "both")
# b <- cobalt::bal.plot(ipw_2018, var.name = "age", which = "both")
# a / b
# 
# a <- cobalt::bal.plot(ipw_2020, var.name = "livenv", which = "both", type = "histogram")
# b <- cobalt::bal.plot(ipw_2018, var.name = "livenv", which = "both", type = "histogram")
# a / b
# 
# a <- cobalt::bal.plot(ipw_2020, var.name = "socdep56", which = "both", type = "histogram")
# b <- cobalt::bal.plot(ipw_2018, var.name = "socdep56", which = "both", type = "histogram")
# a / b
# 
# a <- cobalt::bal.plot(ipw_2020, var.name = "sys_bp_exp", which = "both")
# b <- cobalt::bal.plot(ipw_2018, var.name = "sys_bp_exp", which = "both")
# a / b
# 
# a <- cobalt::bal.plot(ipw_2020, var.name = "ldl_ch_exp", which = "both")
# b <- cobalt::bal.plot(ipw_2018, var.name = "ldl_ch_exp", which = "both")
# a / b
# 
# a <- cobalt::bal.plot(ipw_2020, var.name = "hba1c_exp", which = "both")
# b <- cobalt::bal.plot(ipw_2018, var.name = "hba1c_exp", which = "both")
# a / b
# 
# a <- cobalt::bal.plot(ipw_2020, var.name = "Depression", which = "both")
# b <- cobalt::bal.plot(ipw_2018, var.name = "Depression", which = "both")
# a / b
# 
# a <- cobalt::bal.plot(ipw_2020, var.name = "Asthma", which = "both")
# b <- cobalt::bal.plot(ipw_2018, var.name = "Asthma", which = "both")
# a / b
# 
# 
# a <- cobalt::bal.plot(ipw_2020, var.name = "COPD", which = "both")
# b <- cobalt::bal.plot(ipw_2018, var.name = "COPD", which = "both")
# a / b
# 
# a <- cobalt::bal.plot(ipw_2020, var.name = "Cancer", which = "both")
# b <- cobalt::bal.plot(ipw_2018, var.name = "Cancer", which = "both")
# a / b
# 
# a <- cobalt::bal.plot(ipw_2020, var.name = "d_count", which = "both", type = "histogram")
# b <- cobalt::bal.plot(ipw_2018, var.name = "d_count", which = "both", type = "histogram")
# a / b
# 
# a <- cobalt::bal.plot(ipw_2020, var.name = "smoking", which = "both", type = "histogram")
# b <- cobalt::bal.plot(ipw_2018, var.name = "smoking", which = "both", type = "histogram")
# a / b
# 
# a <- cobalt::bal.plot(ipw_2020, var.name = "n_drugs", which = "both", type = "histogram", mirror = TRUE)
# b <- cobalt::bal.plot(ipw_2018, var.name = "n_drugs", which = "both", type = "histogram", mirror = TRUE)
# a / b

# cobalt::bal.plot(ipw_2020, var.name = "prop.score", which = "both", type = "histogram", mirror = TRUE)
# 
# 
# a <- love.plot(ipw_2020, binary = "std", thresholds = c(m = 0.1),
#                line = TRUE,
#                var.order = "unadjusted",
#                abs = FALSE,
#                drop.distance = TRUE)
# b <- love.plot(ipw_2018, binary = "std", thresholds = c(m = 0.1),
#                line = FALSE,
#                var.order = a,
#                abs = FALSE,
#                drop.distance = TRUE)

# a / b
# clr <- "azure4"
# 
# a + 
#   geom_point(data = b$data |> dplyr::filter(Sample == "Unadjusted"), 
#              colour = clr, shape = 18, fill = clr, size = 4)




  dat_2020$ipw_treatment <- ipw_2020$weights
  dat_2018$ipw_treatment <- ipw_2018$weights

# hist(ipw_2020$weights)
# hist(ipw_2018$weights)

# truncate weights to less than three
  dat_2020$ipw_treatment <- if_else(dat_2020$ipw_treatment > 3, 3, dat_2020$ipw_treatment)
  dat_2018$ipw_treatment <- if_else(dat_2018$ipw_treatment > 3, 3, dat_2018$ipw_treatment)
  
  list(dat_2020, dat_2018)
  
}