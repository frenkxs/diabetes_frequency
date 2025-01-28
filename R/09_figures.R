
# !!! For image 5 (survival curves, see 'bootstrap.R' script) !!!

# this will be plots describing the samples
library(cobalt)
library(tidyverse)
library(patchwork)

load(here::here("datasets", "covar", "nohf", "m2_nc_covar_nohf_mace.RData"))


# Figure 3 --------------------

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
                     name = "\nChange in number of blood pressure measurements") +
  scale_y_continuous(name = "") +
  scale_fill_discrete(name = "Intervention year") +
  scale_colour_discrete(guide = "none") +
  theme_minimal(base_size = 22) +
  theme(axis.title = element_text(size = 17 * 3),
        axis.text  = element_text(size = 12 * 3),
        legend.text = element_text(size = 12 * 3),
        legend.title = element_text(size = 17 * 3),
        legend.key.size = unit(5, "line"), 
        legend.position = "inside",
        legend.position.inside = c(0.80, 0.80),
        legend.box.background = element_rect(),
        legend.box.margin = margin(18, 18, 18, 18))
        

# save to png
ggsave(here::here("figures", "final", "fig3.png"), scale = 3, bg = "white", 
       width = 5000, height = 3628, units = "px", dpi = 350)

# save to tiff
ggsave(here::here("figures", "final", "fig3.tiff"), scale = 3, bg = "white", 
       width = 5000, height = 3628, units = "px", dpi = 350)

# save to svg
ggsave(here::here("figures", "final", "fig3.svg"), scale = 3, bg = "white", 
       width = 5000, height = 3628, units = "px", dpi = 350)



# lower resolution
ggplot(temp, aes(x = delta, fill = sample, colour = sample, group = sample)) +
  geom_histogram(position = "dodge", alpha = 0.5, bins = 25) +
  scale_x_continuous(breaks = seq(-7, 7, by = 1), limits = c(-9, 9), 
                     name = "\nChange in number of blood pressure measurements") +
  scale_y_continuous(name = "") +
  scale_fill_discrete(name = "Intervention year") +
  scale_colour_discrete(guide = "none") +
  theme_minimal(base_size = 17) +
  theme(axis.title = element_text(size = 17 * 2),
        axis.text  = element_text(size = 12 * 2),
        legend.text = element_text(size = 9 * 2),
        legend.title = element_text(size = 17 * 1),
        legend.key.size = unit(3, "line"), 
        legend.position = "inside",
        legend.position.inside = c(0.80, 0.80),
        legend.box.background = element_rect(),
        legend.box.margin = margin(10, 10, 10, 10))



ggsave(here::here("figures", "final", "fig3_small.png"), scale = 2, bg = "white", 
       width = 2500, height = 1814, units = "px")



# ggplot(sample_2020, aes(x = n_drugs, fill = health_avoider_v2, colour = health_avoider_v2, 
#                         group = health_avoider_v2)) +
#   geom_bar(position = "dodge", alpha = 0.5) +
#   scale_x_continuous(breaks = seq(0, 9, by = 1), limits = c(-1, 15), 
#                      name = "Number of drugs presribed") +
#   scale_y_continuous(name = "") +
#   theme_minimal(base_size = 16)
# 
# tapply(sample_2020$n_drugs, sample_2020$health_avoider_v2, mean)
# tapply(sample_2018$n_drugs, sample_2018$health_avoider_v2, mean)
# 
# ggplot(temp, aes(x = delta_median, fill = sample, colour = sample, group = sample)) +
#   geom_density(alpha = 0.5, adjust = 2) +
#   scale_x_continuous(breaks = seq(-20, 20, by = 1), limits = c(-10, 10)) +
#   # facet_wrap(~ sample) +
#   theme_minimal()


# ggplot(temp, aes(x = delta_prescription_mean, fill = sample, colour = sample, group = sample)) +
#   geom_histogram(position = "dodge", alpha = 0.5, bins = 30) +
#   scale_x_continuous(breaks = seq(-20, 20, by = 1), limits = c(-10, 10),
#                      name = "Change in number of prescription") +
#   scale_y_continuous(name = "") +
#   theme_minimal(base_size = 16)
# 
# 
# ggplot(temp, aes(x = delta_prescription_median, fill = sample, colour = sample, group = sample)) +
#   geom_density(alpha = 0.5, adjust = 2) +
#   scale_x_continuous(breaks = seq(-20, 20, by = 1), limits = c(-10, 10)) +
#   # facet_wrap(~ sample) +
#   theme_minimal()
# 
# ggplot(sample_2020, aes(x = delta_prescription_mean, fill =  health_avoider_v2, 
#                         group = health_avoider_v2)) +
#   geom_histogram(bins = 50) +
#   stat_summary(aes(xintercept = after_stat(x), y = 0, colour = health_avoider_v2), fun = mean, 
#                geom = "vline", orientation = "y")
# 
# 
# 
# 
# # standardised mean difference between the change in the number of prescriptions
# m_ha <- mean(sample_2020$delta_prescription_median[sample_2020$health_avoider_v2 == TRUE])
# m_nha <- mean(sample_2020$delta_prescription_median[sample_2020$health_avoider_v2 == FALSE])
# p_sd <- sqrt((var(sample_2020$delta_prescription_median[sample_2020$health_avoider_v2 == FALSE]) + 
#                 var(sample_2020$delta_prescription_median[sample_2020$health_avoider_v2 == TRUE])) / 2)
# 
# (m_ha - m_nha) / p_sd




# median(sample_2020$delta_prescription_mean[sample_2020$health_avoider_v2 == FALSE])
# 
# d_m <- . %>%
#   select(y1_contacts:y6_contacts, health_avoider_v2, pat_id) %>%
#   pivot_longer(cols = c(y1_contacts:y6_contacts)) 
# 
# frmt <- list(
#   geom_line(alpha = 0.1),
#   geom_jitter(height = 0.3, width = 0, aes(alpha = health_avoider_v2)),
#   scale_alpha_discrete(range = c(0.01, 0.5)),
#   scale_y_continuous(limits = c(0, 13))
# )
# 
# 
# sam <- sample_2020 %>% d_m %>%
#   # dplyr::filter(health_avoider_v2 == TRUE) |>
#   ggplot(aes(x = name, y = value, group = pat_id, colour = health_avoider_v2)) + frmt +
#   ggtitle("sample")
# 
# neg_sam <- sample_2018 %>% d_m %>% 
#   # dplyr::filter(health_avoider_v2 == TRUE) |>
#   ggplot(aes(x = name, y = value, group = pat_id, colour = health_avoider_v2)) + frmt +
#   ggtitle("neg sample")
# 
# wrap_elements(sam) / wrap_elements(neg_sam)
# 
# 
# 
# ggplot(temp, aes(x = delta, fill = sample, colour = sample, group = sample)) +
#   geom_histogram(position = "dodge", alpha = 0.5, bins = 30) +
#   scale_x_continuous(breaks = seq(-20, 20, by = 1), limits = c(-10, 10)) +
#   # facet_wrap(~ sample) +
#   theme_minimal()
# 
# ggplot(temp, aes(x = age, fill = health_avoider_v2, group = health_avoider_v2)) +
#   geom_histogram(position = "dodge", alpha = 0.8, bins = 25) +
#   # scale_x_continuous(breaks = seq(-20, 20, by = 1), limits = c(-10, 10)) +
#   scale_fill_discrete(name = "Healthcare avoider") +
#   facet_wrap(~ sample) +
#   scale_y_continuous(name = "") +
#   theme_minimal(base_size = 16)
# 
# 
# ggplot(temp, aes(x = socdep56, fill = health_avoider_v2, group = health_avoider_v2)) +
#   geom_bar(position = "dodge", alpha = 0.5, linewidth = 1.5) +
#   # scale_x_continuous(breaks = seq(-20, 20, by = 1), limits = c(-10, 10)) +
#   scale_fill_discrete(name = "Healthcare avoider") +
#   facet_wrap(~ sample) +
#   scale_y_continuous(name = "") +
#   theme_minimal(base_size = 16)
# 
# 
# 


# -------- Figure 4 --------
# this is what goes to the paper 

formul <- formula(health_avoider_v2 ~ age + sex + socdep56 + livenv + sys_bp_exp +
                    tot_ch_exp + hba1c_exp + Depression + Asthma +
                    COPD + Cancer + smoking + antihypertensives +
                    diabetes_drugs + cholesterol_drugs)


# remove NAs
# impute median values for the sake of this figure
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
  scale_x_continuous(limits = c(-0.31, 0.31), breaks = seq(-0.3, 0.3, length.out = 7), 
                     name = "\nStandardized Mean Differences") +
  theme_minimal(base_size = 14)


b <- love.plot(ipw_2018, binary = "std", thresholds = c(m = 0.1),
               line = TRUE,
               var.order = "unadjusted",
               abs = FALSE,
               drop.distance = TRUE, 
               var.names = pretty_names) +
  scale_x_continuous(limits = c(-0.31, 0.31), breaks = seq(-0.3, 0.3, length.out = 7)) +
  theme_minimal(base_size = 14)


aa <- a$data
bb <- b$data

a + 
  geom_point(data = b$data |> dplyr::filter(Sample == "Unadjusted"), shape = 8,
             size = 10, colour = "black") +
  geom_point(aes(colour = Sample, size = Sample)) +

  scale_size_discrete(range = c(16, 11)) +
  theme_minimal(base_size = 22) +
  theme(axis.title = element_text(size = 17 * 3),
        axis.text  = element_text(size = 12 * 3),
        legend.text = element_text(size = 12 * 3),
        legend.title = element_text(size = 17 * 3),
        legend.key.size = unit(5, "line"),
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.2),
        legend.box.background = element_rect(),
        legend.box.margin = margin(18, 18, 18, 18))



# save to png
ggsave(here::here("figures", "final", "fig4.png"), scale = 3, bg = "white", 
       width = 5000, height = 3628, units = "px", dpi = 350)

# save to tiff
ggsave(here::here("figures", "final", "fig4.tiff"), scale = 3, bg = "white", 
       width = 5000, height = 3628, units = "px", dpi = 350)

# save to svg
ggsave(here::here("figures", "final", "fig4.svg"), scale = 3, bg = "white", 
       width = 5000, height = 3628, units = "px", dpi = 350)

# lower resolution
a + 
  geom_point(data = b$data |> dplyr::filter(Sample == "Unadjusted"), shape = 8,
             size = 4, colour = "black") +
  geom_point(aes(colour = Sample, size = Sample)) +
  
  scale_size_discrete(range = c(10, 6)) +
  theme_minimal(base_size = 17) +
  theme(axis.title = element_text(size = 17 * 2),
        axis.text  = element_text(size = 12 * 2),
        legend.text = element_text(size = 12 * 2),
        legend.title = element_text(size = 17 * 2),
        legend.key.size = unit(3, "line"),
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.2),
        legend.box.background = element_rect(),
        legend.box.margin = margin(18, 18, 18, 18))


ggsave(here::here("figures", "final", "fig4_small.png"), scale = 2, bg = "white", 
       width = 2500, height = 1814, units = "px")


