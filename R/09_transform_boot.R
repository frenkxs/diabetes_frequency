




# convert the bootstrap samples into a dataframe

res <-  data.frame(month = integer(),
                     mean_s_Reduced = numeric(),
                     mean_s_Stable = numeric(),
                     cHR = numeric(),
                     RD = numeric())


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
    res <- bind_rows(res, temp)
  }
  res
}

res <- convert_sam(out1)
res <- convert_sam(out2)
res <- convert_sam(out3)
res <- convert_sam(out4)
res <- convert_sam(out5)
res <- convert_sam(out6)
res <- convert_sam(out7)
res <- convert_sam(out8)
res <- convert_sam(out9)
res <- convert_sam(out10)

r <- res |>
  group_by(month) |>
  summarise(ci_stable_lw = quantile(mean_s_Stable, probs = c(0.025), na.rm = TRUE),
            ci_stable_up = quantile(mean_s_Stable, probs = c(0.975), na.rm = TRUE),
            ci_reduced_lw = quantile(mean_s_Reduced, probs = c(0.025), na.rm = TRUE),
            ci_reduced_up = quantile(mean_s_Reduced, probs = c(0.975), na.rm = TRUE),
            ci_RD_lw = quantile(RD, probs = c(0.025), na.rm = TRUE),
            ci_RD_up = quantile(RD, probs = c(0.975), na.rm = TRUE),
            ci_cHR_lw = quantile(cHR, probs = c(0.025), na.rm = TRUE),
            ci_cHR_up = quantile(cHR, probs = c(0.975), na.rm = TRUE))

rr <- r |>
  dplyr::select(c(month, ci_stable_lw, ci_reduced_lw, ci_stable_up, ci_reduced_up)) |>
  pivot_longer(cols = -1, 
               names_pattern = "(..*)(..)$", names_to = c("status", "bound")) |>
  mutate(status = if_else(status == "ci_stable_", "Stable", "Reduced"))


ggplot(rr, aes(x = month, y = value, group = interaction(status, bound), colour = status)) +
  geom_line() +
  scale_y_continuous(limits = c(0.8, 1))
