library(tidyverse)

# data load --------
weekSIR_rob <- read_csv("03_build/Robustness_check/output/weekSIR_robustness.csv")

# Plot Tradeoff curve ----
weekSIR_rob <- weekSIR_rob %>% 
  mutate(newcaset.2_per = newcaset.2 / (population/10000),
         treat = ifelse(pref == "Yamanashi", "Yamanashi", "Neighboring 5 prefectures"))

Tradeoffcurve <- function(colname = "customers_per", linemethod, log = "") {
  if (log == "log") {
    tradeoff <- ggplot(weekSIR_rob,
                       mapping = aes(x = -log(get(colname)),
                                     y = log(newcaset.2_per + 1),
                                     color = treat)) +
      geom_point() +
      geom_smooth(method = linemethod)+
      scale_color_manual(name = " ", values = c("#ff9900", "#339900")) +
      labs(title = "Tradeoff between # of new cases (2 weeks lag) and economic loss",
           y = "# of new cases per 10000 (log)",
           x = paste0("Economic loss (-logged ", colname, " 10000)"))
    
  } else {
    tradeoff <- ggplot(weekSIR_rob,
                       mapping = aes(x = -get(colname),
                                     y = newcaset.2_per,
                                     color = treat)) +
      geom_point() +
      geom_smooth(method = linemethod)+
      scale_color_manual(name = " ", values = c("#ff9900", "#339900")) +
      labs(title = "Tradeoff between # of new cases (2 weeks lag) and economic loss",
           y = "# of new cases per 10000",
           x = paste0("Economic loss (- ", colname, " 10000)"))
  }
  ggsave(paste0("04_analyze/Tradeoff_curve/output/tradeoff_", colname, "_", linemethod, "_", log, ".png"),
         tradeoff, width = 10, height = 8, dpi = 300)
  return(tradeoff)
}

Tradeoff <- Tradeoffcurve("customers_per", "gam", "log")
Tradeoff2 <- Tradeoffcurve("customers_per", "gam")
Tradeoff3 <- Tradeoffcurve("customers_per", "loess", "log")
Tradeoff4 <- Tradeoffcurve("customers_per", "loess")
Tradeoff5 <- Tradeoffcurve("sales_per", "gam", "log")
Tradeoff6 <- Tradeoffcurve("sales_per", "gam")
Tradeoff7 <- Tradeoffcurve("sales_per", "loess", "log")
Tradeoff8 <- Tradeoffcurve("sales_per", "loess")


# Heterogeneity check-------


hetero_1.1 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)*log(agrgt_potecovid_lag2 + 1) + emergency + log(noftestst.2 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1)| pref+week | 0 | pref, data = weekSIR_rob)

summary(hetero_1.1)





