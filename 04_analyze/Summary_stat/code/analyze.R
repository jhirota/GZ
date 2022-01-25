library(tidyverse)

# data load -------

weekSIR_rob <- read_csv(here::here("03_build/Robust_check/output/weekSIR_robustness.csv")) %>% 
  as.data.frame()


# data making ------- 
  
stat <- stargazer::stargazer(weekSIR_rob %>%
                               select(newcase_day, customers_per, sales_per, avg_temp, avg_rain,
                                      infectious_l2, susceptible, tests),
                     type = "latex",
                     header = FALSE,
                     title = "Summary Statistics",
                     summary.stat = c("n", "mean", "sd"),
                     covariate.labels = c("New cases per day", "Number of customers per restaurant",
                                          "Sales per restaurant", "Average temperature", "Average rainfall",
                                          "Infectious", "Susceptible", "Number of COVID-19 tests"),
                     out = "04_analyze/Summary_stat/output/summary_statistics.tex")









