library(tidyverse)
library(lfe)

# data load ------
postas_day1 <- read_csv(here::here("03_build/Postas/output/postas_daily_data.csv"))
# postas_week <- read_csv(here::here("03_build/Postas/output/postas_weekly_data.csv"))


# Daily analysis ---------

dlmpostas1 <- felm(log(sales_per) ~ log(cumGZ + 1)  + emergency| pref+date | 0 | pref, data = postas_day1)
dlmpostas2 <- felm(log(sales_per) ~ log(cumGZ + 1)  + emergency + log(newcase_day + 1) | pref+date | 0 | pref, data = postas_day1)
dlmpostas3 <- felm(log(sales_per) ~ log(cumGZ + 1)  + emergency + log(newcase_day + 1) + avg_temp + sum_rain| pref+date | 0 | pref, data = postas_day1)
dlmpostas4 <- felm(log(sales_per) ~ log(cumGZ + 1)  + emergency + log(newcase_day + 1) + avg_temp + sum_rain + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1)
dlmpostas.cus1 <- felm(log(customers_per) ~ log(cumGZ + 1)  + emergency| pref+date | 0 | pref, data = postas_day1)
dlmpostas.cus2 <- felm(log(customers_per) ~ log(cumGZ + 1)  + emergency + log(newcase_day + 1) | pref+date | 0 | pref, data = postas_day1)
dlmpostas.cus3 <- felm(log(customers_per) ~ log(cumGZ + 1)  + emergency + log(newcase_day + 1) + avg_temp + sum_rain| pref+date | 0 | pref, data = postas_day1)
dlmpostas.cus4 <- felm(log(customers_per) ~ log(cumGZ + 1)  + emergency + log(newcase_day + 1) + avg_temp + sum_rain + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1)

# dpostashtml <- stargazer(dlmpostas1, dlmpostas2, dlmpostas3, dlmpostas4, dlmpostas.cus1, dlmpostas.cus2, dlmpostas.cus3, dlmpostas.cus4,
#                          title = "TABLE: POSTAS data (sales and customers) and GreenZone certification (daily)",
#                          digits = 3,
#                          digits.extra = 0,
#                          type = "html",
#                          out = "04_analyze/Postas_reg/output/postas_pref_day.html",
#                          add.lines=list(c("Prefecture FE", rep("X",8)), c("Day FE", rep("X",8))),
#                          omit.stat=c("f", "ser"))



# Weekly analysis ----------
# lmpostas4 <- felm(log(Sales) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency| pref+week | 0 | pref, data = postas_week)
# lmpostas5 <- felm(log(Sales) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp + rain| pref+week | 0 | pref, data = postas_week)
# lmpostas6 <- felm(log(Sales) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp + rain + log(cumGZ + 1):log(newcaseday + 1)| pref+week | 0 | pref, data = postas_week)
# lmpostas.cus4 <- felm(log(customers) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency| pref+week | 0 | pref, data = postas_week)
# lmpostas.cus5 <- felm(log(customers) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp + rain| pref+week | 0 | pref, data = postas_week)
# lmpostas.cus6 <- felm(log(customers) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp + rain + log(cumGZ + 1):log(newcaseday + 1)| pref+week | 0 | pref, data = postas_week)


# postashtml1 <- stargazer(lmpostas4, lmpostas5, lmpostas6, lmpostas.cus4, lmpostas.cus5, lmpostas.cus6,
#                          title = "TABLE: POSTAS data (sales & customers) and GreenZone certification (weekly)",
#                          digits = 3,
#                          digits.extra = 0,
#                          type = "html",
#                          out = "04_analyze/Postas_reg/output/postas_pref_week.html",
#                          add.lines=list(c("Prefecture FE", rep("X",6)), c("Week FE", rep("X",6))),
#                          omit.stat=c("f", "ser"))


