library(tidyverse)
library(lfe)

# data load ------
weekSIR3 <- read_csv("03_build/Postas/output/weekSIR3.csv")
postas_day1 <- read_csv("03_build/Postas/output/postas_daily_data.csv")
postas_week <- read_csv("03_build/Postas/output/postas_weekly_data.csv")

# postas weekly analysis ---------
lmpostas1 <- felm(log(sales) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency| pref+week | 0 | pref, data = weekSIR3)
lmpostas2 <- felm(log(sales) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp_q + rain| pref+week | 0 | pref, data = weekSIR3)
lmpostas3 <- felm(log(sales) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp_q + rain + log(cumGZ + 1):log(newcaseday + 1)| pref+week | 0 | pref, data = weekSIR3)
lmpostas.cus1 <- felm(log(customers) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency| pref+week | 0 | pref, data = weekSIR3)
lmpostas.cus2 <- felm(log(customers) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp_q + rain| pref+week | 0 | pref, data = weekSIR3)
lmpostas.cus3 <- felm(log(customers) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp_q + rain + log(cumGZ + 1):log(newcaseday + 1)| pref+week | 0 | pref, data = weekSIR3)


# resview_yama_mean <- mean(weekSIR2$resview[weekSIR2$pref == "Yamanashi"])
# resview_nonyama_mean <- mean(weekSIR2$resview[weekSIR2$pref != "Yamanashi"])

postashtml <- stargazer(lmpostas1, lmpostas2, lmpostas3, lmpostas.cus1, lmpostas.cus2, lmpostas.cus3,
                        title = "TABLE: POSTAS data (sales & customers) and GreenZone certification",
                        digits = 3,
                        digits.extra = 0,
                        type = "html",
                        out = "04_analyze/Postas_reg/output/sales_pref_week.html",
                        add.lines=list(c("Prefecture FE", rep("X",6)), c("Week FE", rep("X",6))),
                        omit.stat=c("f", "ser"))

# postas daily analysis ---------

dlmpostas1 <- felm(log(sales_per) ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency| pref+date | 0 | pref, data = postas_day1)
dlmpostas2 <- felm(log(sales_per) ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain| pref+date | 0 | pref, data = postas_day1)
dlmpostas3 <- felm(log(sales_per) ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_day1)
dlmpostas.cus1 <- felm(log(customers_per) ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency| pref+date | 0 | pref, data = postas_day1)
dlmpostas.cus2 <- felm(log(customers_per) ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain| pref+date | 0 | pref, data = postas_day1)
dlmpostas.cus3 <- felm(log(customers_per) ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_day1)

dpostashtml <- stargazer(dlmpostas1, dlmpostas2, dlmpostas3, dlmpostas.cus1, dlmpostas.cus2, dlmpostas.cus3,
                         title = "TABLE: POSTAS data (sales & customers) and GreenZone certification (daily)",
                         digits = 3,
                         digits.extra = 0,
                         type = "html",
                         out = "04_analyze/Postas_reg/output/postas_pref_day.html",
                         add.lines=list(c("Prefecture FE", rep("X",6)), c("Day FE", rep("X",6))),
                         omit.stat=c("f", "ser"))



# Weekly Analysis again ----------
lmpostas4 <- felm(log(Sales) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency| pref+week | 0 | pref, data = postas_week)
lmpostas5 <- felm(log(Sales) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp_q + rain| pref+week | 0 | pref, data = postas_week)
lmpostas6 <- felm(log(Sales) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp_q + rain + log(cumGZ + 1):log(newcaseday + 1)| pref+week | 0 | pref, data = postas_week)
lmpostas.cus4 <- felm(log(customers) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency| pref+week | 0 | pref, data = postas_week)
lmpostas.cus5 <- felm(log(customers) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp_q + rain| pref+week | 0 | pref, data = postas_week)
lmpostas.cus6 <- felm(log(customers) ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp_q + rain + log(cumGZ + 1):log(newcaseday + 1)| pref+week | 0 | pref, data = postas_week)


postashtml1 <- stargazer(lmpostas4, lmpostas5, lmpostas6, lmpostas.cus4, lmpostas.cus5, lmpostas.cus6,
                         title = "TABLE: POSTAS data (sales & customers) and GreenZone certification (weekly)",
                         digits = 3,
                         digits.extra = 0,
                         type = "html",
                         out = "04_analyze/Postas_reg/output/postas_pref_week.html",
                         add.lines=list(c("Prefecture FE", rep("X",6)), c("Week FE", rep("X",6))),
                         omit.stat=c("f", "ser"))


