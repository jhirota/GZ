library(tidyverse)
library(stargazer)

# reg night selfrest  ------

#data load 
postas_rob <- read_csv("03_build/Robustness_econ/output/postas_rob.csv")


# reg Night_selfrest
nightself1 <- felm(Night_selfrest ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency| pref+date | 0 | pref, data = postas_rob)
nightself2 <- felm(Night_selfrest ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain| pref+date | 0 | pref, data = postas_rob)
nightself3 <- felm(Night_selfrest ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)

nighthtml <- stargazer(nightself1, nightself2, nightself3,
                         title = "TABLE: Night time self-restraint rate and Green Zone",
                         digits = 3,
                         digits.extra = 0,
                         type = "html",
                         out = "04_analyze/Robustness_econ/output/night_selfrest_day.html",
                         add.lines=list(c("Prefecture FE", rep("X",3)),
                                        c("Day FE", rep("X",3))),
                         omit.stat=c("f", "ser"))

# reg selfrest by age (male)---------
male15 <- felm(M15 ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)
male20 <- felm(M20 ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)
male30 <- felm(M30 ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)
male40 <- felm(M40 ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)
male50 <- felm(M50 ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)
male60 <- felm(M60 ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)
male70 <- felm(M70 ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)

malehtml <- stargazer(male15, male20, male30, male40, male50, male60, male70,
                      title = "TABLE: Self-restraint rate by age and Green Zone",
                      digits = 3,
                      digits.extra = 0,
                      type = "html",
                      out = "04_analyze/Robustness_econ/output/selfrest_by_age_male.html",
                      add.lines=list(c("Prefecture FE", rep("X",7)),
                                     c("Day FE", rep("X",7))),
                      omit.stat=c("f", "ser"))


# reg selfrest by age (female)---------
female15 <- felm(F15 ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)
female20 <- felm(F20 ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)
female30 <- felm(F30 ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)
female40 <- felm(F40 ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)
female50 <- felm(F50 ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)
female60 <- felm(F60 ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)
female70 <- felm(F70 ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)

femalehtml <- stargazer(female15, female20, female30, female40, female50, female60, female70,
                      title = "TABLE: Self-restraint rate by age and Green Zone",
                      digits = 3,
                      digits.extra = 0,
                      type = "html",
                      out = "04_analyze/Robustness_econ/output/selfrest_by_age_female.html",
                      add.lines=list(c("Prefecture FE", rep("X",7)),
                                     c("Day FE", rep("X",7))),
                      omit.stat=c("f", "ser"))


# Reg with linear rand ------------
reg_postas1 <- felm(log(sales_per) ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)
reg_postas2 <- felm(log(sales_per) ~ log(cumGZ + 1)  + log(linear_rand_ymns + 1) + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)
reg_postas.cus1 <- felm(log(customers_per) ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)
reg_postas.cus2 <- felm(log(customers_per) ~ log(cumGZ + 1) + log(linear_rand_ymns + 1) + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + log(cumGZ + 1):log(newcase_day + 1)| pref+date | 0 | pref, data = postas_rob)

dpostashtml <- stargazer(reg_postas1, reg_postas2, reg_postas.cus1, reg_postas.cus2,
                         title = "TABLE: Robustness check",
                         digits = 3,
                         digits.extra = 0,
                         type = "html",
                         out = "04_analyze/Robustness_econ/output/postas_with_linear_rand.html",
                         add.lines=list(c("Prefecture FE", rep("X",4)),
                                        c("Day FE", rep("X",4))),
                         omit.stat=c("f", "ser"))









