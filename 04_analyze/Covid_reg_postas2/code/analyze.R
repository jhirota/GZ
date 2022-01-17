library(tidyverse)
library(lfe)
library(stargazer)

#data load ----------
weekSIR_rob <- read_csv(here::here("03_build/Robustness_check/output/weekSIR_robustness.csv")) %>% 
  rename(susceptible = susceptable)


# lag2
covid_50.0 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + log(susceptible + 1) + emergency + log(noftestst.2 + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_50.1 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + log(susceptible + 1) + emergency + log(noftestst.2 + 1) + log(customers_per)| pref+week | 0 | pref, data = weekSIR_rob)
covid_50.2 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + log(susceptible + 1) + emergency + log(noftestst.2 + 1) + log(customers_per) + log(avg_temp) + log(rain + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_50.3 <- felm(log(newcaset.2 + 1) ~ log(cumGZFoodHotel + 1)  + log(agrgt_potecovid_lag2 + 1) + log(susceptible + 1) + emergency + log(noftestst.2 + 1) + log(customers_per) + log(avg_temp) + log(rain + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_50.4 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + log(susceptible + 1) + emergency + log(noftestst.2 + 1) + log(customers_per) + log(avg_temp) + log(rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob)


# intercept50.0 <- tail(getfe(covid_50.0, ef="zm2", se = TRUE), 1)
# intercept50.1 <- tail(getfe(covid_50.1, ef="zm2", se = TRUE), 1)
# intercept50.2 <- tail(getfe(covid_50.2, ef="zm2", se = TRUE), 1)
# intercept50.3 <- tail(getfe(covid_50.3, ef="zm2", se = TRUE), 1)
# intercept50.4 <- tail(getfe(covid_50.4, ef="zm2", se = TRUE), 1)


# prefweek50felmhtml <- stargazer(covid_50.0, covid_50.1, covid_50.2, covid_50.3, covid_50.4,
#                                 dep.var.labels = "log(nofcases + 1)",
#                                 title = "The COVID-19 new infection cases (2 week lag) and the Green Zone certification",
#                                 digits = 3,
#                                 digits.extra = 0,
#                                 out = c("04_analyze/Covid_reg_postas2/output/covid_reg.tex", "05_report/tables_tex/output/covid_reg.tex"),
#                                 covariate.labels = c("log(cumGZ + 1)", "log(cumGZ(Hotel included) + 1)",
#                                                      "log(infectious\\_lag2 + 1)", "emergency", "log(noftestst.2 + 1)",
#                                                      "log(customers\\_per)", "log(avg\\_temp\\_q)", "log(rain + 1)",
#                                                      "school\\_closure\\_dummy", "gathering\\_restriction\\_dummy"),
#                                 add.lines=list(c("Intercept",
#                                                  round(intercept50.0[1][[1]], digits = 3),
#                                                  round(intercept50.1[1][[1]], digits = 3),
#                                                  round(intercept50.2[1][[1]], digits = 3),
#                                                  round(intercept50.3[1][[1]], digits = 3),
#                                                  round(intercept50.4[1][[1]], digits = 3)),
#                                                c("",
#                                                  paste0("(", round(intercept50.0[4][[1]],digits = 3),")"),
#                                                  paste0("(", round(intercept50.1[4][[1]],digits = 3),")"),
#                                                  paste0("(", round(intercept50.2[4][[1]],digits = 3),")"),
#                                                  paste0("(", round(intercept50.3[4][[1]],digits = 3),")"),
#                                                  paste0("(", round(intercept50.4[4][[1]],digits = 3),")")),
#                                                c("Prefecture FE", rep("X", 5)),
#                                                c("Week FE", rep("X", 5))),
#                                 omit.stat=c("f", "ser"))

# lag1
covid_51.0 <- felm(log(newcaset.1 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1) + log(susceptible + 1) + emergency + log(noftestst.1 + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_51.1 <- felm(log(newcaset.1 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1) + log(susceptible + 1) + emergency + log(noftestst.1 + 1) + log(customers_per)| pref+week | 0 | pref, data = weekSIR_rob)
covid_51.2 <- felm(log(newcaset.1 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1) + log(susceptible + 1) + emergency + log(noftestst.1 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_51.3 <- felm(log(newcaset.1 + 1) ~ log(cumGZFoodHotel + 1)  + log(agrgt_potecovid_lag1 + 1) + log(susceptible + 1) + emergency + log(noftestst.1 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_51.4 <- felm(log(newcaset.1 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1) + log(susceptible + 1) + emergency + log(noftestst.1 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob)

intercept51.0 <- tail(getfe(covid_51.0, ef="zm2", se = TRUE), 1)
intercept51.1 <- tail(getfe(covid_51.1, ef="zm2", se = TRUE), 1)
intercept51.2 <- tail(getfe(covid_51.2, ef="zm2", se = TRUE), 1)
intercept51.3 <- tail(getfe(covid_51.3, ef="zm2", se = TRUE), 1)


# prefweek51felmhtml <- stargazer(covid_51.0, covid_51.1, covid_51.2, covid_51.3,
#                                 dep.var.labels = "log(nofcases + 1)",
#                                 title = "TABLE: COVID-19 new cases (1 week lag) and the Green Zone certification",
#                                 digits = 3,
#                                 digits.extra = 0,
#                                 type = "html",
#                                 out = c("04_analyze/Covid_reg_postas2/output/covid_reg_lag1.tex", "05_report/tables_tex/output/covid_reg_lag1.tex"),
#                                 covariate.labels = c("log(cumGZ + 1)", "log(infectious\\_lag1 + 1)", "emergency", "log(noftestst.1 + 1)", "log(customers\\_per)", "log(avg\\_temp\\_q)", "log(rain + 1)", "log(susceptible)"),
#                                 add.lines=list(c("Intercept",
#                                                  round(intercept51.0[1][[1]], digits = 3),
#                                                  round(intercept51.1[1][[1]], digits = 3),
#                                                  round(intercept51.2[1][[1]], digits = 3),
#                                                  round(intercept51.3[1][[1]], digits = 3)),
#                                                c("",
#                                                  paste0("(", round(intercept51.0[4][[1]],digits = 3),")"),
#                                                  paste0("(", round(intercept51.1[4][[1]],digits = 3),")"),
#                                                  paste0("(", round(intercept51.2[4][[1]],digits = 3),")"),
#                                                  paste0("(", round(intercept51.3[4][[1]],digits = 3),")")),
#                                                c("Prefecture FE", rep("X", 4)),
#                                                c("Week FE", rep("X", 4))),
#                                 omit.stat=c("f", "ser"))