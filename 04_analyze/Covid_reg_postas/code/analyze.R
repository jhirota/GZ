library(tidyverse)
library(lfe)

# data load ------
weekSIR3 <- read_csv("03_build/Postas/output/weekSIR3.csv")

# covid analysis with postas -------

colnames(weekSIR3)[which(names(weekSIR3) == "susceptable")] <- "susceptible"

covid_8.0 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + lsales + lcustomers + emergency| pref+week | 0 | pref, data = weekSIR3)
covid_8.1 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + lsales + lcustomers + emergency| pref+week | 0 | pref, data = weekSIR3)
covid_8.2 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + lsales + lcustomers + resview + emergency| pref+week | 0 | pref, data = weekSIR3)
covid_8.3 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + lsales + lcustomers + log(mobility) + resview + emergency| pref+week | 0 | pref, data = weekSIR3)
covid_8.4 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + lsales + lcustomers + log(avg_temp_q) + log(rain + 1) + resview + emergency| pref+week | 0 | pref, data = weekSIR3)
covid_8.5 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + lsales + lcustomers + log(avg_temp_q) + log(rain + 1) + resview + emergency +log(noftestst.2 + 1)| pref+week | 0 | pref, data = weekSIR3)


intercept8.0 <- tail(getfe(covid_8.0, ef="zm2", se = TRUE), 1)
intercept8.1 <- tail(getfe(covid_8.1, ef="zm2", se = TRUE), 1)
intercept8.2 <- tail(getfe(covid_8.2, ef="zm2", se = TRUE), 1)
intercept8.3 <- tail(getfe(covid_8.3, ef="zm2", se = TRUE), 1)
intercept8.4 <- tail(getfe(covid_8.4, ef="zm2", se = TRUE), 1)
intercept8.5 <- tail(getfe(covid_8.5, ef="zm2", se = TRUE), 1)


prefweek8felmhtml <- stargazer(covid_8.0, covid_8.1, covid_8.2, covid_8.3, covid_8.4, covid_8.5,
                               dep.var.labels = "log(nofcases + 1)",
                               title = "TABLE: COVID-19 new cases (2 week lag) and GreenZone certification",
                               digits = 3,
                               digits.extra = 0,
                               type = "html",
                               out = "04_analyze/Covid_reg_postas/output/covid_reg_postas.html",
                               add.lines=list(c("Intercept",
                                                round(intercept8.0[1][[1]], digits = 3),
                                                round(intercept8.1[1][[1]], digits = 3),
                                                round(intercept8.2[1][[1]], digits = 3),
                                                round(intercept8.3[1][[1]], digits = 3),
                                                round(intercept8.4[1][[1]], digits = 3),
                                                round(intercept8.5[1][[1]], digits = 3)),
                                              c("",
                                                paste0("(", round(intercept8.0[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept8.1[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept8.2[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept8.3[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept8.4[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept8.5[4][[1]],digits = 3),")")),
                                              c("Prefecture FE", "X", "X", "X", "X", "X", "X", "X"),
                                              c("Week FE", "X", "X", "X", "X", "X", "X", "X")),
                               omit.stat=c("f", "ser"))


# Without susceptible 
covid_9.0 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency| week | 0 | pref, data = weekSIR3)
covid_9.1 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency| pref+week | 0 | pref, data = weekSIR3)
covid_9.2 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + lcustomers| week | 0 | pref, data = weekSIR3)
covid_9.3 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + lcustomers| pref+week | 0 | pref, data = weekSIR3)
covid_9.4 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + lcustomers + log(avg_temp_q) + log(rain + 1) | week | 0 | pref, data = weekSIR3)
covid_9.5 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + lcustomers + log(avg_temp_q) + log(rain + 1) | pref+week | 0 | pref, data = weekSIR3)


intercept9.0 <- tail(getfe(covid_9.0, ef="zm2", se = TRUE), 1)
intercept9.1 <- tail(getfe(covid_9.1, ef="zm2", se = TRUE), 1)
intercept9.2 <- tail(getfe(covid_9.2, ef="zm2", se = TRUE), 1)
intercept9.3 <- tail(getfe(covid_9.3, ef="zm2", se = TRUE), 1)
intercept9.4 <- tail(getfe(covid_9.4, ef="zm2", se = TRUE), 1)
intercept9.5 <- tail(getfe(covid_9.5, ef="zm2", se = TRUE), 1)


prefweek9felmhtml <- stargazer(covid_9.0, covid_9.1, covid_9.2, covid_9.3, covid_9.4, covid_9.5,
                               dep.var.labels = "log(nofcases + 1)",
                               title = "TABLE: COVID-19 new cases (2 week lag) and GreenZone certification",
                               digits = 3,
                               digits.extra = 0,
                               type = "html",
                               out = "04_analyze/Covid_reg_postas/output/covid_reg_postas_nosus.html",
                               add.lines=list(c("Intercept",
                                                round(intercept9.0[1][[1]], digits = 3),
                                                round(intercept9.1[1][[1]], digits = 3),
                                                round(intercept9.2[1][[1]], digits = 3),
                                                round(intercept9.3[1][[1]], digits = 3),
                                                round(intercept9.4[1][[1]], digits = 3),
                                                round(intercept9.5[1][[1]], digits = 3)),
                                              c("",
                                                paste0("(", round(intercept9.0[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept9.1[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept9.2[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept9.3[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept9.4[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept9.5[4][[1]],digits = 3),")")),
                                              c("Prefecture FE", "", "X", "", "X", "", "X"),
                                              c("Week FE", "X", "X", "X", "X", "X", "X")),
                               omit.stat=c("f", "ser"))



# covid analysis with postas (for report on the web)------
colnames(weekSIR3)[which(names(weekSIR3) == "susceptable")] <- "susceptible"

# lag2
covid_20.0 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + log(noftestst.2 + 1)| pref+week | 0 | pref, data = weekSIR3)
covid_20.1 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + log(noftestst.2 + 1) + log(customers_per)| pref+week | 0 | pref, data = weekSIR3)
covid_20.2 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + log(noftestst.2 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1)| pref+week | 0 | pref, data = weekSIR3)
covid_20.3 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + log(noftestst.2 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1) + log(susceptible)| pref+week | 0 | pref, data = weekSIR3)

intercept20.0 <- tail(getfe(covid_20.0, ef="zm2", se = TRUE), 1)
intercept20.1 <- tail(getfe(covid_20.1, ef="zm2", se = TRUE), 1)
intercept20.2 <- tail(getfe(covid_20.2, ef="zm2", se = TRUE), 1)
intercept20.3 <- tail(getfe(covid_20.3, ef="zm2", se = TRUE), 1)


prefweek20felmhtml <- stargazer(covid_20.0, covid_20.1, covid_20.2, covid_20.3,
                                dep.var.labels = "log(nofcases + 1)",
                                title = "TABLE: COVID-19 new cases (2 week lag) and GreenZone certification",
                                digits = 3,
                                digits.extra = 0,
                                type = "html",
                                out = "04_analyze/Covid_reg_postas/output/covid_reg_postas2.html",
                                covariate.labels = c("log(cumGZ + 1)", "log(infectious\\_lag2 + 1)", "emergency", "log(noftestst.2 + 1)", "log(customers\\_per)", "log(avg\\_temp\\_q)", "log(rain + 1)", "log(susceptible)"),
                                add.lines=list(c("Intercept",
                                                 round(intercept20.0[1][[1]], digits = 3),
                                                 round(intercept20.1[1][[1]], digits = 3),
                                                 round(intercept20.2[1][[1]], digits = 3),
                                                 round(intercept20.3[1][[1]], digits = 3)),
                                               c("",
                                                 paste0("(", round(intercept20.0[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept20.1[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept20.2[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept20.3[4][[1]],digits = 3),")")),
                                               c("Prefecture FE", rep("X", 4)),
                                               c("Week FE", rep("X", 4))),
                                omit.stat=c("f", "ser"))

# lag1
covid_21.0 <- felm(log(newcaset.1 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1) + emergency + log(noftestst.1 + 1)| pref+week | 0 | pref, data = weekSIR3)
covid_21.1 <- felm(log(newcaset.1 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1) + emergency + log(noftestst.1 + 1) + log(customers_per)| pref+week | 0 | pref, data = weekSIR3)
covid_21.2 <- felm(log(newcaset.1 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1) + emergency + log(noftestst.1 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1)| pref+week | 0 | pref, data = weekSIR3)
covid_21.3 <- felm(log(newcaset.1 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1) + emergency + log(noftestst.1 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1) + log(susceptible)| pref+week | 0 | pref, data = weekSIR3)

intercept21.0 <- tail(getfe(covid_21.0, ef="zm2", se = TRUE), 1)
intercept21.1 <- tail(getfe(covid_21.1, ef="zm2", se = TRUE), 1)
intercept21.2 <- tail(getfe(covid_21.2, ef="zm2", se = TRUE), 1)
intercept21.3 <- tail(getfe(covid_21.3, ef="zm2", se = TRUE), 1)


prefweek21felmhtml <- stargazer(covid_21.0, covid_21.1, covid_21.2, covid_21.3,
                                dep.var.labels = "log(nofcases + 1)",
                                title = "TABLE: COVID-19 new cases (1 week lag) and GreenZone certification",
                                digits = 3,
                                digits.extra = 0,
                                type = "html",
                                out = "04_analyze/Covid_reg_postas/output/covid_reg_postas2_lag1.html",
                                covariate.labels = c("log(cumGZ + 1)", "log(infectious\\_lag1 + 1)", "emergency", "log(noftestst.1 + 1)", "log(customers\\_per)", "log(avg\\_temp\\_q)", "log(rain + 1)", "log(susceptible)"),
                                add.lines=list(c("Intercept",
                                                 round(intercept21.0[1][[1]], digits = 3),
                                                 round(intercept21.1[1][[1]], digits = 3),
                                                 round(intercept21.2[1][[1]], digits = 3),
                                                 round(intercept21.3[1][[1]], digits = 3)),
                                               c("",
                                                 paste0("(", round(intercept21.0[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept21.1[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept21.2[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept21.3[4][[1]],digits = 3),")")),
                                               c("Prefecture FE", rep("X", 4)),
                                               c("Week FE", rep("X", 4))),
                                omit.stat=c("f", "ser"))
