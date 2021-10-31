library(tidyverse)
library(lfe)

# data load ------
weekSIR_rob <- read_csv("03_build/Robustness_check/output/weekSIR_robustness.csv")
weekSIR3 <- read_csv("03_build/Postas/output/weekSIR3.csv")

# reg by type ------------

#covid
covid_22.0 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + log(noftestst.2 + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_22.1 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + log(noftestst.2 + 1) + log(customers_per)| pref+week | 0 | pref, data = weekSIR_rob)
covid_22.2 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + log(noftestst.2 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_22.3 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + log(noftestst.2 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1) + log(susceptible)| pref+week | 0 | pref, data = weekSIR_rob)
covid_22.4 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + log(noftestst.2 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1) + log(cumGZHotel + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_22.5 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + log(noftestst.2 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1) + log(cumGZWinery + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_22.6 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + log(noftestst.2 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1) + log(cumGZHotel + 1) + log(cumGZWinery + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_22.7 <- felm(log(newcaset.2 + 1) ~ log(cumGZFoodHotel + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + log(noftestst.2 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_22.8 <- felm(log(newcaset.2 + 1) ~ log(cumGZFoodHotel + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + log(noftestst.2 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1) + log(cumGZWinery + 1)| pref+week | 0 | pref, data = weekSIR_rob)


intercept22.0 <- tail(getfe(covid_22.0, ef="zm2", se = TRUE), 1)
intercept22.1 <- tail(getfe(covid_22.1, ef="zm2", se = TRUE), 1)
intercept22.2 <- tail(getfe(covid_22.2, ef="zm2", se = TRUE), 1)
intercept22.3 <- tail(getfe(covid_22.3, ef="zm2", se = TRUE), 1)
intercept22.4 <- tail(getfe(covid_22.4, ef="zm2", se = TRUE), 1)
intercept22.5 <- tail(getfe(covid_22.5, ef="zm2", se = TRUE), 1)
intercept22.6 <- tail(getfe(covid_22.6, ef="zm2", se = TRUE), 1)
intercept22.7 <- tail(getfe(covid_22.5, ef="zm2", se = TRUE), 1)
intercept22.8 <- tail(getfe(covid_22.6, ef="zm2", se = TRUE), 1)

prefweek22felmhtml <- stargazer(covid_22.0, covid_22.1, covid_22.2, covid_22.3, covid_22.4, covid_22.5, covid_22.6,
                                dep.var.labels = "log(nofcases + 1)",
                                title = "TABLE: COVID-19 new cases (2 week lag) and GreenZone certification",
                                digits = 3,
                                digits.extra = 0,
                                type = "html",
                                out = "04_analyze/Robustness/output/covid_reg_by_type.html",
                                covariate.labels = c("log(cumGZ + 1)", "log(infectious\\_lag2 + 1)", "emergency", "log(noftestst.2 + 1)", "log(customers\\_per)", "log(avg\\_temp\\_q)", "log(rain + 1)", "log(susceptible)"),
                                add.lines=list(c("Intercept",
                                                 round(intercept22.0[1][[1]], digits = 3),
                                                 round(intercept22.1[1][[1]], digits = 3),
                                                 round(intercept22.2[1][[1]], digits = 3),
                                                 round(intercept22.3[1][[1]], digits = 3),
                                                 round(intercept22.4[1][[1]], digits = 3),
                                                 round(intercept22.5[1][[1]], digits = 3),
                                                 round(intercept22.6[1][[1]], digits = 3)),
                                               c("",
                                                 paste0("(", round(intercept22.0[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept22.1[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept22.2[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept22.3[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept22.4[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept22.5[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept22.6[4][[1]],digits = 3),")")),
                                               c("Prefecture FE", rep("X", 7)),
                                               c("Week FE", rep("X", 7))),
                                omit.stat=c("f", "ser"))
prefweek22.2felmhtml <- stargazer(covid_22.4, covid_22.5, covid_22.6, covid_22.7, covid_22.8,
                                  dep.var.labels = "log(nofcases + 1)",
                                  title = "TABLE: COVID-19 new cases (2 week lag) and GreenZone certification",
                                  digits = 3,
                                  digits.extra = 0,
                                  type = "html",
                                  out = "04_analyze/Robustness/output/covid_reg_by_type2.html",
                                  add.lines=list(c("Intercept",
                                                   round(intercept22.4[1][[1]], digits = 3),
                                                   round(intercept22.5[1][[1]], digits = 3),
                                                   round(intercept22.6[1][[1]], digits = 3),
                                                   round(intercept22.7[1][[1]], digits = 3),
                                                   round(intercept22.8[1][[1]], digits = 3)),
                                                 c("",
                                                   paste0("(", round(intercept22.4[4][[1]],digits = 3),")"),
                                                   paste0("(", round(intercept22.5[4][[1]],digits = 3),")"),
                                                   paste0("(", round(intercept22.6[4][[1]],digits = 3),")"),
                                                   paste0("(", round(intercept22.7[4][[1]],digits = 3),")"),
                                                   paste0("(", round(intercept22.8[4][[1]],digits = 3),")")),
                                                 c("Prefecture FE", rep("X", 5)),
                                                 c("Week FE", rep("X", 5))),
                                  omit.stat=c("f", "ser"))


# reg in different ranges of dates -------
colnames(weekSIR3)[which(colnames(weekSIR3) == "susceptable")] <- "susceptible"

# covid analysis since 2020/07/17 ------
covid_5.01 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency| pref+week | 0 | pref, data = weekSIR3%>% filter(week >= 18460))
covid_5.11 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + emergency| pref+week | 0 | pref, data = weekSIR3%>% filter(week >= 18460))
covid_5.21 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + resview + emergency| pref+week | 0 | pref, data = weekSIR3%>% filter(week >= 18460))
covid_5.31 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(mobility) + resview + emergency| pref+week | 0 | pref, data = weekSIR3%>% filter(week >= 18460))
covid_5.41 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency| pref+week | 0 | pref, data = weekSIR3%>% filter(week >= 18460))
covid_5.51 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency +log(noftestst.2 + 1)| pref+week | 0 | pref, data = weekSIR3%>% filter(week >= 18460))


intercept5.01 <- tail(getfe(covid_5.01, ef="zm2", se = TRUE), 1)
intercept5.11 <- tail(getfe(covid_5.11, ef="zm2", se = TRUE), 1)
intercept5.21 <- tail(getfe(covid_5.21, ef="zm2", se = TRUE), 1)
intercept5.31 <- tail(getfe(covid_5.31, ef="zm2", se = TRUE), 1)
intercept5.41 <- tail(getfe(covid_5.41, ef="zm2", se = TRUE), 1)
intercept5.51 <- tail(getfe(covid_5.51, ef="zm2", se = TRUE), 1)


prefweek2felm0717html <- stargazer(covid_5.01, covid_5.11, covid_5.21, covid_5.31, covid_5.41, covid_5.51,
                                   dep.var.labels = "log(nofcases + 1)",
                                   title = "TABLE: COVID-19 new cases (2 week lag) and GreenZone certification (Since 2020/07/17)",
                                   digits = 3,
                                   digits.extra = 0,
                                   type = "html",
                                   out = "04_analyze/Robustness/output/felm_covid2_SIR_pref_week_sinceGZ.html",
                                   add.lines=list(c("Intercept",
                                                    round(intercept5.01[1][[1]], digits = 3),
                                                    round(intercept5.11[1][[1]], digits = 3),
                                                    round(intercept5.21[1][[1]], digits = 3),
                                                    round(intercept5.31[1][[1]], digits = 3),
                                                    round(intercept5.41[1][[1]], digits = 3),
                                                    round(intercept5.51[1][[1]], digits = 3)),
                                                  c("",
                                                    paste0("(", round(intercept5.01[4][[1]],digits = 3),")"),
                                                    paste0("(", round(intercept5.11[4][[1]],digits = 3),")"),
                                                    paste0("(", round(intercept5.21[4][[1]],digits = 3),")"),
                                                    paste0("(", round(intercept5.31[4][[1]],digits = 3),")"),
                                                    paste0("(", round(intercept5.41[4][[1]],digits = 3),")"),
                                                    paste0("(", round(intercept5.51[4][[1]],digits = 3),")")),
                                                  c("Prefecture FE", "X", "X", "X", "X", "X", "X", "X"),
                                                  c("Week FE", "X", "X", "X", "X", "X", "X", "X")),
                                   omit.stat=c("f", "ser"))


# covid analysis since 2020/11/01 ------
covid_5.02 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency| pref+week | 0 | pref, data = weekSIR3%>% filter(week >= 18567))
covid_5.12 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + emergency| pref+week | 0 | pref, data = weekSIR3%>% filter(week >= 18567))
covid_5.22 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + resview + emergency| pref+week | 0 | pref, data = weekSIR3%>% filter(week >= 18567))
covid_5.32 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(mobility) + resview + emergency| pref+week | 0 | pref, data = weekSIR3%>% filter(week >= 18567))
covid_5.42 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency| pref+week | 0 | pref, data = weekSIR3%>% filter(week >= 18567))
covid_5.52 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency +log(noftestst.2 + 1)| pref+week | 0 | pref, data = weekSIR3%>% filter(week >= 18567))


intercept5.02 <- tail(getfe(covid_5.02, ef="zm2", se = TRUE), 1)
intercept5.12 <- tail(getfe(covid_5.12, ef="zm2", se = TRUE), 1)
intercept5.22 <- tail(getfe(covid_5.22, ef="zm2", se = TRUE), 1)
intercept5.32 <- tail(getfe(covid_5.32, ef="zm2", se = TRUE), 1)
intercept5.42 <- tail(getfe(covid_5.42, ef="zm2", se = TRUE), 1)
intercept5.52 <- tail(getfe(covid_5.52, ef="zm2", se = TRUE), 1)


prefweek2felm1101html <- stargazer(covid_5.02, covid_5.12, covid_5.22, covid_5.32, covid_5.42, covid_5.52,
                                   dep.var.labels = "log(nofcases + 1)",
                                   title = "TABLE: COVID-19 new cases (2 week lag) and GreenZone certification (Since 2020/11/01)",
                                   digits = 3,
                                   digits.extra = 0,
                                   type = "html",
                                   out = "04_analyze/Robustness/output/felm_covid2_SIR_pref_week_sinceNov.html",
                                   add.lines=list(c("Intercept",
                                                    round(intercept5.02[1][[1]], digits = 3),
                                                    round(intercept5.12[1][[1]], digits = 3),
                                                    round(intercept5.22[1][[1]], digits = 3),
                                                    round(intercept5.32[1][[1]], digits = 3),
                                                    round(intercept5.42[1][[1]], digits = 3),
                                                    round(intercept5.52[1][[1]], digits = 3)),
                                                  c("",
                                                    paste0("(", round(intercept5.02[4][[1]],digits = 3),")"),
                                                    paste0("(", round(intercept5.12[4][[1]],digits = 3),")"),
                                                    paste0("(", round(intercept5.22[4][[1]],digits = 3),")"),
                                                    paste0("(", round(intercept5.32[4][[1]],digits = 3),")"),
                                                    paste0("(", round(intercept5.42[4][[1]],digits = 3),")"),
                                                    paste0("(", round(intercept5.52[4][[1]],digits = 3),")")),
                                                  c("Prefecture FE", "X", "X", "X", "X", "X", "X", "X"),
                                                  c("Week FE", "X", "X", "X", "X", "X", "X", "X")),
                                   omit.stat=c("f", "ser"))




# covid analysis until 2021/02/01 ------
covid_5.03 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency| pref+week | 0 | pref, data = weekSIR3%>% filter(week <= 18628))
covid_5.13 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + emergency| pref+week | 0 | pref, data = weekSIR3%>% filter(week <= 18628))
covid_5.23 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + resview + emergency| pref+week | 0 | pref, data = weekSIR3%>% filter(week <= 18628))
covid_5.33 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(mobility) + resview + emergency| pref+week | 0 | pref, data = weekSIR3%>% filter(week <= 18628))
covid_5.43 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency| pref+week | 0 | pref, data = weekSIR3%>% filter(week <= 18628))
covid_5.53 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency +log(noftestst.2 + 1)| pref+week | 0 | pref, data = weekSIR3%>% filter(week <= 18628))


intercept5.03 <- tail(getfe(covid_5.03, ef="zm2", se = TRUE), 1)
intercept5.13 <- tail(getfe(covid_5.13, ef="zm2", se = TRUE), 1)
intercept5.23 <- tail(getfe(covid_5.23, ef="zm2", se = TRUE), 1)
intercept5.33 <- tail(getfe(covid_5.33, ef="zm2", se = TRUE), 1)
intercept5.43 <- tail(getfe(covid_5.43, ef="zm2", se = TRUE), 1)
intercept5.53 <- tail(getfe(covid_5.53, ef="zm2", se = TRUE), 1)


prefweek2felm0201html <- stargazer(covid_5.03, covid_5.13, covid_5.23, covid_5.33, covid_5.43, covid_5.53,
                                   dep.var.labels = "log(nofcases + 1)",
                                   title = "TABLE: COVID-19 new cases (2 week lag) and GreenZone certification (Until 2021/01/01)",
                                   digits = 3,
                                   digits.extra = 0,
                                   type = "html",
                                   out = "04_analyze/Robustness/output/felm_covid2_SIR_pref_week_untilJan.html",
                                   add.lines=list(c("Intercept",
                                                    round(intercept5.03[1][[1]], digits = 3),
                                                    round(intercept5.13[1][[1]], digits = 3),
                                                    round(intercept5.23[1][[1]], digits = 3),
                                                    round(intercept5.33[1][[1]], digits = 3),
                                                    round(intercept5.43[1][[1]], digits = 3),
                                                    round(intercept5.53[1][[1]], digits = 3)),
                                                  c("",
                                                    paste0("(", round(intercept5.03[4][[1]],digits = 3),")"),
                                                    paste0("(", round(intercept5.13[4][[1]],digits = 3),")"),
                                                    paste0("(", round(intercept5.23[4][[1]],digits = 3),")"),
                                                    paste0("(", round(intercept5.33[4][[1]],digits = 3),")"),
                                                    paste0("(", round(intercept5.43[4][[1]],digits = 3),")"),
                                                    paste0("(", round(intercept5.53[4][[1]],digits = 3),")")),
                                                  c("Prefecture FE", "X", "X", "X", "X", "X", "X", "X"),
                                                  c("Week FE", "X", "X", "X", "X", "X", "X", "X")),
                                   omit.stat=c("f", "ser"))








