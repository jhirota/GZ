library(tidyverse)
library(lfe)
library(stargazer)

#data load -------
weekSIR2 <- read_csv("03_build/weekSIR/output/weekSIR2.csv")

# COVID mean ------------
#covid mean of yamanashi
covid_yama_mean <- weekSIR2 %>% 
  filter(pref == "Yamanashi") %>% 
  summarize(mean = mean(newcaseday),
            median = median(newcaseday))
covid_yama_mean[1,1]
#covid mean of yamanashi
covid_nonyama_mean <- weekSIR2 %>% 
  filter(pref != "Yamanashi") %>% 
  summarize(mean1 = mean(newcaseday),
            median1 = median(newcaseday))

#regression 1week lag felm---------------
covid_res1.0 <- felm(log(newcaset.1 + 1) ~ 1 + log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1) + emergency | pref+ week | 0 | pref, data = weekSIR2)
covid_res1 <- felm(log(newcaset.1 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1)  + emergency | pref_dummy + week_dummy | 0 | pref, data = weekSIR2)
covid_res1.1 <- felm(log(newcaset.1 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + resview + emergency| pref_dummy + week_dummy | 0 | pref, data = weekSIR2)
covid_res1.2 <- felm(log(newcaset.1 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(mobility) + resview + emergency| pref_dummy + week_dummy | 0 | pref, data = weekSIR2)
covid_res1.3 <- felm(log(newcaset.1 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q) + log(rain +1) + resview + emergency| pref_dummy + week_dummy | 0 | pref, data = weekSIR2)
covid_res1.4 <- felm(log(newcaset.1 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q) + log(rain +1) + resview + emergency + log(noftestst.1 + 1)| pref_dummy + week_dummy | 0 | pref, data = weekSIR2)
covid_res1.5 <- felm(log(newcaset.1 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q) + log(rain +1) + resview + emergency + log(noftests + 1)| pref_dummy + week_dummy | 0 | pref, data = weekSIR2)

getfe(covid_res1.0, ef="zm2")[nrow(getfe(covid_res1.0, ef="zm2")),1]

stargazer(covid_res1.0, covid_res1, covid_res1.1, covid_res1.2, covid_res1.3,covid_res1.4, type ="text")

prefweek1html <- stargazer(covid_res1.0, covid_res1, covid_res1.1, covid_res1.2, covid_res1.3, covid_res1.4,
                           dep.var.labels = "log(nofcases + 1)",
                           title = "TABLE1: COVID new cases (1week lag) and GreenZone certification",
                           digits = 3,
                           digits.extra = 0,
                           type = "html",
                           out = "04_analyze/weekSIR/output/covid1_SIR_pref_week.html",
                           add.lines=list(c("# of COVID Yamanashi mean", "", "",round(covid_yama_mean[1,1], digits = 3)),
                                          c("# of COVID Control mean", "", "",round(covid_nonyama_mean[1,1], digits = 3)),
                                          c("Prefecture FE", "X", "X", "X", "X", "X", "X"), c("Week FE", "X", "X", "X", "X", "X", "X")),
                           omit.stat=c("f", "ser"))

#regression 1week lag plm-----------------


covid_1.0 <- plm(log(newcaset.1 + 1) ~ 1 + log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1) + emergency,  data = weekSIR2, effect = "twoways")
covid_1.01 <- plm(log(newcaset.1 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency, data = weekSIR2, effect = "twoways")
covid_1 <- plm(log(newcaset.1 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1)  + emergency, data = weekSIR2, effect = "twoways")
covid_1.1 <- plm(log(newcaset.1 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + resview + emergency,  data = weekSIR2, effect = "twoways")
covid_1.2 <- plm(log(newcaset.1 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(mobility) + resview + emergency,  data = weekSIR2, effect = "twoways")
covid_1.3 <- plm(log(newcaset.1 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency,  data = weekSIR2, effect = "twoways")
covid_1.4 <- plm(log(newcaset.1 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency + log(noftestst.1 + 1),  data = weekSIR2, effect = "twoways")
covid_1.5 <- plm(log(newcaset.1 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency + log(noftests + 1),  data = weekSIR2, effect = "twoways")

summary(covid_1.4)

stargazer(covid_1.0,covid_1, covid_1.1, covid_1.2, covid_1.3 ,type = "text")

intercept1.0 <- within_intercept(covid_1.0)
intercept1.01 <- within_intercept(covid_1.01)
intercept1 <- within_intercept(covid_1)
intercept1.1 <- within_intercept(covid_1.1)
intercept1.2 <- within_intercept(covid_1.2)
intercept1.3 <- within_intercept(covid_1.3)
intercept1.4 <- within_intercept(covid_1.4)
intercept1.0["(overall_intercept)"]

prefweek1plmhtml <- stargazer(covid_1.0,covid_1.01,covid_1, covid_1.1, covid_1.2, covid_1.3,covid_1.4,
                              dep.var.labels = "log(nofcases + 1)",
                              title = "TABLE1: COVID new cases (1week lag) and GreenZone certification (plm)",
                              digits = 3,
                              digits.extra = 0,
                              type = "html",
                              out = "04_analyze/weekSIR/output/plm_covid1_SIR_pref_week.html",
                              add.lines=list(c("Intercept", round(intercept1.0["(overall_intercept)"], digits = 3), round(intercept1.01["(overall_intercept)"], digits = 3), round(intercept1["(overall_intercept)"], digits = 3),round(intercept1.1["(overall_intercept)"], digits = 3),round(intercept1.2["(overall_intercept)"], digits = 3),round(intercept1.3["(overall_intercept)"], digits = 3),round(intercept1.4["(overall_intercept)"], digits = 3)),
                                             c("# of COVID Yamanashi mean", "", "",round(covid_yama_mean[1,1], digits = 3)),
                                             c("# of COVID Control mean", "", "",round(covid_nonyama_mean[1,1], digits = 3)),
                                             c("Prefecture FE", "X", "X", "X", "X", "X", "X", "X"), c("Week FE", "X", "X", "X", "X", "X","X", "X")),
                              omit.stat=c("f", "ser"))

#regression 2week lag felm-------
covid_res2.0 <- felm(log(newcaset.2 + 1) ~ 1 + log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency | pref_dummy + week_dummy | 0 | pref, data = weekSIR2)
covid_res2 <- felm(log(newcaset.2 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag2 + 1)  + emergency | pref_dummy + week_dummy | 0 | pref, data = weekSIR2)
covid_res2.1 <- felm(log(newcaset.2 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag2 + 1) + resview + emergency| pref_dummy + week_dummy | 0 | pref, data = weekSIR2)
covid_res2.2 <- felm(log(newcaset.2 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag2 + 1) + log(mobility) + resview + emergency| pref_dummy + week_dummy | 0 | pref, data = weekSIR2)
covid_res2.3 <- felm(log(newcaset.2 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency| pref_dummy + week_dummy | 0 | pref, data = weekSIR2)
covid_res2.4 <- felm(log(newcaset.2 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency + log(noftestst.2 + 1)| pref_dummy + week_dummy | 0 | pref, data = weekSIR2)
covid_res2.5 <- felm(log(newcaset.2 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency + log(noftests + 1)| pref_dummy + week_dummy | 0 | pref, data = weekSIR2)


stargazer(covid_res2.0, covid_res2, covid_res2.1, covid_res2.2, covid_res2.3,covid_res2.4, type ="text")

prefweek2html <- stargazer(covid_res2.0, covid_res2, covid_res2.1, covid_res2.2, covid_res2.3,covid_res2.4,
                           dep.var.labels = "log(nofcases + 1)",
                           title = "TABLE1: COVID new cases (2week lag) and GreenZone certification",
                           digits = 3,
                           digits.extra = 0,
                           type = "html",
                           out = "04_analyze/weekSIR/output/covid2_SIR_pref_week.html",
                           add.lines=list(c("# of COVID Yamanashi mean", "", "",round(covid_yama_mean[1,1], digits = 3)),
                                          c("# of COVID Control mean", "", "",round(covid_nonyama_mean[1,1], digits = 3)),
                                          c("Prefecture FE", "X", "X", "X", "X", "X", "X"), c("Week FE", "X", "X", "X", "X", "X", "X")),
                           omit.stat=c("f", "ser"))

#regression 2week lag plm-----------------

covid_2.0 <- plm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1) + emergency,  data = weekSIR2, effect = "twoways")
covid_2.01 <- plm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency, data = weekSIR2, effect = "twoways")
covid_2 <- plm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1)  + emergency, data = weekSIR2, effect = "twoways")
covid_2.1 <- plm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + resview + emergency, , data = weekSIR2, effect = "twoways")
covid_2.2 <- plm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(mobility) + resview + emergency,  data = weekSIR2, effect = "twoways")
covid_2.3 <- plm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency, data = weekSIR2, effect = "twoways")
covid_2.4 <- plm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency +log(noftestst.2 + 1), data = weekSIR2, effect = "twoways")
covid_2.5 <- plm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency +log(noftests + 1), data = weekSIR2, effect = "twoways")


intercept2.0 <- within_intercept(covid_2.0)
intercept2.01 <- within_intercept(covid_2.01)
intercept2 <- within_intercept(covid_2)
intercept2.1 <- within_intercept(covid_2.1)
intercept2.2 <- within_intercept(covid_2.2)
intercept2.3 <- within_intercept(covid_2.3)
intercept2.4 <- within_intercept(covid_2.4)
intercept2.0["(overall_intercept)"]

stargazer(covid_2.0, covid_2.01, covid_2, covid_2.1, covid_2.2, covid_2.3 ,type = "text")

prefweek2plmhtml <- stargazer(covid_2.0, covid_2.01, covid_2, covid_2.1, covid_2.2, covid_2.3,covid_2.4,
                              dep.var.labels = "log(nofcases + 1)",
                              title = "TABLE1: COVID new cases (2week lag) and GreenZone certification (plm)",
                              digits = 3,
                              digits.extra = 0,
                              type = "html",
                              out = "04_analyze/weekSIR/output/plm_covid2_SIR_pref_week.html",
                              add.lines=list(c("Intercept", round(intercept2.0["(overall_intercept)"], digits = 3), round(intercept2.01["(overall_intercept)"], digits = 3), round(intercept2["(overall_intercept)"], digits = 3),round(intercept2.1["(overall_intercept)"], digits = 3),round(intercept2.2["(overall_intercept)"], digits = 3),round(intercept2.3["(overall_intercept)"], digits = 3),round(intercept2.4["(overall_intercept)"], digits = 3)),
                                             c("# of COVID Yamanashi mean", "", "",round(covid_yama_mean[1,1], digits = 3)),
                                             c("# of COVID Control mean", "", "",round(covid_nonyama_mean[1,1], digits = 3)),
                                             c("Prefecture FE", "X", "X", "X", "X", "X", "X", "X"), c("Week FE", "X", "X", "X", "X", "X", "X", "X")),
                              omit.stat=c("f", "ser"))

#mobility　resview mean---------------------------------------
#mobility mean of yamanashi
resview_yama_mean <- weekSIR2 %>% 
  filter(pref == "Yamanashi") %>% 
  summarize(mean = mean(resview))
resview_yama_mean[1,1]
#mobility mean of yamanashi
resview_nonyama_mean <- weekSIR2 %>% 
  filter(pref != "Yamanashi") %>% 
  summarize(mean1 = mean(resview))

#regression V-RESAS restaurant view-----------
resview1 <- plm(resview ~ log(cumGZ + 1)  + newcaseday + emergency,  data = weekSIR2, model = "within")
resview2 <- plm(resview ~ log(cumGZ + 1)  + newcaseday + emergency + avg_temp_q + rain,  data = weekSIR2, model = "within")
resview3 <- plm(resview ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp_q + rain,  data = weekSIR2, model = "within")
resview4 <- plm(resview ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp_q + rain + log(cumGZ + 1):emergency + log(cumGZ + 1):log(newcaseday + 1),  data = weekSIR2, model = "within", effect = "twoways")


resviewplm2html <- stargazer(resview1, resview2,resview3,resview4,
                             dep.var.labels = "Restaurant View (change rate from the same week of 2019) ",
                             title = "TABLE: Restaurant View and GreenZone certification (plm)",
                             digits = 3,
                             digits.extra = 0,
                             type = "html",
                             out = "04_analyze/weekSIR/output/plm_resview2_pref_week.html",
                             add.lines=list(c("Restraurant View Yamanashi mean", "", round(resview_yama_mean[1,1], digits = 3)),
                                            c("Restraurant View Control mean", "",round(resview_nonyama_mean[1,1], digits = 3)),
                                            c("Prefecture FE", "X", "X", "X", "X"), c("Week FE", "X", "X","X", "X")),
                             omit.stat=c("f", "ser"))


# covid more simple? model -----
colnames(weekSIR2)[which(colnames(weekSIR2) == "susceptable")] <- "susceptible"

covid_5.0 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency| pref+week | 0 | pref, data = weekSIR2)
covid_5.1 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + emergency| pref+week | 0 | pref, data = weekSIR2)
covid_5.2 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + resview + emergency| pref+week | 0 | pref, data = weekSIR2)
covid_5.3 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(mobility) + resview + emergency| pref+week | 0 | pref, data = weekSIR2)
covid_5.4 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency| pref+week | 0 | pref, data = weekSIR2)
covid_5.5 <- felm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency +log(noftestst.2 + 1)| pref+week | 0 | pref, data = weekSIR2)


intercept5.0 <- tail(getfe(covid_5.0, ef="zm2", se = TRUE), 1)
intercept5.1 <- tail(getfe(covid_5.1, ef="zm2", se = TRUE), 1)
intercept5.2 <- tail(getfe(covid_5.2, ef="zm2", se = TRUE), 1)
intercept5.3 <- tail(getfe(covid_5.3, ef="zm2", se = TRUE), 1)
intercept5.4 <- tail(getfe(covid_5.4, ef="zm2", se = TRUE), 1)
intercept5.5 <- tail(getfe(covid_5.5, ef="zm2", se = TRUE), 1)


prefweek2felmhtml <- stargazer(covid_5.0, covid_5.1, covid_5.2, covid_5.3, covid_5.4, covid_5.5,
                               dep.var.labels = "log(nofcases + 1)",
                               title = "TABLE: COVID-19 new cases (2 week lag) and GreenZone certification",
                               digits = 3,
                               digits.extra = 0,
                               type = "html",
                               out = "04_analyze/weekSIR/output/felm_covid2_SIR_pref_week.html",
                               add.lines=list(c("Intercept",
                                                round(intercept5.0[1][[1]], digits = 3),
                                                round(intercept5.1[1][[1]], digits = 3),
                                                round(intercept5.2[1][[1]], digits = 3),
                                                round(intercept5.3[1][[1]], digits = 3),
                                                round(intercept5.4[1][[1]], digits = 3),
                                                round(intercept5.5[1][[1]], digits = 3)),
                                              c("",
                                                paste0("(", round(intercept5.0[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept5.1[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept5.2[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept5.3[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept5.4[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept5.5[4][[1]],digits = 3),")")),
                                              c("Prefecture FE", "X", "X", "X", "X", "X", "X", "X"),
                                              c("Week FE", "X", "X", "X", "X", "X", "X", "X")),
                               omit.stat=c("f", "ser"))









