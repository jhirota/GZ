library(tidyverse)
library(lfe)
library(stargazer)

# data load ----

weekSIR2 <- read_csv("03_build/weekSIR/output/weekSIR2.csv")

# Appendix.2 コロナ県別（１週間ラグ）-----------
#7.1.をコピー
covid_6.0 <- felm(log(newcaset.1 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1) + emergency| pref+week | 0 | pref, data = weekSIR2)
covid_6.1 <- felm(log(newcaset.1 + 1) ~ log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + emergency| pref+week | 0 | pref, data = weekSIR2)
covid_6.2 <- felm(log(newcaset.1 + 1) ~ log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + resview + emergency| pref+week | 0 | pref, data = weekSIR2)
covid_6.3 <- felm(log(newcaset.1 + 1) ~ log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(mobility) + resview + emergency| pref+week | 0 | pref, data = weekSIR2)
covid_6.4 <- felm(log(newcaset.1 + 1) ~ log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency| pref+week | 0 | pref, data = weekSIR2)
covid_6.5 <- felm(log(newcaset.1 + 1) ~ log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency +log(noftestst.1 + 1)| pref+week | 0 | pref, data = weekSIR2)


intercept6.0 <- tail(getfe(covid_6.0, ef="zm2", se = TRUE), 1)
intercept6.1 <- tail(getfe(covid_6.1, ef="zm2", se = TRUE), 1)
intercept6.2 <- tail(getfe(covid_6.2, ef="zm2", se = TRUE), 1)
intercept6.3 <- tail(getfe(covid_6.3, ef="zm2", se = TRUE), 1)
intercept6.4 <- tail(getfe(covid_6.4, ef="zm2", se = TRUE), 1)
intercept6.5 <- tail(getfe(covid_6.5, ef="zm2", se = TRUE), 1)


prefweek1felmhtml <- stargazer(covid_6.0, covid_6.1, covid_6.2, covid_6.3, covid_6.4, covid_6.5,
                               dep.var.labels = "log(nofcases + 1)",
                               title = "TABLE: COVID-19 new cases (1 week lag) and GreenZone certification",
                               digits = 3,
                               digits.extra = 0,
                               type = "html",
                               out = "04_analyze/Appendix/output/felm_covid1_SIR_pref_week.html",
                               add.lines=list(c("Intercept",
                                                round(intercept6.0[1][[1]], digits = 3),
                                                round(intercept6.1[1][[1]], digits = 3),
                                                round(intercept6.2[1][[1]], digits = 3),
                                                round(intercept6.3[1][[1]], digits = 3),
                                                round(intercept6.4[1][[1]], digits = 3),
                                                round(intercept6.5[1][[1]], digits = 3)),
                                              c("",
                                                paste0("(", round(intercept6.0[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept6.1[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept6.2[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept6.3[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept6.4[4][[1]],digits = 3),")"),
                                                paste0("(", round(intercept6.5[4][[1]],digits = 3),")")),
                                              c("Prefecture FE", "X", "X", "X", "X", "X", "X", "X"), 
                                              c("Week FE", "X", "X", "X", "X", "X", "X", "X")),
                               omit.stat=c("f", "ser"))

# Appendix. 3. コロナ市町村分析 from Agoop.R-------------
data_city <- read.csv("03_build/Papilio_mobility/output/city_weekly2.csv")
data_city <- pdata.frame(data_city, index = c("city", "week"))
data_city$avg_temp_q <- (data_city$avg_temp)^2

Freq100 <- read_csv("03_build/GZlist/output/Freq100.csv")
data_city_GZ100 <- data_city %>% 
  filter(city %in% Freq100$x) 

#felm regression lag2
ccovid_4.1 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1)| city+week | 0 | city, data = data_city)
ccovid_4.2 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1)| city+week | 0 | city, data = data_city)
ccovid_4.3 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q)| city+week | 0 | city, data = data_city)
ccovid_4.4 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q) + log(avg_rainfall + 1)| city+week | 0 | city, data = data_city)

#felm regression lag2 GZ > 100
ccovid_5.1 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1)| city+week | 0 | city, data = data_city_GZ100)
ccovid_5.2 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1)| city+week | 0 | city, data = data_city_GZ100)
ccovid_5.3 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q)| city+week | 0 | city, data = data_city_GZ100)
ccovid_5.4 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q) + log(avg_rainfall + 1)| city+week | 0 | city, data = data_city_GZ100)

#felm regression lag1
ccovid_6.1 <- felm(log(infected_1w + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1)| city+week | 0 | city, data = data_city)
ccovid_6.2 <- felm(log(infected_1w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag1 + 1)| city+week | 0 | city, data = data_city)
ccovid_6.3 <- felm(log(infected_1w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q)| city+week | 0 | city, data = data_city)
ccovid_6.4 <- felm(log(infected_1w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q) + log(avg_rainfall + 1)| city+week | 0 | city, data = data_city)


# interceptを算出
intercept4.1 <- tail(getfe(ccovid_4.1, ef="zm2", se = TRUE), 1)
intercept4.2 <- tail(getfe(ccovid_4.2, ef="zm2", se = TRUE), 1)
intercept4.3 <- tail(getfe(ccovid_4.3, ef="zm2", se = TRUE), 1)
intercept4.4 <- tail(getfe(ccovid_4.4, ef="zm2", se = TRUE), 1)


intercept5.1G <- tail(getfe(ccovid_5.1, ef="zm2", se = TRUE), 1)
intercept5.2G <- tail(getfe(ccovid_5.2, ef="zm2", se = TRUE), 1)
intercept5.3G <- tail(getfe(ccovid_5.3, ef="zm2", se = TRUE), 1)
intercept5.4G <- tail(getfe(ccovid_5.4, ef="zm2", se = TRUE), 1)

intercept6.1 <- tail(getfe(ccovid_6.1, ef="zm2", se = TRUE), 1)
intercept6.2 <- tail(getfe(ccovid_6.2, ef="zm2", se = TRUE), 1)
intercept6.3 <- tail(getfe(ccovid_6.3, ef="zm2", se = TRUE), 1)
intercept6.4 <- tail(getfe(ccovid_6.4, ef="zm2", se = TRUE), 1)


felmhtml <- stargazer(ccovid_4.1, ccovid_4.2, ccovid_4.3, ccovid_4.4,
                      title = "TABLE: COVID-19 new cases (2 week lag) and GreenZone certification (City-level)",
                      digits = 3,
                      digits.extra = 0,
                      type = "html",
                      out = "04_analyze/Appendix/output/felm1.html",
                      covariate.labels = c("log(cumGZ + 1)","log(susceptible)", "log(infectious\\_lag2 + 1)",  "log(avg\\_temp\\_q)", "log(avg\\_rainfall + 1)"),
                      add.lines=list(c("Intercept",
                                       round(intercept4.1[1,1], digits = 3),
                                       round(intercept4.2[1,1], digits = 3),
                                       round(intercept4.3[1,1], digits = 3),
                                       round(intercept4.4[1,1], digits = 3)),
                                     c("",
                                       paste0("(", round(intercept4.1[1,4],digits = 3),")"),
                                       paste0("(", round(intercept4.2[1,4],digits = 3),")"),
                                       paste0("(", round(intercept4.3[1,4],digits = 3),")"),
                                       paste0("(", round(intercept4.4[1,4],digits = 3),")")),
                                     c("City FE", "X", "X", "X", "X", "X"),
                                     c("Week FE", "X", "X", "X", "X", "X")),
                      omit.stat=c("f", "ser"))

felmhtmlGZ100 <- stargazer(ccovid_5.1, ccovid_5.2, ccovid_5.3, ccovid_5.4,
                           title = "TABLE: COVID-19 new cases (2 week lag) and GreenZone certification in GZ100 cities",
                           digits = 3,
                           digits.extra = 0,
                           type = "html",
                           out = "04_analyze/Appendix/output/felm2.html",
                           covariate.labels = c("log(cumGZ + 1)","log(susceptible)", "log(infectious\\_lag2 + 1)",  "log(avg\\_temp\\_q)", "log(avg\\_rainfall + 1)"),
                           add.lines=list(c("Intercept",
                                            round(intercept5.1G[1,1], digits = 3),
                                            round(intercept5.2G[1,1], digits = 3),
                                            round(intercept5.3G[1,1], digits = 3),
                                            round(intercept5.4G[1,1], digits = 3)),
                                          c("",
                                            paste0("(", round(intercept5.1G[1,4],digits = 3),")"),
                                            paste0("(", round(intercept5.2G[1,4],digits = 3),")"),
                                            paste0("(", round(intercept5.3G[1,4],digits = 3),")"),
                                            paste0("(", round(intercept5.4G[1,4],digits = 3),")")),
                                          c("City FE", "X", "X", "X", "X", "X"), c("Week FE", "X", "X", "X", "X", "X")),
                           omit.stat=c("f", "ser"))

felmhtml2 <- stargazer(ccovid_6.1, ccovid_6.2, ccovid_6.3, ccovid_6.4,
                       title = "TABLE: COVID-19 new cases (1 week lag) and GreenZone certification (City-level)",
                       digits = 3,
                       digits.extra = 0,
                       type = "html",
                       out = "04_analyze/Appendix/output/felm3.html",
                       covariate.labels = c("log(cumGZ + 1)","log(susceptible)", "log(infectious\\_lag1 + 1)",  "log(avg\\_temp\\_q)", "log(avg\\_rainfall + 1)"),
                       add.lines=list(c("Intercept",
                                        round(intercept6.1[1,1], digits = 3),
                                        round(intercept6.2[1,1], digits = 3),
                                        round(intercept6.3[1,1], digits = 3),
                                        round(intercept6.4[1,1], digits = 3)),
                                      c("",
                                        paste0("(", round(intercept6.1[1,4],digits = 3),")"),
                                        paste0("(", round(intercept6.2[1,4],digits = 3),")"),
                                        paste0("(", round(intercept6.3[1,4],digits = 3),")"),
                                        paste0("(", round(intercept6.4[1,4],digits = 3),")")),
                                      c("City FE","X", "X", "X", "X", "X"), c("Week FE", "X", "X", "X", "X", "X")),
                       omit.stat=c("f", "ser"))



