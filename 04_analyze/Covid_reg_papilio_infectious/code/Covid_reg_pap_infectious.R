library(tidyverse)
library(lfe)
library(plm)
library(stargazer)

ppDATA2 <- read_csv("03_build/Papilio_mobility/output/city_weekly2.csv")
Freq100 <- read_csv("03_build/GZlist/output/Freq100.csv")

# Risk of inflow of potential infectious people from 46 prefectures--------

ppDATA2_GZ100 <- ppDATA2 %>% 
  filter(city %in% Freq100$x) 

FE_pp2.1 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1) + log(susceptible) + log(infected + 1) | city+週 | 0 | city, data = ppDATA2)
FE_pp2.2 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1) + log(susceptible) + log(infected + 1) + log(I(avg_temp^2)) | city+週 | 0 | city, data = ppDATA2)
FE_pp2.3 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1) + log(susceptible) + log(infected + 1) + log(I(avg_temp^2)) + log(avg_rainfall + 1)| city+週 | 0 | city, data = ppDATA2)
FE_pp2.4 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1) + log(susceptible) + log(infected + 1) + log(I(avg_temp^2)) + log(avg_rainfall + 1) + log(agrgt_potecovid_lag2+1)| city+週 | 0 | city, data = ppDATA2)

html2 <- stargazer(FE_pp2.1, FE_pp2.2, FE_pp2.3, FE_pp2.4,
                   title = "Table: City-level analysis on COVID-19 cases and GZ certification",
                   notes = "Weekly panel data <br> *p<0.1; **p<0.05; ***p<0.01",
                   notes.align = "l",
                   notes.append = FALSE,
                   digits = 4,
                   digits.extra = 0,
                   type = "html",
                   out = "04_analyze/Covid_reg_papilio_infectious/output/papilio2.html",
                   add.lines = list(c("City FE","X","X", "X", "X"), c("Week FE", "X", "X", "X", "X")),
                   omit.stat=c("f", "ser"))

whysuchdiff <- stargazer(FE_pp2.7, FE_pp2.8, FE_pp2.9, FE_pp2.10,
                         title = "Table: City-level analysis on COVID-19 cases and GZ certification",
                         notes = "Weekly panel data <br> *p<0.1; **p<0.05; ***p<0.01",
                         notes.align = "l",
                         notes.append = FALSE,
                         digits = 4,
                         digits.extra = 0,
                         type = "html",
                         out = "04_analyze/Covid_reg_papilio_infectious/output/papiliodiff.html",
                         add.lines = list(c("City FE", "X", "X", "X", "X"), c("Week FE", "X", "X", "X", "X")),
                         omit.stat=c("f", "ser"))

html4 <- stargazer(FE_pp2.1, FE_pp2.2, FE_pp2.8,
                   title = "Table: City-level analysis on COVID-19 cases and GZ certification",
                   notes = "Weekly panel data <br> *p<0.1; **p<0.05; ***p<0.01",
                   notes.align = "l",
                   notes.append = FALSE,
                   digits = 4,
                   digits.extra = 0,
                   type = "html",
                   out = "04_analyze/Covid_reg_papilio_infectious/output/papilio4.html",
                   add.lines = list(c("City FE","X","X", "X"), c("Week FE","X", "X", "X")),
                   omit.stat=c("f", "ser"))　


# felm covid reg lag2
FE_pp3.1 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1) + log(agrgt_potecovid_lag2 + 1) | city+週 | 0 | city, data = ppDATA2)
FE_pp3.2 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1) + log(agrgt_potecovid_lag2 + 1) + log(susceptible) | city+週 | 0 | city, data = ppDATA2)
FE_pp3.3 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1) + log(agrgt_potecovid_lag2 + 1) + log(susceptible) + log(I(avg_temp^2)) | city+週 | 0 |city, data = ppDATA2)
FE_pp3.4 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1) + log(agrgt_potecovid_lag2 + 1) + log(susceptible) + log(I(avg_temp^2)) + log(mobility_per)| city+週 | 0 | city, data = ppDATA2)
FE_pp3.5 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1) + log(agrgt_potecovid_lag2 + 1) + log(susceptible) + log(I(avg_temp^2)) + log(mobility_per) + log(infected + 1)| city+週 | 0 |city, data = ppDATA2)


html3 <- stargazer(FE_pp3.1, FE_pp3.2, FE_pp3.3, FE_pp3.4, FE_pp3.5,
                   title = "Table: City-level analysis on COVID-19 cases and GZ certification (2 week lag)",
                   notes = "Weekly panel data <br> *p<0.1; **p<0.05; ***p<0.01",
                   notes.align = "l",
                   notes.append = FALSE,
                   digits = 4,
                   digits.extra = 0,
                   type = "html",
                   out = "04_analyze/Covid_reg_papilio_infectious/output/papilio3.html",
                   add.lines = list(c("City FE","X","X", "X", "X", "X"), c("Week FE","X","X", "X", "X", "X")),
                   omit.stat=c("f", "ser"))

# felm covid reg lag1
FE_pp4.1 <- felm(log(infected_1w + 1) ~ log(cumGZ + 1) + log(agrgt_potecovid_lag1 + 1) | city+週 | 0 | city, data = ppDATA2)
FE_pp4.2 <- felm(log(infected_1w + 1) ~ log(cumGZ + 1) + log(agrgt_potecovid_lag1 + 1) + log(susceptible) | city+週 | 0 | city, data = ppDATA2)
FE_pp4.3 <- felm(log(infected_1w + 1) ~ log(cumGZ + 1) + log(agrgt_potecovid_lag1 + 1) + log(susceptible) + log(I(avg_temp^2)) | city+週 | 0 |city, data = ppDATA2)
FE_pp4.4 <- felm(log(infected_1w + 1) ~ log(cumGZ + 1) + log(agrgt_potecovid_lag1 + 1) + log(susceptible) + log(I(avg_temp^2)) + log(mobility_per)| city+週 | 0 | city, data = ppDATA2)
FE_pp4.5 <- felm(log(infected_1w + 1) ~ log(cumGZ + 1) + log(agrgt_potecovid_lag1 + 1) + log(susceptible) + log(I(avg_temp^2)) + log(mobility_per) + log(infected + 1)| city+週 | 0 |city, data = ppDATA2)


html5 <- stargazer(FE_pp4.1, FE_pp4.2, FE_pp4.3, FE_pp4.4, FE_pp4.5,
                   title = "Table: City-level analysis on COVID-19 cases and GZ certification (1 week lag)",
                   notes = "Weekly panel data <br> *p<0.1; **p<0.05; ***p<0.01",
                   notes.align = "l",
                   notes.append = FALSE,
                   digits = 4,
                   digits.extra = 0,
                   type = "html",
                   out = "04_analyze/Covid_reg_papilio_infectious/output/papilio5.html",
                   add.lines = list(c("City FE","X","X", "X", "X", "X"), c("Week FE","X","X", "X", "X", "X")),
                   omit.stat=c("f", "ser"))



# Mobility Analysis ---------


OLS_ppm1 <- lm(log(mobility_per) ~ log(cumGZ + 1) + log(infected + 1), data = ppDATA1)
OLS_ppm2 <- lm(log(mobility_per) ~ log(cumGZ + 1) + log(infected + 1) + avg_temp + avg_rainfall + SOE, data = ppDATA1)
FE_ppm1 <- felm(log(mobility_per) ~ log(cumGZ + 1) + log(infected + 1)| city+週 | 0 | city, data = ppDATA1)
FE_ppm2 <- felm(log(mobility_per) ~ log(cumGZ + 1) + log(infected + 1) + avg_temp + avg_rainfall| 週 | 0 | city, data = ppDATA1)
FE_ppm3 <- felm(log(mobility_per) ~ log(cumGZ + 1) + log(infected + 1) + avg_temp + avg_rainfall + SOE| city | 0 | city, data = ppDATA1)
FE_ppm4 <- felm(log(mobility_per) ~ log(cumGZ + 1) + log(infected + 1) + avg_temp + avg_rainfall| city+週 | 0 | city, data = ppDATA1)

html1 <- stargazer(OLS_ppm1, OLS_ppm2, FE_ppm1, FE_ppm2, FE_ppm3, FE_ppm4,
                   title = "Table: City-level analysis on COVID-19 cases and GZ certification",
                   notes = "Weekly panel data <br> *p<0.1; **p<0.05; ***p<0.01",
                   notes.align = "l",
                   notes.append = FALSE,
                   digits = 4,
                   digits.extra = 0,
                   type = "html",
                   out = "04_analyze/Covid_reg_papilio_infectious/output/papiliomob.html",
                   add.lines = list(c("City FE","","", "X", "", "X", "X"), c("Week FE","","", "X", "X", "", "X")),
                   omit.stat=c("f", "ser"))

# plm ---------

# パネルデータとして認識させる 
ppDATA2 <- pdata.frame(ppDATA2, index = c("city", "week")) 

#plm regression lag2
ccovid_1.1 <- plm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1),  data = ppDATA2, effect = "twoways")
ccovid_1.2 <- plm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1),  data = ppDATA2, effect = "twoways")
ccovid_1.3 <- plm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(I(avg_temp^2)),  data = ppDATA2, effect = "twoways")
ccovid_1.4 <- plm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(I(avg_temp^2)) + log(avg_rainfall + 1),  data = ppDATA2, effect = "twoways")
ccovid_1.5 <- plm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag2 + 1) + log(I(avg_temp^2)) + log(avg_rainfall + 1) + log(infected + 1),  data = ppDATA2, effect = "twoways")

#plm regression lag2 GZ > 100
ccovid_2.1 <- plm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1),  data = ppDATA2_GZ100, effect = "twoways")
ccovid_2.2 <- plm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + log(I(avg_temp^2)),  data = ppDATA2_GZ100, effect = "twoways")
ccovid_2.3 <- plm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + log(I(avg_temp^2)) + log(susceptible),  data = ppDATA2_GZ100, effect = "twoways")
ccovid_2.4 <- plm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + log(I(avg_temp^2)) + log(susceptible) + log(avg_rainfall + 1),  data = ppDATA2_GZ100, effect = "twoways")
ccovid_2.5 <- plm(log(infected_2w + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + log(I(avg_temp^2)) + log(susceptible) + log(avg_rainfall + 1) + log(infected + 1),  data = ppDATA2_GZ100, effect = "twoways")

#plm regression lag1
ccovid_3.1 <- plm(log(infected_1w + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag1 + 1),  data = ppDATA2, effect = "twoways")
ccovid_3.2 <- plm(log(infected_1w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag1 + 1),  data = ppDATA2, effect = "twoways")
ccovid_3.3 <- plm(log(infected_1w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag1 + 1) + log(I(avg_temp^2)),  data = ppDATA2, effect = "twoways")
ccovid_3.4 <- plm(log(infected_1w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag1 + 1) + log(I(avg_temp^2)) + log(avg_rainfall + 1),  data = ppDATA2, effect = "twoways")
ccovid_3.5 <- plm(log(infected_1w + 1) ~ log(cumGZ + 1)  + log(susceptible) + log(agrgt_potecovid_lag1 + 1) + log(I(avg_temp^2)) + log(avg_rainfall + 1) + log(infected + 1),  data = ppDATA2, effect = "twoways")


# interceptを算出----------------------------------
intercept1.1 <- within_intercept(ccovid_1.1)
intercept1.2 <- within_intercept(ccovid_1.2)
intercept1.3 <- within_intercept(ccovid_1.3)
intercept1.4 <- within_intercept(ccovid_1.4)
intercept1.5 <- within_intercept(ccovid_1.5)

intercept2.1 <- within_intercept(ccovid_2.1)
intercept2.2 <- within_intercept(ccovid_2.2)
intercept2.3 <- within_intercept(ccovid_2.3)
intercept2.4 <- within_intercept(ccovid_2.4)
intercept2.5 <- within_intercept(ccovid_2.5)

intercept3.1 <- within_intercept(ccovid_3.1)
intercept3.2 <- within_intercept(ccovid_3.2)
intercept3.3 <- within_intercept(ccovid_3.3)
intercept3.4 <- within_intercept(ccovid_3.4)
intercept3.5 <- within_intercept(ccovid_3.5)

#stargazerで出力--------------------
#基本的に、僕は詳しいやり方を知らないので、interceptも、新規感染者数のmeanもadd.lines で挿入
plmhtml <- stargazer(ccovid_1.1, ccovid_1.2, ccovid_1.3, ccovid_1.4, ccovid_1.5,
                     title = "TABLE: COVID new cases (2week lag) and GreenZone certification (plm)",
                     digits = 3,
                     digits.extra = 0,
                     type = "html",
                     out = "04_analyze/Covid_reg_papilio_infectious/output/plm1.html",
                     add.lines=list(c("Intercept", round(intercept1.1["(overall_intercept)"], digits = 3),
                                      round(intercept1.2["(overall_intercept)"], digits = 3),
                                      round(intercept1.3["(overall_intercept)"], digits = 3),
                                      round(intercept1.4["(overall_intercept)"], digits = 3),
                                      round(intercept1.5["(overall_intercept)"], digits = 3)),
                                    c("City FE", "X", "X", "X", "X", "X", "X"), c("Week FE", "X", "X", "X", "X", "X", "X")),
                     omit.stat=c("f", "ser"))

plmhtmlGZ100 <- stargazer(ccovid_2.1, ccovid_2.2, ccovid_2.3, ccovid_2.4, ccovid_2.5,
                          title = "TABLE2: COVID new cases (2week lag) and GreenZone certification in GZ100 cities(plm)",
                          digits = 3,
                          digits.extra = 0,
                          type = "html",
                          out = "04_analyze/Covid_reg_papilio_infectious/output/plm2.html",
                          add.lines=list(c("Intercept", round(intercept2.1["(overall_intercept)"], digits = 3),
                                           round(intercept2.2["(overall_intercept)"], digits = 3),
                                           round(intercept2.3["(overall_intercept)"], digits = 3),
                                           round(intercept2.4["(overall_intercept)"], digits = 3),
                                           round(intercept2.5["(overall_intercept)"], digits = 3)),
                                         c("Prefecture FE", "X", "X", "X", "X", "X", "X"), c("Week FE", "X", "X", "X", "X", "X", "X")),
                          omit.stat=c("f", "ser"))

plmhtml2 <- stargazer(ccovid_3.1, ccovid_3.2, ccovid_3.3, ccovid_3.4, ccovid_3.5,
                      title = "TABLE: COVID new cases (1week lag) and GreenZone certification (plm)",
                      digits = 3,
                      digits.extra = 0,
                      type = "html",
                      out = "04_analyze/Covid_reg_papilio_infectious/output/plm3.html",
                      add.lines=list(c("Intercept", round(intercept3.1["(overall_intercept)"], digits = 3),
                                       round(intercept3.2["(overall_intercept)"], digits = 3),
                                       round(intercept3.3["(overall_intercept)"], digits = 3),
                                       round(intercept3.4["(overall_intercept)"], digits = 3),
                                       round(intercept3.5["(overall_intercept)"], digits = 3)),
                                     c("City FE", "X", "X", "X", "X", "X", "X"), c("Week FE", "X", "X", "X", "X", "X", "X")),
                      omit.stat=c("f", "ser"))
