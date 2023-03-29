library(tidyverse)
library(lfe)
library(stargazer)

#load data----

weekSIR_rob <- readr::read_csv("03_build/Robust_check/output/weekSIR_robustness.csv")

#build data for lag (weekly data)-----

gz_lags <- weekSIR_rob %>% 
  dplyr::select(week, week_JP, weeknum, pref, newcase_lead2, GZnew, cumGZ)

gz_lead1 <- gz_lags %>% #lead cumGZ because conditioned for newcase_lead2 for the left hand side
  dplyr::select(weeknum, pref, cumGZ) %>% 
  dplyr::mutate(weeknum = weeknum - 1) %>% 
  dplyr::rename(cumGZ_l1 = cumGZ)

gz_lag1 <- gz_lags %>% #lead cumGZ because conditioned for newcase_lead2 for the left hand side
  dplyr::select(weeknum, pref, cumGZ) %>% 
  dplyr::mutate(weeknum = weeknum + 1) %>% 
  dplyr::rename(cumGZ_l3 = cumGZ) 

gz_lag2 <- gz_lags %>% #lead cumGZ because conditioned for newcase_lead2 for the left hand side
  dplyr::select(weeknum, pref, cumGZ) %>% 
  dplyr::mutate(weeknum = weeknum + 2) %>% 
  dplyr::rename(cumGZ_l4 = cumGZ) 

gz_lag3 <- gz_lags %>% #lead cumGZ because conditioned for newcase_lead2 for the left hand side
  dplyr::select(weeknum, pref, cumGZ) %>% 
  dplyr::mutate(weeknum = weeknum + 3) %>% 
  dplyr::rename(cumGZ_l5 = cumGZ) 

weekSIR_rob_lag <- weekSIR_rob %>% 
  dplyr::left_join(., gz_lead1, by = c("weeknum", "pref")) %>% 
  dplyr::left_join(., gz_lag1, by = c("weeknum", "pref")) %>% 
  dplyr::left_join(., gz_lag2, by = c("weeknum", "pref")) %>% 
  dplyr::left_join(., gz_lag3, by = c("weeknum", "pref")) %>% 
  tidyr::replace_na(., list(cumGZ_l3 = 0, cumGZ_l4 = 0, cumGZ_l5 = 0)) %>% 
  dplyr::select(week, week_JP, weeknum, pref, newcase_day, newdeath_day, emergency, GZnew, cumGZ, 
                cumGZ_l1, cumGZ_l3, cumGZ_l4, cumGZ_l5, tidyselect::everything())
  

#main specification (lag2)
covid_l2 <- felm(log(newcase_lead2 + 1) ~ log(cumGZ + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead2 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob)

#supplementary information(lag1)
# covid_51.4 <- felm(log(newcase_lead1 + 1) ~ log(cumGZ + 1)  + log(infectious_l1 + 1) + log(susceptible + 1) + emergency + log(tests_lead1 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob)

#newcase_lead、infectious_、tests_leadだけずらしているか。

#cumGZ lag1 + lag2
covid_l1_2 <- felm(log(newcase_lead2 + 1) ~ log(cumGZ + 1) + log(cumGZ_l1 + 1) + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead2 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob_lag)
#lag1 + lag2 + lag3
covid_l1_2_3 <- felm(log(newcase_lead2 + 1) ~ log(cumGZ + 1) + log(cumGZ_l1 + 1) + log(cumGZ_l3 + 1) + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead2 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob_lag)
#lag1 + lag2 + lag3 + lag4
covid_l1_2_3_4 <- felm(log(newcase_lead2 + 1) ~ log(cumGZ + 1) + log(cumGZ_l1 + 1) + log(cumGZ_l3 + 1) + log(cumGZ_l4 + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead2 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob_lag)

#lag1 + lag2 + lag3 + lag4 + lag5
covid_l1_2_3_4_5 <- felm(log(newcase_lead2 + 1) ~ log(cumGZ + 1) + log(cumGZ_l1 + 1) + log(cumGZ_l3 + 1) + log(cumGZ_l4 + 1) + log(cumGZ_l5 + 1) + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead2 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob_lag)

#alone
covid_l1 <- felm(log(newcase_lead2 + 1) ~ log(cumGZ_l1 + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead2 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob_lag)
covid_l3 <- felm(log(newcase_lead2 + 1) ~ log(cumGZ_l3 + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead2 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob_lag)
covid_l4 <- felm(log(newcase_lead2 + 1) ~ log(cumGZ_l4 + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead2 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob_lag)
covid_l5 <- felm(log(newcase_lead2 + 1) ~ log(cumGZ_l5 + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead2 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob_lag)

aic_covid_l2 <- round(AIC(covid_l2), digits = 2)
aic_covid_l1 <- round(AIC(covid_l1), digits = 2)
aic_covid_l3 <- round(AIC(covid_l3), digits = 2)
aic_covid_l4 <- round(AIC(covid_l4), digits = 2)
aic_covid_l5 <- round(AIC(covid_l5), digits = 2)

# stargazer(covid_l2, covid_l1, covid_l3, covid_l4, covid_l5, type = "text")

aic_covid_l1_2 <- round(AIC(covid_l1_2), digits = 2)
aic_covid_l1_2_3 <- round(AIC(covid_l1_2_3), digits = 2)
aic_covid_l1_2_3_4 <- round(AIC(covid_l1_2_3_4), digits = 2)
aic_covid_l1_2_3_4_5 <- round(AIC(covid_l1_2_3_4_5), digits = 2)

# stargazer(covid_l2, covid_l1_2, covid_l1_2_3, covid_l1_2_3_4, covid_l1_2_3_4_5, covid_l4, type = "text")

table_b1 <- stargazer(covid_l2, covid_l1, covid_l3, covid_l4, covid_l5, covid_l1_2, covid_l1_2_3, covid_l1_2_3_4, covid_l1_2_3_4_5,
                      title = "Model Selection of the time lag (Weekly data) : The COVID-19 new infection cases and the Green Zone certification",
                      digits = 3,
                      digits.extra = 0,
                      type = "latex",
                      dep.var.labels = c("New infection cases, log"),
                      covariate.labels = c("Cumulative GZ-certified restaurants, log (2weeks lag)", "Cumulative GZ-certified restaurants, log (1week lag)", 
                                           "Cumulative GZ-certified restaurants, log (3weeks lag)", "Cumulative GZ-certified restaurants, log (4weeks lag)",
                                           "Cumulative GZ-certified restaurants, log (5weeks lag)"),
                      add.lines=list(c("Unit of time series", "", "", "", "" ,"Weekly", "", "", "", ""), 
                                     c("AIC", aic_covid_l2, aic_covid_l1, aic_covid_l3, aic_covid_l4, aic_covid_l5, 
                                        aic_covid_l1_2, aic_covid_l1_2_3, aic_covid_l1_2_3_4, aic_covid_l1_2_3_4_5)),
                      omit = c(6,7,8,9,10,11,12,13,14),
                      omit.stat=c("f", "ser", "rsq"),
                      header = FALSE,
                      column.sep.width = "1pt",
                      font.size = "scriptsize",
                      notes.align = "l",
                      notes.append = FALSE)

table_b1.note <- "\\multicolumn{10}{l} {\\parbox[t]{21cm}{ \\textit{Notes:} *p<0.1; **p<0.05; ***p<0.01
The dependent variable is the log-transformed value of the number of new infection cases. 
The unit of analysis is prefecture and week, and the fixed effects are introduced in all models. 
For the observations, six prefectures are targeted.
The values in parentheses are cluster-robust standard errors. Clustering is at the prefecture level.
The following variables are included as covariates: Infectious(log), Susceptible(log), State of Emergency dummy, Tests(log), Customers per restaurant(log), 
Average temperature(log), Average rainfall(log), School closure dummy, Gathering restriction dummy.
Cumulative GZ-certified restaurants, log is the log-transformed value of the number of cumulative certified-GZ restaurants.
Infectious(log) is the log-transformed value of the number of potentially infected people.
Susceptible(log) is the log-transformed value of the total number of susceptible population.
State of Emergency is the dummy variable that takes the value 1 if the state of emergency is declared. 
Tests(log) is the log-transformed value of the number of COVID-19 tests.
Customers per restaurant(log) is the log-transformed value of the number of customers per restaurant.
Average temperature(log) is the log-transformed value of the mean temperature (Fahrenheit degrees).
Average rainfall(log) is the log-transformed value of the aggregated rainfall (in millimeters).
School closure is the dummy variable that takes the value 1 if the school closure is declared. 
Gathering restriction is the dummy variable that takes the value 1 if the large-scale gathering restriction is declared.
For the variables that take absolute value 0 (New infection cases(log), Cumulative GZ-certified restaurants, log, Infectious(log), and Tests(log)), we add value 1 before log-transforming to avoid the logarithm of 0.}} \\\\"
table_b1[grepl("Note",table_b1)] <- table_b1.note
cat(table_b1, sep = "\n")
write(table_b1, here::here("04_analyze/Covid/output/covid_weekly_AIC.tex"))


#daily data--------

## load data
dailySIR <- readr::read_csv("03_build/2nd_rebuttal/output/daily_data_SIR.csv")

#build data for lag (daily data)-----
gz_lags_daily <- dailySIR %>% 
  dplyr::select(date, pref, newcase_lead8, GZnew, cumGZ)

gz_lag1_daily <- gz_lags_daily %>% #lead cumGZ because conditioned for newcase_lead8 for the left hand side
  dplyr::select(date, pref, cumGZ) %>% 
  dplyr::mutate(date = date - 7) %>% 
  dplyr::rename(cumGZ_l1 = cumGZ)

gz_lag3_daily <- gz_lags_daily %>% #lead cumGZ because conditioned for newcase_lead8 for the left hand side
  dplyr::select(date, pref, cumGZ) %>% 
  dplyr::mutate(date = date + 7) %>% 
  dplyr::rename(cumGZ_l3 = cumGZ) 

gz_lag4_daily <- gz_lags_daily %>% #lead cumGZ because conditioned for newcase_lead8 for the left hand side
  dplyr::select(date, pref, cumGZ) %>% 
  dplyr::mutate(date = date + 14) %>% 
  dplyr::rename(cumGZ_l4 = cumGZ) 

gz_lag5_daily <- gz_lags_daily %>% #lead cumGZ because conditioned for newcase_lead8 for the left hand side
  dplyr::select(date, pref, cumGZ) %>% 
  dplyr::mutate(date = date + 21) %>% 
  dplyr::rename(cumGZ_l5 = cumGZ) 

dailySIR_lag <- dailySIR %>% 
  dplyr::left_join(., gz_lag1_daily, by = c("date", "pref")) %>% 
  dplyr::left_join(., gz_lag3_daily, by = c("date", "pref")) %>% 
  dplyr::left_join(., gz_lag4_daily, by = c("date", "pref")) %>% 
  dplyr::left_join(., gz_lag5_daily, by = c("date", "pref")) %>% 
  tidyr::replace_na(., list(cumGZ_l1 = 0, cumGZ_l3 = 0, cumGZ_l4 = 0, cumGZ_l5 = 0)) %>% 
  dplyr::select(date, prefcode, pref, newcase_day, GZnew, cumGZ, 
                cumGZ_l1, cumGZ_l3, cumGZ_l4, cumGZ_l5, tidyselect::everything())

#analyze

##AIC-------
covid_l2_daily <- felm(log(newcase_lead8 + 1) ~ log(cumGZ + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead8 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date+weekday+weeknum | 0 | pref, data = dailySIR_lag)

#cumGZ lag1 + lag2
covid_l1_2_daily <- felm(log(newcase_lead8 + 1) ~ log(cumGZ + 1) + log(cumGZ_l1 + 1) + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead8 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date+weekday+weeknum | 0 | pref, data = dailySIR_lag)
#lag1 + lag2 + lag3
covid_l1_2_3_daily <- felm(log(newcase_lead8 + 1) ~ log(cumGZ + 1) + log(cumGZ_l1 + 1) + log(cumGZ_l3 + 1) + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead8 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date+weekday+weeknum | 0 | pref, data = dailySIR_lag)
#lag1 + lag2 + lag3 + lag4
covid_l1_2_3_4_daily <- felm(log(newcase_lead8 + 1) ~ log(cumGZ + 1) + log(cumGZ_l1 + 1) + log(cumGZ_l3 + 1) + log(cumGZ_l4 + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead8 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date+weekday+weeknum | 0 | pref, data = dailySIR_lag)
#lag1 + lag2 + lag3 + lag4 + lag5
covid_l1_2_3_4_5_daily <- felm(log(newcase_lead8 + 1) ~ log(cumGZ + 1) + log(cumGZ_l1 + 1) + log(cumGZ_l3 + 1) + log(cumGZ_l4 + 1) + log(cumGZ_l5 + 1) + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead8 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date+weekday+weeknum | 0 | pref, data = dailySIR_lag)

AIC(covid_l2_daily)
# AIC(covid_l4)
AIC(covid_l1_2_daily)
AIC(covid_l1_2_3_daily)
AIC(covid_l1_2_3_4_daily)
AIC(covid_l1_2_3_4_5_daily)

stargazer(covid_l2_daily, covid_l1_2_daily, covid_l1_2_3_daily, covid_l1_2_3_4_daily, covid_l1_2_3_4_5_daily, type = "text")

#AIC for different calculation------
covid_l2_daily_rev <- felm(log(newcase_lead14 + 1) ~ log(cumGZ + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead14 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date+weekday+weeknum | 0 | pref, data = dailySIR_lag)

#cumGZ lag1 + lag2
covid_l1_2_daily_rev <- felm(log(newcase_lead14 + 1) ~ log(cumGZ + 1) + log(cumGZ_l1 + 1) + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead14 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date+weekday+weeknum | 0 | pref, data = dailySIR_lag)
#lag1 + lag2 + lag3
covid_l1_2_3_daily_rev <- felm(log(newcase_lead14 + 1) ~ log(cumGZ + 1) + log(cumGZ_l1 + 1) + log(cumGZ_l3 + 1) + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead14 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date+weekday+weeknum | 0 | pref, data = dailySIR_lag)
#lag1 + lag2 + lag3 + lag4
covid_l1_2_3_4_daily_rev <- felm(log(newcase_lead14 + 1) ~ log(cumGZ + 1) + log(cumGZ_l1 + 1) + log(cumGZ_l3 + 1) + log(cumGZ_l4 + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead14 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date+weekday+weeknum | 0 | pref, data = dailySIR_lag)
#lag1 + lag2 + lag3 + lag4 + lag5
covid_l1_2_3_4_5_daily_rev <- felm(log(newcase_lead14 + 1) ~ log(cumGZ + 1) + log(cumGZ_l1 + 1) + log(cumGZ_l3 + 1) + log(cumGZ_l4 + 1) + log(cumGZ_l5 + 1) + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead14 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date+weekday+weeknum | 0 | pref, data = dailySIR_lag)

aic_covid_l2_daily <- round(AIC(covid_l2_daily_rev), digits = 2)
# AIC(covid_l4)
AIC(covid_l1_2_daily_rev)
AIC(covid_l1_2_3_daily_rev)
AIC(covid_l1_2_3_4_daily_rev)
AIC(covid_l1_2_3_4_5_daily_rev)

stargazer(covid_l2_daily_rev, covid_l1_2_daily_rev, covid_l1_2_3_daily_rev, covid_l1_2_3_4_daily_rev, covid_l1_2_3_4_5_daily_rev, type = "text")



#regression--------

##main specification
covid_daily_l8 <- felm(log(newcase_lead8 + 1) ~ log(cumGZ + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead8 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date+weekday+weeknum | 0 | pref, data = dailySIR)

## lag14
covid_daily_l14 <- felm(log(newcase_lead14 + 1) ~ log(cumGZ + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead14 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date+weekday+weeknum | 0 | pref, data = dailySIR)

## lag21
covid_daily_l21 <- felm(log(newcase_lead21 + 1) ~ log(cumGZ + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead21 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date+weekday+weeknum | 0 | pref, data = dailySIR)

## lag28
covid_daily_l28 <- felm(log(newcase_lead28 + 1) ~ log(cumGZ + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead28 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date+weekday+weeknum | 0 | pref, data = dailySIR)


stargazer(covid_daily_l8, covid_daily_l14, covid_daily_l21, covid_daily_l28, type = "text")
AIC(covid_daily_l8)
AIC(covid_daily_l14)
AIC(covid_daily_l21)
AIC(covid_daily_l28)

#comparison of weekly and daily-------

stargazer(covid_l2, covid_l2_daily_rev, type = "text")

table_b2 <- stargazer(covid_l2, covid_l2_daily_rev, 
                      title = "Model Selection of the time lag (Weekly and Daily): The COVID-19 new infection cases and the Green Zone certification",
                      digits = 3,
                      digits.extra = 0,
                      type = "latex",
                      dep.var.labels = c("New infection cases, log (Weekly)", "New infection cases, log (Daily)"),
                      covariate.labels = c("Cumulative GZ-certified restaurants, log (2weeks/14days lag)"), 
                      add.lines=list(c("Unit of time series", "Weekly", "Daily"), 
                                     c("AIC", aic_covid_l2, aic_covid_l2_daily)
                                     ),
                      omit = c(2,3,4,5,6,7,8,9,10,11),
                      omit.stat=c("f", "ser", "rsq"),
                      header = FALSE,
                      column.sep.width = "1pt",
                      font.size = "footnotesize",
                      notes.align = "l",
                      notes.append = FALSE)

table_b2.note <- "\\multicolumn{3}{l} {\\parbox[t]{20cm}{ \\textit{Notes:} *p<0.1; **p<0.05; ***p<0.01
The dependent variable is the log-transformed value of the number of new infection cases. 
The unit of analysis is prefecture and week for the first column (1) and day for the second column (2). The fixed effects are introduced in all models. 
For the observations, six prefectures are targeted.
The values in parentheses are cluster-robust standard errors. Clustering is at the prefecture level.
The following variables are included as covariates: Infectious(log), Susceptible(log), State of Emergency dummy, Tests(log), Customers per restaurant(log), 
Average temperature(log), Average rainfall(log), School closure dummy, Gathering restriction dummy.
Cumulative GZ-certified restaurants, log is the log-transformed value of the number of cumulative certified-GZ restaurants.
Infectious(log) is the log-transformed value of the number of potentially infected people.
Susceptible(log) is the log-transformed value of the total number of susceptible population.
State of Emergency is the dummy variable that takes the value 1 if the state of emergency is declared. 
Tests(log) is the log-transformed value of the number of COVID-19 tests.
Customers per restaurant(log) is the log-transformed value of the number of customers per restaurant.
Average temperature(log) is the log-transformed value of the mean temperature (Fahrenheit degrees).
Average rainfall(log) is the log-transformed value of the aggregated rainfall (in millimeters).
School closure is the dummy variable that takes the value 1 if the school closure is declared. 
Gathering restriction is the dummy variable that takes the value 1 if the large-scale gathering restriction is declared.
For the variables that take absolute value 0 (New infection cases(log), Cumulative GZ-certified restaurants, log, Infectious(log), and Tests(log)), we add value 1 before log-transforming to avoid the logarithm of 0.}} \\\\"
table_b2[grepl("Note",table_b2)] <- table_b2.note
cat(table_b2, sep = "\n")
write(table_b2, here::here("04_analyze/Covid/output/covid_weekly_and_daily_AIC.tex"))
