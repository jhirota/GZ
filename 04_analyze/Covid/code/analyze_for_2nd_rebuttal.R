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

covid_l4 <- felm(log(newcase_lead2 + 1) ~ log(cumGZ_l4 + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead2 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob_lag)


AIC(covid_l2)
AIC(covid_l4)
AIC(covid_l1_2)
AIC(covid_l1_2_3)
AIC(covid_l1_2_3_4)
AIC(covid_l1_2_3_4_5)

stargazer(covid_l2, covid_l1_2, covid_l1_2_3, covid_l1_2_3_4, covid_l1_2_3_4_5, covid_l4, type = "text")


