library(tidyverse)

# data load------
postas_day1 <- read_csv(here::here("03_build/Postas/output/postas_daily_data.csv"))

#build lag GZ-----

gz_lags_postas <- postas_day1 %>% 
  dplyr::select(date, pref, GZnew, cumGZ)

gz_lag7_postas <- gz_lags_postas %>% 
  dplyr::select(date, pref, cumGZ) %>% 
  dplyr::mutate(date = date + 7) %>% 
  dplyr::rename(cumGZ_l1 = cumGZ)

gz_lag14_postas <- gz_lags_daily %>% 
  dplyr::select(date, pref, cumGZ) %>% 
  dplyr::mutate(date = date + 14) %>% 
  dplyr::rename(cumGZ_l2 = cumGZ) 

gz_lag21_postas <- gz_lags_daily %>%
  dplyr::select(date, pref, cumGZ) %>% 
  dplyr::mutate(date = date + 21) %>% 
  dplyr::rename(cumGZ_l3 = cumGZ) 

gz_lag28_postas <- gz_lags_daily %>%
  dplyr::select(date, pref, cumGZ) %>% 
  dplyr::mutate(date = date + 28) %>% 
  dplyr::rename(cumGZ_l4 = cumGZ) 

gz_lag35_postas <- gz_lags_daily %>%
  dplyr::select(date, pref, cumGZ) %>% 
  dplyr::mutate(date = date + 35) %>% 
  dplyr::rename(cumGZ_l5 = cumGZ) 


postas_day1_lag <- postas_day1 %>% 
  dplyr::left_join(., gz_lag7_postas, by = c("date", "pref")) %>% 
  dplyr::left_join(., gz_lag14_postas, by = c("date", "pref")) %>% 
  dplyr::left_join(., gz_lag21_postas, by = c("date", "pref")) %>% 
  dplyr::left_join(., gz_lag28_postas, by = c("date", "pref")) %>% 
  dplyr::left_join(., gz_lag35_postas, by = c("date", "pref")) %>% 
  tidyr::replace_na(., list(cumGZ_l1 = 0, cumGZ_l2 = 0, cumGZ_l3 = 0, cumGZ_l4 = 0, cumGZ_l5 = 0)) %>% 
  dplyr::select(date, pref, newcase_day, newdeath_day, GZnew, cumGZ, 
                cumGZ_l1, cumGZ_l2, cumGZ_l3, cumGZ_l4, cumGZ_l5, tidyselect::everything())


#analyze sales-----

postas_sales_l0 <- felm(log(sales_per) ~ log(cumGZ + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)
postas_sales_l1 <- felm(log(sales_per) ~ log(cumGZ_l1 + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)
postas_sales_l2 <- felm(log(sales_per) ~ log(cumGZ_l2 + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)
postas_sales_l3 <- felm(log(sales_per) ~ log(cumGZ_l3 + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)
postas_sales_l4 <- felm(log(sales_per) ~ log(cumGZ_l4 + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)
postas_sales_l5 <- felm(log(sales_per) ~ log(cumGZ_l5 + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)

AIC(postas_sales_l0)
AIC(postas_sales_l1)
AIC(postas_sales_l2)
AIC(postas_sales_l3)
AIC(postas_sales_l4)
AIC(postas_sales_l5)

stargazer(postas_sales_l0, postas_sales_l1, postas_sales_l2, postas_sales_l3, postas_sales_l4, postas_sales_l5, type = "text")



#analyze customers------
postas_cus_l0 <- felm(log(customers_per) ~ log(cumGZ + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)
postas_cus_l1 <- felm(log(customers_per) ~ log(cumGZ_l1 + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)
postas_cus_l2 <- felm(log(customers_per) ~ log(cumGZ_l2 + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)
postas_cus_l3 <- felm(log(customers_per) ~ log(cumGZ_l3 + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)
postas_cus_l4 <- felm(log(customers_per) ~ log(cumGZ_l4 + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)
postas_cus_l5 <- felm(log(customers_per) ~ log(cumGZ_l5 + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)

AIC(postas_cus_l0)
AIC(postas_cus_l1)
AIC(postas_cus_l2)
AIC(postas_cus_l3)
AIC(postas_cus_l4)
AIC(postas_cus_l5)

stargazer(postas_cus_l0, postas_cus_l1, postas_cus_l2, postas_cus_l3, postas_cus_l4, postas_cus_l5, type = "text", 
          omit = c(7,8,9,10,11,12))


