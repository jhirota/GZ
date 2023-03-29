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

aic_sales_l0 <- round(AIC(postas_sales_l0), digits = 1)
aic_sales_l1 <- round(AIC(postas_sales_l1), digits = 1)
aic_sales_l2 <- round(AIC(postas_sales_l2), digits = 1)
aic_sales_l3 <- round(AIC(postas_sales_l3), digits = 1)
aic_sales_l4 <- round(AIC(postas_sales_l4), digits = 1)
aic_sales_l5 <- round(AIC(postas_sales_l5), digits = 1)

stargazer(postas_sales_l0, postas_sales_l1, postas_sales_l2, postas_sales_l3, postas_sales_l4, postas_sales_l5, type = "text")

table_b3 <- stargazer(postas_sales_l0, postas_sales_l1, postas_sales_l2, postas_sales_l3, postas_sales_l4, postas_sales_l5,
                      title = "Model Selection of the time lag: Restaurants' sales (POS) and the Green Zone certification",
                      digits = 3,
                      digits.extra = 0,
                      type = "latex",
                      dep.var.labels = c("Sales per restaurant, log"),
                      covariate.labels = c("Cumulative GZ-certified restaurants, log (0day lag)", "Cumulative GZ-certified restaurants, log (7days lag)", 
                                           "Cumulative GZ-certified restaurants, log (14days lag)", "Cumulative GZ-certified restaurants, log (21days lag)",
                                           "Cumulative GZ-certified restaurants, log (28days lag)", "Cumulative GZ-certified restaurants, log (35days lag)"),
                      add.lines=list( c("AIC", aic_sales_l0, aic_sales_l1, aic_sales_l2, aic_sales_l3, aic_sales_l4, aic_sales_l5)),
                      omit = c(7,8,9,10,11,12),
                      omit.stat=c("f", "ser", "rsq"),
                      header = FALSE,
                      column.sep.width = "1pt",
                      font.size = "scriptsize",
                      notes.align = "l",
                      notes.append = FALSE)

table_b3.note <- "\\multicolumn{7}{l} {\\parbox[t]{16cm}{ \\textit{Notes:} *p<0.1; **p<0.05; ***p<0.01
The dependent variable is the log-transformed value of POS sales per restaurant.
The unit of analysis is prefecture and day, and the fixed effects are introduced in all models. 
For the observations, six prefectures are targeted.
The values in parentheses are cluster-robust standard errors. Clustering is at the prefecture level. The following variables are included as covariates: 
State of Emergency dummy, The number of new COVID-19 cases, log, Average temperature, log, Average rainfall, log, School closure dummy, Gathering restriction dummy.
Cumulative GZ-certified restaurants, log is the log-transformed value of the number of cumulative certified-GZ restaurants.
State of Emergency is the dummy variable that takes the value 1 if the state of emergency is declared. 
The number of new COVID-19 cases, log is the log-transformed value of the daily number of infection cases.
Average temperature, log is the log-transformed value of the mean temperature (Fahrenheit degrees).
Average rainfall, log is the log-transformed value of the aggregated rainfall (in millimeters).
School closure is the dummy variable that takes the value 1 if the school closure is declared. 
Gathering restriction is the dummy variable that takes the value 1 if the large-scale gathering restriction is declared.
For the variables that take absolute value 0 (Cumulative GZ-certified restaurants, log, and The number of new COVID-19 cases, log), we add value 1 before log-transforming to avoid the logarithm of 0.}} \\\\"
table_b3[grepl("Note",table_b3)] <- table_b3.note
cat (table_b3, sep = "\n")
write(table_b3, here::here("04_analyze/Postas/output/postas_sales_AIC.tex"))


#analyze customers------
postas_cus_l0 <- felm(log(customers_per) ~ log(cumGZ + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)
postas_cus_l1 <- felm(log(customers_per) ~ log(cumGZ_l1 + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)
postas_cus_l2 <- felm(log(customers_per) ~ log(cumGZ_l2 + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)
postas_cus_l3 <- felm(log(customers_per) ~ log(cumGZ_l3 + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)
postas_cus_l4 <- felm(log(customers_per) ~ log(cumGZ_l4 + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)
postas_cus_l5 <- felm(log(customers_per) ~ log(cumGZ_l5 + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1_lag)

aic_cus_l0 <- round(AIC(postas_cus_l0), digits = 1)
aic_cus_l1 <- round(AIC(postas_cus_l1), digits = 1)
aic_cus_l2 <- round(AIC(postas_cus_l2), digits = 1)
aic_cus_l3 <- round(AIC(postas_cus_l3), digits = 1)
aic_cus_l4 <- round(AIC(postas_cus_l4), digits = 1)
aic_cus_l5 <- round(AIC(postas_cus_l5), digits = 1)

stargazer(postas_cus_l0, postas_cus_l1, postas_cus_l2, postas_cus_l3, postas_cus_l4, postas_cus_l5, type = "text", 
          omit = c(7,8,9,10,11,12))

table_b4 <- stargazer(postas_cus_l0, postas_cus_l1, postas_cus_l2, postas_cus_l3, postas_cus_l4, postas_cus_l5,
                    title = "Model Selection of the time lag: The number of Restaurants' customers (POS) and the Green Zone certification",
                    digits = 3,
                    digits.extra = 0,
                    type = "latex",
                    dep.var.labels = c("Customers per restaurant, log"),
                    covariate.labels = c("Cumulative GZ-certified restaurants, log (0day lag)", "Cumulative GZ-certified restaurants, log (7days lag)", 
                                         "Cumulative GZ-certified restaurants, log (14days lag)", "Cumulative GZ-certified restaurants, log (21days lag)",
                                         "Cumulative GZ-certified restaurants, log (28days lag)", "Cumulative GZ-certified restaurants, log (35days lag)"),
                    add.lines=list( c("AIC", aic_cus_l0, aic_cus_l1, aic_cus_l2, aic_cus_l3, aic_cus_l4, aic_cus_l5)),
                    omit = c(7,8,9,10,11,12),
                    omit.stat=c("f", "ser", "rsq"),
                    header = FALSE,
                    column.sep.width = "1pt",
                    font.size = "scriptsize",
                    notes.align = "l",
                    notes.append = FALSE)

table_b4.note <- "\\multicolumn{7}{l} {\\parbox[t]{16cm}{ \\textit{Notes:} *p<0.1; **p<0.05; ***p<0.01
The dependent variable is the log-transformed value of the number of customers per restaurant.
The unit of analysis is prefecture and day, and the fixed effects are introduced in all models. 
For the observations, six prefectures are targeted.
The values in parentheses are cluster-robust standard errors. Clustering is at the prefecture level. The following variables are included as covariates: 
State of Emergency dummy, The number of new COVID-19 cases, log, Average temperature, log, Average rainfall, log, School closure dummy, Gathering restriction dummy.
Cumulative GZ-certified restaurants, log is the log-transformed value of the number of cumulative certified-GZ restaurants.
State of Emergency is the dummy variable that takes the value 1 if the state of emergency is declared. 
The number of new COVID-19 cases, log is the log-transformed value of the daily number of infection cases.
Average temperature, log is the log-transformed value of the mean temperature (Fahrenheit degrees).
Average rainfall, log is the log-transformed value of the aggregated rainfall (in millimeters).
School closure is the dummy variable that takes the value 1 if the school closure is declared. 
Gathering restriction is the dummy variable that takes the value 1 if the large-scale gathering restriction is declared.
For the variables that take absolute value 0 (Cumulative GZ-certified restaurants, log, and The number of new COVID-19 cases, log), we add value 1 before log-transforming to avoid the logarithm of 0.}} \\\\"
table_b4[grepl("Note",table_b4)] <- table_b4.note
cat (table_b4, sep = "\n")
write(table_b4, here::here("04_analyze/Postas/output/postas_cus_AIC.tex"))


