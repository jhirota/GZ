library(tidyverse)
library(ggeffects)
library(lubridate)

# data load-----
weekSIR2 <- read_csv(here::here("03_build/Postas/output/weekSIR2.csv"))
postas_day1 <- read_csv(here::here("03_build/Postas/output/postas_daily_data.csv"))

# Economic prevention effect ---------
## fit data (covid)-------

pred_covid <- function(data){
  data <- data %>% arrange(week)
  covidmodel <- lm(log(newcase_lead2+ 1) ~ log(cumGZ + 1) + log(infectious_l2 + 1) + log(susceptible) + emergency +
                     log(tests_lead2 + 1) + factor(pref) + factor(week),
                   data = data)
  pred_value <- exp(fitted(covidmodel))
  return(pred_value)
} 

FittedValue <- weekSIR2 %>% 
  arrange(week) %>% 
  mutate(pred_covid_lead2 = c(pred_covid(weekSIR2),
                              rep(NA, nrow(weekSIR2) - length(pred_covid(weekSIR2))))) %>% 
  filter(pref == "Yamanashi") %>% 
  select(week, pred_covid_lead2)



## counter-factual data (covid)-------

covidmodel <- lm(log(newcase_lead2 + 1) ~ log(cumGZ + 1) + log(infectious_l2 + 1) + log(susceptible) + emergency +
                        log(tests_lead2 + 1) + factor(pref) + factor(week),
                      data = weekSIR2)
CofaValue <- rep(NA, 68)
j <- 3 #2weeklag

predictiondata <- weekSIR2 %>% filter(week <= "2021-04-18")
for (i in 1:nrow(predictiondata)){
  if (predictiondata$pref[i] == "Yamanashi"){
    pred_value <- ggpredict(covidmodel,
                            terms = "cumGZ",
                            type = "fe",
                            condition = c(week = as.character(predictiondata$week[i]),
                                          pref = predictiondata$pref[i],
                                          tests_lead2 = predictiondata$tests_lead2[i],
                                          infectious_l2 = predictiondata$infectious_l2[i],
                                          susceptible = predictiondata$susceptible[i],
                                          emergency = predictiondata$emergency[i]),
                            back.transform = TRUE) %>% 
      filter(x == 0)

    CofaValue[j] <- pred_value$predicted[1]
    j <- j + 1
  }else{
  }
}

cofaplot <- FittedValue %>% 
  rename(fitted = pred_covid_lead2) %>% 
  mutate(counterfactual = CofaValue,
         fitted = lag(fitted, n = 2),
         actual = weekSIR2 %>% 
           filter(pref == "Yamanashi") %>% 
           pull(newcase_day)) %>% 
  pivot_longer(!week,
               names_to = "type",
               values_to = "nofcases")


write_csv(cofaplot, here::here("03_build/Counterfactual/output/cofa_covid.csv"))


# Economic effects (daily)-------

## fit data (sales)----------

pred_sales <- function(data){
  data <- data %>% arrange(date)
  postas_model <- lm(log(sales_per) ~ log(cumGZ + 1) + emergency + factor(pref) + factor(date),
                     data = postas_day1)
  
  pred_value <- exp(fitted(postas_model))
  return(pred_value)
} 

FittedValue_s <- postas_day1 %>% 
  arrange(date) %>% 
  mutate(pred_sales = pred_sales(postas_day1)) %>% 
  filter(pref == "Yamanashi") %>% 
  select(date, pred_sales)

## cofa data (sales) --------
postas_model <- lm(log(sales_per) ~ log(cumGZ + 1) + emergency + factor(pref) + factor(date),
                       data = postas_day1)

CofaValue_s <- rep(NA, 851)
j <- 1
for (i in 1:nrow(postas_day1)){
  if (postas_day1$pref[i] == "Yamanashi"){
    pred_value <- ggpredict(postas_model,
                            terms = "cumGZ",
                            type = "fe",
                            condition = c(date = as.character(postas_day1$date[i]),
                                          pref = postas_day1$pref[i],
                                          emergency = postas_day1$emergency[i]),
                            back.transform = TRUE) %>% 
      filter(x == 0)
    
    CofaValue_s[j] <- pred_value$predicted[1]
    j <- j + 1
  }else{
  }
}

cofaplot_s <- FittedValue_s %>% 
  rename(fitted = pred_sales) %>% 
  mutate(counterfactual = CofaValue_s,
         actual = postas_day1 %>% 
           filter(pref == "Yamanashi") %>% 
           pull(sales_per)) %>% 
  pivot_longer(!date,
               names_to = "type",
               values_to = "sales")


write_csv(cofaplot_s, here::here("03_build/Counterfactual/output/sales_daily_data_for_plot.csv"))

## fit data (customers) --------

pred_customers <- function(data){
  data <- data %>% arrange(date)
  postas.cus_model <- lm(log(customers_per) ~ log(cumGZ + 1) + emergency + factor(pref) + factor(date),
                     data = data)
  
  pred_value <- exp(fitted(postas.cus_model))
  return(pred_value)
} 

FittedValue_c <- postas_day1 %>% 
  arrange(date) %>% 
  mutate(pred_customers = pred_customers(postas_day1)) %>% 
  filter(pref == "Yamanashi") %>% 
  select(date, pred_customers)


## cofa data (customers) ----

postas.cus_model <- lm(log(customers_per) ~ log(cumGZ + 1) + emergency + factor(pref) + factor(date),
                           data = postas_day1)

CofaValue_c <- rep(NA, 851)
j <- 1
for (i in 1:nrow(postas_day1)){
  if (postas_day1$pref[i] == "Yamanashi"){
    pred_value <- ggpredict(postas.cus_model,
                            terms = "cumGZ",
                            type = "fe",
                            condition = c(date = as.character(postas_day1$date[i]),
                                          pref = postas_day1$pref[i],
                                          emergency = postas_day1$emergency[i]),
                            back.transform = TRUE) %>% 
      filter(x == 0)
    
    CofaValue_c[j] <- pred_value$predicted[1]
    j <- j + 1
  }else{
  }
}

cofaplot_c <- FittedValue_c %>% 
  rename(fitted = pred_customers) %>% 
  mutate(counterfactual = CofaValue_c,
         actual = postas_day1 %>% 
           filter(pref == "Yamanashi") %>% 
           pull(customers_per)) %>% 
  pivot_longer(!date,
               names_to = "type",
               values_to = "customers")


write_csv(cofaplot_c, here::here("03_build/Counterfactual/output/customers_daily_data_for_plot.csv"))


## Findings (sales)---------

GZeffect_s <- cofaplot_s %>% 
  filter(date >= "2020-07-17" & date <= "2021-04-30") 

diff <- GZeffect_s$sales[GZeffect_s$type == "actual"] -
  GZeffect_s$sales[GZeffect_s$type == "counterfactual"]

## Approximately 3.52 million JPY in sale per restaurant was created during the period (2020-07-17 to 2021-04-30)
sum(diff, na.rm = TRUE)
## Approximately 371 thousand JPY in monthly sale per restaurant was created during the period (2020-07-17 to 2021-04-30)
sum(diff, na.rm = TRUE)/9.5

#During the period (2020-07-17 to 2021-04-30), sales increased by approximately 14%. 
rate <- sum(GZeffect_s$sales[GZeffect_s$type == "actual"], na.rm = TRUE) /
  sum(GZeffect_s$sales[GZeffect_s$type == "counterfactual"], na.rm = TRUE)
rate 

## Findings (customers)---------

GZeffect_c <- cofaplot_c %>% 
  filter(date >= "2020-07-17" & date <= "2021-04-30") 

diffc <- GZeffect_c$customers[GZeffect_c$type == "actual"] -
  GZeffect_c$customers[GZeffect_c$type == "counterfactual"]

## Number of customers per restaurant increased by approximately 3099, during the period (2020-07-17 to 2021-04-30)
sum(diffc, na.rm = TRUE)
## 326 customers was created per month
sum(diffc, na.rm = TRUE)/9.5

#During the period (2020-07-17 to 2021-04-30), number of customers increased by approximately 32%. 
ratec <- sum(GZeffect_c$customers[GZeffect_c$type == "actual"], na.rm = TRUE) /
  sum(GZeffect_c$customers[GZeffect_c$type == "counterfactual"], na.rm = TRUE)
ratec 


# Economic effects (weeky)-------


cofaplot_sw <- cofaplot_s %>% 
  mutate(week = floor_date(date,
                           "week",
                           week_start = getOption("lubridate.week.start", 1))) %>% 
  group_by(week, type) %>% 
  summarize(sales = sum(sales)) %>% 
  ungroup()
  
  
cofaplot_cw <- cofaplot_c %>% 
  mutate(week = floor_date(date,
                           "week",
                           week_start = getOption("lubridate.week.start", 1))) %>% 
  group_by(week, type) %>% 
  summarize(customers = sum(customers)) %>% 
  ungroup()


write_csv(cofaplot_sw, here::here("03_build/Counterfactual/output/sales_weekly_data_for_plot.csv"))
write_csv(cofaplot_cw, here::here("03_build/Counterfactual/output/customers_weekly_data_for_plot.csv"))

