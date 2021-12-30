library(tidyverse)
library(ggeffects)

# covid-19 counterfactual plot-----
weekSIR3 <- read_csv("03_build/Postas/output/weekSIR3.csv") %>% 
  arrange(week)

covidmodel <- lm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency +
                   log(noftestst.2 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1) + 
                   factor(pref) + factor(week), data = weekSIR3)


weekSIR3$pred_covid <- NA
weekSIR3$pred_covid[1:nrow(weekSIR3)] <- exp(fitted(covidmodel))

# counterfactual
Yamapred <- rep(NA, 68)
j <- 1

for (i in 1:nrow(weekSIR3)){
  if (weekSIR3$pref[i] == "Yamanashi"){
    pred <- ggpredict(covidmodel,
                      terms = "cumGZ",
                      type = "fe",
                      condition = c(week = as.character(weekSIR3$week[i]),
                                    pref = weekSIR3$pref[i],
                                    noftestst.2 = weekSIR3$noftestst.2[i],
                                    agrgt_potecovid_lag2 = weekSIR3$agrgt_potecovid_lag2[i],
                                    avg_temp_q = weekSIR3$avg_temp_q[i],
                                    rain = weekSIR3$rain[i],
                                    customers_per = weekSIR3$customers_per[i],
                                    emergency = weekSIR3$emergency[i]),
                      back.transform = TRUE)
    GZ0value <- pred %>% filter(x == 0)
    Yamapred[j] <- GZ0value$predicted[1]
    j <- j + 1
  }else{
  }
}


YamanashiTrend2 <- data.frame(week = levels(factor(weekSIR3$week)),
                             nofcases = weekSIR3$newcaseday[weekSIR3$pref == "Yamanashi"],
                             counterfactual = c(rep(NA, 2), Yamapred[1:66]),
                             fitted = c(rep(NA,2), weekSIR3$pred_covid[weekSIR3$pref == "Yamanashi"][1:66]))



Yamanashi1 <- YamanashiTrend2[,1:2]
Yamanashi2 <- YamanashiTrend2[,c(1,3)]
Yamanashi3 <- YamanashiTrend2[,c(1,4)]
Yamanashi1$type <-"actual"
Yamanashi2$type <- "counterfactual"
Yamanashi3$type <- "fitted"
names(Yamanashi2)[2] <- "nofcases"
names(Yamanashi3)[2] <- "nofcases"

newYamanashiTrend2 <- rbind(Yamanashi1, Yamanashi2, Yamanashi3) %>% 
  arrange(week)
newYamanashiTrend2$week <- as.Date(newYamanashiTrend$week)

write.csv(newYamanashiTrend2, "03_build/Counterfactual/output/cofa_covid2.csv")



# Economic effects counterfactual plot (daily) -------

postas_day1 <- read_csv("03_build/Postas/output/postas_daily_data.csv")

# same as dlmpostas2
postas_model <- lm(log(sales_per) ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + factor(pref) + factor(date), data = postas_day1)
#same as dlmpostas.cus2
postas.cus_model <- lm(log(customers_per) ~ log(cumGZ + 1)  + log(newcase_day + 1) + emergency + avg_temp_q + sum_rain + factor(pref) + factor(date), data = postas_day1)

postas_day1$pred_sales <- NA
postas_day1$pred_sales[1:nrow(postas_day1)] <- exp(fitted(postas_model))
postas_day1$pred_customers <- NA
postas_day1$pred_customers[1:nrow(postas_day1)] <- exp(fitted(postas.cus_model))

# counterfactual
Yamapred <- rep(NA, 912)
j <- 1

# takes time (20mins)
for (i in 1:nrow(postas_day1)){
  if (postas_day1$pref[i] == "Yamanashi"){
    pred <- ggpredict(postas_model,
                      terms = "cumGZ",
                      type = "fe",
                      condition = c(date = as.character(postas_day1$date[i]),
                                    pref = postas_day1$pref[i],
                                    newcase_day = postas_day1$newcase_day[i],
                                    avg_temp_q = postas_day1$avg_temp_q[i],
                                    sum_rain = postas_day1$sum_rain[i],
                                    emergency = postas_day1$emergency[i]),
                      back.transform = TRUE)
    GZ0value <- pred %>% filter(x == 0)
    Yamapred[j] <- GZ0value$predicted[1]
    j <- j + 1
  }else{
  }
}

Yamapred.cus <- rep(NA, 912)
j <- 1
# takes time (20mins)
for (i in 1:nrow(postas_day1)){
  if (postas_day1$pref[i] == "Yamanashi"){
    pred <- ggpredict(postas.cus_model,
                      terms = "cumGZ",
                      type = "fe",
                      condition = c(date = as.character(postas_day1$date[i]),
                                    pref = postas_day1$pref[i],
                                    newcase_day = postas_day1$newcase_day[i],
                                    avg_temp_q = postas_day1$avg_temp_q[i],
                                    sum_rain = postas_day1$sum_rain[i],
                                    emergency = postas_day1$emergency[i]),
                      back.transform = TRUE)
    GZ0value.cus <- pred %>% filter(x == 0)
    Yamapred.cus[j] <- GZ0value.cus$predicted[1]
    j <- j + 1
  }else{
  }
}

postas_day1$Sales_CofaGZ0 <- NA
postas_day1$customers_CofaGZ0 <- NA

postas_day1$Sales_CofaGZ0[postas_day1$pref == "Yamanashi"] <- Yamapred
postas_day1$customers_CofaGZ0[postas_day1$pref == "Yamanashi"] <- Yamapred.cus

postas_day2 <- postas_day1

write.csv(postas_day2, "03_build/Counterfactual/output/postas_day2.csv", row.names = FALSE)


Yamanashi1 <- data.frame(date = levels(factor(postas_day2$date)),
                         Sales = postas_day2$sales_per[postas_day2$pref == "Yamanashi"],
                         type = "actual")
Yamanashi2 <- data.frame(date = levels(factor(postas_day2$date)),
                         Sales = postas_day2$Sales_CofaGZ0[postas_day2$pref == "Yamanashi"],
                         type = "counterfactual")
Yamanashi3 <- data.frame(date = levels(factor(postas_day2$date)),
                         Sales = postas_day2$pred_sales[postas_day2$pref == "Yamanashi"],
                         type = "fitted")

YamanashiTrend <- rbind(Yamanashi1, Yamanashi2, Yamanashi3) %>%
  arrange(date)
YamanashiTrend$date <- as.Date(YamanashiTrend$date)

Yamanashi.cus1 <- data.frame(date = levels(factor(postas_day2$date)),
                             customers = postas_day2$customers_per[postas_day2$pref == "Yamanashi"],
                             type = "actual")
Yamanashi.cus2 <- data.frame(date = levels(factor(postas_day2$date)),
                             customers = postas_day2$customers_CofaGZ0[postas_day2$pref == "Yamanashi"],
                             type = "counterfactual")
Yamanashi.cus3 <- data.frame(date = levels(factor(postas_day2$date)),
                             customers = postas_day2$pred_customers[postas_day2$pref == "Yamanashi"],
                             type = "fitted")

YamanashiTrend.cus <- rbind(Yamanashi.cus1, Yamanashi.cus2, Yamanashi.cus3) %>%
  arrange(date)
YamanashiTrend.cus$date <- as.Date(YamanashiTrend.cus$date)



write.csv(YamanashiTrend, "03_build/Counterfactual/output/sales_daily_data_for_plot.csv", row.names = FALSE)
write.csv(YamanashiTrend.cus, "03_build/Counterfactual/output/customers_daily_data_for_plot.csv", row.names = FALSE)


# Findings---------
# sinceGZ <- YamanashiTrend %>% filter(date >= 18475) %>% filter(date <= 18747) #8月以降4月いっぱい
# a <- sinceGZ$Sales[sinceGZ$type == "actual"] -
#   sinceGZ$Sales[sinceGZ$type == "counterfactual"]
# sum(a, na.rm = TRUE)##単純計算で調査期間の2020年8月-2021年4月で1店舗あたり267万円の売上
# sum(a, na.rm = TRUE)/9 ##1店舗1月あたり最大29.6万円
# rate <- sum(sinceGZ$Sales[sinceGZ$type == "actual"], na.rm = TRUE) /
#   sum(sinceGZ$Sales[sinceGZ$type == "counterfactual"], na.rm = TRUE)
# rate #調査期間の9ヶ月で11.0%増加させた。


b <- YamanashiTrend.cus$customers[YamanashiTrend.cus$type == "actual"] -
  YamanashiTrend.cus$customers[YamanashiTrend.cus$type == "counterfactual"]
sum(b, na.rm = TRUE) ##1店舗あたり2286人増加をもたらした。


# For postas counterfactual plot (daily) -------
# counterfactual plot (week)--------

YamanashiTrend$week <- floor_date(YamanashiTrend$date,
                                  "week",
                                  week_start = getOption("lubridate.week.start", 1)) 

YamanashiTrend_w <- YamanashiTrend %>% 
  group_by(week, type) %>% 
  summarize(Sales = sum(Sales)) %>% 
  ungroup()

YamanashiTrend.cus$week <- floor_date(YamanashiTrend.cus$date,
                                      "week",
                                      week_start = getOption("lubridate.week.start", 1)) 

YamanashiTrend.cus_w <- YamanashiTrend.cus %>% 
  group_by(week, type) %>% 
  summarize(customers = sum(customers)) %>% 
  ungroup()


write.csv(YamanashiTrend_w, "03_build/Counterfactual/output/sales_weekly_data_for_plot.csv", row.names = FALSE)
write.csv(YamanashiTrend.cus_w, "03_build/Counterfactual/output/customers_weekly_data_for_plot.csv", row.names = FALSE)

