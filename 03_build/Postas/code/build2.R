library(tidyverse)


# data load ------

wdata <- read_csv("03_build/Weather/output/weather data 6pref 2019_final.csv")
data2 <- read_csv("03_build/Weather/output/weather_location.csv") #2020
weekSIR3 <- read_csv("03_build/Postas/output/weekSIR3.csv")

# Daily data making ------
wdata$Date <- as.Date(wdata$Date)
data2$Date <- as.Date(data2$Date)
colnames(wdata)[5] <- "sum_rain"

weatherfinal <- rbind(wdata, data2) %>% 
  select(c(2,4,5,7)) %>% 
  group_by(Date, Pref) %>% 
  summarize_all(mean) %>% 
  ungroup()

Prefs <- c("Gunma", "Ibaragi", "Yamanashi", "Tochigi", "Nagano", "Shizuoka")
postas_day <- left_join(postas2, postas.cus2, by = c("集計対象営業日", "pref")) %>% 
  filter(pref %in% Prefs) %>% 
  select(集計対象営業日, pref, Sales, customers) 
colnames(postas_day)[1:2]<-c("Date", "Pref")

data9 <- read.csv("03_build/Pref_covid/output/pref_bet_day_COVID_GZ.csv") %>% 
  select(date, pref, newcase_day, total_case, newdeath_day, total_death, emergency, GZnew, cumGZ, newcaseday14) 
data9$date <- as.Date(data9$date)
data9$pref <- gsub("Ibaraki", "Ibaragi", data9$pref)

postas_day1 <- left_join(postas_day, weatherfinal, by = c("Date", "Pref"))  %>% 
  left_join(data9, by = c("Date" = "date", "Pref" = "pref"))

postas_day1[is.na(postas_day1)] <- 0

postas_day1$avg_temp_q <- (postas_day1$avg_temp)^2 
colnames(postas_day1)[colnames(postas_day1) == "Date"] <- "date"
colnames(postas_day1)[colnames(postas_day1) == "Pref"] <- "pref"
colnames(postas_day1)[which(names(postas_day1) == "Sales")] <- "sales"

postas_day1$lsales <- log(postas_day1$sales)
postas_day1$lcustomers <- log(postas_day1$customers)

postas_day1$sales_per <- NA
postas_day1$sales_per[postas_day1$pref == "Yamanashi"] <- postas_day1$sales[postas_day1$pref == "Yamanashi"] / 18
postas_day1$sales_per[postas_day1$pref == "Gunma"] <- postas_day1$sales[postas_day1$pref == "Gunma"] / 100
postas_day1$sales_per[postas_day1$pref == "Tochigi"] <- postas_day1$sales[postas_day1$pref == "Tochigi"] / 58
postas_day1$sales_per[postas_day1$pref == "Ibaragi"] <- postas_day1$sales[postas_day1$pref == "Ibaragi"] / 86
postas_day1$sales_per[postas_day1$pref == "Nagano"] <- postas_day1$sales[postas_day1$pref == "Nagano"] / 68
postas_day1$sales_per[postas_day1$pref == "Shizuoka"] <- postas_day1$sales[postas_day1$pref == "Shizuoka"] / 116

postas_day1$customers_per <- NA
postas_day1$customers_per[postas_day1$pref == "Yamanashi"] <- postas_day1$customers[postas_day1$pref == "Yamanashi"] / 18
postas_day1$customers_per[postas_day1$pref == "Gunma"] <- postas_day1$customers[postas_day1$pref == "Gunma"] / 100
postas_day1$customers_per[postas_day1$pref == "Tochigi"] <- postas_day1$customers[postas_day1$pref == "Tochigi"] / 58
postas_day1$customers_per[postas_day1$pref == "Ibaragi"] <- postas_day1$customers[postas_day1$pref == "Ibaragi"] / 86
postas_day1$customers_per[postas_day1$pref == "Nagano"] <- postas_day1$customers[postas_day1$pref == "Nagano"] / 68
postas_day1$customers_per[postas_day1$pref == "Shizuoka"] <- postas_day1$customers[postas_day1$pref == "Shizuoka"] / 116


write.csv(postas_day1, "03_build/Postas/output/postas_daily_data.csv", row.names = FALSE)

# weekly data making (2019-2021)----------

weatherfinal1 <- left_join(weatherfinal, postas_day, by = c("Date","Pref"))

weatherfinal1$week <- floor_date(weatherfinal$Date,
                                 "week",
                                 week_start = getOption("lubridate.week.start", 1))

weekweather1 <- weatherfinal1 %>% 
  group_by(week, Pref) %>% 
  summarize(rain = sum(sum_rain)) %>% 
  ungroup()
weekweather2 <- weatherfinal1 %>% 
  group_by(week, Pref) %>% 
  summarize(avg_temp = mean(avg_temp)) %>% 
  ungroup()
weekweather3 <- weatherfinal1 %>% 
  group_by(week, Pref) %>% 
  summarize_at(vars(4, 5), funs(sum(., na.rm=TRUE))) %>% 
  ungroup()

weekweather <- left_join(weekweather1, weekweather2, by = c("week", "Pref")) %>% 
  left_join(weekweather3)
colnames(weekweather)[2]<-"pref"

data8 <- weekSIR3 %>% 
  select(week, pref, newcaseday, GZnew, cumGZ, emergency)
postas_week <- left_join(weekweather, data8, by = c("week", "pref")) %>% 
  filter(week != "2018-12-24")

postas_week[is.na(postas_week)] <- 0

postas_week$avg_temp_q <- (postas_week$avg_temp)^2

write.csv(postas_week, "03_build/Postas/output/postas_weekly_data.csv", row.names = FALSE)

