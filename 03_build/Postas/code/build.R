library(readxl)
library(tidyverse)
library(reshape2)
library(lubridate)

#data load
data <- read.csv("03_build/weekSIR/output/weekSIR2.csv")
postas <- read_excel("02_bring/Postas/data/国立大学法人東京大学公共政策大学院データ提供.xlsx")
postas.cus <- read_excel("02_bring/Postas/data/国立大学法人東京大学公共政策大学院データ提供.xlsx",2)
postas4 <- read_excel("02_bring/Postas/data/国立大学法人東京大学公共政策大学院データ提供.xlsx",3)
postas.cus4 <- read_excel("02_bring/Postas/data/国立大学法人東京大学公共政策大学院データ提供.xlsx",4)


#postas cleaning ------

postas$集計対象営業日 <- as.Date(postas$集計対象営業日)
colnames(postas) <- gsub("_売上合計", "", colnames(postas))


postas2 <- melt(data = postas,
                id.vars = "集計対象営業日",
                measure.vars = colnames(postas)[8:54],
                variable.name = "pref",
                value.name = "Sales")


postas2$pref <- gsub("群馬県", "Gunma", postas2$pref)
postas2$pref <- gsub("山梨県", "Yamanashi", postas2$pref)
postas2$pref <- gsub("栃木県", "Tochigi", postas2$pref)
postas2$pref <- gsub("茨城県", "Ibaragi", postas2$pref)
postas2$pref <- gsub("長野県", "Nagano", postas2$pref)
postas2$pref <- gsub("静岡県", "Shizuoka", postas2$pref)

postas2$week <- floor_date(postas2$集計対象営業日,
                           "week",
                           week_start = getOption("lubridate.week.start", 1))


postas3 <- postas2 %>% 
  group_by(pref, week) %>% 
  summarize(sales = sum(Sales)) %>% 
  ungroup()

#postas.cus cleaning ------

postas.cus$集計対象営業日 <- as.Date(postas.cus$集計対象営業日)

colnames(postas.cus) <- gsub("_客数合計", "", colnames(postas.cus))


postas.cus2 <- melt(data = postas.cus,
                    id.vars = "集計対象営業日",
                    measure.vars = colnames(postas.cus)[8:54],
                    variable.name = "pref",
                    value.name = "customers")

postas.cus2$pref <- gsub("群馬県", "Gunma", postas.cus2$pref)
postas.cus2$pref <- gsub("山梨県", "Yamanashi", postas.cus2$pref)
postas.cus2$pref <- gsub("栃木県", "Tochigi", postas.cus2$pref)
postas.cus2$pref <- gsub("茨城県", "Ibaragi", postas.cus2$pref)
postas.cus2$pref <- gsub("長野県", "Nagano", postas.cus2$pref)
postas.cus2$pref <- gsub("静岡県", "Shizuoka", postas.cus2$pref)

postas.cus2$week <- floor_date(postas.cus2$集計対象営業日,
                               "week",
                               week_start = getOption("lubridate.week.start", 1))


postas.cus3 <- postas.cus2 %>% 
  group_by(pref, week) %>% 
  summarize(customers = sum(customers)) %>% 
  ungroup()

#postas data merge --------

postas_new <- left_join(postas3, postas.cus3, by = c("week", "pref"))

data$week <- as.Date(data$week)
weekSIR3 <- left_join(x = data,
                      y = postas_new,
                      by = c("week", "pref"))

weekSIR3$lsales <- log(weekSIR3$sales)
weekSIR3$lcustomers <- log(weekSIR3$customers)


# Scaled by # of registered shops in the prefecture
weekSIR3$sales_per <- NA
weekSIR3$sales_per[weekSIR3$pref == "Yamanashi"] <- weekSIR3$sales[weekSIR3$pref == "Yamanashi"] / 18
weekSIR3$sales_per[weekSIR3$pref == "Gunma"] <- weekSIR3$sales[weekSIR3$pref == "Gunma"] / 100
weekSIR3$sales_per[weekSIR3$pref == "Tochigi"] <- weekSIR3$sales[weekSIR3$pref == "Tochigi"] / 58
weekSIR3$sales_per[weekSIR3$pref == "Ibaragi"] <- weekSIR3$sales[weekSIR3$pref == "Ibaragi"] / 86
weekSIR3$sales_per[weekSIR3$pref == "Nagano"] <- weekSIR3$sales[weekSIR3$pref == "Nagano"] / 68
weekSIR3$sales_per[weekSIR3$pref == "Shizuoka"] <- weekSIR3$sales[weekSIR3$pref == "Shizuoka"] / 116

# Scaled by # of registered shops in the prefecture
weekSIR3$customers_per <- NA
weekSIR3$customers_per[weekSIR3$pref == "Yamanashi"] <- weekSIR3$customers[weekSIR3$pref == "Yamanashi"] / 18
weekSIR3$customers_per[weekSIR3$pref == "Gunma"] <- weekSIR3$customers[weekSIR3$pref == "Gunma"] / 100
weekSIR3$customers_per[weekSIR3$pref == "Tochigi"] <- weekSIR3$customers[weekSIR3$pref == "Tochigi"] / 58
weekSIR3$customers_per[weekSIR3$pref == "Ibaragi"] <- weekSIR3$customers[weekSIR3$pref == "Ibaragi"] / 86
weekSIR3$customers_per[weekSIR3$pref == "Nagano"] <- weekSIR3$customers[weekSIR3$pref == "Nagano"] / 68
weekSIR3$customers_per[weekSIR3$pref == "Shizuoka"] <- weekSIR3$customers[weekSIR3$pref == "Shizuoka"] / 116

write.csv(weekSIR3, "03_build/Postas/output/weekSIR3.csv", row.names = FALSE)

# postas data for plot ------

postas_new2 <- postas_new

postas_new2$treat <- ifelse(postas_new2$pref == "Yamanashi", "山梨県", "近隣5県")
postas_new2$sales_per <- NA
postas_new2$sales_per[postas_new2$pref == "Yamanashi"] <- postas_new2$sales[postas_new2$pref == "Yamanashi"] / 18
postas_new2$sales_per[postas_new2$pref == "Gunma"] <- postas_new2$sales[postas_new2$pref == "Gunma"] / 100
postas_new2$sales_per[postas_new2$pref == "Tochigi"] <- postas_new2$sales[postas_new2$pref == "Tochigi"] / 58
postas_new2$sales_per[postas_new2$pref == "Ibaragi"] <- postas_new2$sales[postas_new2$pref == "Ibaragi"] / 86
postas_new2$sales_per[postas_new2$pref == "Nagano"] <- postas_new2$sales[postas_new2$pref == "Nagano"] / 68
postas_new2$sales_per[postas_new2$pref == "Shizuoka"] <- postas_new2$sales[postas_new2$pref == "Shizuoka"] / 116

postas_new2$customers_per <- NA
postas_new2$customers_per[postas_new2$pref == "Yamanashi"] <- postas_new2$customers[postas_new2$pref == "Yamanashi"] / 18
postas_new2$customers_per[postas_new2$pref == "Gunma"] <- postas_new2$customers[postas_new2$pref == "Gunma"] / 100
postas_new2$customers_per[postas_new2$pref == "Tochigi"] <- postas_new2$customers[postas_new2$pref == "Tochigi"] / 58
postas_new2$customers_per[postas_new2$pref == "Ibaragi"] <- postas_new2$customers[postas_new2$pref == "Ibaragi"] / 86
postas_new2$customers_per[postas_new2$pref == "Nagano"] <- postas_new2$customers[postas_new2$pref == "Nagano"] / 68
postas_new2$customers_per[postas_new2$pref == "Shizuoka"] <- postas_new2$customers[postas_new2$pref == "Shizuoka"] / 116

write.csv(postas_new2, "03_build/Postas/output/postas_2019_2021.csv", row.names = FALSE)



# Update datasets ---------

# data load

wdata <- read_csv("03_build/Weather/output/weather data 6pref 2019_final.csv")
data2 <- read_csv("03_build/Weather/output/weather_location.csv") #2020
weekSIR3 <- read_csv("03_build/Postas/output/weekSIR3.csv")

# Daily data making 
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

# newdummy
dummy <- read_excel(here::here("02_bring/Dummy_vars/data/Dummies_edited1115.xlsx")) %>% 
  rename(pref = Pref,
         date = day) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate_if(is.character, 
            str_replace_all, pattern = "Ibaraki", replacement = "Ibaragi")

# data merge

postas_day1 <- left_join(x = postas_day1,
                         y = dummy, 
                         by = c("date", "pref")) %>% 
  mutate(dummy_school_closure = replace_na(dummy_school_closure, 0),
         dummy_gathering_restriction = replace_na(dummy_gathering_restriction, 0))


write_csv(postas_day1, "03_build/Postas/output/postas_daily_data.csv")

# weekly data (2019-2021)

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




