#weekSIRの作り方復元不可能

library(tidyverse)
library(lubridate)

weather <- read.csv("03_build/Weather/output/weather_pref.csv", header = TRUE)
weekmain <- read.csv("03_build/Pref_covid/output/weekly_vresas.csv", header = TRUE)
inflow <- read.csv("県別流入リスク.csv", header = TRUE)

weather$date <- as.Date(weather$date)

weather_2020 <- weather %>% 
  filter(date >= "2019-12-30")

weather_2020 <- weather_2020 %>% 
  arrange(date)

a <- rep(seq(as.Date("2019-12-30"), as.Date("2021-04-30"), by = "week"), each = 42)
a <- data.frame(a)
a2 <- a[1:2928,]
weather_2020$week <- a2

#abc <- floor_date(weather_2020$date, "week",
                    #      week_start = getOption("lubridate.week.start", 1))

weather_2020_week <- weather_2020 %>% 
  group_by(pref, week) %>% 
  summarize(avg_temp = mean(avg_temp),
            avg_rain = mean(avg_rain))


b <- rep(seq(as.Date("2020-01-13"), as.Date("2021-04-30"), by = "week"), each = 6)
#b <- data.frame(b)
weekmain$week <- b

##都道府県名をローマ字に変換
colnames(inflow)[which(colnames(inflow) == "都道府県")] <- "pref"
inflow$pref <- lapply(inflow$pref, gsub, pattern="山梨県", replacement = "Yamanashi")
inflow$pref <- lapply(inflow$pref, gsub, pattern="栃木県", replacement = "Tochigi")
inflow$pref <- lapply(inflow$pref, gsub, pattern="群馬県", replacement = "Gunma")
inflow$pref <- lapply(inflow$pref, gsub, pattern="茨城県", replacement = "Ibaragi")
inflow$pref <- lapply(inflow$pref, gsub, pattern="静岡県", replacement = "Shizuoka")
inflow$pref <- lapply(inflow$pref, gsub, pattern="長野県", replacement = "Nagano")
inflow$pref <- as.character(inflow$pref)

#mergeする
inflow$week <- as.Date(inflow$week)

weekfinal <- left_join(weekmain, weather_2020_week, by = c("pref", "week"))
weekfinal2 <- left_join(weekfinal, inflow, by = c("pref", "week"))
weekfinal3 <- weekfinal2 %>% 
  select(-X.y, -in_risk_lag1, -in_risk_lag2, -X.x)

weekfinal_lag2 <- weekfinal3 %>% 
  mutate(newcaset.1 = lead(newcaseday, n = 6),
         newcaset.2 = lead(newcaseday, n=12))

#累積を出す
weekfinal_lag2$cumcases <- ave(weekfinal_lag2$newcaseday, weekfinal_lag2$pref, FUN = cumsum)#convert newcases to cumulative cases 

weekfinal_lag2$susceptible <- weekfinal_lag2$population - weekfinal_lag2$cumcases


weekfinal_lag2 <- weekfinal_lag2 %>% 
  mutate(avg_temp_q = avg_temp^2,
         rain_q = rain^2)

write.csv(weekfinal_lag2, "weekSIR.csv")
