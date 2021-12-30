library(lubridate)
library(tidyverse)

#load data--------
yamaCOVID <- read_csv("03_build/Case_positive/output/市町村別感染者数.csv") 
GZdata <- read_csv("03_build/GZlist/output/GZlist_timeseries.csv")
preresdata <- read_csv("03_build/Vresas/output/restraunt_all_genre.csv")
preCOVID <- read_csv("03_build/GZ_covid/output/GZ_covid.csv")

#clean & merge data--------
GZdata2 <- GZdata%>%
  group_by(Date_approval)%>%
  summarize(GZnew = sum(sum))%>%
  mutate(pref = "Yamanashi")
GZdata2$Date_approval <- as.Date(GZdata2$Date_approval) #convert the type of objects
GZdata3 <- GZdata2 %>%  #generate the missing dates
  group_by(pref) %>% 
  complete(Date_approval = seq.Date(min(Date_approval), max(Date_approval), by="day"), fill = list(GZnew = 0))
GZdata3$cumGZ <- ave(GZdata3$GZnew, GZdata3$pref, FUN = cumsum)#convert newcases to cumulative cases 

## restraunt data-----------
preresdata2 <- preresdata %>% 
  pivot_longer(-c(week, X1), names_to = "pref", values_to = "resview") %>% 
  mutate(Week = week) %>% 
  separate(week, into = c("abandon", "weeknum"), sep = "第") %>% 
  select(-c(abandon, X1))
preresdata2["weeknum"] <- lapply(preresdata2["weeknum"], gsub, pattern="週", replacement = "")


write.csv(preresdata2, "03_build/Pref_covid/output/V-RESAS_restaurant.csv", row.names=FALSE)

##COVID ------------

preCOVID$date <- as.Date(preCOVID$date)

##merge daily data------------
preCOVID_GZ <- preCOVID %>% 
  left_join(GZdata3, by = c("date" = "Date_approval", "pref")) %>% 
  arrange(date)

preCOVID_GZ$newcaseday14 <- sapply(1:nrow(preCOVID_GZ), function(x) preCOVID_GZ$newcase_day[x+14*6])

preCOVID_GZ_final <- preCOVID_GZ %>% 
  replace_na(list(GZnew = 0,
                  cumGZ = 0)) %>% 
  mutate(code = factor(prefcode),
         Date_dummy = paste("D", date, sep = "_"),
         pref_dummy = paste("D", prefcode, sep = "_")) %>% 
  select(-code) 

# nofcases 1 day before 
preCOVID_GZ_final$COVID1 <- sapply(1:nrow(preCOVID_GZ_final), function(x) preCOVID_GZ_final$newcase_day[x+6])

write.csv(preCOVID_GZ_final, "03_build/Pref_covid/output/pref_bet_day_COVID_GZ.csv", row.names=FALSE)

#merge week data Hirota-san--------

## kenkan load ---

vresas <- read.csv("02_bring/Vresas/data/VRESAS_Mobility.csv")

# Data wrangling ----
preCOVID_GZ_final$Date <- as.Date(preCOVID_GZ_final$Date)
names(vresas)[2] <- "pref"
names(preCOVID_GZ_final)[3] <- "pref"

# Weeknum assignment ----
b <- c(rep(1:71), rep(1:71), rep(1:71), rep(1:71), rep(1:71), rep(1:71), rep(1:71), rep(1:71), rep(1:71))

preresdata2 <- preresdata2 %>% 
  arrange(pref) %>% 
  filter(Week != 2)
preresdata2$weeknum <- b

preCOVID_GZ_final.1 <- preCOVID_GZ_final %>% 
  arrange(pref) %>% 
  filter(pref != "Kanagawa") %>% 
  filter(pref != "Chiba") %>% 
  filter(pref != "Saitama") %>%
  filter(pref != "Tokyo")
a <- c(rep(3,4), rep(4:69, each = 7), rep(70, 5))
aa <- c(a, a, a, a, a, a)

preCOVID_GZ_final.1$weeknum <- aa

# Merge data ------
join3 <- preresdata2 %>% 
  select(pref, resview, weeknum)

preCOVID_GZ_final.1 <- preCOVID_GZ_final.1 %>% 
  left_join(y = join3, by = c("pref", "weeknum"))

# weekly data ----
newdata <- preCOVID_GZ_final.1 %>% 
  group_by(weeknum, pref) %>% 
  summarize_at(vars(3, 5, 14, 16), funs(sum(., na.rm=TRUE))) 
#summarize at "newcaseday", "newdeath", "GZnew", "newcaseday14"


newdata <- left_join(x = newdata,
                     y = preresdata2,
                     by = c("pref", "weeknum"))

cleandata <- newdata %>%
  left_join(y = vresas, by = c("pref", "Week"))

cleandata$cumGZ <- ave(cleandata$GZnew, cleandata$pref, FUN = cumsum)
cleandata <- cleandata[,c(7,1:5,12,6,8:11)]

write.csv(cleandata, "03_build/Pref_covid/output/weekly_vresas.csv", row.names=FALSE)


# First weekSIR data ---------

# data load
weather <- read.csv("03_build/Weather/output/weather_pref.csv", header = TRUE)
weekmain <- read.csv("03_build/Pref_covid/output/weekly_vresas.csv", header = TRUE)
inflow <- read.csv("県別流入リスク.csv", header = TRUE)


weather_2020 <- weather %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= "2019-12-30") %>% 
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

