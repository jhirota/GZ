library(lubridate)
library(tidyverse)

# data load --------

NHK <- read_csv("02_bring/Covid_cases/data/nhk_news_covid19_prefectures_daily_data(1).csv")
Gmobility_2020 <- read_csv("02_bring/Google_mobility/data/2020_JP_Region_Mobility_Report(1).csv") 
Gmobility_2021 <- read_csv("02_bring/Google_mobility/data/2021_JP_Region_Mobility_Report.csv") 
emergency <- read_csv("02_bring/Covid_cases/data/GZ_COVID.csv")

GZdata <- read_csv(here::here("03_build/GZlist/output/GZlist_timeseries.csv"))

weather2019 <- read_csv("02_bring/Weather/data/weather_data_6pref2019_rev.csv")
weather2020 <- read_csv("02_bring/Weather/data/weather_data_6pref2020_rev.csv")

# NHK data clean ----

NHK <- NHK %>% 
  select(!各地の直近1週間の人口10万人あたりの感染者数) %>% 
  rename(date = "日付",
         prefcode = "都道府県コード",
         pref = "都道府県名",
         newcase_day = "各地の感染者数_1日ごとの発表数",
         total_case = "各地の感染者数_累計",
         newdeath_day = "各地の死者数_1日ごとの発表数" ,
         total_death = "各地の死者数_累計") %>% 
  mutate(pref = str_replace_all(pref,
                                c(山梨県 = "Yamanashi",
                                     茨城県 = "Ibaraki",
                                     栃木県 = "Tochigi",
                                     群馬県 = "Gunma",
                                     長野県 = "Nagano",
                                     静岡県 = "Shizuoka")),
         date = as.Date(date)) %>% 
  filter(pref %in% c("Yamanashi",
                     "Ibaraki",
                     "Tochigi",
                     "Gunma",
                     "Nagano",
                     "Shizuoka")) %>% 
  filter(date <= 18747) # till April 30th


# Goolmobility data clean -------

Gmob_clean <- function(data){
  data <- data %>% 
    rename(pref = "sub_region_1",
           retail_and_recreation = "retail_and_recreation_percent_change_from_baseline",
           grocery_and_pharmacy = "grocery_and_pharmacy_percent_change_from_baseline",
           parks = "parks_percent_change_from_baseline",
           transit_stations = "transit_stations_percent_change_from_baseline",
           workplaces = "workplaces_percent_change_from_baseline",
           residential = "residential_percent_change_from_baseline") %>% 
    select(pref, date, retail_and_recreation, grocery_and_pharmacy, parks,
           transit_stations, workplaces, residential) %>% 
    filter(pref %in% c("Yamanashi", "Ibaraki", "Tochigi", "Gunma",
                       "Nagano", "Shizuoka"),
           date <= 18747)# till April 30th
  
  return(data)
}

Gmobility_2020 <- Gmob_clean(Gmobility_2020)
Gmobility_2021 <- Gmob_clean(Gmobility_2021)
Gmob <- rbind(Gmobility_2020, Gmobility_2021)


# Emergency data clean------

emergency <- emergency %>% 
  select(Pref, Date, emergency) %>% 
  rename(pref = "Pref",
         date = "Date") %>% 
  mutate(pref = str_replace_all(pref,
                                c(山梨県 = "Yamanashi",
                                     茨城県 = "Ibaraki",
                                     栃木県 = "Tochigi",
                                     群馬県 = "Gunma",
                                     長野県 = "Nagano",
                                     静岡県 = "Shizuoka")),
         date = as.Date(date))


# GZ data clean --------
GZdata2 <- GZdata %>%
  group_by(Date_approval)%>%
  summarize(GZnew = sum(N))%>%
  mutate(pref = "Yamanashi",
         Date_approval = as.Date(Date_approval)) %>% 
  ungroup() %>% 
  complete(Date_approval = seq.Date(min(Date_approval), max(Date_approval), by="day"), fill = list(GZnew = 0)) %>% 
  group_by(pref) %>% 
  mutate(cumGZ = cumsum(GZnew))

# weather data clean -------------

weatherclean <- function(data){
  data <- data %>% 
    rename(date = 1) %>% 
    slice(-1)
  
  cutfunc <- function(x){
    strsplit(x, "[...]")[[1]][1]
  }
  
  temperature <- data %>% 
    pivot_longer(cols = seq(3,33, by = 2),
                 names_to = "city",
                 values_to = "temp") %>% 
    select(date, city, temp) %>% 
    mutate(city = map(city, cutfunc))
  
  rain <- data %>% 
    pivot_longer(cols = seq(4,34, by = 2),
                 names_to = "city",
                 values_to = "rain") %>% 
    select(date, city, rain)%>% 
    mutate(city = map(city, cutfunc))
  
  weatherdata <- left_join(temperature, rain, by = c("date", "city")) %>% 
    mutate(date = as.Date(date),
           temp = as.numeric(temp),
           rain = as.numeric(rain))
  
  return(weatherdata)
}

city2pref <- function(data){
  data$city <- gsub("(前橋|前橋.1)", "Gunma", data$city)
  data$city <- gsub("(伊勢崎|伊勢崎.1)", "Gunma", data$city)
  data$city <- gsub("(長野|長野.1)", "Nagano", data$city)
  data$city <- gsub("(松本|松本.1)", "Nagano", data$city)
  data$city <- gsub("(上田|上田.1)", "Nagano", data$city)
  data$city <- gsub("(飯田|飯田.1)", "Nagano", data$city)
  data$city <- gsub("(甲府|甲府.1)", "Yamanashi", data$city)
  data$city <- gsub("(河口湖|河口湖.1)", "Yamanashi", data$city)
  data$city <- gsub("(浜松|浜松.1)", "Shizuoka", data$city)
  data$city <- gsub("(静岡|静岡.1)", "Shizuoka", data$city)
  data$city <- gsub("(富士|富士.1)", "Shizuoka", data$city)
  data$city <- gsub("(つくば.館野.|つくば.館野..1)", "Ibaraki", data$city)
  data$city <- gsub("(水戸|水戸.1)", "Ibaraki", data$city)
  data$city <- gsub("(日立|日立.1)", "Ibaraki", data$city)
  data$city <- gsub("(宇都宮|宇都宮.1)", "Tochigi", data$city)
  data$city <- gsub("(小山|小山.1)", "Tochigi", data$city)
  
  data <- rename(data, pref = "city")
  return(data)
}

weatherdata <- rbind(weatherclean(weather2019) %>% 
                        city2pref(),
                      weatherclean(weather2020) %>% 
                        city2pref()) %>% 
  group_by(date, pref) %>% 
  summarise(avg_temp = mean(temp),
            avg_rain = mean(rain))

write_csv(weatherdata, here::here("03_build/GZ_covid/output/weather_pref.csv"))

# data merge (GZ&COVID)------------

C2F <- function(c){
  f <- (9/5) * c + 32
  return(f)
}

preCOVID_GZ <- left_join(x = NHK,
                         y = Gmob,
                         by = c("pref", "date")) %>% 
  left_join(emergency,
            by = c("pref", "date")) %>% 
  left_join(GZdata2, by = c("date" = "Date_approval", "pref")) %>% 
  arrange(date) %>% 
  mutate(date = as.Date(date),
         GZnew = replace_na(GZnew, 0)) %>% 
  group_by(pref) %>% 
  mutate(date14 = lead(date, order_by = date, n = 14),
         newcaseday14 = if_else(date + 14 == date14,
                                lead(newcase_day, order_by = date),
                                NA_real_),
         date1 = lead(date, order_by = date, n = 1),
         newcaseday1 = if_else(date + 1 == date1,
                               lead(newcase_day, order_by = date),
                               NA_real_),
         cumGZ = cumsum(GZnew)) %>% 
  ungroup() %>%
  left_join(weatherdata,
            by = c("date", "pref")) %>% 
  mutate(week = floor_date(date,
                           "week",
                           week_start = getOption("lubridate.week.start", 1))) %>% 
  arrange(week) %>%
  mutate(weeknum = c(rep(3, 4*6),rep(4:69, each = 7*6), rep(70, 5*6)),
         treat = if_else(pref == "Yamanashi",
                         "Yamanashi",
                         "Neighboring_prefs"),
         avg_temp = C2F(avg_temp)) 


write_csv(preCOVID_GZ, here::here("03_build/GZ_covid/output/pref_bet_day_COVID_GZ.csv"))

