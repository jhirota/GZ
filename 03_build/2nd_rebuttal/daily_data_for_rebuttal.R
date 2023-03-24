library(lubridate)
library(tidyverse)

# data load --------

NHK <- read_csv("02_bring/Covid_cases/data/nhk_news_covid19_prefectures_daily_data(1).csv")
# Gmobility_2020 <- read_csv("02_bring/Google_mobility/data/2020_JP_Region_Mobility_Report(1).csv") 
# Gmobility_2021 <- read_csv("02_bring/Google_mobility/data/2021_JP_Region_Mobility_Report.csv") 
emergency <- read_csv("03_build/GZ_covid/output/pref_bet_day_COVID_GZ.csv")

dummy <- read_excel("02_bring/Dummy_vars/data/Dummies_edited0125.xlsx")

# GZdata_raw <- read_csv("02_bring/GZlist/data/GZlist.csv")
GZdata <- read_csv("03_build/GZlist/output/GZlist_timeseries.csv")

postas <- read_csv("03_build/Postas/output/postas_daily_data.csv")

# weather2019 <- read_csv("02_bring/Weather/data/weather_data_6pref2019_rev.csv")
# weather2020 <- read_csv("02_bring/Weather/data/weather_data_6pref2020_rev.csv")

weather <- read_csv("03_build/GZ_covid/output/weather_pref.csv")

# daily_data_raw <- read_csv("03_build/GZ_covid/output/pref_bet_day_COVID_GZ.csv")

testdata <- read.csv("02_bring/Tests/data/testmerge.csv")

# data build----------

## NHK
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
  filter(date <= 18747) %>% # till April 30th 
  select(-c(newdeath_day, total_death))

## GZ
# ・週別でleft_joinしてもOK。キーとなる日だけ注意する。

GZdata2 <- GZdata %>%
  group_by(Date_approval)%>%
  summarize(GZnew = sum(N))%>%
  mutate(pref = "Yamanashi",
         Date_approval = as.Date(Date_approval)) %>% 
  ungroup() %>% 
  complete(Date_approval = seq.Date(min(Date_approval), max(Date_approval), by="day"), fill = list(GZnew = 0)) %>% 
  group_by(pref) %>% 
  mutate(cumGZ = cumsum(GZnew)) %>% 
  rename(date = Date_approval)

## postas
postas <- postas %>% 
  select(date, pref, sales_per, customers_per)

## emergency
emergency <- emergency %>% 
  select(date, prefcode, pref, emergency)

## dummy
dummy <- dummy %>% 
  rename(pref = Pref,
         date = day) %>% 
  mutate(date = as.Date(date))

## test
Testdata <- testdata %>% 
  select(Date, Pref, noftests) %>% 
  rename(date = Date,
         pref = Pref) %>% 
  mutate(date = gsub("\\T.*", "", date),
         date = as.Date(gsub("/", "-", date)),
         noftests = replace_na(noftests, 0),
         pref = as.character(pref) %>% str_replace_all(c(山梨県 = "Yamanashi",
                                                            茨城県 = "Ibaraki",
                                                            栃木県 = "Tochigi",
                                                            群馬県 = "Gunma",
                                                            長野県 = "Nagano",
                                                            静岡県 = "Shizuoka"))) %>% 
  complete(date,
           pref,
           fill = list(noftests = 0)) 

# merge to build master data-----------
data <- NHK %>% 
  dplyr::left_join(., GZdata2, by = c("date","pref")) %>% 
  tidyr::replace_na(., list(GZnew = 0, cumGZ = 0)) %>% 
  dplyr::left_join(., postas, by = c("date","pref")) %>% 
  dplyr::left_join(., weather, by = c("date","pref")) %>% 
  dplyr::left_join(., emergency, by = c("date","prefcode","pref")) %>% 
  dplyr::left_join(., dummy, by = c("date","pref")) %>% 
  tidyr::replace_na(., list(dummy_school_closure = 0, dummy_gathering_restriction = 0)) %>% 
  dplyr::left_join(., Testdata, by = c("date","pref")) %>% 
  tidyr::replace_na(., list(noftests = 0)) 




