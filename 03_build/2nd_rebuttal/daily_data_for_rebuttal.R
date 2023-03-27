library(lubridate)
library(tidyverse)
library(readxl)
library(RcppRoll)

# data load --------

NHK <- read_csv("02_bring/Covid_cases/data/nhk_news_covid19_prefectures_daily_data(1).csv")
# Gmobility_2020 <- read_csv("02_bring/Google_mobility/data/2020_JP_Region_Mobility_Report(1).csv") 
# Gmobility_2021 <- read_csv("02_bring/Google_mobility/data/2021_JP_Region_Mobility_Report.csv") 
emergency <- read_csv("03_build/GZ_covid/output/pref_bet_day_COVID_GZ.csv")

dummy <- read_excel("02_bring/Dummy_vars/data/Dummies_edited0125.xlsx")

GZdata <- read_csv("03_build/GZlist/output/GZlist_timeseries.csv")

Agoop_list <- ("02_bring/Agoop/data") %>% 
  list.files(full.names = T) %>% 
  lapply(read.csv)
pop47 <- read_csv("02_bring/Pop/data/Population_JPN.csv")
covid47 <- read_csv("02_bring/Covid_cases/data/nhk_news_covid19_prefectures_daily_data(1).csv")

postas <- read_csv("03_build/Postas/output/postas_daily_data.csv")

weather <- read_csv("03_build/GZ_covid/output/weather_pref.csv")

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
  mutate(Date_approval = as.Date(Date_approval)) %>% 
  ungroup() %>% 
  complete(Date_approval = seq.Date(min(Date_approval), max(Date_approval), by="day"), fill = list(GZnew = 0)) %>% 
  mutate(pref = "Yamanashi") %>% 
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

## weather
C2F <- function(c){
  f <- (9/5) * c + 32
  return(f)
}

weather <- weather %>% 
  dplyr::mutate(avg_temp = C2F(avg_temp))
  

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

## agoop 
cleanfunc <- function(data){
  data <- data %>% 
    rename(week = 1,
           weekday_flag = 2,
           daytime_flag = 3,
           pref_resid = 6,
           city = 7,
           pref = 8) %>% 
    group_by(week, pref, pref_resid, start_day, end_day) %>% 
    summarize(sum_pop = sum(as.integer(population_inflow))) %>% 
    ungroup()
}

cutfunc <- function(x){
  str_split(x, "[:]")[[1]][2]
}

mob6_47 <- rbind(cleanfunc(Agoop_list[[1]]),
                 cleanfunc(Agoop_list[[2]]),
                 cleanfunc(Agoop_list[[3]]),
                 cleanfunc(Agoop_list[[4]]),
                 cleanfunc(Agoop_list[[5]]), 
                 cleanfunc(Agoop_list[[6]]),
                 cleanfunc(Agoop_list[[7]]),
                 cleanfunc(Agoop_list[[8]]),
                 cleanfunc(Agoop_list[[9]]),
                 cleanfunc(Agoop_list[[10]]),
                 cleanfunc(Agoop_list[[11]]),
                 cleanfunc(Agoop_list[[12]])) %>% 
  select(-week) %>% 
  mutate(start_day = as.Date(ymd(start_day)),
         end_day = as.Date(ymd(end_day)),
         pref =  map(pref, cutfunc),
         pref = as.character(pref) %>% str_replace_all(c(山梨県 = "Yamanashi",
                                                            茨城県 = "Ibaraki",
                                                            栃木県 = "Tochigi",
                                                            群馬県 = "Gunma",
                                                            長野県 = "Nagano",
                                                            静岡県 = "Shizuoka")),
         pref_resid = map(pref_resid, cutfunc),
         pref_resid = as.character(pref_resid) %>% str_replace_all(c(山梨県 = "Yamanashi",
                                                                        茨城県 = "Ibaraki",
                                                                        栃木県 = "Tochigi",
                                                                        群馬県 = "Gunma",
                                                                        長野県 = "Nagano",
                                                                        静岡県 = "Shizuoka"))) 


## Covid data clean ----
covid47 <- covid47 %>%
  rename(date = 1,
         pref_resid = 3,
         newcase_resid = 4,
         newdeath_resid = 6) %>% 
  select(date, pref_resid, newcase_resid, newdeath_resid) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(week = floor_date(date, "week",
                           week_start = getOption("lubridate.week.start", 1))) %>% 
  # group_by(pref_resid, week) %>%  comment out because the unit of analysis here is daily.
  # summarize(across(c(newcase_resid, newdeath_resid),
  #                  sum)) %>%
  # ungroup() %>% 
  mutate(pref_resid = str_replace_all(pref_resid,
                                      c(山梨県 = "Yamanashi",
                                           茨城県 = "Ibaraki",
                                           栃木県 = "Tochigi",
                                           群馬県 = "Gunma",
                                           長野県 = "Nagano",
                                           静岡県 = "Shizuoka")))



## Population data clean --------
pop47 <- pop47 %>% 
  rename(pref_resid = 1,
         pop_resid = 2) %>% 
  mutate(pop_resid = pop_resid * 1000,
         pref_resid = str_replace_all(pref_resid,
                                      c(山梨県 = "Yamanashi",
                                           茨城県 = "Ibaraki",
                                           栃木県 = "Tochigi",
                                           群馬県 = "Gunma",
                                           長野県 = "Nagano",
                                           静岡県 = "Shizuoka")))

## build infectious (merge population flow & newcases)------

date_list <- seq(as.Date("2019-12-30"), as.Date("2021-06-20"), by = "day")
# 日付の一覧と都道府県の一覧を組み合わせてデータを作成
date_pref <- expand.grid(date = date_list, pref = unique(mob6_47$pref), pref_resid = unique(mob6_47$pref_resid))

agoop_daily_raw <- date_pref %>% 
  left_join(mob6_47, by = c("pref", "pref_resid")) %>% 
  filter(date >= start_day, date <= end_day) 

infectious_raw_bind <- agoop_daily_raw %>% 
  dplyr::left_join(., covid47, by = c("date", "pref_resid")) %>% 
  dplyr::select(-week) %>% 
  tidyr::replace_na(., list(newcase_resid = 0, newdeath_resid = 0)) %>% 
  dplyr::left_join(., pop47, by = c("pref_resid"))

infectious_raw <- infectious_raw_bind %>% 
  dplyr::mutate(infectious = sum_pop*newcase_resid/pop_resid) %>% 
  dplyr::group_by(date, pref) %>% 
  dplyr::summarise(sum_infectious = sum(infectious)) %>% 
  dplyr::ungroup()

#generate cumulative number of infectious population
infectious_sum2week_raw <- infectious_raw %>% 
  dplyr::group_by(pref) %>% 
  dplyr::mutate(infectious_cum = cumsum(sum_infectious)) 

infectious_sum2week_lag <- infectious_sum2week_raw %>% 
  dplyr::mutate(date = date + 15) %>%  #subtract cumulative infectious population of 15days ago
  dplyr::rename(infectious_cum_15lag = infectious_cum) %>%  
  dplyr::select(-sum_infectious)

infectious_sum2week <- infectious_sum2week_raw %>% 
  dplyr::left_join(., infectious_sum2week_lag, by = c("date", "pref")) %>% 
  tidyr::replace_na(., list(infectious_cum_15lag = 0)) %>% 
  dplyr::mutate(infectious_l2 = infectious_cum - infectious_cum_15lag - sum_infectious) %>% 
  #subtract cumulative infectious population of 15days ago and new infectious population, 
  #so that infectious_l2 denotes the total number of infectious population during the past 2 weeks.
  dplyr::select(!c("infectious_cum",  "infectious_cum_15lag", "sum_infectious"))
  

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
  tidyr::replace_na(., list(noftests = 0)) %>% 
  dplyr::left_join(., pop47, by = c("pref" = "pref_resid"))


# merge susceptible and infectious population
data2 <- data %>% 
  dplyr::mutate(susceptible = pop_resid - total_case) %>% #calculate susceptible population
  dplyr::left_join(., infectious_sum2week, by = c("date", "pref"))

# lag for newcases and test cases

newcase_tests_lead1 <- data %>% 
  dplyr::select(date, pref, newcase_day, noftests) %>% 
  dplyr::mutate(date = as.Date(date - 1)) %>% 
  dplyr::rename(newcase_lead1 = newcase_day,
                tests_lead1 = noftests)

newcase_tests_lead7 <- data %>% 
  dplyr::select(date, pref, newcase_day, noftests) %>% 
  dplyr::mutate(date = as.Date(date - 7)) %>% 
  dplyr::rename(newcase_lead7 = newcase_day,
                tests_lead7 = noftests)

newcase_tests_lead8 <- data %>% 
  dplyr::select(date, pref, newcase_day, noftests) %>% 
  dplyr::mutate(date = as.Date(date - 8)) %>% 
  dplyr::rename(newcase_lead8 = newcase_day,
                tests_lead8 = noftests)

newcase_tests_lead14 <- data %>% 
  dplyr::select(date, pref, newcase_day, noftests) %>% 
  dplyr::mutate(date = as.Date(date - 14)) %>% 
  dplyr::rename(newcase_lead14 = newcase_day,
                tests_lead14 = noftests)

newcase_tests_lead15 <- data %>% 
  dplyr::select(date, pref, newcase_day, noftests) %>% 
  dplyr::mutate(date = as.Date(date - 15)) %>% 
  dplyr::rename(newcase_lead15 = newcase_day,
                tests_lead15 = noftests)

newcase_tests_lead21 <- data %>% 
  dplyr::select(date, pref, newcase_day, noftests) %>% 
  dplyr::mutate(date = as.Date(date - 21)) %>% 
  dplyr::rename(newcase_lead21 = newcase_day,
                tests_lead21 = noftests)

newcase_tests_lead22 <- data %>% 
  dplyr::select(date, pref, newcase_day, noftests) %>% 
  dplyr::mutate(date = as.Date(date - 22)) %>% 
  dplyr::rename(newcase_lead22 = newcase_day,
                tests_lead22 = noftests)

newcase_tests_lead28 <- data %>% 
  dplyr::select(date, pref, newcase_day, noftests) %>% 
  dplyr::mutate(date = as.Date(date - 28)) %>% 
  dplyr::rename(newcase_lead28 = newcase_day,
                tests_lead28 = noftests)

daily_data_SIR <- data2 %>% 
  dplyr::left_join(., newcase_tests_lead8, by = c("date", "pref")) %>% 
  # tidyr::replace_na(., list(newcase_lead8 = 0, tests_lead8 = 0)) %>% 
  dplyr::left_join(., newcase_tests_lead14, by = c("date", "pref")) %>% 
  # tidyr::replace_na(., list(newcase_lead14 = 0, tests_lead14 = 0)) %>% 
  dplyr::left_join(., newcase_tests_lead1, by = c("date", "pref")) %>% 
  # tidyr::replace_na(., list(newcase_lead1 = 0, tests_lead1 = 0)) %>% 
  dplyr::left_join(., newcase_tests_lead7, by = c("date", "pref")) %>% 
  # tidyr::replace_na(., list(newcase_lead7 = 0, tests_lead7 = 0)) %>% 
  dplyr::left_join(., newcase_tests_lead15, by = c("date", "pref")) %>% 
  # tidyr::replace_na(., list(newcase_lead15 = 0, tests_lead15 = 0)) %>% 
  dplyr::left_join(., newcase_tests_lead21, by = c("date", "pref")) %>% 
  # tidyr::replace_na(., list(newcase_lead21 = 0, tests_lead21 = 0)) %>% 
  dplyr::left_join(., newcase_tests_lead22, by = c("date", "pref")) %>% 
  # tidyr::replace_na(., list(newcase_lead22 = 0, tests_lead22 = 0)) %>% 
  dplyr::left_join(., newcase_tests_lead28, by = c("date", "pref"))
  # tidyr::replace_na(., list(newcase_lead28 = 0, tests_lead28 = 0))

## make day(曜日) and week dummy
daily_data_SIR <- daily_data_SIR %>% 
  dplyr::mutate(weekday = weekdays(date, abbreviate = TRUE),
                weeknum = isoweek(date))

#write
write_csv(daily_data_SIR, here::here("03_build/2nd_rebuttal/output/daily_data_SIR.csv"))



