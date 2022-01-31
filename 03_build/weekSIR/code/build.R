library(tidyverse)
library(lubridate)

# data load ----
weather <- read_csv(here::here("03_build/GZ_covid/output/weather_pref.csv"))
inflow <- read_csv(here::here("03_build/Controls/output/infectious_by_pref.csv"))
Testdata <- read.csv(here::here("03_build/Controls/output/tests.csv")) 
COVID_GZ <- read_csv(here::here("03_build/GZ_covid/output/pref_bet_day_COVID_GZ.csv"))

data_mobility <- here::here("02_bring/Vresas/data/mobility") %>% 
  list.files(full.names = T) %>% 
  lapply(read.csv)
data_restaurant <- here::here("02_bring/Vresas/data/restaurant") %>% 
  list.files(full.names = T) %>% 
  lapply(read.csv)

# data clean (VRESAS week)--------
clean_mob <- function(data){
  data <- data %>% 
    select(-2) %>% 
    rename(pref = 1,
           week_JP = 2,
           kind = 3,
           ratio = 4) %>% 
    pivot_wider(names_from = "kind",
                values_from = "ratio") %>% 
    rename(in_city = 3,
           in_pref = 4,
           out_pref = 5)
  return(data)
}

VRmob <- rbind(clean_mob(data_mobility[[1]]),
               clean_mob(data_mobility[[2]]), 
               clean_mob(data_mobility[[3]]), 
               clean_mob(data_mobility[[4]]), 
               clean_mob(data_mobility[[5]]), 
               clean_mob(data_mobility[[6]])) %>% 
  mutate(pref = str_replace_all(pref,
                                c(山梨県全体 = "Yamanashi",
                                       茨城県全体 = "Ibaraki",
                                       栃木県全体 = "Tochigi",
                                       群馬県全体 = "Gunma",
                                       長野県全体 = "Nagano",
                                       静岡県全体 = "Shizuoka")),
         weeknum = rep(1:93, 6))

clean_res <- function(data){
  data <- data %>% 
    rename(pref = 1,
           week_JP = 2,
           kind = 3,
           resview = 4) %>% 
    filter(kind == "すべて") %>% 
    select(-kind)
  
  return(data)
}

VRres <- rbind(clean_res(data_restaurant[[1]]),
               clean_res(data_restaurant[[2]]),
               clean_res(data_restaurant[[3]]), 
               clean_res(data_restaurant[[4]]), 
               clean_res(data_restaurant[[5]]), 
               clean_res(data_restaurant[[6]])) %>% 
  arrange(pref) %>% 
  mutate(pref = str_replace_all(pref,
                                c(山梨県全体 = "Yamanashi",
                                       茨城県全体 = "Ibaraki",
                                       栃木県全体 = "Tochigi",
                                       群馬県全体 = "Gunma",
                                       長野県全体 = "Nagano",
                                       静岡県全体 = "Shizuoka")),
         weeknum = rep(1:93, 6)) 

vresas <- left_join(VRmob, VRres,
                    by = c("pref", "weeknum", "week_JP")) %>% 
  mutate(treat = if_else(pref == "Yamanashi",
                         "Yamanashi",
                         "Neighboring_prefs"))



# data clean (week data of weather) ------
weather_2020 <- weather %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= "2019-12-30") %>% 
  mutate(week = floor_date(date,
                           "week",
                           week_start = getOption("lubridate.week.start", 1)) ) %>% 
  group_by(pref, week) %>% 
  summarize(avg_temp = mean(avg_temp),
            avg_rain = mean(avg_rain))

# Merge Vresas data ------

VRweek <- COVID_GZ %>% 
  group_by(week, pref) %>% 
  summarize(across(c(newcase_day, newdeath_day, GZnew, newcaseday14, emergency),
                   sum),
            across(c(weeknum),
                   unique)) %>% 
  ungroup() %>% 
  mutate(emergency = if_else(emergency >= 1, 1, 0)) %>% 
  left_join(y = vresas,
            by = c("pref", "weeknum")) %>% 
  group_by(pref) %>% 
  mutate(cumGZ = cumsum(GZnew)) %>% 
  ungroup() %>% 
  select(week, week_JP, weeknum, pref, newcase_day, newdeath_day, emergency,
         GZnew, cumGZ, newcaseday14, resview, in_city,
         in_pref, out_pref, treat)

write_csv(VRweek, here::here("03_build/weekSIR/output/weekly_vresas.csv"))


# data merge (week SIR)--------
weekSIR <- left_join(VRweek,
                     weather_2020,
                     by = c("pref", "week")) %>% 
  left_join(inflow,
            by = c("pref", "week")) %>% 
  left_join(Testdata %>% 
              mutate(week = as.Date(week)),
            by = c("week", "pref")) %>% 
  group_by(pref) %>% 
  mutate(newcase_lead1 = lead(newcase_day,
                              order_by = week,
                              n = 1),
         newcase_lead2 = lead(newcase_day,
                              order_by = week,
                              n = 2),
         tests = replace_na(tests, 0),
         tests_lead1 = lead(tests,
                            order_by = week,
                            n = 1),
         tests_lead2 = lead(tests,
                            order_by = week,
                            n = 2),
         cumcases = cumsum(newcase_day),
         susceptible = pop_resid - cumcases) %>% 
  ungroup() 
  
write_csv(weekSIR, here::here("03_build/weekSIR/output/weekSIR.csv"))

# data for time series plot-------
plotdata <- weekSIR %>% 
  mutate(treat = if_else(pref == "Yamanashi",
                         "Yamanashi",
                         "Neighboring_prefs"),
         newcase_day_per = (newcase_day / pop_resid) *100000) %>% 
  group_by(treat, week) %>% 
  summarize(newcase_day_per = mean(newcase_day_per)) %>% 
  ungroup() %>% 
  mutate(lnewcase_day_per = log(newcase_day_per + 1))

write_csv(plotdata, here::here("03_build/weekSIR/output/cases_timeseries_plot.csv"))

