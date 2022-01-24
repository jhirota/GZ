library(tidyverse)
library(lubridate)

# data load ----
weather <- read_csv(here::here("03_build/Weather/output/weather_pref.csv"))
weekmain <- read_csv(here::here("03_build/Pref_covid/output/weekly_vresas.csv"))
inflow <- read_csv(here::here("03_build/Controls/output/県別流入リスク.csv"))

Testdata <- read.csv(here::here("03_build/Controls/output/tests.csv")) 

# data clean ------
weather_2020 <- weather %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= "2019-12-30") %>% 
  mutate(week = floor_date(date,
                           "week",
                           week_start = getOption("lubridate.week.start", 1)) ) %>% 
  group_by(pref, week) %>% 
  summarize(avg_temp = mean(avg_temp),
            avg_rain = mean(avg_rain))


weekSIR <- left_join(weekmain,
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
                            n = 2)) %>% 
  ungroup() %>% 
  mutate(cumcases = cumsum(newcase_day),
         susceptible = pop_resid - cumcases,
         avg_temp_q = avg_temp^2,
         avg_rain_q = avg_rain^2)
  

write_csv(weekSIR, here::here("03_build/weekSIR/output/weekSIR.csv"))


