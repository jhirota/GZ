library(tidyverse)
library(lubridate)

# data load ----
weather <- read_csv(here::here("03_build/Weather/output/weather_pref.csv"))
weekmain <- read_csv(here::here("03_build/GZ_covid/output/weekly_vresas.csv"))
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

