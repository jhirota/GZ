library(tidyverse)
library(lubridate)

# Data load----------
Agoop_list <- here::here("02_bring/Agoop/data") %>% 
  list.files(full.names = T) %>% 
  lapply(read.csv)
pop47 <- read_csv(here::here("02_bring/Pop/data/Population_JPN.csv")) 
covid47 <- read_csv(here::here("02_bring/Covid_cases/data/nhk_news_covid19_prefectures_daily_data(1).csv"))
Testmerge <- read_csv(here::here("02_bring/Tests/data/testmerge.csv"))

# Agoop clean ---------
cleanfunc <- function(data){
  data <- data %>% 
    rename(week = 1,
           weekday_flag = 2,
           daytime_flag = 3,
           pref_resid = 6,
           city = 7,
           pref = 8) %>% 
    group_by(week, pref, pref_resid, start_day) %>% 
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
  mutate(week = as.Date(ymd(start_day)),
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
                                                                       静岡県 = "Shizuoka"))) %>% 
  select(-start_day)


# Covid data clean -----

covid47 <- covid47 %>%
  rename(date = 1,
         pref_resid = 3,
         newcase_resid = 4,
         newdeath_resid = 6) %>% 
  select(date, pref_resid, newcase_resid, newdeath_resid) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(week = floor_date(date, "week",
                           week_start = getOption("lubridate.week.start", 1))) %>% 
  group_by(pref_resid, week) %>% 
  summarize(across(c(newcase_resid, newdeath_resid),
                   sum)) %>%
  ungroup() %>% 
  mutate(pref_resid = str_replace_all(pref_resid,
                                      c(山梨県 = "Yamanashi",
                                           茨城県 = "Ibaraki",
                                           栃木県 = "Tochigi",
                                           群馬県 = "Gunma",
                                           長野県 = "Nagano",
                                           静岡県 = "Shizuoka")))



# Population data clean --------

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


# Infectious data merge and build ----------

newmob6_47 <- left_join(x = mob6_47,
                        y = covid47,
                        by = c("pref_resid", "week")) %>% 
  left_join(y = pop47, by = c("pref_resid")) %>% 
  complete(pref_resid, nesting(pref, week)) %>% 
  group_by(pref) %>% 
  mutate(week_lead1 = lead(week, order_by = week, n = 47),
         newcase_resid_lead1 = if_else(week + 7 == week_lead1,
                                 lead(newcase_resid, order_by = week, n = 47),
                                 NA_real_),
         week_lead2 = lead(week, order_by = week, n = 94),
         newcase_resid_lead2 = if_else(week + 14 == week_lead2,
                                 lead(newcase_resid, order_by = week, n = 94),
                                 NA_real_),
         week_lag1 = lag(week, order_by = week, n = 47),
         week_lag2 = lag(week, order_by = week, n = 94)) %>% 
  ungroup() %>% 
  mutate(newcase_resid_per = newcase_resid / pop_resid,
         newcase_lead1_per = newcase_resid_lead1 / pop_resid,
         newcase_lead2_per = newcase_resid_lead2 / pop_resid,
         inflow_l1 = sum_pop * newcase_lead1_per,
         inflow_l2 = sum_pop * newcase_lead2_per) %>% 
  group_by(pref) %>% 
  mutate(inflow_l1_lag1 = if_else(week - 7 == week_lag1,
                                  lag(inflow_l1, order_by = week, n = 47),
                                  NA_real_),
         inflow_l1_lag2 = if_else(week - 14 == week_lag2,
                                  lag(inflow_l1, order_by = week, n = 94),
                                  NA_real_),
         inflow_l2_lag1 = if_else(week - 7 == week_lag1,
                                  lag(inflow_l2, order_by = week, n = 47),
                                  NA_real_),
         inflow_l2_lag2 = if_else(week - 14 == week_lag2,
                                  lag(inflow_l2, order_by = week, n = 94),
                                  NA_real_),
         infectious_l1 = inflow_l1_lag1 + inflow_l1_lag2,
         infectious_l2 = inflow_l2_lag1 + inflow_l2_lag2) %>% 
  ungroup()


## Infectious by pref -------

Aggregate <- newmob6_47 %>%
  mutate(infectious_l1 = replace_na(infectious_l1, 0),
         infectious_l2 = replace_na(infectious_l2, 0),
         sum_pop = replace_na(sum_pop, 0)) %>% 
  group_by(pref, week) %>% 
  summarize(across(c(infectious_l1, infectious_l2, sum_pop),
            sum)) %>% 
  ungroup() %>% 
  left_join(pop47,
            by = c("pref" = "pref_resid"))

write_csv(Aggregate, here::here("03_build/Controls/output/infectious_by_pref.csv"))


## Test data clean ----

Testdata <- Testmerge %>% 
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
           fill = list(noftests = 0)) %>%
  mutate(week = floor_date(date, "week",
                           week_start = getOption("lubridate.week.start", 1)) %>% 
           as.Date()) %>% 
  group_by(pref, week) %>%
  summarize(tests = sum(noftests)) %>%
  ungroup()

write_csv(Testdata, here::here("03_build/Controls/output/tests.csv"))


