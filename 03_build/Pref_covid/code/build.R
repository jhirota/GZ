library(lubridate)
library(tidyverse)

#load data--------
GZdata <- read_csv("03_build/GZlist/output/GZlist_timeseries.csv")
preresdata <- read_csv("03_build/Vresas/output/restraunt_all_genre.csv")
preCOVID <- read_csv("03_build/GZ_covid/output/GZ_covid.csv")
vresas <- read_csv(here::here("03_build/Vresas/output/VRESAS_Mobility.csv"))

#clean & merge data--------
GZdata2 <- GZdata%>%
  group_by(Date_approval)%>%
  summarize(GZnew = sum(N))%>%
  mutate(pref = "Yamanashi",
         Date_approval = as.Date(Date_approval)) %>% 
  ungroup() %>% 
  complete(Date_approval = seq.Date(min(Date_approval), max(Date_approval), by="day"), fill = list(GZnew = 0)) %>% 
  group_by(pref) %>% 
  mutate(cumGZ = cumsum(GZnew))

## restaurant data-----------
preresdata2 <- preresdata %>% 
  pivot_longer(-c(week, ...1), names_to = "pref", values_to = "resview") %>% 
  select(-1) %>% 
  mutate(weeknum = rep(1:93, each = 6))
# write_csv(preresdata2, "03_build/Pref_covid/output/V-RESAS_restaurant.csv")

##COVID ------------

preCOVID_GZ <- preCOVID %>% 
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
  mutate(code = factor(prefcode),
         Date_dummy = paste("D", date, sep = "_"),
         pref_dummy = paste("D", prefcode, sep = "_")) %>% 
  select(-code) %>% 
  mutate(week = floor_date(date,
                           "week",
                           week_start = getOption("lubridate.week.start", 1))) %>% 
  arrange(week) %>%
  mutate(weeknum = c(rep(3, 4*6),rep(4:69, each = 7*6), rep(70, 5*6)))


# write_csv(preCOVID_GZ, "03_build/Pref_covid/output/pref_bet_day_COVID_GZ.csv")


# Merge Vresas data ------


newdata <- preCOVID_GZ %>% 
  group_by(weeknum, pref) %>% 
  summarize(across(c(newcase_day, newdeath_day, GZnew, newcaseday14),
                   sum)) %>% 
  ungroup() %>% 
  left_join(y = vresas,
            by = c("pref", "weeknum")) %>% 
  mutate(cumGZ = cumsum(GZnew)) %>% 
  left_join(y = preresdata2 %>% select(pref, resview, weeknum),
            by = c("pref", "weeknum")) %>% 
  select(week, weeknum, pref, newcase_day, newdeath_day,
         GZnew, cumGZ, newcaseday14, resview, in_city,
         in_pref, out_pref)


# write_csv(newdata, "03_build/Pref_covid/output/weekly_vresas.csv")
