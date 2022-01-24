library(readxl)
library(tidyverse)
library(lubridate)

#data load
weekSIR <- read_csv(here::here("03_build/weekSIR/output/weekSIR.csv"))
postas <- read_excel(here::here("02_bring/Postas/data/国立大学法人東京大学公共政策大学院データ提供.xlsx"))
postas.cus <- read_excel(here::here("02_bring/Postas/data/国立大学法人東京大学公共政策大学院データ提供.xlsx"),2)
# postas4 <- read_excel(here::here("02_bring/Postas/data/国立大学法人東京大学公共政策大学院データ提供.xlsx"),3)
# postas.cus4 <- read_excel(here::here("02_bring/Postas/data/国立大学法人東京大学公共政策大学院データ提供.xlsx"),4)
wdata <- read_csv(here::here("03_build/Weather/output/weather data 6pref 2019_final.csv"))
wloc <- read_csv(here::here("03_build/Weather/output/weather_location.csv"))
pref_covid <- read_csv(here::here("03_build/Pref_covid/output/pref_bet_day_COVID_GZ.csv"))
dummy <- read_excel(here::here("02_bring/Dummy_vars/data/Dummies_edited1115.xlsx"))

#postas cleaning ------

rename_fun <- function(data){
  colnames(data) <- gsub("_売上合計", "", colnames(data))
  colnames(data) <- gsub("_客数合計", "", colnames(data))
  return(data)
}

postas2 <- rename_fun(postas) %>% 
  pivot_longer(-c(1:7),
               names_to = "pref",
               values_to = "sales") %>%
  mutate(date = 集計対象営業日 %>% 
           as.Date(),
         pref = str_replace_all(pref,
                                c(山梨県 = "Yamanashi",
                                  茨城県 = "Ibaraki",
                                  栃木県 = "Tochigi",
                                  群馬県 = "Gunma",
                                  長野県 = "Nagano",
                                  静岡県 = "Shizuoka")))
postas2_week <- postas2 %>% 
  mutate(week = floor_date(date, "week",
                           week_start = getOption("lubridate.week.start", 1))) %>% 
  group_by(pref, week) %>% 
  summarize(sales = sum(sales)) %>% 
  ungroup()

#postas.cus cleaning ------

postas.cus2 <- rename_fun(postas.cus) %>%
  pivot_longer(-c(1:7),
               names_to = "pref",
               values_to = "customers") %>%
  mutate(date = 集計対象営業日 %>% 
           as.Date(),
         pref = str_replace_all(pref,
                                c(山梨県 = "Yamanashi",
                                  茨城県 = "Ibaraki",
                                  栃木県 = "Tochigi",
                                  群馬県 = "Gunma",
                                  長野県 = "Nagano",
                                  静岡県 = "Shizuoka")))

postas.cus2_week <- postas.cus2 %>% 
  mutate(week = floor_date(date, "week",
                           week_start = getOption("lubridate.week.start", 1))) %>% 
  group_by(pref, week) %>% 
  summarize(customers = sum(customers)) %>% 
  ungroup()


#postas data merge --------

weekSIR2 <- weekSIR %>%
  left_join(postas2_week,
            by = c("week", "pref")) %>% 
  left_join(postas.cus2_week,
            by = c("week", "pref")) %>% 
  mutate(lsales = log(sales),
         lcustomers = log(customers),
         sales_per = case_when(pref == "Yamanashi" ~ sales / 18,
                               pref == "Gunma" ~ sales / 100,
                               pref == "Tochigi" ~ sales / 58,
                               pref == "Ibaraki" ~ sales / 86,
                               pref == "Nagano" ~ sales / 68,
                               pref == "Shizuoka" ~ sales / 116),
         customers_per = case_when(pref == "Yamanashi" ~ customers / 18,
                                   pref == "Gunma" ~ customers / 100,
                                   pref == "Tochigi" ~ customers / 58,
                                   pref == "Ibaraki" ~ customers / 86,
                                   pref == "Nagano" ~ customers / 68,
                                   pref == "Shizuoka" ~ customers / 116))


write_csv(weekSIR2, here::here("03_build/Postas/output/weekSIR2.csv"))


# postas week data for plot ------
Prefs <- c("Gunma", "Ibaraki", "Yamanashi", "Tochigi", "Nagano", "Shizuoka")

postas_week <- postas2_week %>% 
  left_join(postas.cus2_week,
            by = c("week", "pref")) %>% 
  filter(pref %in% Prefs) %>% 
  mutate(treat = if_else(pref == "Yamanashi",
                         "Yamanashi",
                         "Neighboring_prefs"),
         sales_per = case_when(pref == "Yamanashi" ~ sales / 18,
                               pref == "Gunma" ~ sales / 100,
                               pref == "Tochigi" ~ sales / 58,
                               pref == "Ibaraki" ~ sales / 86,
                               pref == "Nagano" ~ sales / 68,
                               pref == "Shizuoka" ~ sales / 116),
         customers_per = case_when(pref == "Yamanashi" ~ customers / 18,
                                   pref == "Gunma" ~ customers / 100,
                                   pref == "Tochigi" ~ customers / 58,
                                   pref == "Ibaraki" ~ customers / 86,
                                   pref == "Nagano" ~ customers / 68,
                                   pref == "Shizuoka" ~ customers / 116))

write_csv(postas_week, here::here("03_build/Postas/output/postas_2019_2021.csv"))

# daily dataset with weather vars ---------

# Daily data clean
weatherfinal <- rbind(wdata %>% rename(sum_rain = 5),
                      wloc) %>% 
  mutate(date = as.Date(Date),
         pref = Pref,
         pref = str_replace_all(pref,
                                c(Ibaragi = "Ibaraki"))) %>% 
  select(date, pref, avg_temp, sum_rain) %>% 
  group_by(date, pref) %>% 
  summarize(across(c(avg_temp, sum_rain),
                   mean)) %>% 
  ungroup() %>% 
  rename(avg_rain = sum_rain) 

pref_covid <- pref_covid %>% 
  select(date, pref, newcase_day, total_case, newdeath_day, total_death, emergency, GZnew, cumGZ, newcaseday14) 


##ここから再開
NA20 <- function(x){
  newx <- replace_na(x , 0)
  return(newx)
}

postas_day <- left_join(postas2,
                        postas.cus2,
                        by = c("date", "pref")) %>% 
  filter(pref %in% Prefs) %>% 
  select(date, pref, sales, customers) %>% 
  left_join(weatherfinal,
            by = c("date", "pref")) %>% 
  left_join(pref_covid,
            by = c("date", "pref")) %>% 
  mutate_at(vars(7:14), NA20) %>% 
  mutate(avg_temp_q = avg_temp^2,
         lsales = log(sales),
         lcustomers = log(customers),
         sales_per = case_when(pref == "Yamanashi" ~ sales / 18,
                               pref == "Gunma" ~ sales / 100,
                               pref == "Tochigi" ~ sales / 58,
                               pref == "Ibaraki" ~ sales / 86,
                               pref == "Nagano" ~ sales / 68,
                               pref == "Shizuoka" ~ sales / 116),
         customers_per = case_when(pref == "Yamanashi" ~ customers / 18,
                                   pref == "Gunma" ~ customers / 100,
                                   pref == "Tochigi" ~ customers / 58,
                                   pref == "Ibaraki" ~ customers / 86,
                                   pref == "Nagano" ~ customers / 68,
                                   pref == "Shizuoka" ~ customers / 116))



# newdummy
dummy <- dummy %>% 
  rename(pref = Pref,
         date = day) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate_if(is.character, 
            str_replace_all, pattern = "Ibaraki", replacement = "Ibaragi")

# data merge

postas_day1 <- left_join(x = postas_day,
                         y = dummy, 
                         by = c("date", "pref")) %>% 
  mutate(dummy_school_closure = replace_na(dummy_school_closure, 0),
         dummy_gathering_restriction = replace_na(dummy_gathering_restriction, 0))


write_csv(postas_day1, here::here("03_build/Postas/output/postas_daily_data.csv"))

# weekly data (2019-2021)

postas_week2 <- postas_day %>% 
  mutate(week = floor_date(date,
                           "week",
                           week_start = getOption("lubridate.week.start", 1))) %>% 
  group_by(week, pref) %>% 
  summarize(across(c(avg_temp, avg_rain),
                   mean),
            across(c(sales, customers),
                   sum)) %>% 
  mutate(avg_temp_q = avg_temp^2) %>% 
  ungroup() %>% 
  left_join(weekSIR2 %>% select(week, pref, newcase_day, GZnew, cumGZ, emergency),
            by = c("week", "pref")) %>% 
  mutate_at(vars(8:11), NA20)
    


write_csv(postas_week2, "03_build/Postas/output/postas_weekly_data.csv")




