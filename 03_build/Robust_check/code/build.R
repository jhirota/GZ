library(readxl)
library(lubridate)
library(tidyverse)


# data load -------------
GZall_list4 <- read_csv(here::here("03_build/GZlist/output/GZalllist_week.csv"))
weekSIR2 <- read_csv(here::here("03_build/Postas/output/weekSIR2.csv"))
postas_day <- read_csv(here::here("03_build/Postas/output/postas_daily_data.csv"))

dummy <- read_excel(here::here("02_bring/Dummy_vars/data/Dummies_edited0125.xlsx"))

data_genage <- here::here("02_bring/Stayhome_rate/data/gender_age_self-restraint_rate_202001_202103") %>% 
  list.files(full.names = T) %>% 
  lapply(read.csv)

data_night <- here::here("02_bring/Stayhome_rate/data/night_self-restraint_rate_202001_202103") %>% 
  list.files(full.names = T) %>% 
  lapply(read_csv, locale = locale(encoding = "shift-jis"))

# Infection prevention effects-------
## data clean (Different types of GZ) --------

NA20 <- function(x){
  x <- replace_na(x, 0)
  return(x)
}

GZall_list4 <- GZall_list4 %>% 
  select(!GZnew) %>% 
  pivot_wider(names_from = Type,
              values_from = cumGZ,
              names_prefix = "cumGZ") %>% 
  mutate(pref = "Yamanashi",
         cumGZFoodHotel = cumGZFood + cumGZHotel)

weekSIR_rob <- weekSIR2 %>% 
  left_join(GZall_list4,
            by = c("week", "pref")) %>% 
  mutate(across(c(cumGZFood, cumGZHotel, cumGZWinery,
                  cumGZTransition, cumGZShuzou, cumGZFoodHotel),
                NA20))

##data clean (New dummy vars) ---------

dummy_w <- dummy %>% 
  rename(pref = Pref,
         date = day) %>% 
  mutate(date = as.Date(date),
         week = floor_date(date,
                           "week",
                           week_start = getOption("lubridate.week.start", 1))) %>% 
  group_by(week, pref) %>% 
  summarize(dummy_school_closure = max(dummy_school_closure),# if a week contains even a single day of restriction, the week has value 1.
            dummy_gathering_restriction = max(dummy_gathering_restriction)) %>% 
  ungroup()



##data clean (SHR) -------------

credit_delete <- function(data){
  if (any(colnames(data) %in% "state")){
    data <- data %>% 
      filter(is.na(state) == FALSE)
  }else{
    data <- data %>%
      filter(is.na(M70) == FALSE)
  }
  
  return(data)
}

Night <- tibble()
Genage <- tibble()

for (i in 1:length(data_night)){
  cleaned <- credit_delete(data_night[[i]])
  Night <- rbind(Night, cleaned)
}

for (i in 1:length(data_genage)){
  cleaned <- credit_delete(data_genage[[i]])
  Genage <- rbind(Genage, cleaned)
}

Night <- Night %>% 
  rename(pref = 2,
         NSHR = 3) %>% 
  mutate(date = as.Date(date)) %>% 
  complete(date, pref)

Genage <- Genage %>% 
  mutate(date = strptime(as.character(date), "%m/%d/%y") %>% 
           as.Date()) %>% 
  rename(pref = 2)

SHR <- left_join(x = Night,
                 y = Genage,
                 by = c("pref", "date"))

SHR_w <- SHR %>% 
  mutate(week = floor_date(date,
                           "week",
                           week_start = getOption("lubridate.week.start", 1)),
         pref = str_replace_all(pref,
                                c(山梨県 = "Yamanashi",
                                     茨城県 = "Ibaraki",
                                     栃木県 = "Tochigi",
                                     群馬県 = "Gunma",
                                     長野県 = "Nagano",
                                     静岡県 = "Shizuoka"))) %>% 
  filter(pref %in% c("Ibaraki", "Shizuoka", "Tochigi", "Gunma", "Yamanashi", "Nagano")) %>%
  select(!date) %>% 
  group_by(pref, week) %>% 
  summarize_all(mean) %>% 
  ungroup()


## data merge ---------

weekSIR_rob <- left_join(x = weekSIR_rob,
                         y = dummy_w, 
                         by = c("week", "pref")) %>% 
  left_join(y = SHR_w, 
            by = c("week", "pref")) %>% 
  mutate(across(c("dummy_school_closure", "dummy_gathering_restriction"),
                NA20))


write_csv(weekSIR_rob, here::here("03_build/Robust_check/output/weekSIR_robustness.csv"))



# Economic effects-------

##data clean ------

SHR_d <- SHR %>% 
  mutate(pref = str_replace_all(pref,
                                c(山梨県 = "Yamanashi",
                                     茨城県 = "Ibaraki",
                                     栃木県 = "Tochigi",
                                     群馬県 = "Gunma",
                                     長野県 = "Nagano",
                                     静岡県 = "Shizuoka")))

## data merge -------
postas_rob <- postas_day %>% 
  left_join(SHR_d, by = c("date", "pref")) %>% 
  filter(date <= "2021-04-30")


write_csv(postas_rob, here::here("03_build/Robust_check/output/postas_rob.csv"))


