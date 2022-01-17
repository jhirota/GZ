library(tidyverse)
library(jpndistrict)
library(reshape2)
library(lubridate)

# GZall_list by week -------

#Not only food but hotels, wineries etc.
GZall_list <- read_csv("02_bring/GZlist/data/GZlist.csv") %>% 
  filter(Keido <= 139.5) %>% 
  filter(Ido >= 35 & Ido <= 37) %>% 
  select(Date_approval, Type) %>% 
  mutate(Approved = 1,
         Date_approval = as.Date(Date_approval)) %>% 
  complete(Date_approval = seq.Date(min(Date_approval), max(Date_approval), by="day"),
           fill = list(Approved = 0)) %>% 
  complete(Date_approval, Type, fill = list(Approved = 0, Type = "Food"))

GZall_list$week <- lubridate::floor_date(GZall_list$Date_approval, "week",
                               week_start = getOption("lubridate.week.start", 1))

GZall_list2 <- GZall_list %>% 
  group_by(week, Type) %>% 
  summarize(GZnew = sum(Approved)) %>% 
  ungroup() %>% 
  complete(week, Type, fill = list(GZnew = 0))

GZall_list2$cumGZ <- ave(GZall_list2$GZnew, GZall_list2$Type, FUN = cumsum)

write_csv(GZall_list2, "03_build/GZlist/output/GZalllist_week.csv")


# GZ times series------------------
cities <- c("甲府市", "富士吉田市", "都留市","山梨市", "大月市", "韮崎市", "南アルプス市", "北杜市",
            "甲斐市", "笛吹市", "上野原市", "甲州市", "中央市", "市川三郷町", "早川町", "身延町",
            "南部町", "富士川町", "昭和町", "道志村", "西桂町", "忍野村", "山中湖村", "鳴沢村",
            "富士河口湖町", "小菅村", "丹波山村")
cityfunc <- function(x){
  for (i in cities){
    if (str_detect(x, pattern = i)) {
      x <- i
    }
  }
  return(x)
}

#Not only food but hotels, wineries etc.
GZall_list <- read_csv("02_bring/GZlist/data/GZlist.csv") %>% 
  filter(Keido <= 139.5) %>% 
  filter(Ido >= 35 & Ido <= 37) %>% 
  mutate(city = map(Address, cityfunc))

t_GZall_list <- GZall_list %>% 
  select(Date_approval, Type, city) %>%
  count(Date_approval, city) %>% 
  pivot_wider(names_from = city,
              values_from = n,
              values_fill = list(n = 0)) %>% 
  pivot_longer(cols = !Date_approval,
               names_to = "city",
               values_to = "N")


# write_csv(t_GZall_list, "03_build/GZlist/output/GZalllist_timeseries.csv")


# Food only list
GZlist <- read_csv("02_bring/GZlist/data/GZlist.csv") %>% 
  filter(Keido <= 139.5,
         Ido >= 35 & Ido <= 37,
         Type == "Food") %>% 
  mutate(city = map(Address, cityfunc),
         Date_approval = as.Date(Date_approval))

t_GZlist <- GZlist %>% 
  select(Date_approval, city) %>%
  count(Date_approval, city) %>% 
  pivot_wider(names_from = city,
              values_from = n,
              values_fill = list(n = 0)) %>% 
  pivot_longer(cols = !Date_approval,
               names_to = "city",
               values_to = "N")

# write_csv(t_GZlist, "03_build/GZlist/output/GZlist_timeseries.csv")









