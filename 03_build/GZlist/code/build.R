library(tidyverse)
library(jpndistrict)
library(reshape2)
library(lubridate)


# GZall_list by week -------

#Not only food but hotels, wineries etc.
GZall_list <-read.csv("02_bring/GZlist/data/GZlist.csv") %>% 
  filter(Keido <= 139.5) %>% 
  filter(Ido >= 35 & Ido <= 37) 

for (i in 1:nrow(GZall_list)){
  for(j in 1:length(cities)){
    if(GZall_list[i, 5] %in% grep(cities[j], GZall_list[i, 5], value = TRUE)){
      GZall_list[i, "city"] <- cities[j]
    }else{
    } 
  }
}

GZall_list2 <- GZall_list %>% 
  select(Date_approval, Type) %>% 
  mutate(Approved = 1)


GZall_list2$Date_approval <- as.Date(GZall_list2$Date_approval)
GZall_list2 <- complete(GZall_list2,
                        Date_approval = seq.Date(min(Date_approval), max(Date_approval), by="day"),
                        fill = list(Approved = 0))
GZall_list2 <- complete(GZall_list2, Date_approval, Type, fill = list(Approved = 0, Type = "Food"))

GZall_list2$week <- floor_date(GZall_list2$Date_approval, "week",
                               week_start = getOption("lubridate.week.start", 1))


GZall_list3 <- GZall_list2 %>% 
  group_by(week, Type) %>% 
  summarize(GZnew = sum(Approved)) %>% 
  ungroup() %>% 
  complete(week, Type, fill = list(GZnew = 0))


GZall_list3$cumGZ <- ave(GZall_list3$GZnew, GZall_list3$Type, FUN = cumsum)

write.csv(GZall_list3, "03_build/GZlist/output/GZalllist_week.csv", row.names=FALSE)


# GZall_list_wide by week -------

GZall_list4 <- data.frame(week = levels(factor(GZall_list3$week)),
                          cumGZFood = GZall_list3$cumGZ[GZall_list3$Type == "Food"],
                          GZnewFood = GZall_list3$GZnew[GZall_list3$Type == "Food"],
                          cumGZHotel = GZall_list3$cumGZ[GZall_list3$Type == "Hotel"],
                          GZnewHotel = GZall_list3$GZnew[GZall_list3$Type == "Hotel"],
                          cumGZShuzou = GZall_list3$cumGZ[GZall_list3$Type == "Shuzou"],
                          GZnewShuzou = GZall_list3$GZnew[GZall_list3$Type == "Shuzou"],
                          cumGZWinery = GZall_list3$cumGZ[GZall_list3$Type == "Winery"],
                          GZnewWinery = GZall_list3$GZnew[GZall_list3$Type == "Winery"],
                          cumGZTransition = GZall_list3$cumGZ[GZall_list3$Type == "Transition"],
                          GZnewTransition = GZall_list3$GZnew[GZall_list3$Type == "Transition"])
GZall_list4$week <- as.Date(GZall_list4$week)

write.csv(GZall_list4, "03_build/GZlist/output/GZalllist_week_wide.csv", row.names=FALSE)
