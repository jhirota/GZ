library(tidyverse)
library(lubridate)

#　weekly data on mobility in Yamanashi -------
pp2019 <- read.csv("02_bring/Papilio_mobility/data/市区町村_standard_chart_data_2019.csv") %>% 
  filter(都道府県 == "19:山梨県") %>% 
  filter(平休日フラグ == "全日")

pp2020 <- read.csv("02_bring/Papilio_mobility/data/市区町村_standard_chart_data_2020.csv") %>% 
  filter(都道府県 == "19:山梨県") %>% 
  filter(平休日フラグ == "全日")

pp2021 <- read.csv("02_bring/Papilio_mobility/data/市区町村_standard_chart_data_2021.csv") %>% 
  filter(都道府県 == "19:山梨県") %>% 
  filter(平休日フラグ == "全日")


ppmob <- rbind(pp2019, pp2020) %>% 
  rbind(pp2021)

names(ppmob)[6] <- "city"

for (i in 1:nrow(ppmob)) {
  ppmob$city[i] <- unlist(strsplit(ppmob$city[i], split = ':', fixed=TRUE))[2]
}

a <- rep(seq(as.Date("2018-12-31"), as.Date("2021-06-13"), by = "week"), each = 27)
ppmob$week <- a

# GZ_and_covid_weekly_data with papilio mobility　----------


ppDATA <- read.csv("03_build/GZ_covid_MLITmobility/output/GZ_covid_dailymobility.csv")
ppDATA$date <- as.Date(ppDATA$date)

ppDATA$week <- floor_date(ppDATA$date, "week",
                          week_start = getOption("lubridate.week.start", 1)) 

ppDATA <- ppDATA[,c(21,3:20)]


ppDATAa <- ppDATA %>% 
  group_by(city, week) %>% 
  summarize_at(vars(2, 3), funs(sum(., na.rm=TRUE))) %>% #nofcases & GZapproved
  arrange(week) %>% 
  ungroup()

ppDATAb <- ppDATA %>% 
  group_by(city, week) %>% 
  summarize_at(vars(9, 10), funs(mean(., na.rm=TRUE))) %>% #temp and rainfall
  arrange(week) %>% 
  ungroup()


ppDATA1 <- left_join(x = ppDATAa,
                    y = ppDATAb,
                    by = c("week", "city")) %>% 
  left_join(y = ppmob[,c(1,6,8,9)],
            by = c("week", "city")) 

names(ppDATA1)[3] <- "infected"
names(ppDATA1)[4] <- "newGZ"
names(ppDATA1)[6] <- "avg_rainfall"
ppDATA1$cuminfected <- ave(ppDATA1$infected, ppDATA1$city, FUN = cumsum)
ppDATA1$cumGZ <- ave(ppDATA1$newGZ, ppDATA1$city, FUN = cumsum)
ppDATA1$infected_2w <- sapply(1:nrow(ppDATA1), function(x) ppDATA1$infected[x+54])
ppDATA1$infected_1w <- sapply(1:nrow(ppDATA1), function(x) ppDATA1$infected[x+27])
ppDATA1$infected_yest <- sapply(1:nrow(ppDATA1), function(x) ppDATA1$infected[x+54])


poplist <- read.csv("03_build/Case_positive/output/List_pop_yamanashi.csv")
poplist$population <- as.numeric(poplist$population)
ppDATA1$mobility <- as.numeric(ppDATA1$mobility)

for (i in 1:nrow(ppDATA1)) {
  City = ppDATA1$city[i]
  ppDATA1$pop[i] <- poplist$population[poplist$City == City]
  ppDATA1$mobility_per[i] <- (ppDATA1$mobility[i] / ppDATA1$pop[[i]]) 
}

ppDATA1$cuminfected <- as.numeric(ppDATA1$cuminfected)
ppDATA1$pop <- as.numeric(ppDATA1$pop)
ppDATA1$susceptible <- ppDATA1$pop - ppDATA1$cuminfected

## SOE in Yamanashi is from 2020/04/16-2020/05/14
for (i in 1:nrow(ppDATA1)) {
  if (ppDATA1$week[i]>18364 &&
      ppDATA1$week[i]<18396){
    ppDATA1$SOE[i] <- 1
  }else{
    ppDATA1$SOE[i] <- 0
  }
  
}


write.csv(ppDATA1, "03_build/Papilio_mobility/output/city_weekly.csv", row.names = FALSE)

