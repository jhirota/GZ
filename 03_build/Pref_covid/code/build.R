library(lubridate)
library(tidyverse)


#load data--------
yamaCOVID <- read_csv("03_build/Case_positive/output/市町村別感染者数.csv") 
GZdata <- read_csv("03_build/GZlist/output/GZlist_timeseries.csv")
preresdata <- read_csv("03_build/Vresas/output/restraunt_all_genre.csv")
preCOVID <- read_csv("03_build/GZ_covid/output/GZ_covid.csv")

#clean & merge data--------
GZdata2 <- GZdata%>%
  group_by(Date_approval)%>%
  summarize(GZnew = sum(sum))%>%
  mutate(pref = "Yamanashi")
GZdata2$Date_approval <- as.Date(GZdata2$Date_approval) #convert the type of objects
GZdata3 <- GZdata2 %>%  #generate the missing dates
  group_by(pref) %>% 
  complete(Date_approval = seq.Date(min(Date_approval), max(Date_approval), by="day"), fill = list(GZnew = 0))
GZdata3$cumGZ <- ave(GZdata3$GZnew, GZdata3$pref, FUN = cumsum)#convert newcases to cumulative cases 

## restraunt data-----------
preresdata2 <- preresdata %>% 
  pivot_longer(-c(week, X1), names_to = "pref", values_to = "resview") %>% 
  mutate(Week = week) %>% 
  separate(week, into = c("abandon", "weeknum"), sep = "第") %>% 
  select(-c(abandon, X1))
preresdata2["weeknum"] <- lapply(preresdata2["weeknum"], gsub, pattern="週", replacement = "")


write.csv(preresdata2, "03_build/Pref_covid/output/V-RESAS_restaurant.csv", row.names=FALSE)

##COVID ------------

preCOVID$date <- as.Date(preCOVID$date)

##merge daily data------------
preCOVID_GZ <- preCOVID %>% 
  left_join(GZdata3, by = c("date" = "Date_approval", "pref")) %>% 
  arrange(date)

preCOVID_GZ$newcaseday14 <- sapply(1:nrow(preCOVID_GZ), function(x) preCOVID_GZ$newcase_day[x+14*6])

preCOVID_GZ_final <- preCOVID_GZ %>% 
  replace_na(list(GZnew = 0,
                  cumGZ = 0)) %>% 
  mutate(code = factor(prefcode),
         Date_dummy = paste("D", date, sep = "_"),
         pref_dummy = paste("D", prefcode, sep = "_")) %>% 
  select(-code) 

# nofcases 1 day before 
preCOVID_GZ_final$COVID1 <- sapply(1:nrow(preCOVID_GZ_final), function(x) preCOVID_GZ_final$newcase_day[x+6])

write.csv(preCOVID_GZ_final, "03_build/Pref_covid/output/pref_bet_day_COVID_GZ.csv", row.names=FALSE)

#merge week data Hirota-san--------

## kenkan load ---

vresas <- read.csv("02_bring/Vresas/data/VRESAS_Mobility.csv")

# Data wrangling ----
preCOVID_GZ_final$Date <- as.Date(preCOVID_GZ_final$Date)
names(vresas)[2] <- "pref"
names(preCOVID_GZ_final)[3] <- "pref"

# Weeknum assignment ----
b <- c(rep(1:71), rep(1:71), rep(1:71), rep(1:71), rep(1:71), rep(1:71), rep(1:71), rep(1:71), rep(1:71))

preresdata2 <- preresdata2 %>% 
  arrange(pref) %>% 
  filter(Week != 2)
preresdata2$weeknum <- b

preCOVID_GZ_final.1 <- preCOVID_GZ_final %>% 
  arrange(pref) %>% 
  filter(pref != "Kanagawa") %>% 
  filter(pref != "Chiba") %>% 
  filter(pref != "Saitama") %>%
  filter(pref != "Tokyo")
a <- c(rep(3,4), rep(4:69, each = 7), rep(70, 5))
aa <- c(a, a, a, a, a, a)

preCOVID_GZ_final.1$weeknum <- aa

# Merge data ------
join3 <- preresdata2 %>% 
  select(pref, resview, weeknum)

preCOVID_GZ_final.1 <- preCOVID_GZ_final.1 %>% 
  left_join(y = join3, by = c("pref", "weeknum"))

# weekly data ----
newdata <- preCOVID_GZ_final.1 %>% 
  group_by(weeknum, pref) %>% 
  summarize_at(vars(3, 5, 14, 16), funs(sum(., na.rm=TRUE))) 
#summarize at "newcaseday", "newdeath", "GZnew", "newcaseday14"


newdata <- left_join(x = newdata,
                     y = preresdata2,
                     by = c("pref", "weeknum"))

cleandata <- newdata %>%
  left_join(y = vresas, by = c("pref", "Week"))

cleandata$cumGZ <- ave(cleandata$GZnew, cleandata$pref, FUN = cumsum)
cleandata <- cleandata[,c(7,1:5,12,6,8:11)]

write.csv(cleandata, "03_build/Pref_covid/output/weekly_vresas.csv", row.names=FALSE)

