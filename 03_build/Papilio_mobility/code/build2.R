library(tidyverse)
library(lfe)
library(stargazer)

ppDATA1 <- read_csv("03_build/Papilio_mobility/output/city_weekly.csv")
Agrgt_city <- read_csv("03_build/Controls/output/市町村別流入リスク.csv")
Freq100 <- read_csv("03_build/GZlist/output/Freq100.csv")

# Risk of inflow of potential infectious people from 46 prefectures--------
ppDATA2 <- left_join(x = ppDATA1,
                     y = Agrgt_city %>% select(!X1, !週),
                     by = c("city", "week"))
names(ppDATA2)[which(colnames(ppDATA2) == "agrgt潜在的感染者人流_lag2")] <- "agrgt_potecovid_lag2"
names(ppDATA2)[which(colnames(ppDATA2) == "agrgt潜在的感染者人流_lag1")] <- "agrgt_potecovid_lag1"

ppDATA2_GZ100 <- ppDATA2 %>% 
  filter(city %in% Freq100$x) #Freq100 is from GZlocation.R


write.csv(ppDATA2, "03_build/Papilio_mobility/output/city_weekly2.csv", row.names = FALSE)
write.csv(ppDATA2_GZ100, "03_build/Papilio_mobility/output/city_weekly2_GZ100.csv", row.names = FALSE)
