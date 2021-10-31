library(tidyverse)
library(lfe)
library(stargazer)
library(lubridate)

#data load------
GZ <- read.csv("03_build/GZlist/output/GZlist_timeseries.csv")
Covid19 <- read.csv("03_build/Case_positive/output/市町村別感染者数.csv")

#data wrangling ------
names(GZ)[2] <- "date"
names(GZ)[4] <- "GZapproved"
names(Covid19)[3] <- "date"
names(Covid19)[4] <- "nofcases"

GZ <- GZ[,-1]
Covid19 <- Covid19[,-1]


DATA <- Covid19 %>% 
  left_join(y = GZ, by = c("date", "city")) 
DATA$date <- as.Date(DATA$date)  

DATA <- DATA %>%   
  group_by(city) %>% 
  complete(date = seq.Date(min(date), max(date), by = "day"),
           fill = list(nofcases = 0)) %>% 
  arrange(date)

DATA$GZapproved <- ifelse(is.na(DATA$GZapproved), 0, DATA$GZapproved)
DATA$cumGZ <- ave(DATA$GZapproved, DATA$city, FUN = cumsum)


# nofcases14 & lnGZ 
DATA <- DATA %>% 
  mutate(lnGZ = log(cumGZ + 1)) 

# 14 days lag
DATA$nofcases14 <- sapply(1:nrow(DATA), function(x) DATA$nofcases[x+378]) 



# mobility data as control -------

mesh1 <- read.csv("02_bring/MLIT/data/all/201901monthly_mdp_mesh1km.csv")
mesh2 <- read.csv("02_bring/MLIT/data/all/201902monthly_mdp_mesh1km.csv")
mesh3 <- read.csv("02_bring/MLIT/data/all/201903monthly_mdp_mesh1km.csv")
mesh4 <- read.csv("02_bring/MLIT/data/all/201904monthly_mdp_mesh1km.csv")
mesh5 <- read.csv("02_bring/MLIT/data/all/201905monthly_mdp_mesh1km.csv")
mesh6 <- read.csv("02_bring/MLIT/data/all/201906monthly_mdp_mesh1km.csv")
mesh7 <- read.csv("02_bring/MLIT/data/all/201907monthly_mdp_mesh1km.csv")
mesh8 <- read.csv("02_bring/MLIT/data/all/201908monthly_mdp_mesh1km.csv")
mesh9 <- read.csv("02_bring/MLIT/data/all/201909monthly_mdp_mesh1km.csv")
mesh10 <- read.csv("02_bring/MLIT/data/all/201910monthly_mdp_mesh1km.csv")
mesh11 <- read.csv("02_bring/MLIT/data/all/201911monthly_mdp_mesh1km.csv")
mesh12 <- read.csv("02_bring/MLIT/data/all/201912monthly_mdp_mesh1km.csv")
mesh13 <- read.csv("02_bring/MLIT/data/all/202001monthly_mdp_mesh1km.csv")
mesh14 <- read.csv("02_bring/MLIT/data/all/202002monthly_mdp_mesh1km.csv")
mesh15 <- read.csv("02_bring/MLIT/data/all/202003monthly_mdp_mesh1km.csv")
mesh16 <- read.csv("02_bring/MLIT/data/all/202004monthly_mdp_mesh1km.csv")
mesh17 <- read.csv("02_bring/MLIT/data/all/202005monthly_mdp_mesh1km.csv")
mesh18 <- read.csv("02_bring/MLIT/data/all/202006monthly_mdp_mesh1km.csv")
mesh19 <- read.csv("02_bring/MLIT/data/all/202007monthly_mdp_mesh1km.csv")
mesh20 <- read.csv("02_bring/MLIT/data/all/202008monthly_mdp_mesh1km.csv")
mesh21 <- read.csv("02_bring/MLIT/data/all/202009monthly_mdp_mesh1km.csv")
mesh22 <- read.csv("02_bring/MLIT/data/all/202010monthly_mdp_mesh1km.csv")
mesh23 <- read.csv("02_bring/MLIT/data/all/202011monthly_mdp_mesh1km.csv")
mesh24 <- read.csv("02_bring/MLIT/data/all/202012monthly_mdp_mesh1km.csv")


meshdata <- rbind(mesh1, mesh2, mesh3, mesh4, mesh5, mesh6, mesh7, mesh8, mesh9,
                  mesh10, mesh11, mesh12, mesh13, mesh14, mesh15, mesh16, mesh17,
                  mesh18, mesh19, mesh20, mesh21, mesh22, mesh23, mesh24)


attribute1 <- read.csv("02_bring/MLIT/data/all/attribute_mesh1km_2019.csv") %>% 
  mutate(year = 2019)
attribute2 <- read.csv("02_bring/MLIT/data/all/attribute_mesh1km_2020.csv") %>% 
  mutate(year = 2020)

attributes <- rbind(attribute1, attribute2)


citycode1 <- read.csv("02_bring/MLIT/data/all/prefcode_citycode_master_utf8_2019.csv") %>% 
  mutate(year = 2019)
citycode2 <- read.csv("02_bring/MLIT/data/all/prefcode_citycode_master_utf8_2020.csv") %>% 
  mutate(year = 2020)
citycodes <- rbind(citycode1, citycode2) %>% 
  filter(prefcode == 19) %>%  # yamanashi
  filter(year == 2019) # No need to use 2020 data (duplication error) 

citycodes$cityname <- gsub("西八代郡", "", citycodes$cityname)
citycodes$cityname <- gsub("南都留郡", "", citycodes$cityname)
citycodes$cityname <- gsub("北都留郡", "", citycodes$cityname)
citycodes$cityname <- gsub("中巨摩郡", "", citycodes$cityname)
citycodes$cityname <- gsub("南巨摩郡", "", citycodes$cityname)

# meshdata wrangling------
cleanmeshdata <- meshdata %>% 
  filter(dayflag == 2) %>% #全日（週末と平日）
  filter(timezone == 2) %>%  #終日（昼と深夜）
  group_by(citycode, year, month) %>% 
  summarize(mobility = sum(population)) %>% 
  ungroup()


join <- citycodes[,3:4]
cleanmeshdata <- left_join(x = cleanmeshdata,
                           y = join,
                           by = "citycode")

cleanmeshdata$date <- with(cleanmeshdata, sprintf("%d-%02d-01", year, month)) %>% 
  as.Date()
names(cleanmeshdata)[5] <- "city"


cleanmeshdata1 <- cleanmeshdata %>%   
  group_by(city) %>% 
  complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
  arrange(date) %>% 
  fill(mobility)


join1 <- cleanmeshdata1 %>% 
  select(city, date, mobility)

cleanDATA <- DATA %>% 
  left_join(y = join1, by = c("date", "city"))
# 小菅村は2019/4-2019/9までしか人流データない

# weather data as control-----

weather <- read.csv("02_bring/Weather/data/weatherdataforwrangling.csv") %>% 
  filter(観測所 != "富士川") %>% 
  filter(観測所 != "大菩薩") %>% 
  mutate(city = 0)

names(weather)[1] <- "date"
weather$date <- gsub("/", "-", weather$date)
weather$date <- as.Date(weather$date)  

names(weather)[c(4,7)] <- c("avg_temp", "rainfall")

# weather data wrangling-----
for (i in 1:nrow(cleanDATA)){
  if (cleanDATA$city[i] == "甲府市"||
      cleanDATA$city[i] =="甲斐市"||
      cleanDATA$city[i] =="昭和町"||
      cleanDATA$city[i] =="中央市"){
    cleanDATA$観測所[i] <- "甲府"
  }else if (cleanDATA$city[i] == "身延町"||
            cleanDATA$city[i] =="富士川町"||
            cleanDATA$city[i] =="市川三郷町"||
            cleanDATA$city[i] =="早川町"){
    cleanDATA$観測所[i] <- "切石"
  }else if (cleanDATA$city[i] == "鳴沢村"||
            cleanDATA$city[i] =="富士吉田市"||
            cleanDATA$city[i] =="西桂町"||
            cleanDATA$city[i] =="富士河口湖町"){
    cleanDATA$観測所[i] <- "河口湖"
  }else if (cleanDATA$city[i] =="山梨市"||
            cleanDATA$city[i] =="丹波山村"||
            cleanDATA$city[i] =="小菅村"||
            cleanDATA$city[i] =="甲州市"){
    cleanDATA$観測所[i] <- "勝沼"
  }else if (cleanDATA$city[i] == "韮崎市"||
            cleanDATA$city[i] =="南アルプス市"){
    cleanDATA$観測所[i] <- "韮崎"
  }else if (cleanDATA$city[i] == "大月市"||
            cleanDATA$city[i] =="都留市"||
            cleanDATA$city[i] == "上野原市"||
            cleanDATA$city[i] =="道志村"){
    cleanDATA$観測所[i] <- "大月"
  }else if (cleanDATA$city[i] == "山中湖村"||
            cleanDATA$city[i] =="忍野村"){
    cleanDATA$観測所[i] <- "山中"
  }else if (cleanDATA$city[i] == "北杜市"){
    cleanDATA$観測所[i] <- "大泉"
  }else if (cleanDATA$city[i] == "笛吹市"){
    cleanDATA$観測所[i] <- "古関"
  }else{
    cleanDATA$観測所[i] <- "南部"
  }
}

join5 <- weather %>% 
  select(date, 観測所, avg_temp, rainfall)

DATAfinal <- cleanDATA %>% 
  left_join(y = join5, by = c("date", "観測所"))


# SOE dummy as control ----
# SOE in Yamanashi is from 2020/04/16-2020/05/14
for (i in 1:nrow(DATAfinal)) {
  if (DATAfinal$date[i]>18367 &&
      DATAfinal$date[i]<18396){
    DATAfinal$SOE[i] <- 1
  }else{
    DATAfinal$SOE[i] <- 0
  }
  
}



# other vars -----
DATAfinal$datedum <- factor(DATAfinal$date)
DATAfinal$lmobility <- log(DATAfinal$mobility)
DATAfinal$cumcases <- ave(DATA$nofcases, DATA$city, FUN = cumsum)

poplist <- read.csv("03_build/Case_positive/output/List_pop_yamanashi.csv")
poplist$population <- as.numeric(poplist$population)

for (i in 1:nrow(DATAfinal)) {
  City <- DATAfinal$city[i]
  DATAfinal$pop[i] <- poplist$population[poplist$City == City]
  DATAfinal$scaled_cases[i] <- (DATAfinal$nofcases[i] / DATAfinal$pop[[i]]) * 100000 
}

DATAfinal$scaled_cases14 <- sapply(1:nrow(DATAfinal), function(x) DATAfinal$scaled_cases[x+378])
DATAfinal$pop <- as.numeric(DATAfinal$pop)

write.csv(DATAfinal, "03_build/GZ_covid_MLITmobility/output/GZ_covid_dailymobility.csv", row.names=FALSE)

