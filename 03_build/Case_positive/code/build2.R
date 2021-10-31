library(tidyverse)
library(tidyr)
library(readr)

# cleaned data with members
covid <- read.csv("02_bring/Covid_cases/data/yousei20210512Final.csv")

# setting
pop <- read.csv("02_bring/Pop/data/山梨県人口202004.csv")
pop <- pop[-34:-40,]
pop[1,2] <- "合計"
poplist <- data.frame(pop[, c(2,5)])
colnames(poplist)[1:2] <- c("City","population")
poplist[,2] <- as.numeric(sub(",", "", poplist[,2]))
poplist$population <- as.numeric(poplist$population)

# write.csv(poplist, "03_build/Case_positive/output/List_pop_yamanashi.csv",row.names=FALSE)


cities <- c("甲府市", "富士吉田市", "都留市","山梨市", "大月市", "韮崎市", "南アルプス市", "北杜市",
            "甲斐市", "笛吹市", "上野原市", "甲州市", "中央市", "市川三郷町", "早川町", "身延町",
            "南部町", "富士川町", "昭和町", "道志村", "西桂町", "忍野村", "山中湖村", "鳴沢村",
            "富士河口湖町", "小菅村", "丹波山村")


names(covid)[1] <- "id"

for (i in 1:nrow(covid)){
  if(covid$id[i] <= 570){
    covid$公表日[i] <- paste("2020/", covid$公表日[i], sep = "")
  }else{
    covid$公表日[i] <- paste("2021/", covid$公表日[i], sep = "")
  } 
  
}

covid$公表日 <- as.Date(covid$公表日)

# data range is from the first day to 2021-04-30(18747 as numeric)
covid <- subset(x = covid, subset = (公表日 <= 18747))

# data writing ---

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県(甲府市外)"){
    covid[i, 9] <- 0
    covid[i, 10] <- poplist["富士吉田市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 11] <- poplist["都留市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 12] <- poplist["山梨市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 13] <- poplist["大月市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 14] <- poplist["韮崎市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 15] <- poplist["南アルプス市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 16] <- poplist["北杜市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 17] <- poplist["甲斐市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 18] <- poplist["笛吹市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 19] <- poplist["上野原市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 20] <- poplist["甲州市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 21] <- poplist["中央市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 22] <- poplist["市川三郷町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 23] <- poplist["早川町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 24] <- poplist["身延町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 25] <- poplist["南部町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 26] <- poplist["富士川町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 27] <- poplist["昭和町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 28] <- poplist["道志村",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 29] <- poplist["西桂町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 30] <- poplist["忍野村",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 31] <- poplist["山中湖村",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 32] <- poplist["鳴沢村",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 33] <- poplist["富士河口湖町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 34] <- poplist["小菅村",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 35] <- poplist["丹波山村",] / (poplist["合計",] - poplist["甲府市",])
  }else{
  } 
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "甲府市外"){
    covid[i, 9] <- 0
    covid[i, 10] <- poplist["富士吉田市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 11] <- poplist["都留市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 12] <- poplist["山梨市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 13] <- poplist["大月市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 14] <- poplist["韮崎市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 15] <- poplist["南アルプス市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 16] <- poplist["北杜市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 17] <- poplist["甲斐市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 18] <- poplist["笛吹市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 19] <- poplist["上野原市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 20] <- poplist["甲州市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 21] <- poplist["中央市",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 22] <- poplist["市川三郷町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 23] <- poplist["早川町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 24] <- poplist["身延町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 25] <- poplist["南部町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 26] <- poplist["富士川町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 27] <- poplist["昭和町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 28] <- poplist["道志村",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 29] <- poplist["西桂町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 30] <- poplist["忍野村",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 31] <- poplist["山中湖村",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 32] <- poplist["鳴沢村",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 33] <- poplist["富士河口湖町",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 34] <- poplist["小菅村",] / (poplist["合計",] - poplist["甲府市",])
    covid[i, 35] <- poplist["丹波山村",] / (poplist["合計",] - poplist["甲府市",])
  }else{
  } 
}


for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県 (中北地域、峡南地域)"){
    covid[i,9:35] <- 0
    covid[i, 16] <- poplist["北杜市",] / (poplist["中北地域",] + poplist["峡南地域",])
    covid[i, 14] <- poplist["韮崎市",] / (poplist["中北地域",] + poplist["峡南地域",])
    covid[i, 15] <- poplist["南アルプス市",] /  (poplist["中北地域",] + poplist["峡南地域",])
    covid[i, 17] <- poplist["甲斐市",] /  (poplist["中北地域",] + poplist["峡南地域",])
    covid[i, 9] <- poplist["甲府市",] /  (poplist["中北地域",] + poplist["峡南地域",])
    covid[i, 27] <- poplist["昭和町",] /  (poplist["中北地域",] + poplist["峡南地域",])
    covid[i, 21] <- poplist["中央市",] /  (poplist["中北地域",] + poplist["峡南地域",])
    covid[i, 22] <- poplist["市川三郷町",] / (poplist["中北地域",] + poplist["峡南地域",])
    covid[i, 23] <- poplist["早川町",] / (poplist["中北地域",] + poplist["峡南地域",])
    covid[i, 24] <- poplist["身延町",] / (poplist["中北地域",] + poplist["峡南地域",])
    covid[i, 25] <- poplist["南部町",] / (poplist["中北地域",] + poplist["峡南地域",])
    covid[i, 26] <- poplist["富士川町",] / (poplist["中北地域",] + poplist["峡南地域",])
  }else{
  } 
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県 (中央市、山梨市、東部地域)"){
    covid[i,9:35] <- 0
    covid[i, 21] <- poplist["中央市",] / (poplist["中央市",] + poplist["山梨市",] + poplist["東部地域",])
    covid[i, 12] <- poplist["山梨市",] / (poplist["中央市",] + poplist["山梨市",] + poplist["東部地域",])
    covid[i, 11] <- poplist["都留市",] / (poplist["中央市",] + poplist["山梨市",] + poplist["東部地域",])
    covid[i, 13] <- poplist["大月市",] / (poplist["中央市",] + poplist["山梨市",] + poplist["東部地域",])
    covid[i, 19] <- poplist["上野原市",] / (poplist["中央市",] + poplist["山梨市",] + poplist["東部地域",])
    covid[i, 28] <- poplist["道志村",] / (poplist["中央市",] + poplist["山梨市",] + poplist["東部地域",])
    covid[i, 34] <- poplist["小菅村",] / (poplist["中央市",] + poplist["山梨市",] + poplist["東部地域",])
    covid[i, 35] <- poplist["丹波山村",] / (poplist["中央市",] + poplist["山梨市",] + poplist["東部地域",])
    
  }else{
  } 
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県 (南アルプス市、峡南地域)"){
    covid[i,9:35] <- 0
    covid[i, 15] <- poplist["南アルプス市",] / (poplist["南アルプス市",] + poplist["峡南地域",])
    covid[i, 22] <- poplist["市川三郷町",] / (poplist["南アルプス市",] + poplist["峡南地域",])
    covid[i, 23] <- poplist["早川町",] / (poplist["南アルプス市",] + poplist["峡南地域",])
    covid[i, 24] <- poplist["身延町",] / (poplist["南アルプス市",] + poplist["峡南地域",])
    covid[i, 25] <- poplist["南部町",] / (poplist["南アルプス市",] + poplist["峡南地域",])
    covid[i, 26] <- poplist["富士川町",] / (poplist["南アルプス市",] + poplist["峡南地域",])
  }else{
  } 
  
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県 (富士北麓地域、笛吹市)"){
    covid[i,9:35] <- 0
    covid[i, 18] <- poplist["笛吹市",] /  (poplist["富士北麓地域",] + poplist["笛吹市",])
    covid[i, 33] <- poplist["富士河口湖町",] / (poplist["富士北麓地域",] + poplist["笛吹市",])
    covid[i, 32] <- poplist["鳴沢村",] / (poplist["富士北麓地域",] + poplist["笛吹市",])
    covid[i, 10] <- poplist["富士吉田市",] /  (poplist["富士北麓地域",] + poplist["笛吹市",])
    covid[i, 29] <- poplist["西桂町",] /  (poplist["富士北麓地域",] + poplist["笛吹市",])
    covid[i, 30] <- poplist["忍野村",] /  (poplist["富士北麓地域",] + poplist["笛吹市",])
    covid[i, 31] <- poplist["山中湖村",] /  (poplist["富士北麓地域",] + poplist["笛吹市",])
  }else{
  } 
  
}

# delete 県外居住者
covid <- na.omit(covid)


# Melting-----

cleancovid <- covid %>% 
  select("id", "公表日", cities, ) 

###同じ日付の感染数を市ごとでまとめる



cleancovid <- melt(cleancovid, id.vars = "公表日",
                   measure.vars = cities,
                   variable.name = "city",
                   value.name = "Nofcases") %>% 
  group_by(公表日, city) %>% 
  summarize(Nofcases = round(sum(Nofcases), digits = 3)) %>%
  ungroup()

cleancovid <- cleancovid %>% 
  group_by(city) %>% 
  complete(公表日 = seq.Date(min(公表日), max(公表日), by = "day"),
           fill = list(Nofcases = 0)) %>% 
  arrange(公表日) %>% 
  ungroup()

cleancovid$cum_cases <- ave(cleancovid$Nofcases, cleancovid$city, FUN = cumsum)

# write.csv(cleancovid, "03_build/Case_positive/output/市町村別感染者数.csv")

# weekly data-----
weekcovid <- cleancovid
weekcovid$公表日 <- floor_date(weekcovid$公表日, "week")

weekcovid <- weekcovid %>% 
  group_by(公表日, city) %>% 
  summarize_at(vars(2), funs(sum(., na.rm=TRUE))) %>% 
  ungroup()


write.csv(weekcovid, "03_build/Case_positive/output/市町村別感染者数_週別.csv", row.names=FALSE)




