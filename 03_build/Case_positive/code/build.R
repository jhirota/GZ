library(readxl)
library(tidyverse)

# data load ------
covid <- read_excel("02_bring/Covid_cases/data/yousei20210512.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
pop <- read.csv("02_bring/Covid_cases/data/population/山梨県人口202004.csv")

# data clean ------
pop <- pop[-34:-40,]
pop[1,2] <- "合計"
poplist <- data.frame(pop[, 5])
row.names(poplist) <- pop[, 2]
poplist[,1] <- as.numeric(sub(",", "", poplist[,1]))
poplist <- t(poplist) 

# List of cities in Yamanashi
cities <- c("甲府市", "富士吉田市", "都留市","山梨市", "大月市", "韮崎市", "南アルプス市", "北杜市",
            "甲斐市", "笛吹市", "上野原市", "甲州市", "中央市", "市川三郷町", "早川町", "身延町",
            "南部町", "富士川町", "昭和町", "道志村", "西桂町", "忍野村", "山中湖村", "鳴沢村",
            "富士河口湖町", "小菅村", "丹波山村")

N = nrow(covid)
createEmptyDf = function(nrow, ncol, colnames = c() ){
  data.frame( matrix( vector(), nrow, ncol, dimnames = list( c(), colnames ) ) )
}
x = createEmptyDf( N, 27, colnames = cities )

covid <- cbind(covid, x)


# data wrangling ---------

# Apportioning new cases to cities by ratio of city population
for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県"){
    covid[i, 9] <- poplist["甲府市",] / poplist["合計",]
    covid[i, 10] <- poplist["富士吉田市",] / poplist["合計",]
    covid[i, 11] <- poplist["都留市",] / poplist["合計",]
    covid[i, 12] <- poplist["山梨市",] / poplist["合計",]
    covid[i, 13] <- poplist["大月市",] / poplist["合計",]
    covid[i, 14] <- poplist["韮崎市",] / poplist["合計",]
    covid[i, 15] <- poplist["南アルプス市",] / poplist["合計",]
    covid[i, 16] <- poplist["北杜市",] / poplist["合計",]
    covid[i, 17] <- poplist["甲斐市",] / poplist["合計",]
    covid[i, 18] <- poplist["笛吹市",] / poplist["合計",]
    covid[i, 19] <- poplist["上野原市",] / poplist["合計",]
    covid[i, 20] <- poplist["甲州市",] / poplist["合計",]
    covid[i, 21] <- poplist["中央市",] / poplist["合計",]
    covid[i, 22] <- poplist["市川三郷町",] / poplist["合計",]
    covid[i, 23] <- poplist["早川町",] / poplist["合計",]
    covid[i, 24] <- poplist["身延町",] / poplist["合計",]
    covid[i, 25] <- poplist["南部町",] / poplist["合計",]
    covid[i, 26] <- poplist["富士川町",] / poplist["合計",]
    covid[i, 27] <- poplist["昭和町",] / poplist["合計",]
    covid[i, 28] <- poplist["道志村",] / poplist["合計",]
    covid[i, 29] <- poplist["西桂町",] / poplist["合計",]
    covid[i, 30] <- poplist["忍野村",] / poplist["合計",]
    covid[i, 31] <- poplist["山中湖村",] / poplist["合計",]
    covid[i, 32] <- poplist["鳴沢村",] / poplist["合計",]
    covid[i, 33] <- poplist["富士河口湖町",] / poplist["合計",]
    covid[i, 34] <- poplist["小菅村",] / poplist["合計",]
    covid[i, 35] <- poplist["丹波山村",] / poplist["合計",]
  }else{
  } 
  
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "甲府市"){
    covid[i, 9] <- 1
    covid[i, 10:35] <- 0
  }else{
  } 
}

for (i in 1:nrow(covid)){
  for(j in 1:length(cities)){
    if(covid[i, 8] %in% paste("山梨県(", cities[j], ")", sep = "")){
      covid[i, 9:35] <- 0
      covid[i, cities[j]] <- 1
    }else{
    } 
  }
} 

for (i in 1:nrow(covid)){
  for(j in 1:length(cities)){
    if(covid[i, 8] %in% paste("山梨県 (", cities[j], ")", sep = "")){
      covid[i, 9:35] <- 0
      covid[i, cities[j]] <- 1
    }else{
    } 
  }
} 


for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県 (東部地域)"){
    covid[i,9:35] <- 0
    covid[i, 11] <- poplist["都留市",] / poplist["東部地域",]
    covid[i, 13] <- poplist["大月市",] / poplist["東部地域",]
    covid[i, 19] <- poplist["上野原市",] / poplist["東部地域",]
    covid[i, 28] <- poplist["道志村",] / poplist["東部地域",]
    covid[i, 34] <- poplist["小菅村",] / poplist["東部地域",]
    covid[i, 35] <- poplist["丹波山村",] / poplist["東部地域",]
  }else{
  } 
  
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県 (富士東部地域)"){
    covid[i,9:35] <- 0
    covid[i, 11] <- poplist["都留市",] / (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 13] <- poplist["大月市",] / (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 19] <- poplist["上野原市",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 28] <- poplist["道志村",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 34] <- poplist["小菅村",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 35] <- poplist["丹波山村",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 33] <- poplist["富士河口湖町",] / (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 32] <- poplist["鳴沢村",] / (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 10] <- poplist["富士吉田市",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 29] <- poplist["西桂町",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 30] <- poplist["忍野村",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 31] <- poplist["山中湖村",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
  }else{
  } 
  
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県 (富士・東部地域)"){
    covid[i,9:35] <- 0
    covid[i, 11] <- poplist["都留市",] / (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 13] <- poplist["大月市",] / (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 19] <- poplist["上野原市",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 28] <- poplist["道志村",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 34] <- poplist["小菅村",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 35] <- poplist["丹波山村",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 33] <- poplist["富士河口湖町",] / (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 32] <- poplist["鳴沢村",] / (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 10] <- poplist["富士吉田市",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 29] <- poplist["西桂町",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 30] <- poplist["忍野村",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 31] <- poplist["山中湖村",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
  }else{
  } 
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県 (富士北麓地域、東部地域)"){
    covid[i,9:35] <- 0
    covid[i, 11] <- poplist["都留市",] / (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 13] <- poplist["大月市",] / (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 19] <- poplist["上野原市",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 28] <- poplist["道志村",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 34] <- poplist["小菅村",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 35] <- poplist["丹波山村",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 33] <- poplist["富士河口湖町",] / (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 32] <- poplist["鳴沢村",] / (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 10] <- poplist["富士吉田市",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 29] <- poplist["西桂町",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 30] <- poplist["忍野村",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
    covid[i, 31] <- poplist["山中湖村",] /  (poplist["東部地域",] + poplist["富士北麓地域",])
  }else{
  } 
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県 (富士北麓地域)"){
    covid[i,9:35] <- 0
    covid[i, 33] <- poplist["富士河口湖町",] / poplist["富士北麓地域",]
    covid[i, 32] <- poplist["鳴沢村",] / poplist["富士北麓地域",]
    covid[i, 10] <- poplist["富士吉田市",] /  poplist["富士北麓地域",]
    covid[i, 29] <- poplist["西桂町",] /  poplist["富士北麓地域",]
    covid[i, 30] <- poplist["忍野村",] /  poplist["富士北麓地域",]
    covid[i, 31] <- poplist["山中湖村",] /  poplist["富士北麓地域",]
  }else{
  } 
  
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県 (甲府市、富士北麓地域)"){
    covid[i,9:35] <- 0
    covid[i, 9] <- poplist["甲府市",] /  (poplist["富士北麓地域",] + poplist["甲府市",])
    covid[i, 33] <- poplist["富士河口湖町",] / (poplist["富士北麓地域",] + poplist["甲府市",])
    covid[i, 32] <- poplist["鳴沢村",] / (poplist["富士北麓地域",] + poplist["甲府市",])
    covid[i, 10] <- poplist["富士吉田市",] /  (poplist["富士北麓地域",] + poplist["甲府市",])
    covid[i, 29] <- poplist["西桂町",] /  (poplist["富士北麓地域",] + poplist["甲府市",])
    covid[i, 30] <- poplist["忍野村",] /  (poplist["富士北麓地域",] + poplist["甲府市",])
    covid[i, 31] <- poplist["山中湖村",] /  (poplist["富士北麓地域",] + poplist["甲府市",])
  }else{
  } 
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県 (中北地域)"){
    covid[i,9:35] <- 0
    covid[i, 16] <- poplist["北杜市",] / poplist["中北地域",]
    covid[i, 14] <- poplist["韮崎市",] / poplist["中北地域",]
    covid[i, 15] <- poplist["南アルプス市",] /  poplist["中北地域",]
    covid[i, 17] <- poplist["甲斐市",] /  poplist["中北地域",]
    covid[i, 9] <- poplist["甲府市",] /  poplist["中北地域",]
    covid[i, 27] <- poplist["昭和町",] /  poplist["中北地域",]
    covid[i, 21] <- poplist["中央市",] /  poplist["中北地域",]
  }else{
  } 
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "甲府市外 (中北地域)"){
    covid[i,9:35] <- 0
    covid[i, 16] <- poplist["北杜市",] / poplist["中北地域",]
    covid[i, 14] <- poplist["韮崎市",] / poplist["中北地域",]
    covid[i, 15] <- poplist["南アルプス市",] /  poplist["中北地域",]
    covid[i, 17] <- poplist["甲斐市",] /  poplist["中北地域",]
    covid[i, 9] <- poplist["甲府市",] /  poplist["中北地域",]
    covid[i, 27] <- poplist["昭和町",] /  poplist["中北地域",]
    covid[i, 21] <- poplist["中央市",] /  poplist["中北地域",]
  }else{
  } 
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "中北地域"){
    covid[i,9:35] <- 0
    covid[i, 16] <- poplist["北杜市",] / poplist["中北地域",]
    covid[i, 14] <- poplist["韮崎市",] / poplist["中北地域",]
    covid[i, 15] <- poplist["南アルプス市",] /  poplist["中北地域",]
    covid[i, 17] <- poplist["甲斐市",] /  poplist["中北地域",]
    covid[i, 9] <- poplist["甲府市",] /  poplist["中北地域",]
    covid[i, 27] <- poplist["昭和町",] /  poplist["中北地域",]
    covid[i, 21] <- poplist["中央市",] /  poplist["中北地域",]
  }else{
  } 
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県 (峡南地域)"){
    covid[i,9:35] <- 0
    covid[i, 22] <- poplist["市川三郷町",] / poplist["峡南地域",]
    covid[i, 23] <- poplist["早川町",] / poplist["峡南地域",]
    covid[i, 24] <- poplist["身延町",] / poplist["峡南地域",]
    covid[i, 25] <- poplist["南部町",] / poplist["峡南地域",]
    covid[i, 26] <- poplist["富士川町",] / poplist["峡南地域",]
  }else{
  } 
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県(峡南地域)"){
    covid[i,9:35] <- 0
    covid[i, 22] <- poplist["市川三郷町",] / poplist["峡南地域",]
    covid[i, 23] <- poplist["早川町",] / poplist["峡南地域",]
    covid[i, 24] <- poplist["身延町",] / poplist["峡南地域",]
    covid[i, 25] <- poplist["南部町",] / poplist["峡南地域",]
    covid[i, 26] <- poplist["富士川町",] / poplist["峡南地域",]
  }else{
  } 
}

for (i in 1:nrow(covid)){
  if(covid[i, 8] %in% "山梨県 (峡東地域)"){
    covid[i,9:35] <- 0
    covid[i, 12] <- poplist["山梨市",] / poplist["峡東地域",]
    covid[i, 18] <- poplist["笛吹市",] / poplist["峡東地域",]
    covid[i, 20] <- poplist["甲州市",] / poplist["峡東地域",]
  }else{
  } 
}


write.csv(covid, "covidcases.csv", row.names=FALSE)
