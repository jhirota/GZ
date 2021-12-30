library(tidyverse)
library(jpndistrict)
library(reshape2)
library(lubridate)


cities <- c("甲府市", "富士吉田市", "都留市","山梨市", "大月市", "韮崎市", "南アルプス市", "北杜市",
            "甲斐市", "笛吹市", "上野原市", "甲州市", "中央市", "市川三郷町", "早川町", "身延町",
            "南部町", "富士川町", "昭和町", "道志村", "西桂町", "忍野村", "山中湖村", "鳴沢村",
            "富士河口湖町", "小菅村", "丹波山村")

# data load------
yamanashi <- jpn_pref(19) #yamanashi mapping data

# data clean -------
GZlist <-read.csv("02_bring/GZlist/data/GZlist.csv") %>% 
  filter(Keido <= 139.5) %>% 
  filter(Ido >= 35 & Ido <= 37) %>% 
  filter(Type == "Food")

for (i in 1:nrow(GZlist)){
  for(j in 1:length(cities)){
    if(GZlist[i, 5] %in% grep(cities[j], GZlist[i, 5], value = TRUE)){
      GZlist[i, "city"] <- cities[j]
    }else{
    } 
  }
}

# only once
# write.csv(GZlist, "03_build/GZlist/output/GZlist_Food.csv")

table <- table(GZlist$city)
data <- data.frame(table)
names(data)[1] <- "city"

yamanashi$city <- gsub("西八代郡 ", "", yamanashi$city)
yamanashi$city <- gsub("南都留郡 ", "", yamanashi$city)
yamanashi$city <- gsub("北都留郡 ", "", yamanashi$city)
yamanashi$city <- gsub("中巨摩郡 ", "", yamanashi$city)
yamanashi$city <- gsub("南巨摩郡 ", "", yamanashi$city)

yamanashi <- left_join(yamanashi, data, by = "city") 

# Frequency
Freq100 <- subset(yamanashi, subset = (Freq >= 100))["city"][[1]]
Freq200 <- subset(yamanashi, subset = (Freq >= 200))["city"][[1]]

write.csv(Freq100, "03_build/GZlist/output/Freq100.csv", row.names = FALSE)


# data wrangling GZ40週時点(2020/10/04まで) ---------
vresasdata <- read.csv("02_bring/Vresas/data/weekly_vresas.csv")
GZlist$Date_approval <- as.Date(GZlist$Date_approval)

GZlist40th <- GZlist %>% 
  filter(Date_approval <= 18539) # 2020/10/04まで

table2 <- table(GZlist40th$city) %>% 
  data.frame()
names(table2)[1] <- "city"
names(table2)[2] <- "Freq40th"

yamanashi <- left_join(yamanashi, table2, by = "city") 


# data wrangling GZ52週時点(2020/12/27まで) ----------
GZlist52nd <- GZlist %>% 
  filter(Date_approval <= 18623)

table3 <- table(GZlist52nd$city) %>% 
  data.frame()
names(table3)[1] <- "city"
names(table3)[2] <- "Freq52nd"

yamanashi <- left_join(yamanashi, table3, by = "city") 

# yamanashi_comp <- yamanashi
# only once
# write.csv(yamanashi_comp, "03_build/GZlist/output/GZ_mapping.csv")


# GZ list with time series making ---

N <- nrow(GZlist)
createEmptyDf = function( nrow, ncol, colnames = c() ){
  data.frame( matrix( vector(), nrow, ncol, dimnames = list( c(), colnames ) ) )
}
x = createEmptyDf(N, 27, colnames = cities )

t_GZlist <- cbind(GZlist, x)


for (i in 1:nrow(t_GZlist)){
  for(j in 1:length(cities)){
    if(t_GZlist[i, 10] %in% cities[j]){
      t_GZlist[i, 11:37] <- 0
      t_GZlist[i, cities[j]] <- 1
    }else{
    } 
  }
} 


t_GZlist <- t_GZlist %>% 
  melt(id.vars = "Date_approval",
       measure.vars = cities,
       variable.name = "city",
       value.name = "Nofcases") %>% 
  group_by(Date_approval, city) %>% 
  summarize(sum = sum(Nofcases)) %>% 
  ungroup()


# write.csv(t_GZlist, "03_build/GZlist/output/GZlist_timeseries.csv")


# GZall times series------------------

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

N = nrow(GZall_list)
createEmptyDf = function( nrow, ncol, colnames = c() ){
  data.frame( matrix( vector(), nrow, ncol, dimnames = list( c(), colnames ) ) )
}
x = createEmptyDf( N, 27, colnames = cities )
t_GZall_list <- cbind(GZall_list, x)


for (i in 1:nrow(t_GZall_list)){
  for(j in 1:length(cities)){
    if(t_GZall_list[i, 10] %in% cities[j]){
      t_GZall_list[i, 11:37] <- 0
      t_GZall_list[i, cities[j]] <- 1
    }else{
    } 
  }
} 

t_GZall_list <- t_GZall_list %>% 
  melt(id.vars = "Date_approval",
       measure.vars = cities,
       variable.name = "city",
       value.name = "Nofcases") %>% 
  group_by(Date_approval, city) %>% 
  summarize(sum = sum(Nofcases)) %>% 
  ungroup()


write.csv(t_GZall_list, "03_build/GZlist/output/GZalllist_timeseries.csv", row.names=FALSE)


