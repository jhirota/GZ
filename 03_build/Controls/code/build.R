library(tidyverse)
library(lubridate)

# Data load----------
pop47 <- read_csv(here::here("02_bring/Pop/data/Population_JPN.csv")) 
covid47 <- read_csv(here::here("03_build/GZ_covid/output/GZ_covid.csv"))
mob6_47 <- read_csv(here::here("03_build/Agoop/output/sixpref.csv"))

ymob_city47 <- rbind(read_csv(here::here("02_bring/Papilio_mobility/data/市区町村_prefecture_chart_data_yamanashi2020のコピー.csv")),
                     read_csv(here::here("02_bring/Papilio_mobility/data/市区町村_prefecture_chart_data_yamanashi2021のコピー.csv")))


# Data clean ---------

cutfunc <- function(x){
  str_split(x, "[:]")[[1]][2]
}

mob6_47 <- mob6_47 %>% 
  select(-week) %>% 
  mutate(week = as.Date(ymd(start_day)),
         pref =  map(pref, cutfunc),
         pref = as.character(pref) %>% str_replace_all(c(山梨県 = "Yamanashi",
                                                             茨城県 = "Ibaraki",
                                                             栃木県 = "Tochigi",
                                                             群馬県 = "Gunma",
                                                             長野県 = "Nagano",
                                                             静岡県 = "Shizuoka")),
         pref_resid = map(pref_resid, cutfunc),
         pref_resid = as.character(pref_resid) %>% str_replace_all(c(山梨県 = "Yamanashi",
                                                                       茨城県 = "Ibaraki",
                                                                       栃木県 = "Tochigi",
                                                                       群馬県 = "Gunma",
                                                                       長野県 = "Nagano",
                                                                       静岡県 = "Shizuoka"))) %>% 
  select(-start_day)


# Data wrangling covid -----

covid47 <- covid47 %>%
  mutate(week = floor_date(covid47$date, "week",
                           week_start = getOption("lubridate.week.start", 1))) %>% 
  group_by(pref, week) %>% 
  summarize(across(c(newcase_day, newdeath_day),
                   sum)) %>%
  ungroup()


# pop data cleaning --------

pop47 <- pop47 %>% 
  rename(pref = 1,
         pop = 2) %>% 
  mutate(pop = pop * 1000,
         pref = str_replace_all(pref,
                                c(山梨県 = "Yamanashi",
                                  茨城県 = "Ibaraki",
                                  栃木県 = "Tochigi",
                                  群馬県 = "Gunma",
                                  長野県 = "Nagano",
                                  静岡県 = "Shizuoka")))


# New data merge and wrangling ----------

newmob6_47 <- left_join(x = mob6_47,
                        y = covid47,
                        by = c("pref", "week")) %>% 
  left_join(y = pop47, by = "pref") %>% 
  mutate(newcases_per = newcase_day / pop) %>%
  complete(pref_resid, nesting(pref, week)) %>% 
  group_by(pref) %>% 
  mutate(week_lead1 = lead(week, order_by = week, n = 48),
         newcase_lead1 = if_else(week + 7 == week_lead1,
                                 lead(newcase_day, order_by = week, n = 48),
                                 NA_real_),
         week_lead2 = lead(week, order_by = week, n = 95),
         newcase_lead2 = if_else(week + 14 == week_lead2,
                                 lead(newcase_day, order_by = week, n = 95),
                                 NA_real_)) %>% 
  ungroup()



#####ここから再開

# newmob6_47$newexposed_lag1w <- sapply(1:nrow(newmob6_47), function(x) newmob6_47$newcases_week_announced[x-47])#一週間後の公表数(暴露が一週間前と想定)
# newmob6_47$newexposed_lag2w <- sapply(1:nrow(newmob6_47), function(x) newmob6_47$newcases_week_announced[x-94])#二週間後の公表数(暴露が二週間前と想定)
for (i in 1:nrow(newmob6_47)) {
  sub <- filter(newmob6_47, (都道府県==newmob6_47$都道府県[i]) & (居住都道府県==newmob6_47$居住都道府県[i]))
  Row_lag1 = which(sub$week == newmob6_47$week[i] + 7)
  Row_lag2 = which(sub$week == newmob6_47$week[i] + 14)
  if (length(Row_lag1) != 0){
    newmob6_47$newexposed_lag1w[i] <- sub$newcases_week_announced[Row_lag1]#一週間後の公表数(暴露が一週間前と想定)
  }else{
    newmob6_47$newexposed_lag1w[i] <- NA
  }
  if (length(Row_lag2) != 0){
    newmob6_47$newexposed_lag2w[i] <- sub$newcases_week_announced[Row_lag2]#二週間後の公表数(暴露が二週間前と想定)
  }else{
    newmob6_47$newexposed_lag2w[i] <- NA
  }
} 


newmob6_47$newexposed_lag1w_per <- newmob6_47$newexposed_lag1w / newmob6_47$居住都道府県の人口

newmob6_47$newexposed_lag2w_per <- newmob6_47$newexposed_lag2w / newmob6_47$居住都道府県の人口


# newmob6_47$潜在的感染者人流 <- newmob6_47$sum_pop * newmob6_47$newcases_per #公表数と人流をかけたもの。(多分使っちゃダメ)
# newmob6_47 <- newmob6_47[,-10]

newmob6_47$潜在的感染者人流_lag1 <- newmob6_47$sum_pop * newmob6_47$newexposed_lag1w_per #一週間後の人口あたり公表数*流入人流(暴露が一週間前と想定)
newmob6_47$潜在的感染者人流_lag2 <- newmob6_47$sum_pop * newmob6_47$newexposed_lag2w_per #二週間後の人口あたり公表数*流入人流(暴露が二週間前と想定)
# 潜在的感染者人流を一週間後の新規感染者の公表数✖流入人口で考えてよいのか？
# 正直、感染して以降は隔離すると想定すれば、
# あえて二週間分とかの感染者数の合計を取らずに新規感染者数だけで見るのもメイクセンスだが。果たして。

for (i in 1:nrow(newmob6_47)) {
  sub <- filter(newmob6_47, (都道府県==newmob6_47$都道府県[i]) & (居住都道府県==newmob6_47$居住都道府県[i]))
  Row_1wb4 = which(sub$week == newmob6_47$week[i] - 7)
  Row_2wb4 = which(sub$week == newmob6_47$week[i] - 14)
  if ((length(Row_1wb4) != 0)&(length(Row_2wb4) != 0)){
    newmob6_47$agrgt潜在的感染者人流_lag1[i] <- sub$潜在的感染者人流_lag1[Row_1wb4] + sub$潜在的感染者人流_lag1[Row_2wb4]
  }else{
    newmob6_47$agrgt潜在的感染者人流_lag1[i] <- NA
  }
  if ((length(Row_1wb4) != 0)&(length(Row_2wb4) != 0)){
    newmob6_47$agrgt潜在的感染者人流_lag2[i] <- sub$潜在的感染者人流_lag2[Row_1wb4] + sub$潜在的感染者人流_lag2[Row_2wb4]
  }else{
    newmob6_47$agrgt潜在的感染者人流_lag2[i] <- NA
  }
} 
# 一週間前の潜在的感染者人流、あるいは二週間前の潜在的感染者人流がpapilioでとれていない場合に、どうするか。
# 上記のコードでは、どちらかが欠けた場合には直ちにNAとなる。
# 一週間前と二週間前の潜在的感染者人流をaggregateしたものが、上記。

newmob6_47$Scaled_agrgt潜感人流_lag1 <- newmob6_47$agrgt潜在的感染者人流_lag1 / newmob6_47$居住都道府県の人口
newmob6_47$Scaled_agrgt潜感人流_lag2 <- newmob6_47$agrgt潜在的感染者人流_lag2 / newmob6_47$居住都道府県の人口



write_csv(format(newmob6_47, scientific = FALSE), "03_build/Controls/output/47都道府県から６県への潜在的感染者人流.csv")

## 潜在的感染者人流aggregate 用 (県別)-------
Agrgt <- newmob6_47 %>% 
  group_by(週, week, 都道府県) %>% 
  summarize_at(vars(2, 12:15), funs(sum(., na.rm=TRUE))) %>% 
  arrange() %>% 
  ungroup()

write.csv(Agrgt, "03_build/Controls/output/県別流入リスク.csv", row.names = FALSE)


## city level data cleaning----------

names(ymob_city47)[7] <- "city"
names(ymob_city47)[5] <- "week"
x <- substr(ymob_city47$week, 1, 4)
y <- substr(ymob_city47$week, 5, 6)
z <- substr(ymob_city47$week, 7, 8)
ymob_city47$week <- paste(x, y, z, sep = "-")
ymob_city47$week <- as.Date(ymob_city47$week)

for (i in 1:nrow(ymob_city47)) {
  ymob_city47$city[i] <- unlist(strsplit(ymob_city47$city[i], split = ':', fixed=TRUE))[2]
}

for (i in 1:nrow(ymob_city47)) {
  ymob_city47$居住都道府県[i] <- unlist(strsplit(ymob_city47$居住都道府県[i], split = ':', fixed=TRUE))[2]
}



# city level data merge and wrangling-------

newymob_city47 <- left_join(x = ymob_city47,
                            y = covid47_wk,
                            by = c("居住都道府県", "week"))

for (i in 1:nrow(newymob_city47)) {
  Pref = newymob_city47$居住都道府県[i]
  Row = which(pop47$Pref == Pref)
  newymob_city47$居住都道府県の人口[i] <- pop47[Row,2]
  newymob_city47$covid_per[i] <- newymob_city47$各地の感染者数_1日ごとの発表数[i] / newymob_city47$居住都道府県の人口[i] #公表数を人口で割ったもの
}


names(newymob_city47)[10] <- "newcases_week_announced"
names(newymob_city47)[11] <- "newdeath_week_announced"

for (i in 1:nrow(newymob_city47)) {
  sub <- filter(newymob_city47, (city==newymob_city47$city[i]) & (居住都道府県==newymob_city47$居住都道府県[i]))
  Row_lag1 = which(sub$week == newymob_city47$week[i] + 7)
  Row_lag2 = which(sub$week == newymob_city47$week[i] + 14)
  if (length(Row_lag1) != 0){
    newymob_city47$newexposed_lag1w[i] <- sub$newcases_week_announced[Row_lag1]#一週間後の公表数(暴露が一週間前と想定)
  }else{
    newymob_city47$newexposed_lag1w[i] <- NA
  }
  if (length(Row_lag2) != 0){
    newymob_city47$newexposed_lag2w[i] <- sub$newcases_week_announced[Row_lag2]#二週間後の公表数(暴露が二週間前と想定)
  }else{
    newymob_city47$newexposed_lag2w[i] <- NA
  }
} 


newymob_city47$newexposed_lag1w_per <- newymob_city47$newexposed_lag1w / newymob_city47$居住都道府県の人口

newymob_city47$newexposed_lag2w_per <- newymob_city47$newexposed_lag2w / newymob_city47$居住都道府県の人口



newymob_city47$潜在的感染者人流_lag1 <- newymob_city47$population_inflow * newymob_city47$newexposed_lag1w_per #一週間後の人口あたり公表数*流入人流(暴露が一週間前と想定)
newymob_city47$潜在的感染者人流_lag2 <- newymob_city47$population_inflow * newymob_city47$newexposed_lag2w_per #二週間後の人口あたり公表数*流入人流(暴露が二週間前と想定)
# 潜在的感染者人流を一週間後の新規感染者の公表数✖流入人口で考えてよいのか？
# 正直、感染して以降は隔離すると想定すれば、
# あえて二週間分とかの感染者数の合計を取らずに新規感染者数だけで見るのもメイクセンスだが。果たして。

for (i in 1:nrow(newymob_city47)) {
  sub <- filter(newymob_city47, (city==newymob_city47$city[i]) & (居住都道府県==newymob_city47$居住都道府県[i]))
  Row_1wb4 = which(sub$week == newymob_city47$week[i] - 7)
  Row_2wb4 = which(sub$week == newymob_city47$week[i] - 14)
  if ((length(Row_1wb4) != 0)&(length(Row_2wb4) != 0)){
    newymob_city47$agrgt潜在的感染者人流_lag1[i] <- sub$潜在的感染者人流_lag1[Row_1wb4] + sub$潜在的感染者人流_lag1[Row_2wb4]
  }else{
    newymob_city47$agrgt潜在的感染者人流_lag1[i] <- NA
  }
  if ((length(Row_1wb4) != 0)&(length(Row_2wb4) != 0)){
    newymob_city47$agrgt潜在的感染者人流_lag2[i] <- sub$潜在的感染者人流_lag2[Row_1wb4] + sub$潜在的感染者人流_lag2[Row_2wb4]
  }else{
    newymob_city47$agrgt潜在的感染者人流_lag2[i] <- NA
  }
} 
# 一週間前の潜在的感染者人流、あるいは二週間前の潜在的感染者人流がpapilioでとれていない場合に、どうするか。
# 上記のコードでは、どちらかが欠けた場合には直ちにNAとなる。
# 一週間前と二週間前の潜在的感染者人流をaggregateしたものが、上記。

newymob_city47$Scaled_agrgt潜感人流_lag1 <- newymob_city47$agrgt潜在的感染者人流_lag1 / newymob_city47$居住都道府県の人口
newymob_city47$Scaled_agrgt潜感人流_lag2 <- newymob_city47$agrgt潜在的感染者人流_lag2 / newymob_city47$居住都道府県の人口

write.csv(newymob_city47, "03_build/Controls/output/47都道府県から山梨市町村への潜在的感染者人流.csv", row.names = FALSE)


## 潜在的感染者人流aggregate 用 (県別)-------
Agrgt_city <- newymob_city47 %>% 
  group_by(週, week, city) %>% 
  summarize_at(vars(6, 18:21), funs(sum(., na.rm=TRUE))) %>% #temp and rainfall
  arrange() %>% 
  ungroup()

names(Agrgt_city)[7:8] <- c("in_risk_lag1","in_risk_lag2")

# write.csv(Agrgt_city, "03_build/Controls/output/市町村別流入リスク.csv", row.names = FALSE)

## 検査数 cleaning ----

PCRmerge <- read.csv("02_bring/PCRtest/data/PCRmerge.csv")

PCRmerge <- PCRmerge[,c(1,2,5)]

for (i in 1:nrow(PCRmerge)) {
  if (PCRmerge$Pref[i] == "茨城県"){
    PCRmerge$Date[i] <- unlist(strsplit(PCRmerge$Date[i], split = 'T', fixed=TRUE))[1]
  }
}


PCRmerge$Date <- gsub("/", "-", PCRmerge$Date)
PCRmerge$Date <- as.Date(PCRmerge$Date)
PCRmerge <- PCRmerge %>% 
  arrange(Date)

for (i in 1:nrow(PCRmerge)) {
  if (PCRmerge$noftests[i] == ""){
    PCRmerge$noftests[i] <- 0
  }
}


PCRmerge <- PCRmerge %>% 
  group_by(Pref, Date) %>%
  complete(Pref = levels(factor(PCRmerge$Pref)),
           fill = list(noftests = 0)) %>%
  arrange(Date)

PCRmerge$Week <- floor_date(PCRmerge$Date, "week",
                            week_start = getOption("lubridate.week.start", 1)) 

PCRmerge$noftests <- as.numeric(PCRmerge$noftests)

PCRmerge <- PCRmerge %>% 
  group_by(Pref, Week) %>%
  summarize(noftests = sum(noftests)) %>%
  ungroup() %>% 
  arrange(Week)

# write.csv(PCRmerge, "PCRtests.csv", row.names = FALSE)

