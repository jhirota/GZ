library(tidyverse)

# New data merge and wrangling ----------

NAnewmob6_47 <- newmob6_47

for (i in 1:nrow(NAnewmob6_47)) {
  Pref = NAnewmob6_47$居住都道府県[i]
  Row = which(pop47$Pref == Pref)
  NAnewmob6_47$居住都道府県の人口[i] <- pop47[Row,2]
  NAnewmob6_47$newcases_per[i] <- NAnewmob6_47$newcases_week_announced[i] / NAnewmob6_47$居住都道府県の人口[i] #公表数を人口で割ったもの。
}




names(NAnewmob6_47)[6] <- "newcases_week_announced"
names(NAnewmob6_47)[7] <- "newdeath_week_announced"


for (i in 1:nrow(NAnewmob6_47)) {
  sub <- filter(NAnewmob6_47, (都道府県==NAnewmob6_47$都道府県[i]) & (居住都道府県==NAnewmob6_47$居住都道府県[i]))
  Row_lag1 = which(sub$week == NAnewmob6_47$week[i] + 7)
  Row_lag2 = which(sub$week == NAnewmob6_47$week[i] + 14)
  if (length(Row_lag1) != 0){
    NAnewmob6_47$newexposed_lag1w[i] <- sub$newcases_week_announced[Row_lag1]#一週間後の公表数(暴露が一週間前と想定)
  }else{
    NAnewmob6_47$newexposed_lag1w[i] <- 0
  }
  if (length(Row_lag2) != 0){
    NAnewmob6_47$newexposed_lag2w[i] <- sub$newcases_week_announced[Row_lag2]#二週間後の公表数(暴露が二週間前と想定)
  }else{
    NAnewmob6_47$newexposed_lag2w[i] <- 0
  }
} 


NAnewmob6_47$newexposed_lag1w_per <- NAnewmob6_47$newexposed_lag1w / NAnewmob6_47$居住都道府県の人口

NAnewmob6_47$newexposed_lag2w_per <- NAnewmob6_47$newexposed_lag2w / NAnewmob6_47$居住都道府県の人口


NAnewmob6_47$潜在的感染者人流_lag1 <- NAnewmob6_47$sum_pop * NAnewmob6_47$newexposed_lag1w_per #一週間後の人口あたり公表数*流入人流(暴露が一週間前と想定)
NAnewmob6_47$潜在的感染者人流_lag2 <- NAnewmob6_47$sum_pop * NAnewmob6_47$newexposed_lag2w_per #二週間後の人口あたり公表数*流入人流(暴露が二週間前と想定)


for (i in 1:nrow(NAnewmob6_47)) {
  sub <- filter(NAnewmob6_47, (都道府県==NAnewmob6_47$都道府県[i]) & (居住都道府県==NAnewmob6_47$居住都道府県[i]))
  Row_1wb4 = which(sub$week == NAnewmob6_47$week[i] - 7)
  Row_2wb4 = which(sub$week == NAnewmob6_47$week[i] - 14)
  if ((length(Row_1wb4) != 0)&(length(Row_2wb4) != 0)){
    NAnewmob6_47$agrgt潜在的感染者人流_lag1[i] <- sub$潜在的感染者人流_lag1[Row_1wb4] + sub$潜在的感染者人流_lag1[Row_2wb4]
  }else{
    NAnewmob6_47$agrgt潜在的感染者人流_lag1[i] <- 0
  }
  if ((length(Row_1wb4) != 0)&(length(Row_2wb4) != 0)){
    NAnewmob6_47$agrgt潜在的感染者人流_lag2[i] <- sub$潜在的感染者人流_lag2[Row_1wb4] + sub$潜在的感染者人流_lag2[Row_2wb4]
  }else{
    NAnewmob6_47$agrgt潜在的感染者人流_lag2[i] <- 0
  }
} 
# 一週間前の潜在的感染者人流、あるいは二週間前の潜在的感染者人流がpapilioでとれていない場合に、どうするか。
# 上記のコードでは、どちらかが欠けた場合には直ちにNAとなる。
# 一週間前と二週間前の潜在的感染者人流をaggregateしたものが、上記。



write.csv(format(NAnewmob6_47, scientific = FALSE), "NAto047都道府県から６県への潜在的感染者人流.csv")

## 潜在的感染者人流aggregate 用 (県別)-------
NAAgrgt <- NAnewmob6_47 %>% 
  group_by(週, week, 都道府県) %>% 
  summarize_at(vars(2, 12:15), funs(sum(., na.rm=TRUE))) %>% 
  arrange() %>% 
  ungroup()



ggplot(data = NAAgrgt,
       mapping = aes(x = week,
                     y = agrgt潜在的感染者人流_lag1,
                     color = 都道府県)) +
  geom_point(size = 0.1) +
  geom_line(size = 0.5) +
  ggtitle("他県からの感染者流入リスクの推移(県別)") +
  theme_gray (base_family = "HiraKakuPro-W3") +
  scale_color_manual(values=c("red",rep("grey",5)))


ggplot(data = NAAgrgt,
       mapping = aes(x = week,
                     y = sum_pop,
                     color = 都道府県)) +
  geom_point(size = 0.1) +
  geom_line(size = 0.5) +
  ggtitle("他県からの人流流入の推移(県別)") +
  theme_gray (base_family = "HiraKakuPro-W3") +
  scale_color_brewer(palette = "Greys")


# Agrgt[,5:8] <- base::format(Agrgt[,5:8], scientific = FALSE)
write.csv(NAAgrgt, "NA県別流入リスク.csv")




# NAto0city level data merge and wrangling-------

NAnewymob_city47 <- newymob_city47

for (i in 1:nrow(NAnewymob_city47)) {
  Pref = NAnewymob_city47$居住都道府県[i]
  Row = which(pop47$Pref == Pref)
  NAnewymob_city47$居住都道府県の人口[i] <- pop47[Row,2]
  NAnewymob_city47$covid_per[i] <- NAnewymob_city47$各地の感染者数_1日ごとの発表数[i] / NAnewymob_city47$居住都道府県の人口[i] #公表数を人口で割ったもの
}


names(NAnewymob_city47)[10] <- "newcases_week_announced"
names(NAnewymob_city47)[11] <- "newdeath_week_announced"

for (i in 1:nrow(NAnewymob_city47)) {
  sub <- filter(NAnewymob_city47, (city==NAnewymob_city47$city[i]) & (居住都道府県==NAnewymob_city47$居住都道府県[i]))
  Row_lag1 = which(sub$week == NAnewymob_city47$week[i] + 7)
  Row_lag2 = which(sub$week == NAnewymob_city47$week[i] + 14)
  if (length(Row_lag1) != 0){
    NAnewymob_city47$newexposed_lag1w[i] <- sub$newcases_week_announced[Row_lag1]#一週間後の公表数(暴露が一週間前と想定)
  }else{
    NAnewymob_city47$newexposed_lag1w[i] <- NA
  }
  if (length(Row_lag2) != 0){
    NAnewymob_city47$newexposed_lag2w[i] <- sub$newcases_week_announced[Row_lag2]#二週間後の公表数(暴露が二週間前と想定)
  }else{
    NAnewymob_city47$newexposed_lag2w[i] <- NA
  }
} 


NAnewymob_city47$newexposed_lag1w_per <- NAnewymob_city47$newexposed_lag1w / NAnewymob_city47$居住都道府県の人口

NAnewymob_city47$newexposed_lag2w_per <- NAnewymob_city47$newexposed_lag2w / NAnewymob_city47$居住都道府県の人口



NAnewymob_city47$潜在的感染者人流_lag1 <- NAnewymob_city47$population_inflow * NAnewymob_city47$newexposed_lag1w_per #一週間後の人口あたり公表数*流入人流(暴露が一週間前と想定)
NAnewymob_city47$潜在的感染者人流_lag2 <- NAnewymob_city47$population_inflow * NAnewymob_city47$newexposed_lag2w_per #二週間後の人口あたり公表数*流入人流(暴露が二週間前と想定)
# 潜在的感染者人流を一週間後の新規感染者の公表数✖流入人口で考えてよいのか？
# 正直、感染して以降は隔離すると想定すれば、
# あえて二週間分とかの感染者数の合計を取らずに新規感染者数だけで見るのもメイクセンスだが。果たして。

for (i in 1:nrow(NAnewymob_city47)) {
  sub <- filter(NAnewymob_city47, (city==NAnewymob_city47$city[i]) & (居住都道府県==NAnewymob_city47$居住都道府県[i]))
  Row_1wb4 = which(sub$week == NAnewymob_city47$week[i] - 7)
  Row_2wb4 = which(sub$week == NAnewymob_city47$week[i] - 14)
  if ((length(Row_1wb4) != 0)&(length(Row_2wb4) != 0)){
    NAnewymob_city47$agrgt潜在的感染者人流_lag1[i] <- sub$潜在的感染者人流_lag1[Row_1wb4] + sub$潜在的感染者人流_lag1[Row_2wb4]
  }else{
    NAnewymob_city47$agrgt潜在的感染者人流_lag1[i] <- NA
  }
  if ((length(Row_1wb4) != 0)&(length(Row_2wb4) != 0)){
    NAnewymob_city47$agrgt潜在的感染者人流_lag2[i] <- sub$潜在的感染者人流_lag2[Row_1wb4] + sub$潜在的感染者人流_lag2[Row_2wb4]
  }else{
    NAnewymob_city47$agrgt潜在的感染者人流_lag2[i] <- NA
  }
} 
# 一週間前の潜在的感染者人流、あるいは二週間前の潜在的感染者人流がpapilioでとれていない場合に、どうするか。
# 上記のコードでは、どちらかが欠けた場合には直ちにNAとなる。
# 一週間前と二週間前の潜在的感染者人流をaggregateしたものが、上記。

NAnewymob_city47$Scaled_agrgt潜感人流_lag1 <- NAnewymob_city47$agrgt潜在的感染者人流_lag1 / NAnewymob_city47$居住都道府県の人口
NAnewymob_city47$Scaled_agrgt潜感人流_lag2 <- NAnewymob_city47$agrgt潜在的感染者人流_lag2 / NAnewymob_city47$居住都道府県の人口

write.csv(NAnewymob_city47, "NAto047都道府県から山梨市町村への潜在的感染者人流.csv")


## 潜在的感染者人流aggregate 用 (県別)-------
NAAgrgt_city <- NAnewymob_city47 %>% 
  group_by(週, week, city) %>% 
  summarize_at(vars(6, 18:21), funs(sum(., na.rm=TRUE))) %>% #temp and rainfall
  arrange() %>% 
  ungroup()

names(NAAgrgt_city)[7:8] <- c("in_risk_lag1","in_risk_lag2")

write.csv(NAAgrgt_city, "NA市町村別流入リスク.csv")

