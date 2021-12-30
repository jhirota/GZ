setwd("/Users/kazuyahirokawa/OneDrive - The University of Tokyo/university-related/1S/事例研究/月5 政策評価のための因果推論/データ分析/0627分析/Papilio mobility 6 prefectures")
library(tidyverse)
library(reshape2)

yama2020 <- read.csv("市区町村_prefecture_chart_data_yamanashi2020.csv", header = TRUE)
yama2021 <- read.csv("市区町村_prefecture_chart_data_yamanashi2021.csv", header = TRUE)
shizu2020 <- read.csv("市区町村_prefecture_chart_data_shizuoka2020.csv", header = TRUE)
shizu2021 <- read.csv("市区町村_prefecture_chart_data_shizuoka2021.csv", header = TRUE)
nagano2020 <- read.csv("市区町村_prefecture_chart_data_nagano2020.csv", header = TRUE)
nagano2021 <- read.csv("市区町村_prefecture_chart_data_nagano2021.csv", header = TRUE)
gunma2020 <- read.csv("市区町村_prefecture_chart_data_gunma2020.csv", header = TRUE)
gunma2021 <- read.csv("市区町村_prefecture_chart_data_gunma2021.csv", header = TRUE)
tochi2020 <- read.csv("市区町村_prefecture_chart_data_tochigi2020.csv", header = TRUE)
tochi2021 <- read.csv("市区町村_prefecture_chart_data_tochigi2021.csv", header = TRUE)
ibaragi2020 <- read.csv("市区町村_prefecture_chart_data_ibaragi2020.csv", header = TRUE)
ibaragi2021 <- read.csv("市区町村_prefecture_chart_data_ibaragi2021.csv", header = TRUE)

#
yama2020_1 <- yama2020 %>% 
  group_by(週, 都道府県, 居住都道府県) %>% 
  summarize(sum_pop = sum(as.integer(population_inflow)))

yama2021_1 <- yama2021 %>% 
  group_by(週, 都道府県, 居住都道府県) %>% 
  summarize(sum_pop = sum(as.integer(population_inflow)))

shizu2020_1 <- shizu2020 %>% 
  group_by(週, 都道府県, 居住都道府県) %>% 
  summarize(sum_pop = sum(as.integer(population_inflow)))

shizu2021_1 <- shizu2021 %>% 
  group_by(週, 都道府県, 居住都道府県) %>% 
  summarize(sum_pop = sum(as.integer(population_inflow)))

nagano2020_1 <- nagano2020 %>% 
  group_by(週, 都道府県, 居住都道府県) %>% 
  summarize(sum_pop = sum(as.integer(population_inflow)))

nagano2021_1 <- nagano2021 %>% 
  group_by(週, 都道府県, 居住都道府県) %>% 
  summarize(sum_pop = sum(as.integer(population_inflow)))

gunma2020_1 <- gunma2020 %>% 
  group_by(週, 都道府県, 居住都道府県) %>% 
  summarize(sum_pop = sum(as.integer(population_inflow)))

gunma2021_1 <- gunma2021 %>% 
  group_by(週, 都道府県, 居住都道府県) %>% 
  summarize(sum_pop = sum(as.integer(population_inflow)))

tochi2020_1 <- tochi2020 %>% 
  group_by(週, 都道府県, 居住都道府県) %>% 
  summarize(sum_pop = sum(as.integer(population_inflow)))

tochi2021_1 <- tochi2021 %>% 
  group_by(週, 都道府県, 居住都道府県) %>% 
  summarize(sum_pop = sum(as.integer(population_inflow)))

ibaragi2020_1 <- ibaragi2020 %>% 
  group_by(週, 都道府県, 居住都道府県) %>% 
  summarize(sum_pop = sum(as.integer(population_inflow)))

ibaragi2021_1 <- ibaragi2021 %>% 
  group_by(週, 都道府県, 居住都道府県) %>% 
  summarize(sum_pop = sum(as.integer(population_inflow)))

sixpref <- rbind(yama2020_1, yama2021_1, shizu2020_1, shizu2021_1, nagano2020_1, nagano2021_1, 
      gunma2020_1, gunma2021_1, tochi2020_1, tochi2021_1, ibaragi2020_1, ibaragi2021_1)

length(sixpref$sum_pop[sixpref$sum_pop == NA])

write_csv(sixpref, "sixpref.csv", row.names = FALSE)

# sixpref_rev <- dcast(sixpref, 週 + 都道府県 ~ 居住都道府県, value.var = "sum_pop")
# 
# write_csv(sixpref_rev, "sixpref_rev.csv")
