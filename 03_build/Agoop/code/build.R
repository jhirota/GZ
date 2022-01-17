library(tidyverse)
library(reshape2)

yama2020 <- read_csv("02_bring/Agoop/data/市区町村_prefecture_chart_data_yamanashi2020.csv")
yama2021 <- read_csv("02_bring/Agoop/data/市区町村_prefecture_chart_data_yamanashi2021.csv")
shizu2020 <- read_csv("02_bring/Agoop/data/市区町村_prefecture_chart_data_shizuoka2020.csv")
shizu2021 <- read_csv("02_bring/Agoop/data/市区町村_prefecture_chart_data_shizuoka2021.csv")
nagano2020 <- read_csv("02_bring/Agoop/data/市区町村_prefecture_chart_data_nagano2020.csv")
nagano2021 <- read_csv("02_bring/Agoop/data/市区町村_prefecture_chart_data_nagano2021.csv")
gunma2020 <- read_csv("02_bring/Agoop/data/市区町村_prefecture_chart_data_gunma2020.csv")
gunma2021 <- read_csv("02_bring/Agoop/data/市区町村_prefecture_chart_data_gunma2021.csv")
tochi2020 <- read_csv("02_bring/Agoop/data/市区町村_prefecture_chart_data_tochigi2020.csv")
tochi2021 <- read_csv("02_bring/Agoop/data/市区町村_prefecture_chart_data_tochigi2021.csv")
ibaragi2020 <- read_csv("02_bring/Agoop/data/市区町村_prefecture_chart_data_ibaragi2020.csv")
ibaragi2021 <- read_csv("02_bring/Agoop/data/市区町村_prefecture_chart_data_ibaragi2021.csv")

#
cleanfunc <- function(data){
  data <- data %>% 
    rename(week = 1,
           weekday_flag = 2,
           daytime_flag = 3,
           pref_resid = 6,
           city = 7,
           pref = 8) %>% 
    group_by(week, pref, pref_resid, start_day) %>% 
    summarize(sum_pop = sum(as.integer(population_inflow))) %>% 
    ungroup()
}

sixpref <- rbind(cleanfunc(yama2020),
                 cleanfunc(yama2021),
                 cleanfunc(shizu2020),
                 cleanfunc(shizu2021),
                 cleanfunc(nagano2020), 
                 cleanfunc(nagano2021),
                 cleanfunc(gunma2020),
                 cleanfunc(gunma2021),
                 cleanfunc(tochi2020),
                 cleanfunc(tochi2021),
                 cleanfunc(ibaragi2020),
                 cleanfunc(ibaragi2021))


# write_csv(sixpref, "03_build/Agoop/output/sixpref.csv")

