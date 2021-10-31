
library(tidyverse)


#2019 data load -----
weather2019 <- read.csv("02_bring/Weather/data/weather_data_6pref2019_rev.csv", header = TRUE)

#2019 weather data clean -----

weatherclean <- function(data){
  colnames(data)[1:2] <- c("date", "day")
  temp <- data[,- grep(".1", colnames(data))]
  rain <- data[,c(1:2,grep(".1", colnames(data)))]
  temp_new <- temp[-1,] %>% 
    pivot_longer(cols = 3:ncol(temp),
                 names_to = "city",
                 values_to = "temp")
  temp_new$temp <- as.numeric(temp_new$temp)
  temp_new$date <- as.Date(temp_new$date)
  rain_new <- rain[-1,] %>% 
    pivot_longer(cols = 3:ncol(rain),
                 names_to = "city",
                 values_to = "rain")
  rain_new$rain <- as.numeric(rain_new$rain)
  rain_new$date <- as.Date(rain_new$date)
  datalist <- list(temp_new, rain_new)
  return(datalist)
}

temp_new <- weatherclean(weather2019)[[1]]
rain_new <- weatherclean(weather2019)[[2]]

city2pref <- function(data){
  data$city <- gsub("(前橋|前橋.1)", "Gunma", data$city)
  data$city <- gsub("(伊勢崎|伊勢崎.1)", "Gunma", data$city)
  data$city <- gsub("(長野|長野.1)", "Nagano", data$city)
  data$city <- gsub("(松本|松本.1)", "Nagano", data$city)
  data$city <- gsub("(上田|上田.1)", "Nagano", data$city)
  data$city <- gsub("(飯田|飯田.1)", "Nagano", data$city)
  data$city <- gsub("(甲府|甲府.1)", "Yamanashi", data$city)
  data$city <- gsub("(河口湖|河口湖.1)", "Yamanashi", data$city)
  data$city <- gsub("(浜松|浜松.1)", "Shizuoka", data$city)
  data$city <- gsub("(静岡|静岡.1)", "Shizuoka", data$city)
  data$city <- gsub("(富士|富士.1)", "Shizuoka", data$city)
  data$city <- gsub("(つくば.館野.|つくば.館野..1)", "Ibaraki", data$city)
  data$city <- gsub("(水戸|水戸.1)", "Ibaraki", data$city)
  data$city <- gsub("(日立|日立.1)", "Ibaraki", data$city)
  data$city <- gsub("(宇都宮|宇都宮.1)", "Tochigi", data$city)
  data$city <- gsub("(小山|小山.1)", "Tochigi", data$city)
  return(data)
}


temp_new2 <- city2pref(temp_new) %>% 
  group_by(date, city) %>% 
  summarize(avg_temp = mean(temp)) %>% 
  ungroup()

rain_new2 <- city2pref(rain_new) %>% 
  group_by(date, city) %>% 
  summarize(avg_rain = mean(rain)) %>% 
  ungroup()

weather2019_2 <- left_join(x = temp_new2,
                           y = rain_new2,
                           by = c("date", "city"))

colnames(weather2019_2)[which(colnames(weather2019_2) == "city")] <- "pref"



# 2020 data load ------
weather2020 <- read.csv("02_bring/Weather/data/weather_data_6pref2020_rev.csv", header = TRUE)

# 2020 data clean ----

temp_new_20 <- weatherclean(weather2020)[[1]]
rain_new_20 <- weatherclean(weather2020)[[2]]

temp_new2_20 <- city2pref(temp_new_20) %>% 
  group_by(date, city) %>% 
  summarize(avg_temp = mean(temp)) %>% 
  ungroup()

rain_new2_20 <- city2pref(rain_new_20) %>% 
  group_by(date, city) %>% 
  summarize(avg_rain = mean(rain)) %>% 
  ungroup()

weather2020_2 <- left_join(x = temp_new2_20,
                           y = rain_new2_20,
                           by = c("date", "city"))

colnames(weather2020_2)[which(colnames(weather2020_2) == "city")] <- "pref"

# data merge ------

weather_pref <- rbind(weather2019_2, weather2020_2)

write.csv(weather_pref, "03_build/Weather/output/weather_pref.csv",row.names=FALSE)

