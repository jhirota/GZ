
library(tidyverse)

#data load -----
weather2019 <- read_csv("02_bring/Weather/data/weather_data_6pref2019_rev.csv")
weather2020 <- read_csv("02_bring/Weather/data/weather_data_6pref2020_rev.csv")

#data clean -----

weatherclean <- function(data){
  data <- data %>% 
    rename(date = 1) %>% 
    slice(-1)
  
  cutfunc <- function(x){
    strsplit(x, "[...]")[[1]][1]
  }
  
  temperature <- data %>% 
    pivot_longer(cols = seq(3,33, by = 2),
                 names_to = "city",
                 values_to = "temp") %>% 
    select(date, city, temp) %>% 
    mutate(city = map(city, cutfunc))
    
  rain <- data %>% 
    pivot_longer(cols = seq(4,34, by = 2),
                 names_to = "city",
                 values_to = "rain") %>% 
    select(date, city, rain)%>% 
    mutate(city = map(city, cutfunc))
  
  weatherdata <- left_join(temperature, rain, by = c("date", "city")) %>% 
    mutate(date = as.Date(date),
           temp = as.numeric(temp),
           rain = as.numeric(rain))
  
  return(weatherdata)
}

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
  
  data <- rename(data, pref = "city")
  return(data)
}



weather_pref <- rbind(weatherclean(weather2019) %>% 
                        city2pref(),
                      weatherclean(weather2020) %>% 
                        city2pref()) %>% 
  group_by(date, pref) %>% 
  summarise(avg_temp = mean(temp),
            avg_rain = mean(rain))

write_csv(weather_pref, here::here("03_build/Weather/output/weather_pref.csv"))




