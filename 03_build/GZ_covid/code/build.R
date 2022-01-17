library(tidyverse)

# data load ----

NHK <- read_csv("02_bring/Covid_cases/data/nhk_news_covid19_prefectures_daily_data(1).csv")
Gmobility_2020 <- read_csv("02_bring/Google_mobility/2020_JP_Region_Mobility_Report(1).csv") 
Gmobility_2021 <- read_csv("02_bring/Google_mobility/2021_JP_Region_Mobility_Report.csv") 
emergency <- read_csv("02_bring/Covid_cases/data/GZ_COVID.csv")

# NHK data clean ----

NHK <- NHK %>% 
  select(!各地の直近1週間の人口10万人あたりの感染者数) %>% 
  rename(date = "日付",
         prefcode = "都道府県コード",
         pref = "都道府県名",
         newcase_day = "各地の感染者数_1日ごとの発表数",
         total_case = "各地の感染者数_累計",
         newdeath_day = "各地の死者数_1日ごとの発表数" ,
         total_death = "各地の死者数_累計") %>% 
  mutate(pref = str_replace_all(pref,
                                c(山梨県 = "Yamanashi",
                                  茨城県 = "Ibaraki",
                                  栃木県 = "Tochigi",
                                  群馬県 = "Gunma",
                                  長野県 = "Nagano",
                                  静岡県 = "Shizuoka")),
         date = as.Date(date)) %>% 
  filter(pref %in% c("Yamanashi",
                       "Ibaraki",
                       "Tochigi",
                       "Gunma",
                       "Nagano",
                       "Shizuoka")) %>% 
  filter(date <= 18747) # till April 30th


# Goolmobility data clean -------

Gmob_clean <- function(data){
  data <- data %>% 
    rename(pref = "sub_region_1",
           retail_and_recreation = "retail_and_recreation_percent_change_from_baseline",
           grocery_and_pharmacy = "grocery_and_pharmacy_percent_change_from_baseline",
           parks = "parks_percent_change_from_baseline",
           transit_stations = "transit_stations_percent_change_from_baseline",
           workplaces = "workplaces_percent_change_from_baseline",
           residential = "residential_percent_change_from_baseline") %>% 
    select(pref, date, retail_and_recreation, grocery_and_pharmacy, parks,
           transit_stations, workplaces, residential) %>% 
    filter(pref %in% c("Yamanashi", "Ibaraki", "Tochigi", "Gunma",
                       "Nagano", "Shizuoka"),
           date <= 18747)# till April 30th
  
  return(data)
}

Gmobility_2020 <- Gmob_clean(Gmobility_2020)
Gmobility_2021 <- Gmob_clean(Gmobility_2021)
Gmob <- rbind(Gmobility_2020, Gmobility_2021)


# data merge -----

GZ_COVID <- left_join(x = NHK,
                      y = Gmob,
                      by = c("pref", "date"))

# emergency merge (manual data)------

emergency <- emergency %>% 
  select(Pref, Date, emergency) %>% 
  rename(pref = "Pref",
         date = "Date") %>% 
  mutate(pref = str_replace_all(pref,
                                c(山梨県 = "Yamanashi",
                                   茨城県 = "Ibaraki",
                                   栃木県 = "Tochigi",
                                   群馬県 = "Gunma",
                                   長野県 = "Nagano",
                                   静岡県 = "Shizuoka")),
         date = as.Date(date))



GZ_COVID <- left_join(GZ_COVID, emergency, by = c("pref", "date"))


write_csv(GZ_COVID, "03_build/GZ_covid/output/GZ_covid.csv")


