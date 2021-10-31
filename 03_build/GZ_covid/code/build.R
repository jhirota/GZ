library(tidyverse)

# data load ----

NHK <- read_csv("02_bring/Covid_cases/data/nhk_news_covid19_prefectures_daily_data(1).csv")
Gmobility_2020 <- read_csv("02_bring/Google_mobility/2020_JP_Region_Mobility_Report(1).csv") 
Gmobility_2021 <- read_csv("02_bring/Google_mobility/2021_JP_Region_Mobility_Report.csv") 


# NHK data clean ----

NHK <- NHK[,1:7]
colnames(NHK) <- c("date", "prefcode", "pref", "newcase_day", "total_case", "newdeath_day", "total_death")

NHK$pref <- gsub("山梨県", "Yamanashi", NHK$pref)
NHK$pref <- gsub("茨城県", "Ibaraki", NHK$pref)
NHK$pref <- gsub("栃木県", "Tochigi", NHK$pref)
NHK$pref <- gsub("群馬県", "Gunma", NHK$pref)
NHK$pref <- gsub("長野県", "Nagano", NHK$pref)
NHK$pref <- gsub("静岡県", "Shizuoka", NHK$pref)
  
NHK$date <- as.Date(NHK$date)

NHK_new <- NHK %>% 
  filter(pref %in% c("Yamanashi",
                       "Ibaraki",
                       "Tochigi",
                       "Gunma",
                       "Nagano",
                       "Shizuoka")) %>% 
  filter(date <= 18747) # till April 30th


# Goolmobility data clean -------

Gmobility_2020 <- Gmobility_2020[,c(3,9:15)]
Gmobility_2021 <- Gmobility_2021[,c(3,9:15)]
Gmob <- rbind(Gmobility_2020, Gmobility_2021)


colnames(Gmob)[which(colnames(Gmob) == "sub_region_1")] <- "pref"
colnames(Gmob)[which(colnames(Gmob) == "retail_and_recreation_percent_change_from_baseline")] <- "retail_and_recreation"
colnames(Gmob)[which(colnames(Gmob) == "grocery_and_pharmacy_percent_change_from_baseline")] <- "grocery_and_pharmacy"
colnames(Gmob)[which(colnames(Gmob) == "parks_percent_change_from_baseline")] <- "parks"
colnames(Gmob)[which(colnames(Gmob) == "transit_stations_percent_change_from_baseline")] <- "transit_stations"
colnames(Gmob)[which(colnames(Gmob) == "workplaces_percent_change_from_baseline")] <- "workplaces"
colnames(Gmob)[which(colnames(Gmob) == "residential_percent_change_from_baseline")] <- "residential"

Gmob_new <- Gmob %>% 
  filter(pref %in% c("Yamanashi",
                     "Ibaraki",
                     "Tochigi",
                     "Gunma",
                     "Nagano",
                     "Shizuoka")) %>% 
  filter(date <= 18747) # till April 30th



# data merge -----

GZ_COVID <- left_join(x = NHK_new,
                      y = Gmob_new,
                      by = c("pref", "date"))

# emergency merge (manual data)------
emergency <- read_csv("02_bring/Covid_cases/data/GZ_COVID.csv")
emergency <- emergency %>% 
  select(Pref, Date, emergency)
colnames(emergency)[which(colnames(emergency) == "Pref")] <- "pref"
colnames(emergency)[which(colnames(emergency) == "Date")] <- "date"

emergency$pref <- gsub("山梨県", "Yamanashi", emergency$pref)
emergency$pref <- gsub("茨城県", "Ibaraki", emergency$pref)
emergency$pref <- gsub("栃木県", "Tochigi", emergency$pref)
emergency$pref <- gsub("群馬県", "Gunma", emergency$pref)
emergency$pref <- gsub("長野県", "Nagano", emergency$pref)
emergency$pref <- gsub("静岡県", "Shizuoka", emergency$pref)

emergency$date <- as.Date(emergency$date)

GZ_COVID <- left_join(GZ_COVID, emergency, by = c("pref", "date"))


write.csv(GZ_COVID, "03_build/GZ_covid/output/GZ_covid.csv", row.names=FALSE)


