library(tidyverse)
library(lfe)
library(stargazer)


# First weekSIR data ---------

# data load
weather <- read.csv("03_build/Weather/output/weather_pref.csv", header = TRUE)
weekmain <- read.csv("03_build/Pref_covid/output/weekly_vresas.csv", header = TRUE)
inflow <- read.csv("県別流入リスク.csv", header = TRUE)


weather_2020 <- weather %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= "2019-12-30") %>% 
  arrange(date)

a <- rep(seq(as.Date("2019-12-30"), as.Date("2021-04-30"), by = "week"), each = 42)
a <- data.frame(a)
a2 <- a[1:2928,]
weather_2020$week <- a2


weather_2020_week <- weather_2020 %>% 
  group_by(pref, week) %>% 
  summarize(avg_temp = mean(avg_temp),
            avg_rain = mean(avg_rain))


b <- rep(seq(as.Date("2020-01-13"), as.Date("2021-04-30"), by = "week"), each = 6)
#b <- data.frame(b)
weekmain$week <- b

##都道府県名をローマ字に変換
colnames(inflow)[which(colnames(inflow) == "都道府県")] <- "pref"
inflow$pref <- lapply(inflow$pref, gsub, pattern="山梨県", replacement = "Yamanashi")
inflow$pref <- lapply(inflow$pref, gsub, pattern="栃木県", replacement = "Tochigi")
inflow$pref <- lapply(inflow$pref, gsub, pattern="群馬県", replacement = "Gunma")
inflow$pref <- lapply(inflow$pref, gsub, pattern="茨城県", replacement = "Ibaragi")
inflow$pref <- lapply(inflow$pref, gsub, pattern="静岡県", replacement = "Shizuoka")
inflow$pref <- lapply(inflow$pref, gsub, pattern="長野県", replacement = "Nagano")
inflow$pref <- as.character(inflow$pref)

#mergeする
inflow$week <- as.Date(inflow$week)

weekfinal <- left_join(weekmain, weather_2020_week, by = c("pref", "week"))
weekfinal2 <- left_join(weekfinal, inflow, by = c("pref", "week"))
weekfinal3 <- weekfinal2 %>% 
  select(-X.y, -in_risk_lag1, -in_risk_lag2, -X.x)

weekfinal_lag2 <- weekfinal3 %>% 
  mutate(newcaset.1 = lead(newcaseday, n = 6),
         newcaset.2 = lead(newcaseday, n=12))

#累積を出す
weekfinal_lag2$cumcases <- ave(weekfinal_lag2$newcaseday, weekfinal_lag2$pref, FUN = cumsum)#convert newcases to cumulative cases 

weekfinal_lag2$susceptible <- weekfinal_lag2$population - weekfinal_lag2$cumcases


weekfinal_lag2 <- weekfinal_lag2 %>% 
  mutate(avg_temp_q = avg_temp^2,
         rain_q = rain^2)

write_csv(weekfinal_lag2, "03_build/weekSIR/output/weekSIR.csv")


# PCR merge --------

PCRmerge1 <- read.csv("03_build/Controls/output/PCRtests.csv") 

PCRmerge1$Pref <- gsub("群馬県", "Gunma", PCRmerge1$Pref)
PCRmerge1$Pref <- gsub("山梨県", "Yamanashi", PCRmerge1$Pref)
PCRmerge1$Pref <- gsub("栃木県", "Tochigi", PCRmerge1$Pref)
PCRmerge1$Pref <- gsub("茨城県", "Ibaragi", PCRmerge1$Pref)
PCRmerge1$Pref <- gsub("長野県", "Nagano", PCRmerge1$Pref)
PCRmerge1$Pref <- gsub("静岡県", "Shizuoka", PCRmerge1$Pref)

names(PCRmerge1)[2:3] <- c("pref", "week")
weekSIR$week <- as.Date(weekSIR$week)
PCRmerge1$week <- as.Date(PCRmerge1$week)

weekSIR <- left_join(x = weekSIR,
                     y = PCRmerge1,
                     by = c("week", "pref"))

weekSIR$noftests[is.na(weekSIR$noftests)] <- 0
weekSIR$noftestst.1 <- sapply(1:nrow(weekSIR), function(x) weekSIR$noftests[x+6])
weekSIR$noftestst.2 <- sapply(1:nrow(weekSIR), function(x) weekSIR$noftests[x+12])


weekSIR$emergency <- gsub(2, 1, weekSIR$emergency)
weekSIR$emergency <- gsub(5, 1, weekSIR$emergency)
weekSIR$emergency <- gsub(4, 1, weekSIR$emergency)
weekSIR$emergency <- gsub(7, 1, weekSIR$emergency)
weekSIR$emergency <- as.numeric(weekSIR$emergency)

# write.csv(weekSIR, "03_build/weekSIR/output/weekSIR2.csv", row.names = FALSE)


