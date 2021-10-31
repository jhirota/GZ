library(tidyverse)

# data load ------ 

Rt <- read.csv("02_bring/Rt/data/prefectures.csv")# Don't use read_csv() for this file
weekSIR_rob <- read_csv("03_build/Robustness_check/output/weekSIR_robustness.csv")


# data clean ------
pref6 <- c("Yamanashi", "Shizuoka", 'Nagano', "Gunma", "Ibaraki", "Tochigi")

Rt$date <- paste(Rt$year, Rt$month, Rt$date, sep = "-")
Rt$date <- as.Date(Rt$date)

Rt1 <- Rt %>%
  select(date, prefectureNameE, effectiveReproductionNumber) %>% 
  filter(prefectureNameE %in% pref6) %>% 
  mutate(week = floor_date(date,"week",week_start = getOption("lubridate.week.start", 1)))

Rt1$effectiveReproductionNumber[Rt1$prefectureNameE == "Yamanashi"][418]<- mean(Rt1$effectiveReproductionNumber[Rt1$prefectureNameE == "Yamanashi"][411:417])


Rt2 <- Rt1 %>% 
  group_by(prefectureNameE, week) %>% 
  summarize(Rt = mean(effectiveReproductionNumber)) %>% 
  ungroup()
colnames(Rt2)[which(names(Rt2) == "prefectureNameE")] <- "pref"

weekSIR_rob_rt <- weekSIR_rob
weekSIR_rob_rt$pref <- gsub("Ibaragi", "Ibaraki", weekSIR_rob_rt$pref)

weekSIR_rob_rt <- left_join(x = weekSIR_rob_rt, y = Rt2, by = c("pref", "week")) %>% 
  arrange(week)
weekSIR_rob_rt$Rt.lag2 <- sapply(1:nrow(weekSIR_rob_rt), function(x) weekSIR_rob_rt$Rt[x+12])


write.csv(weekSIR_rob_rt, "03_build/Rt/output/weekSIR_rt.csv", row.names = FALSE)


# merge with postas data --------

# data load 
postas <- read_excel("02_bring/Postas/data/国立大学法人東京大学公共政策大学院データ提供.xlsx")
postas.cus <- read_excel("02_bring/Postas/data/国立大学法人東京大学公共政策大学院データ提供.xlsx",2)

colnames(postas) <- gsub("_売上合計", "", colnames(postas))
colnames(postas.cus) <- gsub("_客数合計", "", colnames(postas.cus))

# data clean
postas_a <- melt(data = postas,
                 id.vars = "集計対象営業日",
                 measure.vars = colnames(postas)[8:54],
                 variable.name = "pref",
                 value.name = "Sales")

postas.cus_a <- melt(data = postas.cus,
                     id.vars = "集計対象営業日",
                     measure.vars = colnames(postas)[8:54],
                     variable.name = "pref",
                     value.name = "customers")
postas7 <- left_join(x = postas_a,
                     y = postas.cus_a,
                     by = c("pref", "集計対象営業日"))
colnames(postas7)[which(names(postas7) == "集計対象営業日")]<-"date"

Rt7 <- Rt %>% 
  select(date, prefectureNameJ, effectiveReproductionNumber)
colnames(Rt7)[which(names(Rt7) == "prefectureNameJ")]<-"pref"
colnames(Rt7)[which(names(Rt7) == "effectiveReproductionNumber")]<-"Rt"

Rtpostas <- left_join(postas7, Rt7, by = c("pref", "date"))
Rtpostas$Ymns <- ifelse(Rtpostas$pref == "山梨県", 1, 0)
Rtpostas <- Rtpostas %>% 
  arrange(date)
Rtpostas$Rt.lag7 <- sapply(1:nrow(Rtpostas), function(x) Rtpostas$Rt[x+47*7])
Rtpostas$Rt.lag14 <- sapply(1:nrow(Rtpostas), function(x) Rtpostas$Rt[x+47*14])
Rtpostas$Rt <- ifelse(Rtpostas$Rt == Inf, NA, Rtpostas$Rt)
Rtpostas$Rt.lag7 <- ifelse(Rtpostas$Rt.lag7 == Inf, NA, Rtpostas$Rt.lag7)
Rtpostas$Rt.lag14 <- ifelse(Rtpostas$Rt.lag14 == Inf, NA, Rtpostas$Rt.lag14)

write.csv(Rtpostas, "03_build/Rt/output/rt_postas.csv", row.names = FALSE)
