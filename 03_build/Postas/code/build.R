library(readxl)
library(tidyverse)
library(reshape2)

data <- read.csv("03_build/weekSIR/output/weekSIR2.csv")
postas <- read_excel("02_bring/Postas/data/国立大学法人東京大学公共政策大学院データ提供.xlsx")
postas.cus <- read_excel("02_bring/Postas/data/国立大学法人東京大学公共政策大学院データ提供.xlsx",2)
postas4 <- read_excel("02_bring/Postas/data/国立大学法人東京大学公共政策大学院データ提供.xlsx",3)
postas.cus4 <- read_excel("02_bring/Postas/data/国立大学法人東京大学公共政策大学院データ提供.xlsx",4)


#postas cleaning ------

postas$集計対象営業日 <- as.Date(postas$集計対象営業日)
colnames(postas) <- gsub("_売上合計", "", colnames(postas))


postas2 <- melt(data = postas,
                id.vars = "集計対象営業日",
                measure.vars = colnames(postas)[8:54],
                variable.name = "pref",
                value.name = "Sales")


postas2$pref <- gsub("群馬県", "Gunma", postas2$pref)
postas2$pref <- gsub("山梨県", "Yamanashi", postas2$pref)
postas2$pref <- gsub("栃木県", "Tochigi", postas2$pref)
postas2$pref <- gsub("茨城県", "Ibaragi", postas2$pref)
postas2$pref <- gsub("長野県", "Nagano", postas2$pref)
postas2$pref <- gsub("静岡県", "Shizuoka", postas2$pref)

postas2$week <- floor_date(postas2$集計対象営業日,
                           "week",
                           week_start = getOption("lubridate.week.start", 1))


postas3 <- postas2 %>% 
  group_by(pref, week) %>% 
  summarize(sales = sum(Sales)) %>% 
  ungroup()

#postas.cus cleaning ------

postas.cus$集計対象営業日 <- as.Date(postas.cus$集計対象営業日)

colnames(postas.cus) <- gsub("_客数合計", "", colnames(postas.cus))


postas.cus2 <- melt(data = postas.cus,
                    id.vars = "集計対象営業日",
                    measure.vars = colnames(postas.cus)[8:54],
                    variable.name = "pref",
                    value.name = "customers")

postas.cus2$pref <- gsub("群馬県", "Gunma", postas.cus2$pref)
postas.cus2$pref <- gsub("山梨県", "Yamanashi", postas.cus2$pref)
postas.cus2$pref <- gsub("栃木県", "Tochigi", postas.cus2$pref)
postas.cus2$pref <- gsub("茨城県", "Ibaragi", postas.cus2$pref)
postas.cus2$pref <- gsub("長野県", "Nagano", postas.cus2$pref)
postas.cus2$pref <- gsub("静岡県", "Shizuoka", postas.cus2$pref)

postas.cus2$week <- floor_date(postas.cus2$集計対象営業日,
                               "week",
                               week_start = getOption("lubridate.week.start", 1))


postas.cus3 <- postas.cus2 %>% 
  group_by(pref, week) %>% 
  summarize(customers = sum(customers)) %>% 
  ungroup()

#postas data merge --------

postas_new <- left_join(postas3, postas.cus3, by = c("week", "pref"))

data$week <- as.Date(data$week)
weekSIR3 <- left_join(x = data,
                      y = postas_new,
                      by = c("week", "pref"))

weekSIR3$lsales <- log(weekSIR3$sales)
weekSIR3$lcustomers <- log(weekSIR3$customers)


# Scaled by # of registered shops in the prefecture
weekSIR3$sales_per <- NA
weekSIR3$sales_per[weekSIR3$pref == "Yamanashi"] <- weekSIR3$sales[weekSIR3$pref == "Yamanashi"] / 18
weekSIR3$sales_per[weekSIR3$pref == "Gunma"] <- weekSIR3$sales[weekSIR3$pref == "Gunma"] / 100
weekSIR3$sales_per[weekSIR3$pref == "Tochigi"] <- weekSIR3$sales[weekSIR3$pref == "Tochigi"] / 58
weekSIR3$sales_per[weekSIR3$pref == "Ibaragi"] <- weekSIR3$sales[weekSIR3$pref == "Ibaragi"] / 86
weekSIR3$sales_per[weekSIR3$pref == "Nagano"] <- weekSIR3$sales[weekSIR3$pref == "Nagano"] / 68
weekSIR3$sales_per[weekSIR3$pref == "Shizuoka"] <- weekSIR3$sales[weekSIR3$pref == "Shizuoka"] / 116

# Scaled by # of registered shops in the prefecture
weekSIR3$customers_per <- NA
weekSIR3$customers_per[weekSIR3$pref == "Yamanashi"] <- weekSIR3$customers[weekSIR3$pref == "Yamanashi"] / 18
weekSIR3$customers_per[weekSIR3$pref == "Gunma"] <- weekSIR3$customers[weekSIR3$pref == "Gunma"] / 100
weekSIR3$customers_per[weekSIR3$pref == "Tochigi"] <- weekSIR3$customers[weekSIR3$pref == "Tochigi"] / 58
weekSIR3$customers_per[weekSIR3$pref == "Ibaragi"] <- weekSIR3$customers[weekSIR3$pref == "Ibaragi"] / 86
weekSIR3$customers_per[weekSIR3$pref == "Nagano"] <- weekSIR3$customers[weekSIR3$pref == "Nagano"] / 68
weekSIR3$customers_per[weekSIR3$pref == "Shizuoka"] <- weekSIR3$customers[weekSIR3$pref == "Shizuoka"] / 116

write.csv(weekSIR3, "03_build/Postas/output/weekSIR3.csv", row.names = FALSE)

# postas data for plot ------

postas_new2 <- postas_new

postas_new2$treat <- ifelse(postas_new2$pref == "Yamanashi", "山梨県", "近隣5県")
postas_new2$sales_per <- NA
postas_new2$sales_per[postas_new2$pref == "Yamanashi"] <- postas_new2$sales[postas_new2$pref == "Yamanashi"] / 18
postas_new2$sales_per[postas_new2$pref == "Gunma"] <- postas_new2$sales[postas_new2$pref == "Gunma"] / 100
postas_new2$sales_per[postas_new2$pref == "Tochigi"] <- postas_new2$sales[postas_new2$pref == "Tochigi"] / 58
postas_new2$sales_per[postas_new2$pref == "Ibaragi"] <- postas_new2$sales[postas_new2$pref == "Ibaragi"] / 86
postas_new2$sales_per[postas_new2$pref == "Nagano"] <- postas_new2$sales[postas_new2$pref == "Nagano"] / 68
postas_new2$sales_per[postas_new2$pref == "Shizuoka"] <- postas_new2$sales[postas_new2$pref == "Shizuoka"] / 116

postas_new2$customers_per <- NA
postas_new2$customers_per[postas_new2$pref == "Yamanashi"] <- postas_new2$customers[postas_new2$pref == "Yamanashi"] / 18
postas_new2$customers_per[postas_new2$pref == "Gunma"] <- postas_new2$customers[postas_new2$pref == "Gunma"] / 100
postas_new2$customers_per[postas_new2$pref == "Tochigi"] <- postas_new2$customers[postas_new2$pref == "Tochigi"] / 58
postas_new2$customers_per[postas_new2$pref == "Ibaragi"] <- postas_new2$customers[postas_new2$pref == "Ibaragi"] / 86
postas_new2$customers_per[postas_new2$pref == "Nagano"] <- postas_new2$customers[postas_new2$pref == "Nagano"] / 68
postas_new2$customers_per[postas_new2$pref == "Shizuoka"] <- postas_new2$customers[postas_new2$pref == "Shizuoka"] / 116

write.csv(postas_new2, "03_build/Postas/output/postas_2019_2021.csv", row.names = FALSE)



