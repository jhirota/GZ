library(tidyverse)
library(lfe)
library(stargazer)
library(lubridate)

# Analyses Daily-------
DATAfinal <- read.csv("03_build/GZ_covid_MLITmobility/output/GZ_covid_dailymobility.csv")

Reg1 <- felm(nofcases14 ~ lnGZ*nofcases*mobility + avg_temp + rainfall + SOE| 0 | 0 | city, data = DATAfinal)
Reg1.1 <- felm(nofcases14 ~ lnGZ*nofcases*mobility + avg_temp + rainfall + SOE| city | 0 | city, data = DATAfinal)
Reg1.2 <- felm(nofcases14 ~ lnGZ*nofcases*mobility + avg_temp + rainfall + SOE| datedum | 0 | city, data = DATAfinal)
Reg1.3 <- felm(nofcases14 ~ lnGZ*nofcases*mobility + avg_temp + rainfall + SOE| city+datedum | 0 | city, data = DATAfinal)


reg_table_html <- stargazer(Reg1, Reg1.1, Reg1.2, Reg1.3,
                            title = "TABLE 1: The number of GreenZone shops and COVID New cases",
                            dep.var.labels = c("COVID-19 new cases by city"),
                            digits = 4,
                            digits.extra = 0,
                            notes = c("Standard errors are clustered at the prefecture <br> level. *p<0.1; **p<0.05; ***p<0.01"),
                            notes.align = "l",
                            notes.append = FALSE,
                            type = "html",
                            out = "04_analyze/Covid_reg/output/Preliminary_analyses.html",
                            add.lines = list(c("City FE", " ", "X", " ", "X"), c("Date FE", " ", " ", "X", "X")),
                            omit.stat = c("f", "ser"))


FE_GZ <- felm(nofcases14 ~ lnGZ| city+datedum | 0 | city, data = DATAfinal)
FE_GZ1 <- felm(nofcases14 ~ lnGZ + lmobility| city+datedum | 0 | city, data = DATAfinal)
FE_GZ2 <- felm(nofcases14 ~ lnGZ*lmobility| city+datedum | 0 | city, data = DATAfinal)
FE_GZ3 <- felm(nofcases14 ~ lnGZ*lmobility + lmobility:nofcases| city+datedum | 0 | city, data = DATAfinal)
FE_GZ4 <- felm(nofcases14 ~ lnGZ*lmobility + lmobility:nofcases + rainfall| city+datedum | 0 | city, data = DATAfinal)
FE_GZ5 <- felm(nofcases14 ~ lnGZ*lmobility*nofcases + rainfall| city+datedum | 0 | city, data = DATAfinal)

aaahtml <- stargazer(FE_GZ, FE_GZ1, FE_GZ2, FE_GZ3, FE_GZ4, FE_GZ5,
                     dep.var.labels = "# of cases 14days later",
                     title = "Table1: COVID-19 cases and GZ certification",
                     notes = "Daily panel data",
                     notes.align = "l",
                     notes.append = FALSE,
                     digits = 4,
                     digits.extra = 0,
                     type = "html",
                     out = "04_analyze/Covid_reg/output/see.html",
                     add.lines = list(c("City FE", "X", "X", "X", "X", "X", "X"), c("Date FE","X", "X", "X", "X", "X", "X")),
                     omit.stat = c("f", "ser"))


FE_GZw <- felm(nofcases14 ~ lnGZ| city+datedum | 0 | city, data = DATAfinal)
FE_GZw1 <- felm(nofcases14 ~ lnGZ + I(avg_temp^2) + rainfall| city+datedum | 0 | city, data = DATAfinal)
FE_GZw2 <- felm(nofcases14 ~ lnGZ*rainfall + I(avg_temp^2)| city+datedum | 0 | city, data = DATAfinal)
FE_GZw3 <- felm(nofcases14 ~ lnGZ*rainfall + I(avg_temp^2) + rainfall:nofcases| city+datedum | 0 | city, data = DATAfinal)

bbbhtml <- stargazer(FE_GZw, FE_GZw1, FE_GZw2, FE_GZw3,
                     dep.var.labels = "# of cases 14days later",
                     title = "Table2: COVID-19 cases and GZ certification",
                     notes = "Daily panel data",
                     digits = 4,
                     digits.extra = 0,
                     type = "html",
                     out = "04_analyze/Covid_reg/output/see2.html",
                     notes.align = "l",
                     notes.append = FALSE,
                     add.lines = list(c("City FE", "X", "X", "X", "X"), c("Date FE", "X", "X", "X", "X")),
                     omit.stat = c("f", "ser"))

FE_GZc <- felm(nofcases14 ~ cumGZ| city+datedum | 0 | city, data = DATAfinal)
FE_GZ1c <- felm(nofcases14 ~ cumGZ + lmobility| city+datedum | 0 | city, data = DATAfinal)
FE_GZ2c <- felm(nofcases14 ~ cumGZ*lmobility| city+datedum | 0 | city, data = DATAfinal)
FE_GZ3c <- felm(nofcases14 ~ cumGZ*lmobility + lmobility:nofcases| city+datedum | 0 | city, data = DATAfinal)
FE_GZ4c <- felm(nofcases14 ~ cumGZ*lmobility + lmobility:nofcases + rainfall| city+datedum | 0 | city, data = DATAfinal)
FE_GZ5c <- felm(nofcases14 ~ cumGZ*lmobility*nofcases| city+datedum | 0 | city, data = DATAfinal)

hhhhtml <- stargazer(FE_GZc, FE_GZ1c, FE_GZ2c, FE_GZ3c, FE_GZ4c, FE_GZ5c,
                     dep.var.labels = "# of cases 14days later",
                     title = "Table1: COVID-19 cases and GZ certification",
                     notes = "Daily panel data",
                     notes.align = "l",
                     notes.append = FALSE,
                     digits = 4,
                     digits.extra = 0,
                     type = "html",
                     out = "04_analyze/Covid_reg/output/see8.html",
                     add.lines = list(c("City FE", "X", "X", "X", "X", "X"), c("Date FE", "X", "X", "X", "X", "X")),
                     omit.stat = c("f", "ser"))

# 感染者数を人口１０万人あたりに標準化したら有意性消えた。
FE_GZs <- felm(scaled_cases14 ~ lnGZ| city+datedum | 0 | city, data = DATAfinal)
FE_GZ1s <- felm(scaled_cases14 ~ lnGZ + lmobility| city+datedum | 0 | city, data = DATAfinal)
FE_GZ2s <- felm(scaled_cases14 ~ lnGZ*lmobility| city+datedum | 0 | city, data = DATAfinal)
FE_GZ3s <- felm(scaled_cases14 ~ lnGZ*lmobility + lmobility:nofcases| city+datedum | 0 | city, data = DATAfinal)
FE_GZ4s <- felm(scaled_cases14 ~ lnGZ*lmobility + lmobility:nofcases + rainfall| city+datedum | 0 | city, data = DATAfinal)
FE_GZ5s <- felm(scaled_cases14 ~ lnGZ*lmobility*nofcases + rainfall| city+datedum | 0 | city, data = DATAfinal)

iiihtml <- stargazer(FE_GZs, FE_GZ1s, FE_GZ2s, FE_GZ3s, FE_GZ4s, FE_GZ5s,
                     dep.var.labels = "# of cases 14days later",
                     title = "Table1: COVID-19 cases and GZ certification",
                     notes = "Daily panel data",
                     notes.align = "l",
                     notes.append = FALSE,
                     digits = 4,
                     digits.extra = 0,
                     type = "html",
                     out = "04_analyze/Covid_reg/output/see10.html",
                     add.lines = list(c("City FE", "X", "X", "X", "X", "X", "X"), c("Date FE","X", "X", "X", "X", "X", "X")),
                     omit.stat = c("f", "ser"))


# Analyses monthly ---------
cleanmondata <- read.csv("03_build/GZ_covid_MLITmobility/output/mondata.csv")

FE_GZm <- felm(nofcases ~ lnGZ| city+datedum | 0 | city, data = cleanmondata)
FE_GZm1 <- felm(nofcases ~ lnGZ + lmobility| city+datedum | 0 | city, data = cleanmondata)
FE_GZm2 <- felm(nofcases ~ lnGZ*lmobility| city+datedum | 0 | city, data = cleanmondata)
FE_GZm3 <- felm(nofcases ~ lnGZ*lmobility + lmobility:nofcases_lm| city+datedum | 0 | city, data = cleanmondata)
FE_GZm4 <- felm(nofcases ~ lnGZ*lmobility*nofcases_lm| city+datedum | 0 | city, data = cleanmondata)

ccchtml <- stargazer(FE_GZm, FE_GZm1, FE_GZm2, FE_GZm3, FE_GZm4,
                     dep.var.labels = "# of COVID19 cases",
                     title = "Table3: COVID-19 cases and GZ certification",
                     notes = "Monthly panel data",
                     notes.align = "l",
                     notes.append = FALSE,
                     digits = 4,
                     digits.extra = 0,
                     type = "html",
                     out = "04_analyze/Covid_reg/output/see3.html",
                     add.lines = list(c("City FE", "X", "X", "X", "X", "X"), c("Date FE", "X", "X", "X", "X", "X")))

FE_GZmw <- felm(nofcases ~ lnGZ| city+datedum | 0 | city, data = cleanmondata)
FE_GZmw1 <- felm(nofcases ~ lnGZ + I(avg_temp^2) + rainfall| city+datedum | 0 | city, data = cleanmondata)
FE_GZmw2 <- felm(nofcases ~ lnGZ*rainfall + I(avg_temp^2)| city+datedum | 0 | city, data = cleanmondata)
FE_GZmw3 <- felm(nofcases ~ lnGZ*rainfall + I(avg_temp^2) + rainfall:nofcases_lm| city+datedum | 0 | city, data = cleanmondata)

dddhtml <- stargazer(FE_GZmw, FE_GZmw1, FE_GZmw2, FE_GZmw3,
                     dep.var.labels = "# of COVID19 cases",
                     title = "Table3: COVID-19 cases and GZ certification",
                     notes = "Monthly panel data",
                     notes.align = "l",
                     notes.append = FALSE,
                     digits = 4,
                     digits.extra = 0,
                     type = "html",
                     out = "04_analyze/Covid_reg/output/see4.html",
                     add.lines = list(c("City FE", "X", "X", "X", "X"), c("Date FE", "X", "X", "X", "X")))

# Analyses monthly data in cities with more than 100 GZ sites for now ---------

Freq100 <- c("甲府市","富士吉田市","都留市","山梨市","南アルプス市","北杜市",      
             "甲斐市","笛吹市","甲州市","昭和町","富士河口湖町")
mondata100 <- cleanmondata %>% 
  filter(city %in% Freq100) 

FE_GZma <- felm(nofcases ~ lnGZ| city+datedum | 0 | city, data = mondata100)
FE_GZma1 <- felm(nofcases ~ lnGZ + lmobility| city+datedum | 0 | city, data = mondata100)
FE_GZma2 <- felm(nofcases ~ lnGZ*lmobility| city+datedum | 0 | city, data = mondata100)
FE_GZma3 <- felm(nofcases ~ lnGZ*lmobility + lmobility:nofcases_lm| city+datedum | 0 | city, data = mondata100)
FE_GZma4 <- felm(nofcases ~ lnGZ*lmobility*nofcases_lm| city+datedum | 0 | city, data = mondata100)

eeehtml <- stargazer(FE_GZma, FE_GZma1, FE_GZma2, FE_GZma3, FE_GZma4,
                     dep.var.labels = "# of COVID19 cases",
                     title = "Table5: COVID-19 cases and GZ certification",
                     notes = "Monthly panel data, cities with >=100 GZ sites at present",
                     notes.align = "l",
                     notes.append = FALSE,
                     digits = 4,
                     digits.extra = 0,
                     type = "html",
                     out = "04_analyze/Covid_reg/output/see5.html",
                     add.lines = list(c("City FE", "X", "X", "X", "X", "X"), c("Date FE", "X", "X", "X", "X", "X")))

FE_GZmb <- felm(nofcases ~ cumGZ| city+datedum | 0 | city, data = mondata100)
FE_GZmb1 <- felm(nofcases ~ cumGZ + lmobility| city+datedum | 0 | city, data = mondata100)
FE_GZmb2 <- felm(nofcases ~ cumGZ*lmobility| city+datedum | 0 | city, data = mondata100)
FE_GZmb3 <- felm(nofcases ~ cumGZ*lmobility + nofcases_lm| city+datedum | 0 | city, data = mondata100)
FE_GZmb4 <- felm(nofcases ~ cumGZ*lmobility*nofcases_lm| city+datedum | 0 | city, data = mondata100)


ggghtml <- stargazer(FE_GZmb, FE_GZmb1, FE_GZmb2, FE_GZmb3, FE_GZmb4,
                     dep.var.labels = "# of COVID19 cases",
                     title = "Table7: COVID-19 cases and GZ certification",
                     notes = "Monthly panel data, cities with >=100 GZ sites at present",
                     notes.align = "l",
                     notes.append = FALSE,
                     digits = 4,
                     digits.extra = 0,
                     type = "html",
                     out = "04_analyze/Covid_reg/output/see7.html",
                     add.lines = list(c("City FE", "X", "X", "X", "X"), c("Date FE", "X", "X", "X", "X")))


FE_GZmaw <- felm(nofcases ~ lnGZ| city+datedum | 0 | city, data = mondata100)
FE_GZmaw1 <- felm(nofcases ~ lnGZ + I(avg_temp^2) + rainfall| city+datedum | 0 | city, data = mondata100)
FE_GZmaw2 <- felm(nofcases ~ lnGZ*rainfall + I(avg_temp^2)| city+datedum | 0 | city, data = mondata100)
FE_GZmaw3 <- felm(nofcases ~ lnGZ*rainfall + I(avg_temp^2) + rainfall:nofcases_lm| city+datedum | 0 | city, data = mondata100)

fffhtml <- stargazer(FE_GZmaw, FE_GZmaw1, FE_GZmaw2, FE_GZmaw3,
                     dep.var.labels = "# of COVID19 cases",
                     title = "Table6: COVID-19 cases and GZ certification",
                     notes = "Monthly panel data, cities with >=100 GZ sites at present",
                     notes.align = "l",
                     notes.append = FALSE,
                     digits = 4,
                     digits.extra = 0,
                     type = "html",
                     out = "04_analyze/Covid_reg/output/see6.html",
                     add.lines = list(c("City FE", "X", "X", "X", "X"), c("Date FE", "X", "X", "X", "X")))

cleanmondata$lnofcases <- log(cleanmondata$nofcases + 1)
mondata100$lnofcases <- log(mondata100$nofcases + 1)

FE_GZnew <- felm(nofcases ~ lnGZ| city+datedum | 0 | city, data = mondata100)
FE_GZnew2 <- felm(nofcases ~ lnGZ + avg_temp + rainfall| city+datedum | 0 | city, data = mondata100)
FE_GZnew3 <- felm(nofcases ~ lnGZ + avg_temp + rainfall + lmobility| city+datedum | 0 | city, data = mondata100)
FE_GZnew4 <- felm(nofcases ~ lnGZ + avg_temp + rainfall + lmobility + lnofcases| city+datedum | 0 | city, data = mondata100)
FE_GZnew5 <- felm(nofcases ~ lnGZ + avg_temp + rainfall + lmobility:lnofcases| city+datedum | 0 | city, data = mondata100)
FE_GZnew6 <- felm(nofcases ~ lnGZ + avg_temp + rainfall + lmobility:lnofcases + lnGZ:lmobility:lnofcases| city+datedum | 0 | city, data = mondata100)

jjjhtml <- stargazer(FE_GZnew, FE_GZnew2, FE_GZnew3, FE_GZnew4,FE_GZnew5,FE_GZnew6,
                     dep.var.labels = "# of COVID19 cases",
                     title = "Table6: COVID-19 cases and GZ certification",
                     notes = "Monthly panel data, cities with >=100 GZ sites at present <br> *p<0.1; **p<0.05; ***p<0.01",
                     notes.align = "l",
                     notes.append = FALSE,
                     digits = 4,
                     digits.extra = 0,
                     type = "html",
                     out = "04_analyze/Covid_reg/output/see9.html",
                     add.lines = list(c("City FE", "X", "X", "X", "X", "X", "X"), c("Date FE", "X", "X", "X", "X","X", "X")),
                     omit.stat=c("f", "ser"))



# check ------

mob_rain <- felm(mobility ~ avg_temp + rainfall| city+datedum | 0 | city, data = cleanmondata)
summary(mob_rain)
