library(lfe)

#data load -------
Gmobdata <- read_csv("03_build/Pref_covid/output/pref_bet_day_COVID_GZ.csv")
Gmobdata$date <- as.Date(Gmobdata$date)


pref_weather <- read_csv("03_build/Weather/output/weather_pref.csv") 
pref_weather$date <- as.Date(pref_weather$date)
pref_weather <- pref_weather[-which(colnames(pref_weather) == "X1")]

Gmobdata <- left_join(x = Gmobdata,
                      y = pref_weather,
                      by = c("date", "pref"))
Gmobdata$tempq <- (Gmobdata$avg_temp)^2

retrec1 <- felm(retail_and_recreation ~ log(cumGZ + 1) + log(newcase_day + 1) + emergency + tempq + avg_rain| date+pref | 0 | pref, data = Gmobdata)
retrec2 <- felm(retail_and_recreation ~ log(cumGZ + 1) + log(newcase_day + 1) + emergency + tempq + avg_rain + log(cumGZ + 1):log(newcase_day + 1)| date+pref | 0 | pref, data = Gmobdata)
gropha1 <- felm(grocery_and_pharmacy ~ log(cumGZ + 1) + log(newcase_day + 1) + emergency + tempq + avg_rain | date+pref | 0 | pref, data = Gmobdata)
gropha2 <- felm(grocery_and_pharmacy ~ log(cumGZ + 1) + log(newcase_day + 1) + emergency + tempq + avg_rain + log(cumGZ + 1):log(newcase_day + 1)| date+pref | 0 | pref, data = Gmobdata)
park1 <- felm(parks ~ log(cumGZ + 1) + log(newcase_day + 1) + emergency + tempq + avg_rain | date+pref | 0 | pref, data = Gmobdata)
park2 <- felm(parks ~ log(cumGZ + 1) + log(newcase_day + 1) + emergency + tempq + avg_rain + log(cumGZ + 1):log(newcase_day + 1)| date+pref | 0 | pref, data = Gmobdata)
station1 <- felm(transit_stations ~ log(cumGZ + 1) + log(newcase_day + 1) + emergency + tempq + avg_rain | date+pref | 0 | pref, data = Gmobdata)
station2 <- felm(transit_stations ~ log(cumGZ + 1) + log(newcase_day + 1) + emergency + tempq + avg_rain + log(cumGZ + 1):log(newcase_day + 1)| date+pref | 0 | pref, data = Gmobdata)
work1 <- felm(workplaces ~ log(cumGZ + 1) + log(newcase_day + 1) + emergency + tempq + avg_rain | date+pref | 0 | pref, data = Gmobdata)
work2 <- felm(workplaces ~ log(cumGZ + 1) + log(newcase_day + 1) + emergency + tempq + avg_rain + log(cumGZ + 1):log(newcase_day + 1)| date+pref | 0 | pref, data = Gmobdata)
residence1 <- felm(residential ~ log(cumGZ + 1) + log(newcase_day + 1) + emergency + tempq + avg_rain | date+pref | 0 | pref, data = Gmobdata)
residence2 <- felm(residential ~ log(cumGZ + 1) + log(newcase_day + 1) + emergency + tempq + avg_rain + log(cumGZ + 1):log(newcase_day + 1)| date+pref | 0 | pref, data = Gmobdata)



reg_table_html <- stargazer(retrec1, retrec2, gropha1, gropha2, park1, park2, station1, station2, work1, work2, residence1, residence2,
                            title = "TABLE: The number of GreenZone shops and Mobility type(Google Mobility)",
                            dep.var.labels = c("retail and recreation", "grocery and pharmacy", "parks", "transit stations", "workplaces", "residential"),
                            digits =3,
                            digits.extra = 0, 
                            notes = c("Standard errors are clustered at the prefecture level. <br> *p<0.1; **p<0.05; ***p<0.01"),
                            notes.align = "l",
                            notes.append = FALSE,
                            type = "html",
                            out = "04_analyze/Google_mobility/output/Google Mobility.html",
                            add.lines=list(c("Prefecture FE", rep("X",12)),
                                           c("Date FE", rep("X",12))),
                            omit.stat=c("f", "ser"))
