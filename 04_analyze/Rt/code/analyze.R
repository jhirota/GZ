library(tidyverse)
library(lfe)

# data load -------
weekSIR_rob_rt <- read_csv("03_build/Rt/output/weekSIR_rt.csv") 
Rtpostas <- read_csv("03_build/Rt/output/rt_postas.csv") 

# Rt analysis ----------

covid_10.0 <- felm(Rt.lag2 ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency| week | 0 | pref, data = weekSIR_rob_rt%>% filter(week >= 18477))
covid_10.1 <- felm(Rt.lag2 ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency| pref+week | 0 | pref, data = weekSIR_rob_rt%>% filter(week >= 18477))
covid_10.2 <- felm(Rt.lag2 ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + lcustomers| week | 0 | pref, data = weekSIR_rob_rt%>% filter(week >= 18477))
covid_10.3 <- felm(Rt.lag2 ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + lcustomers| pref+week | 0 | pref, data = weekSIR_rob_rt%>% filter(week >= 18477))
covid_10.4 <- felm(Rt.lag2 ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + lcustomers + log(avg_temp_q) + log(rain + 1) | week | 0 | pref, data = weekSIR_rob_rt%>% filter(week >= 18477))
covid_10.5 <- felm(Rt.lag2 ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + lcustomers + log(avg_temp_q) + log(rain + 1) | pref+week | 0 | pref, data = weekSIR_rob_rt%>% filter(week >= 18477))


intercept10.0 <- tail(getfe(covid_10.0, ef="zm2", se = TRUE), 1)
intercept10.1 <- tail(getfe(covid_10.1, ef="zm2", se = TRUE), 1)
intercept10.2 <- tail(getfe(covid_10.2, ef="zm2", se = TRUE), 1)
intercept10.3 <- tail(getfe(covid_10.3, ef="zm2", se = TRUE), 1)
intercept10.4 <- tail(getfe(covid_10.4, ef="zm2", se = TRUE), 1)
intercept10.5 <- tail(getfe(covid_10.5, ef="zm2", se = TRUE), 1)


prefweek10felmhtml <- stargazer(covid_10.0, covid_10.1, covid_10.2, covid_10.3, covid_10.4, covid_10.5,
                                dep.var.labels = "log(Rt + 1)",
                                title = "TABLE: Rt (2 week lag) and GreenZone certification",
                                digits = 3,
                                digits.extra = 0,
                                type = "html",
                                out = "04_analyze/Rt/output/Rt_week.html",
                                add.lines=list(c("Intercept",
                                                 round(intercept10.0[1][[1]], digits = 3),
                                                 round(intercept10.1[1][[1]], digits = 3),
                                                 round(intercept10.2[1][[1]], digits = 3),
                                                 round(intercept10.3[1][[1]], digits = 3),
                                                 round(intercept10.4[1][[1]], digits = 3),
                                                 round(intercept10.5[1][[1]], digits = 3)),
                                               c("",
                                                 paste0("(", round(intercept10.0[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept10.1[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept10.2[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept10.3[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept10.4[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept10.5[4][[1]],digits = 3),")")),
                                               c("Prefecture FE", "", "X", "", "X", "", "X"),
                                               c("Week FE", "X", "X", "X", "X", "X", "X")),
                                omit.stat=c("f", "ser"))


covid_12.0 <- felm(Rt.lag2 ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency| pref+week | 0 | pref, data = weekSIR_rob_rt)
covid_12.1 <- felm(Rt.lag2 ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency| pref+week | 0 | pref, data = weekSIR_rob_rt%>% filter(week >= 18567))#11月以降
covid_12.2 <- felm(Rt.lag2 ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + lcustomers| pref+week | 0 | pref, data = weekSIR_rob_rt)
covid_12.3 <- felm(Rt.lag2 ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + lcustomers| pref+week | 0 | pref, data = weekSIR_rob_rt%>% filter(week >= 18567))#11月以降
covid_12.4 <- felm(Rt.lag2 ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + lcustomers + log(avg_temp_q) + log(rain + 1) | pref+week | 0 | pref, data = weekSIR_rob_rt)
covid_12.5 <- felm(Rt.lag2 ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency + lcustomers + log(avg_temp_q) + log(rain + 1) | pref+week | 0 | pref, data = weekSIR_rob_rt%>% filter(week >= 18567))#11月以降


intercept12.0 <- tail(getfe(covid_12.0, ef="zm2", se = TRUE), 1)
intercept12.1 <- tail(getfe(covid_12.1, ef="zm2", se = TRUE), 1)
intercept12.2 <- tail(getfe(covid_12.2, ef="zm2", se = TRUE), 1)
intercept12.3 <- tail(getfe(covid_12.3, ef="zm2", se = TRUE), 1)
intercept12.4 <- tail(getfe(covid_12.4, ef="zm2", se = TRUE), 1)
intercept12.5 <- tail(getfe(covid_12.5, ef="zm2", se = TRUE), 1)


prefweek12felmhtml <- stargazer(covid_12.0, covid_12.1, covid_12.2, covid_12.3, covid_12.4, covid_12.5,
                                dep.var.labels = "log(Rt + 1)",
                                title = "TABLE: Rt (2 week lag) and GreenZone certification",
                                column.labels = c("Full", "Since Nov", "Full data", "Since Nov","Full data", "Since Nov"),
                                digits = 3,
                                digits.extra = 0,
                                type = "html",
                                out = "04_analyze/Rt/output/Rt_week2.html",
                                add.lines=list(c("Intercept",
                                                 round(intercept12.0[1][[1]], digits = 3),
                                                 round(intercept12.1[1][[1]], digits = 3),
                                                 round(intercept12.2[1][[1]], digits = 3),
                                                 round(intercept12.3[1][[1]], digits = 3),
                                                 round(intercept12.4[1][[1]], digits = 3),
                                                 round(intercept12.5[1][[1]], digits = 3)),
                                               c("",
                                                 paste0("(", round(intercept12.0[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept12.1[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept12.2[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept12.3[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept12.4[4][[1]],digits = 3),")"),
                                                 paste0("(", round(intercept12.5[4][[1]],digits = 3),")")),
                                               c("Prefecture FE", rep("X",6)),
                                               c("Week FE", rep("X",6))),
                                omit.stat=c("f", "ser"))

# Rt and postas ---------

lmrt <- felm(log(Rt + 1) ~ log(customers + 1) + log(customers + 1):Ymns| pref+date | 0 | pref, data = Rtpostas)
lmrt.afNov <- felm(log(Rt + 1) ~ log(customers + 1) + log(customers + 1):Ymns| pref+date | 0 | pref, data = Rtpostas %>% filter(date >= 18567))# since 2020-11-01
lmrt.lag7 <- felm(log(Rt.lag7 + 1) ~ log(customers + 1) + log(customers + 1):Ymns| pref+date | 0 | pref, data = Rtpostas)
lmrt.lag7.afNov <- felm(log(Rt.lag7 + 1) ~ log(customers + 1) + log(customers + 1):Ymns| pref+date | 0 | pref, data = Rtpostas %>% filter(date >= 18567))# since 2020-11-01
lmrt.lag14 <- felm(log(Rt.lag14 + 1) ~ log(customers + 1) + log(customers + 1):Ymns| pref+date | 0 | pref, data = Rtpostas)
lmrt.lag14.afNov <- felm(log(Rt.lag14 + 1) ~ log(customers + 1) + log(customers + 1):Ymns| pref+date | 0 | pref, data = Rtpostas %>% filter(date >= 18567))# since 2020-11-01

interceptrt <- tail(getfe(lmrt, ef="zm2", se = TRUE), 1)
interceptrt.afNov <- tail(getfe(lmrt.afNov, ef="zm2", se = TRUE), 1)
interceptrt.lag7 <- tail(getfe(lmrt.lag7, ef="zm2", se = TRUE), 1)
interceptrt.lag7.afNov <- tail(getfe(lmrt.lag7.afNov, ef="zm2", se = TRUE), 1)
interceptrt.lag14 <- tail(getfe(lmrt.lag14, ef="zm2", se = TRUE), 1)
interceptrt.lag14.afNov <- tail(getfe(lmrt.lag14.afNov, ef="zm2", se = TRUE), 1)

Rtfelmhtml <- stargazer(lmrt,lmrt.afNov,lmrt.lag7, lmrt.lag7.afNov,lmrt.lag14, lmrt.lag14.afNov,
                        # dep.var.labels = "log(Rt + 1)",
                        title = "TABLE: Rt and postas customers",
                        digits = 3,
                        digits.extra = 0,
                        type = "html",
                        out = "04_analyze/Rt/output/Rtpostas.html",
                        column.labels = c("Full", "Since Nov", "Full data", "Since Nov","Full data", "Since Nov"),
                        add.lines=list(c("Intercept",
                                         round(interceptrt[1][[1]], digits = 3),
                                         round(interceptrt.afNov[1][[1]], digits = 3),
                                         round(interceptrt.lag7[1][[1]], digits = 3),
                                         round(interceptrt.lag7.afNov[1][[1]], digits = 3),
                                         round(interceptrt.lag14[1][[1]], digits = 3),
                                         round(interceptrt.lag14.afNov[1][[1]], digits = 3)),
                                       c("",
                                         paste0("(", round(interceptrt[4][[1]],digits = 3),")"),
                                         paste0("(", round(interceptrt.afNov[4][[1]],digits = 3),")"),
                                         paste0("(", round(interceptrt.lag7[4][[1]],digits = 3),")"),
                                         paste0("(", round(interceptrt.lag7.afNov[4][[1]],digits = 3),")"),
                                         paste0("(", round(interceptrt.lag14[4][[1]],digits = 3),")"),
                                         paste0("(", round(interceptrt.lag14.afNov[4][[1]],digits = 3),")")),
                                       c("Prefecture FE", rep("X", 6)),
                                       c("Date FE", rep("X", 6))),
                        omit.stat=c("f", "ser"))


pref6J <- c("山梨県", "静岡県", '長野県', "群馬県", "茨城県", "栃木県")
Rtpostas6 <- Rtpostas %>% 
  filter(pref %in% pref6J)

lmrt.6 <- felm(Rt ~ log(customers + 1) + log(customers + 1):Ymns| pref+date | 0 | pref, data = Rtpostas6)
lmrt.afNov.6 <- felm(Rt ~ log(customers + 1) + log(customers + 1):Ymns| pref+date | 0 | pref, data = Rtpostas6 %>% filter(date >= 18567))# since 2020-11-01
lmrt.lag7.6 <- felm(Rt.lag7 ~ log(customers + 1) + log(customers + 1):Ymns| pref+date | 0 | pref, data = Rtpostas6)
lmrt.lag7.afNov.6 <- felm(Rt.lag7 ~ log(customers + 1) + log(customers + 1):Ymns| pref+date | 0 | pref, data = Rtpostas6 %>% filter(date >= 18567))# since 2020-11-01
lmrt.lag14.6 <- felm(Rt.lag14 ~ log(customers + 1) + log(customers + 1):Ymns| pref+date | 0 | pref, data = Rtpostas6)
lmrt.lag14.afNov.6 <- felm(Rt.lag14 ~ log(customers + 1) + log(customers + 1):Ymns| pref+date | 0 | pref, data = Rtpostas6 %>% filter(date >= 18567))# since 2020-11-01

interceptrt.6 <- tail(getfe(lmrt.6, ef="zm2", se = TRUE), 1)
interceptrt.afNov.6 <- tail(getfe(lmrt.afNov.6, ef="zm2", se = TRUE), 1)
interceptrt.lag7.6 <- tail(getfe(lmrt.lag7.6, ef="zm2", se = TRUE), 1)
interceptrt.lag7.afNov.6 <- tail(getfe(lmrt.lag7.afNov.6, ef="zm2", se = TRUE), 1)
interceptrt.lag14.6 <- tail(getfe(lmrt.lag14.6, ef="zm2", se = TRUE), 1)
interceptrt.lag14.afNov.6 <- tail(getfe(lmrt.lag14.afNov.6, ef="zm2", se = TRUE), 1)

Rtfelm.6html <- stargazer(lmrt.6,lmrt.afNov.6,lmrt.lag7.6, lmrt.lag7.afNov.6,lmrt.lag14.6, lmrt.lag14.afNov.6,
                          # dep.var.labels = "Rt + 1",
                          title = "TABLE: Rt and postas customers (6 Prefectures)",
                          digits = 3,
                          digits.extra = 0,
                          type = "html",
                          out = "04_analyze/Rt/output/Rtpostas6prefs.html",
                          column.labels = c("Full", "Since Nov", "Full data", "Since Nov","Full data", "Since Nov"),
                          add.lines=list(c("Intercept",
                                           round(interceptrt.6[1][[1]], digits = 3),
                                           round(interceptrt.afNov.6[1][[1]], digits = 3),
                                           round(interceptrt.lag7.6[1][[1]], digits = 3),
                                           round(interceptrt.lag7.afNov.6[1][[1]], digits = 3),
                                           round(interceptrt.lag14.6[1][[1]], digits = 3),
                                           round(interceptrt.lag14.afNov.6[1][[1]], digits = 3)),
                                         c("",
                                           paste0("(", round(interceptrt.6[4][[1]],digits = 3),")"),
                                           paste0("(", round(interceptrt.afNov.6[4][[1]],digits = 3),")"),
                                           paste0("(", round(interceptrt.lag7.6[4][[1]],digits = 3),")"),
                                           paste0("(", round(interceptrt.lag7.afNov.6[4][[1]],digits = 3),")"),
                                           paste0("(", round(interceptrt.lag14.6[4][[1]],digits = 3),")"),
                                           paste0("(", round(interceptrt.lag14.afNov.6[4][[1]],digits = 3),")")),
                                         c("Prefecture FE", rep("X", 6)),
                                         c("Date FE", rep("X", 6))),
                          omit.stat=c("f", "ser"))



# checked why Rt can be Inf
# check <- Rt[which(Rt$effectiveReproductionNumber == Inf),]
# Akita <- Rt %>% filter(prefectureNameE == "Akita")
# cuz 0 cases during 8-14 days ago

