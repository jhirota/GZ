
library(tidyverse)
library(lfe)
library(stargazer)

# data load -----
weekSIR_rob <- read_csv(here::here("03_build/Robust_check/output/weekSIR_robustness.csv"))

# Vresas time series analysis --------------
incity_yama_mean <- mean(weekSIR_rob$in_city[weekSIR_rob$pref == "Yamanashi"])
incity_nonyama_mean <- mean(weekSIR_rob$in_city[weekSIR_rob$pref != "Yamanashi"])
inpref_yama_mean <- mean(weekSIR_rob$in_pref[weekSIR_rob$pref == "Yamanashi"])
inpref_nonyama_mean <- mean(weekSIR_rob$in_pref[weekSIR_rob$pref != "Yamanashi"])
outpref_yama_mean <- mean(weekSIR_rob$out_pref[weekSIR_rob$pref == "Yamanashi"])
outpref_nonyama_mean <- mean(weekSIR_rob$out_pref[weekSIR_rob$pref != "Yamanashi"])

mob1.0 <- felm(in_city ~ log(cumGZ + 1) + emergency| pref + week | 0 | pref, data = weekSIR_rob)
mob1.1 <- felm(in_city ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref + week | 0 | pref, data = weekSIR_rob)
mob1.2 <- felm(in_city ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref + week | 0 | pref, data = weekSIR_rob)
mob2.0 <- felm(in_pref ~ log(cumGZ + 1) + emergency| pref + week | 0 | pref, data = weekSIR_rob)
mob2.1 <- felm(in_pref ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref + week | 0 | pref, data = weekSIR_rob)
mob2.2 <- felm(in_pref ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref + week | 0 | pref, data = weekSIR_rob)
mob3.0 <- felm(out_pref ~ log(cumGZ + 1) + emergency| pref + week | 0 | pref, data = weekSIR_rob)
mob3.1 <- felm(out_pref ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref + week | 0 | pref, data = weekSIR_rob)
mob3.2 <- felm(out_pref ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref + week | 0 | pref, data = weekSIR_rob)

vresas_reg <- stargazer(mob1.0, mob1.1, mob1.2, mob2.0, mob2.1, mob2.2, mob3.0, mob3.1, mob3.2,
                    dep.var.labels = c("intracity","intercity","interprefectural"),
                    title = "Inter-regional mobility and the Green Zone certification",
                    digits = 3,
                    digits.extra = 0,
                    type = "latex",
                    covariate.labels = c("Cumulative GZ-certified restaurants, log",
                                         "State of Emergency",
                                         "The number of new COVID-19 cases, log",
                                         "Average temperature, log",
                                         "Average rainfall, log",
                                         "School closure",
                                         "Gathering restriction"),
                    add.lines=list(c("Mean of Mobility in Yamanashi", "",
                                     round(incity_yama_mean, digits = 3),"","",
                                     round(inpref_yama_mean, digits = 3), "","",
                                     round(outpref_yama_mean, digits = 3), ""),
                                   c("Mean of Mobility in Control","", 
                                     round(incity_nonyama_mean, digits = 3),"","",
                                     round(inpref_nonyama_mean, digits = 3),"","",
                                     round(outpref_nonyama_mean, digits = 3),""),
                                   c("Prefecture FE", rep("X",9)),
                                   c("Week FE", rep("X",9))),
                    omit.stat=c("f", "ser"),
                    header = FALSE,
                    column.sep.width = "1pt",
                    font.size = "scriptsize",
                    notes.align = "l",
                    notes.append = FALSE)

vresas_reg.note <- "\\multicolumn{10}{l} {\\parbox[t]{20cm}{ \\textit{Notes:} *p<0.1; **p<0.05; ***p<0.01
The dependent variable is the percentage change in inter-regional human flow (within a city, within a prefecture, and across prefectures) compared to the 2019 baseline (V-RESAS).
The unit of analysis is prefecture and week, and the fixed effects are introduced in all models. 
For the observations, six prefectures are targeted, and the period of analysis is for 68 weeks from the third week of January, 2020 to the fifth week of April, 2021. 
The values in parentheses are cluster-robust standard errors. Clustering is at the prefecture level.
Cumulative GZ-certified restaurants, log is the log-transformed value of the number of cumulative certified-GZ restaurants.
State of Emergency is the dummy variable that takes the value 1 if the state of emergency is declared. 
The number of new COVID-19 cases, log is the log-transformed value of the daily number of infection cases.
Average temperature, log is the log-transformed value of the mean temperature (Fahrenheit degrees).
Average rainfall, log is the log-transformed value of the aggregated rainfall (in millimeters).
School closure is the dummy variable that takes the value 1 if the school closure is declared. 
Gathering restriction is the dummy variable that takes the value 1 if the large-scale gathering restriction is declared.
For the variables that take absolute value 0 (Cumulative GZ-certified restaurants, log, and The number of new COVID-19 cases, log), we add value 1 before log-transforming to avoid the logarithm of 0.}} \\\\"
vresas_reg[grepl("Note",vresas_reg)] <- vresas_reg.note
cat (vresas_reg, sep = "\n")
write(vresas_reg, here::here("04_analyze/Vresas/output/vresas_mob_reg.tex"))

#p-value
stargazer(mob1.0,  type="text", report=('vc*p'))
stargazer(mob2.0,  type="text", report=('vc*p'))
stargazer(mob3.0,  type="text", report=('vc*p'))

# Restaurant's view analysis------------

resview1 <- felm(resview ~ log(cumGZ + 1) + emergency| pref + week | 0 | pref, data = weekSIR_rob)
resview2 <- felm(resview ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref + week | 0 | pref, data = weekSIR_rob)
resview3 <- felm(resview ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref + week | 0 | pref, data = weekSIR_rob)

resview_yama_mean <- mean(weekSIR_rob$resview[weekSIR_rob$pref == "Yamanashi"])
resview_nonyama_mean <- mean(weekSIR_rob$resview[weekSIR_rob$pref != "Yamanashi"])

resview_reg <- stargazer(resview1, resview2, resview3,
                        dep.var.labels = "Restaurant information views online (percentage change) ",
                        title = "Restaurant information views online (percentage change) and the Green Zone certification",
                        digits = 3,
                        digits.extra = 0,
                        type = "latex",
                        covariate.labels = c("Cumulative GZ-certified restaurants, log",
                                             "State of Emergency",
                                             "The number of new COVID-19 cases, log",
                                             "Average temperature, log",
                                             "Average rainfall, log",
                                             "School closure",
                                             "Gathering restriction"),
                        add.lines=list(c("The mean of dep. variable in Yamanashi Prefecture", "",
                                         round(resview_yama_mean, digits = 3)),
                                       c("The mean of dep. variable  in the control group", "",
                                         round(resview_nonyama_mean, digits = 3)),
                                       c("Prefecture FE", "X", "X", "X"),
                                       c("Week FE", "X", "X","X")),
                        omit.stat = c("f", "ser"),
                        header = FALSE,
                        column.sep.width = "1pt",
                        font.size = "footnotesize",
                        notes.align = "l",
                        notes.append = FALSE)

resview_reg.note <- "\\multicolumn{4}{l} {\\parbox[t]{18cm}{ \\textit{Notes:} *p<0.1; **p<0.05; ***p<0.01
The dependent variable is the percentage change in the number of restaurant information views online compared to the 2019 baseline (V-RESAS). 
The unit of analysis is prefecture-week, and the fixed effects are introduced in all models. 
For the observations, six prefectures are targeted, and the period of analysis is for 68 weeks from the third week of January, 2020 to the fifth week of April, 2021.
The values in parentheses are cluster-robust standard errors. Clustering is at the prefecture level.
Cumulative GZ-certified restaurants, log is the log-transformed value of the number of cumulative certified-GZ restaurants.
State of Emergency is the dummy variable that takes the value 1 if the state of emergency is declared. 
The number of new COVID-19 cases, log is the log-transformed value of the daily number of infection cases.
Average temperature, log is the log-transformed value of the mean temperature (Fahrenheit degrees).
Average rainfall, log is the log-transformed value of the aggregated rainfall (in millimeters).
School closure is the dummy variable that takes the value 1 if the school closure is declared. 
Gathering restriction is the dummy variable that takes the value 1 if the large-scale gathering restriction is declared.
For the variables that take absolute value 0 (Cumulative GZ-certified restaurants, log, and The number of new COVID-19 cases, log), we add value 1 before log-transforming to avoid the logarithm of 0.}} \\\\"
resview_reg[grepl("Note",resview_reg)] <- resview_reg.note
cat (resview_reg, sep = "\n")
write(resview_reg, here::here("04_analyze/Vresas/output/resview_reg.tex"))


