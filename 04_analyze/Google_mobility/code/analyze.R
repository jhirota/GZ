library(tidyverse)
library(lfe)

#data  load -------
Gmobdata <- read_csv(here::here("03_build/GZ_covid/output/pref_bet_day_COVID_GZ.csv")) 

# reg -------

retrec1 <- felm(retail_and_recreation ~ log(cumGZ + 1) + emergency| pref + date | 0 | pref, data = Gmobdata)
retrec2 <- felm(retail_and_recreation ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref + date | 0 | pref, data = Gmobdata)
gropha1 <- felm(grocery_and_pharmacy ~ log(cumGZ + 1) + emergency| pref + date | 0 | pref, data = Gmobdata)
gropha2 <- felm(grocery_and_pharmacy ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref + date | 0 | pref, data = Gmobdata)
park1 <- felm(parks ~ log(cumGZ + 1) + emergency| pref + date | 0 | pref, data = Gmobdata)
park2 <- felm(parks ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref + date | 0 | pref, data = Gmobdata)
station1 <- felm(transit_stations ~ log(cumGZ + 1) + emergency| pref + date | 0 | pref, data = Gmobdata)
station2 <- felm(transit_stations ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref + date | 0 | pref, data = Gmobdata)
work1 <- felm(workplaces ~ log(cumGZ + 1) + emergency| pref + date | 0 | pref, data = Gmobdata)
work2 <- felm(workplaces ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref + date | 0 | pref, data = Gmobdata)
residence1 <- felm(residential ~ log(cumGZ + 1) + emergency| pref + date | 0 | pref, data = Gmobdata)
residence2 <- felm(residential ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref + date | 0 | pref, data = Gmobdata)


Gmob <- stargazer(retrec1, retrec2, gropha1, gropha2, park1, park2,
                    station1, station2, work1, work2, residence1, residence2,
                    title = "Mobility type (Google Mobility) the Green Zone certification",
                    dep.var.labels = c("retail/recreation", "grocery/pharmacy", "parks",
                                       "transit stations", "workplaces", "residential"),
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
                    add.lines=list(c("Prefecture FE", rep("X",12)),
                                   c("Date FE", rep("X",12))),
                    omit.stat=c("f", "ser"),
                    header = FALSE,
                    column.sep.width = "-12pt",
                    font.size = "footnotesize",
                    notes.align = "l",
                    notes.append = FALSE)

Gmob.note <- "\\multicolumn{13}{l} {\\parbox[t]{23cm}{ \\textit{Notes:} *p<0.1; **p<0.05; ***p<0.01
The dependent variable is the percent change in human flow for a given facility type compared to the January 2020 baseline (Google Mobility).
The unit of analysis is prefecture and day, and the fixed effects are introduced in all models. 
For the observations, six prefectures are targeted, and the period of analysis is for 441 days from February 15th, 2020 to April 30, 2021.
The values in parentheses are cluster-robust standard errors. Clustering is at the prefecture level.
Cumulative GZ-certified restaurants, log is the log-transformed value of the number of cumulative certified-GZ restaurants plus one.
State of Emergency is the dummy variable that takes the value 1 if the state of emergency is declared. 
The number of new COVID-19 cases, log is the log-transformed value of the daily number of infection cases plus one.
Average temperature, log is the log-transformed value of the mean temperature (Fahrenheit degrees).
Average rainfall, log is the log-transformed value of the aggregated rainfall (in millimeters).
School closure is the dummy variable that takes the value 1 if the school closure is declared. 
Gathering restriction is the dummy variable that takes the value 1 if the large-scale gathering restriction is declared.}} \\\\"
Gmob[grepl("Note",Gmob)] <- Gmob.note
cat (Gmob, sep = "\n")
write(Gmob, here::here("04_analyze/Google_mobility/output/Google Mobility.tex"))
