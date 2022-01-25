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
                    dep.var.labels = c("retail and recreation", "grocery and pharmacy", "parks",
                                       "transit stations", "workplaces", "residential"),
                    digits = 3,
                    digits.extra = 0, 
                    type = "latex",
                    add.lines=list(c("Prefecture FE", rep("X",12)),
                                   c("Date FE", rep("X",12))),
                    omit.stat=c("f", "ser"),
                    header = FALSE,
                    column.sep.width = "-25pt",
                    float.env = "sidewaystable",
                    notes = c("Standard errors are clustered at the prefecture level. <br> *p<0.1; **p<0.05; ***p<0.01"),
                    out = "04_analyze/Google_mobility/output/Google Mobility.tex")
