library(tidyverse)
library(lfe)
library(stargazer)

ppDATA1 <- read_csv("03_build/Papilio_mobility/output/city_weekly.csv")

# Covid Analysis ---------
OLS_pp1.1 <- lm(log(infected_2w + 1) ~ log(susceptible) + log(infected + 1) + I(2*log(mobility_per)), data = ppDATA1)
OLS_pp1.2 <- lm(log(infected_2w + 1) ~ log(susceptible) + log(infected + 1) + I(2*log(mobility_per)) + log(cumGZ + 1), data = ppDATA1)
FE_pp1.1 <- felm(log(infected_2w + 1) ~ log(susceptible) + log(infected + 1) + I(2*log(mobility_per)) + log(cumGZ + 1)| city | 0 | city, data = ppDATA1)
FE_pp1.2 <- felm(log(infected_2w + 1) ~ log(susceptible) + log(infected + 1) + I(2*log(mobility_per)) + log(cumGZ + 1)| 週 | 0 | city, data = ppDATA1)
FE_pp1.3 <- felm(log(infected_2w + 1) ~ log(susceptible) + log(infected + 1) + log(cumGZ + 1)| city | 0 | city, data = ppDATA1)
FE_pp1.4 <- felm(log(infected_2w + 1) ~ log(susceptible) + log(infected + 1) + log(cumGZ + 1)| 週 | 0 | city, data = ppDATA1)
FE_pp1.5 <- felm(log(infected_2w + 1) ~ log(susceptible) + log(infected + 1) + log(cumGZ + 1)| city+週 | 0 | city, data = ppDATA1)
FE_pp1.6 <- felm(log(infected_2w + 1) ~ log(susceptible) + log(infected + 1) + I(2*log(mobility_per)) + log(cumGZ + 1)| city+週 | 0 | city, data = ppDATA1)


html <- stargazer(OLS_pp1.1, OLS_pp1.2, FE_pp1.1, FE_pp1.2, FE_pp1.3, FE_pp1.4,FE_pp1.5,FE_pp1.6,
                  title = "Table: City-level analysis on COVID-19 cases and GZ certification",
                  notes = "Weekly panel data <br> *p<0.1; **p<0.05; ***p<0.01",
                  notes.align = "l",
                  notes.append = FALSE,
                  digits = 4,
                  digits.extra = 0,
                  type = "html",
                  out = "04_analyze/Covid_reg_papilio/output/papilio.html",
                  add.lines = list(c("City FE","","", "X", "", "X", "", "X", "X"), c("Week FE","","", "", "X", "", "X", "X", "X")),
                  omit.stat=c("f", "ser"))

