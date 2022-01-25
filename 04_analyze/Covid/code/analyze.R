library(tidyverse)
library(lfe)
library(stargazer)

#data load ----------
weekSIR_rob <- read_csv(here::here("03_build/Robust_check/output/weekSIR_robustness.csv")) 


# lag2
covid_50.0 <- felm(log(newcase_lead2 + 1) ~ log(cumGZ + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead2 + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_50.1 <- felm(log(newcase_lead2 + 1) ~ log(cumGZ + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead2 + 1) + log(customers_per)| pref+week | 0 | pref, data = weekSIR_rob)
covid_50.2 <- felm(log(newcase_lead2 + 1) ~ log(cumGZ + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead2 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_50.3 <- felm(log(newcase_lead2 + 1) ~ log(cumGZFoodHotel + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead2 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_50.4 <- felm(log(newcase_lead2 + 1) ~ log(cumGZ + 1)  + log(infectious_l2 + 1) + log(susceptible + 1) + emergency + log(tests_lead2 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob)

table1 <- stargazer(covid_50.0, covid_50.1, covid_50.2, covid_50.3, covid_50.4,
                    dep.var.labels = "New infection cases (2 week lag), log",
                    title = "The COVID-19 new infection cases (2 week lag) and the Green Zone certification",
                    digits = 3,
                    digits.extra = 0,
                    type = "latex",
                    covariate.labels = c("Cumulative GZ-certified restaurants, log",
                                         "Cumulative GZ-certified restaurants and hotels, log",
                                         "Infectious, log",
                                         "Susceptible, log",
                                         "State of Emergency",
                                         "Tests (2 week lag), log",
                                         "Customers per restaurant, log", 
                                         "Average temperature, log",
                                         "Average rainfall, log",
                                         "School closure",
                                         "Gathering restriction"),
                    add.lines=list(c("Prefecture FE", rep("X", 5)),
                                   c("Week FE", rep("X", 5))),
                    omit.stat=c("f", "ser"),
                    header = FALSE,
                    out = "04_analyze/Covid/output/covid_reg.tex")

# lag1
covid_51.0 <- felm(log(newcase_lead1 + 1) ~ log(cumGZ + 1)  + log(infectious_l1 + 1) + log(susceptible + 1) + emergency + log(tests_lead1 + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_51.1 <- felm(log(newcase_lead1 + 1) ~ log(cumGZ + 1)  + log(infectious_l1 + 1) + log(susceptible + 1) + emergency + log(tests_lead1 + 1) + log(customers_per)| pref+week | 0 | pref, data = weekSIR_rob)
covid_51.2 <- felm(log(newcase_lead1 + 1) ~ log(cumGZ + 1)  + log(infectious_l1 + 1) + log(susceptible + 1) + emergency + log(tests_lead1 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_51.3 <- felm(log(newcase_lead1 + 1) ~ log(cumGZFoodHotel + 1)  + log(infectious_l1 + 1) + log(susceptible + 1) + emergency + log(tests_lead1 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_51.4 <- felm(log(newcase_lead1 + 1) ~ log(cumGZ + 1)  + log(infectious_l1 + 1) + log(susceptible + 1) + emergency + log(tests_lead1 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob)

table3 <- stargazer(covid_51.0, covid_51.1, covid_51.2, covid_51.3,
                    dep.var.labels = "New infection cases (1 week lag), log",
                    title = "TABLE: COVID-19 new cases (1 week lag) and the Green Zone certification",
                    digits = 3,
                    digits.extra = 0,
                    type = "latex",
                    covariate.labels = c("Cumulative GZ-certified restaurants, log",
                                         "Cumulative GZ-certified restaurants and hotels, log",
                                         "Infectious, log",
                                         "Susceptible, log",
                                         "State of Emergency",
                                         "Tests (1 week lag), log",
                                         "Customers per restaurant, log", 
                                         "Average temperature, log",
                                         "Average rainfall, log",
                                         "School closure",
                                         "Gathering restriction"),
                    add.lines=list(c("Prefecture FE", rep("X", 4)),
                                   c("Week FE", rep("X", 4))),
                    omit.stat=c("f", "ser"),
                    header = FALSE,
                    out = "04_analyze/Covid/output/covid_reg_lag1.tex")

