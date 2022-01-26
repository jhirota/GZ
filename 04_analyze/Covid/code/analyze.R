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
                    notes.align = "l",notes.append = FALSE,
                    column.sep.width = "1pt",
                    font.size = "scriptsize")

table1.note <- "\\multicolumn{6}{l} {\\parbox[t]{15cm}{ \\textit{Notes:} *p<0.1; **p<0.05; ***p<0.01
The dependent variable is the log-transformed value of the number of new infection cases (2 week lag) plus one. 
The unit of analysis is prefecture and week, and the fixed effects are introduced in all models. 
For the observations, six prefectures are targeted, and the period of analysis is for 66 weeks from January XX, 2020 to April 30, 2021.
The values in parentheses are cluster-robust standard errors. Clustering is at the prefecture level.
Cumulative GZ-certified restaurants, log is the log-transformed value of the number of cumulative certified-GZ restaurants plus one.
Cumulative GZ-certified restaurants and hotels, log is the log-transformed value of the number of cumulative certified-GZ restaurants and hotels plus one.
Infectious, log is the log-transformed value of the number of potentially infected people plus one.
Susceptible, log is the log-transformed value of the total number of susceptible population.
State of Emergency is the dummy variable that takes the value 1 if the state of emergency is declared. 
Tests (2 week lag), log is the log-transformed value of the number of COVID-19 tests plus one.
Customers per restaurant, log is the log-transformed value of the number of customers per restaurant.
Average temperature, log is the log-transformed value of the squared mean temperature (Celsius degrees).
Average rainfall, log is the log-transformed value of the aggregated rainfall (in millimeters).
School closure is the dummy variable that takes the value 1 if the school closure is declared. 
Gathering restriction is the dummy variable that takes the value 1 if the large-scale gathering restriction is declared.}} \\\\"
table1[grepl("Note",table1)] <- table1.note
cat (table1, sep = "\n")
write(table1, here::here("04_analyze/Covid/output/covid_reg.tex"))

# lag1
covid_51.0 <- felm(log(newcase_lead1 + 1) ~ log(cumGZ + 1)  + log(infectious_l1 + 1) + log(susceptible + 1) + emergency + log(tests_lead1 + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_51.1 <- felm(log(newcase_lead1 + 1) ~ log(cumGZ + 1)  + log(infectious_l1 + 1) + log(susceptible + 1) + emergency + log(tests_lead1 + 1) + log(customers_per)| pref+week | 0 | pref, data = weekSIR_rob)
covid_51.2 <- felm(log(newcase_lead1 + 1) ~ log(cumGZ + 1)  + log(infectious_l1 + 1) + log(susceptible + 1) + emergency + log(tests_lead1 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_51.3 <- felm(log(newcase_lead1 + 1) ~ log(cumGZFoodHotel + 1)  + log(infectious_l1 + 1) + log(susceptible + 1) + emergency + log(tests_lead1 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1)| pref+week | 0 | pref, data = weekSIR_rob)
covid_51.4 <- felm(log(newcase_lead1 + 1) ~ log(cumGZ + 1)  + log(infectious_l1 + 1) + log(susceptible + 1) + emergency + log(tests_lead1 + 1) + log(customers_per) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+week | 0 | pref, data = weekSIR_rob)

table2 <- stargazer(covid_51.0, covid_51.1, covid_51.2, covid_51.3, covid_51.4,
                    dep.var.labels = "New infection cases (1 week lag), log",
                    title = "COVID-19 new cases (1 week lag) and the Green Zone certification",
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
                    add.lines=list(c("Prefecture FE", rep("X", 5)),
                                   c("Week FE", rep("X", 5))),
                    omit.stat=c("f", "ser"),
                    header = FALSE,
                    notes.align = "l",notes.append = FALSE,
                    column.sep.width = "1pt",
                    font.size = "scriptsize")

table2.note <- "\\multicolumn{6}{l} {\\parbox[t]{15cm}{ \\textit{Notes:} *p<0.1; **p<0.05; ***p<0.01
The dependent variable is the log-transformed value of the number of new infection cases (1 week lag) plus one. 
The unit of analysis is prefecture and week, and the fixed effects are introduced in all models. 
For the observations, six prefectures are targeted, and the period of analysis is for 67 weeks from January XX, 2020 to April 30, 2021.
The values in parentheses are cluster-robust standard errors. Clustering is at the prefecture level.
Cumulative GZ-certified restaurants, log is the log-transformed value of the number of cumulative certified-GZ restaurants plus one.
Cumulative GZ-certified restaurants and hotels, log is the log-transformed value of the number of cumulative certified-GZ restaurants and hotels plus one.
Infectious, log is the log-transformed value of the number of potentially infected people plus one.
Susceptible, log is the log-transformed value of the total number of susceptible population.
State of Emergency is the dummy variable that takes the value 1 if the state of emergency is declared. 
Tests (1 week lag), log is the log-transformed value of the number of COVID-19 tests plus one.
Customers per restaurant, log is the log-transformed value of the number of customers per restaurant.
Average temperature, log is the log-transformed value of the squared mean temperature (Celsius degrees).
Average rainfall, log is the log-transformed value of the aggregated rainfall (in millimeters).
School closure is the dummy variable that takes the value 1 if the school closure is declared. 
Gathering restriction is the dummy variable that takes the value 1 if the large-scale gathering restriction is declared.}} \\\\"
table2[grepl("Note",table2)] <- table3.note
cat (table2, sep = "\n")
write(table2, here::here("04_analyze/Covid/output/covid_reg_lag1.tex"))

