library(tidyverse)
library(stargazer)
library(lfe)

# reg night selfrest  ------

#data load 
postas_rob <- read_csv(here::here("03_build/Robust_check/output/postas_rob.csv"), 
                       guess_max = 50000)

# reg Night_selfrest
NSHR1 <- felm(NSHR ~ log(cumGZ + 1) + emergency| pref+date | 0 | pref, data = postas_rob)
NSHR2 <- felm(NSHR ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref+date | 0 | pref, data = postas_rob)
NSHR3 <- felm(NSHR ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)

table6.1 <- stargazer(NSHR1, NSHR2, NSHR3,
                      title = "The night-time stay-home rate and the Green Zone certification",
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
                      add.lines=list(c("Prefecture FE", rep("X",3)),
                                     c("Day FE", rep("X",3))),
                      omit.stat=c("f", "ser"),
                      header = FALSE,
                      column.sep.width = "1pt",
                      font.size = "footnotesize",
                      notes.align = "l", notes.append = FALSE)

table6.1.note <- "\\multicolumn{4}{l} {\\parbox[t]{14cm}{ \\textit{Notes:} *p<0.1; **p<0.05; ***p<0.01
The dependent variable is the night-time (from 8pm to 0am) stay-home rate, which indicates the percentage of people who refrain from going out compared to the baseline value; the closer to 1, the more people refrain from going out, and the closer to 0, the more people go out.
The unit of analysis is prefecture and day, and the fixed effects are introduced in all models. 
For the observations, six prefectures are targeted, and the period of analysis is for 454 days from January XX, 2020 to April 30, 2021. 
The values in parentheses are cluster-robust standard errors. Clustering is at the prefecture level.
Cumulative GZ-certified restaurants, log is the log-transformed value of the number of cumulative certified-GZ restaurants plus one.
State of Emergency is the dummy variable that takes the value 1 if the state of emergency is declared. 
The number of new COVID-19 cases, log is the log-transformed value of the daily number of infection cases plus one.
Average temperature, log is the log-transformed value of the squared mean temperature (Celsius degrees).
Average rainfall, log is the log-transformed value of the aggregated rainfall (in millimeters).
School closure is the dummy variable that takes the value 1 if the school closure is declared. 
Gathering restriction is the dummy variable that takes the value 1 if the large-scale gathering restriction is declared.}} \\\\"
table6.1[grepl("Note",table6.1)] <- table6.1.note
cat (table6.1, sep = "\n")
write(table6.1, here::here("04_analyze/Stayhome_rate/output/night_stayhome.tex"))

# reg selfrest by age (male)---------
male15 <- felm(M15 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
male20 <- felm(M20 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
male30 <- felm(M30 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
male40 <- felm(M40 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
male50 <- felm(M50 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
male60 <- felm(M60 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
male70 <- felm(M70 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)

table6.2 <- stargazer(male15, male20, male30, male40, male50, male60, male70,
                      title = "The self-restraint rate by male age group and the Green Zone certification",
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
                      add.lines=list(c("Prefecture FE", rep("X",7)),
                                     c("Day FE", rep("X",7))),
                      omit.stat=c("f", "ser"),
                      header = FALSE,
                      column.sep.width = "-11pt",
                      font.size = "footnotesize",
                      notes.align = "l", notes.append = FALSE)

table6.2.note <- "\\multicolumn{8}{l} {\\parbox[t]{16cm}{ \\textit{Notes:} *p<0.1; **p<0.05; ***p<0.01
The dependent variable is the day-time (from 9 am to 6 pm) stay-home rate for males, which indicates the percentage of people who refrain from going out compared to the baseline value; the closer to 1, the more people refrain from going out, and the closer to 0, the more people go out. 
The unit of analysis is prefecture and day, and the fixed effects are introduced in all models. 
For the observations, six prefectures are targeted, and the period of analysis is for 449 days from January XX, 2020 to April 30, 2021.
The values in parentheses are cluster-robust standard errors. Clustering is at the prefecture level.
Cumulative GZ-certified restaurants, log is the log-transformed value of the number of cumulative certified-GZ restaurants plus one.
State of Emergency is the dummy variable that takes the value 1 if the state of emergency is declared. 
The number of new COVID-19 cases, log is the log-transformed value of the daily number of infection cases plus one.
Average temperature, log is the log-transformed value of the squared mean temperature (Celsius degrees).
Average rainfall, log is the log-transformed value of the aggregated rainfall (in millimeters).
School closure is the dummy variable that takes the value 1 if the school closure is declared. 
Gathering restriction is the dummy variable that takes the value 1 if the large-scale gathering restriction is declared.}} \\\\"
table6.2[grepl("Note",table6.2)] <- table6.2.note
cat (table6.2, sep = "\n")
write(table6.2, here::here("04_analyze/Stayhome_rate/output/SHR_by_age_male.tex"))

# reg selfrest by age (female)---------
female15 <- felm(F15 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
female20 <- felm(F20 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
female30 <- felm(F30 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
female40 <- felm(F40 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
female50 <- felm(F50 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
female60 <- felm(F60 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
female70 <- felm(F70 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)

table6.3 <- stargazer(female15, female20, female30, female40, female50, female60, female70,
                      title = "The self-restraint rate by female age group and the Green Zone certification",
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
                      add.lines=list(c("Prefecture FE", rep("X",7)),
                                     c("Day FE", rep("X",7))),
                      omit.stat=c("f", "ser"),
                      header = FALSE,
                      column.sep.width = "-11pt",
                      font.size = "footnotesize",
                      notes.align = "l", notes.append = FALSE)

table6.3.note <- "\\multicolumn{8}{l} {\\parbox[t]{16cm}{ \\textit{Notes:} *p<0.1; **p<0.05; ***p<0.01
The dependent variable is the day-time (from 9 am to 6 pm) stay-home rate for females, which indicates the percentage of people who refrain from going out compared to the baseline value; the closer to 1, the more people refrain from going out, and the closer to 0, the more people go out. 
The unit of analysis is prefecture and day, and the fixed effects are introduced in all models. 
For the observations, six prefectures are targeted, and the period of analysis is for 449 days from January XX, 2020 to April 30, 2021.
The values in parentheses are cluster-robust standard errors. Clustering is at the prefecture level.
Cumulative GZ-certified restaurants, log is the log-transformed value of the number of cumulative certified-GZ restaurants plus one.
State of Emergency is the dummy variable that takes the value 1 if the state of emergency is declared. 
The number of new COVID-19 cases, log is the log-transformed value of the daily number of infection cases plus one.
Average temperature, log is the log-transformed value of the squared mean temperature (Celsius degrees).
Average rainfall, log is the log-transformed value of the aggregated rainfall (in millimeters).
School closure is the dummy variable that takes the value 1 if the school closure is declared. 
Gathering restriction is the dummy variable that takes the value 1 if the large-scale gathering restriction is declared.}} \\\\"
table6.3[grepl("Note",table6.3)] <- table6.3.note
cat (table6.3, sep = "\n")
write(table6.3, here::here("04_analyze/Stayhome_rate/output/SHR_by_age_female.tex"))

