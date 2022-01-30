library(tidyverse)

# data load------
postas_day1 <- read_csv(here::here("03_build/Postas/output/postas_daily_data.csv"))

# postas regression --------

# Daily analysis
dlmpostas1 <- felm(log(sales_per) ~ log(cumGZ + 1)  + emergency| pref+date | 0 | pref, data = postas_day1)
dlmpostas2 <- felm(log(sales_per) ~ log(cumGZ + 1)  + emergency + log(newcase_day + 1) | pref+date | 0 | pref, data = postas_day1)
dlmpostas3 <- felm(log(sales_per) ~ log(cumGZ + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref+date | 0 | pref, data = postas_day1)
dlmpostas4 <- felm(log(sales_per) ~ log(cumGZ + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1)
dlmpostas.cus1 <- felm(log(customers_per) ~ log(cumGZ + 1)  + emergency| pref+date | 0 | pref, data = postas_day1)
dlmpostas.cus2 <- felm(log(customers_per) ~ log(cumGZ + 1)  + emergency + log(newcase_day + 1) | pref+date | 0 | pref, data = postas_day1)
dlmpostas.cus3 <- felm(log(customers_per) ~ log(cumGZ + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref+date | 0 | pref, data = postas_day1)
dlmpostas.cus4 <- felm(log(customers_per) ~ log(cumGZ + 1)  + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_day1)

table3 <- stargazer(dlmpostas1, dlmpostas2, dlmpostas3, dlmpostas4,
                    dlmpostas.cus1, dlmpostas.cus2, dlmpostas.cus3, dlmpostas.cus4,
                    title = "Restaurants' sales and customers (POS) and the Green Zone certification",
                    digits = 3,
                    digits.extra = 0,
                    type = "latex",
                    dep.var.labels = c("Sales per restaurant, log",
                                       "Customers per restaurant, log"),
                    covariate.labels = c("Cumulative GZ-certified restaurants, log",
                                         "State of Emergency",
                                         "The number of new COVID-19 cases, log",
                                         "Average temperature, log",
                                         "Average rainfall, log",
                                         "School closure",
                                         "Gathering restriction"),
                    add.lines=list(c("Prefecture FE", rep("X",8)), c("Day FE", rep("X",8))),
                    omit.stat=c("f", "ser"),
                    header = FALSE,
                    column.sep.width = "1pt",
                    float.env = "sidewaystable",
                    font.size = "footnotesize",
                    notes.align = "l",
                    notes.append = FALSE)

table3.note <- "\\multicolumn{9}{l} {\\parbox[t]{22cm}{ \\textit{Notes:} *p<0.1; **p<0.05; ***p<0.01
The dependent variable is the log-transformed value of POS sales per restaurant and the number of customers per restaurant.
The unit of analysis is prefecture and day, and the fixed effects are introduced in all models. 
For the observations, six prefectures are targeted, and the period of analysis is for 912 days from January 1st, 2019 to April 30th, 2021. 
The values in parentheses are cluster-robust standard errors. Clustering is at the prefecture level.
Cumulative GZ-certified restaurants, log is the log-transformed value of the number of cumulative certified-GZ restaurants plus one.
State of Emergency is the dummy variable that takes the value 1 if the state of emergency is declared. 
The number of new COVID-19 cases, log is the log-transformed value of the daily number of infection cases plus one.
Average temperature, log is the log-transformed value of the mean temperature (Fahrenheit degrees).
Average rainfall, log is the log-transformed value of the aggregated rainfall (in millimeters).
School closure is the dummy variable that takes the value 1 if the school closure is declared. 
Gathering restriction is the dummy variable that takes the value 1 if the large-scale gathering restriction is declared.}} \\\\"
table3[grepl("Note",table3)] <- table3.note
cat (table3, sep = "\n")
write(table3, here::here("04_analyze/Postas/output/postas_pref_week.tex"))

#p-value
# stargazer(dlmpostas1,  type="text", report=('vc*p'))
# stargazer(dlmpostas.cus1,  type="text", report=('vc*p'))

