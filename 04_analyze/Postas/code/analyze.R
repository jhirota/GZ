library(tidyverse)


# data load------
postas_day1 <- read_csv(here::here("03_build/Postas/output/postas_daily_data.csv"))
postas_week <- read_csv(here::here("03_build/Postas/output/postas_2019_2021.csv"))

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
                    notes.align = "l",notes.append = FALSE)

table3.note <- "\\multicolumn{9}{l} {\\parbox[t]{22cm}{ \\textit{Notes:} *p<0.1; **p<0.05; ***p<0.01
The dependent variable is the log-transformed value of POS sales per restaurant and the number of customers per restaurant.
The unit of analysis is prefecture and day, and the fixed effects are introduced in all models. 
For the observations, six prefectures are targeted, and the period of analysis is for 912 days from January XX, 2019 to April 30, 2021. 
The values in parentheses are cluster-robust standard errors. Clustering is at the prefecture level.
Cumulative GZ-certified restaurants, log is the log-transformed value of the number of cumulative certified-GZ restaurants plus one.
State of Emergency is the dummy variable that takes the value 1 if the state of emergency is declared. 
The number of new COVID-19 cases, log is the log-transformed value of the daily number of infection cases plus one.
Average temperature, log is the log-transformed value of the squared mean temperature (Celsius degrees).
Average rainfall, log is the log-transformed value of the aggregated rainfall (in millimeters).
School closure is the dummy variable that takes the value 1 if the school closure is declared. 
Gathering restriction is the dummy variable that takes the value 1 if the large-scale gathering restriction is declared.}} \\\\"
table3[grepl("Note",table3)] <- table3.note
cat (table3, sep = "\n")
write(table3, here::here("04_analyze/Postas/output/postas_pref_week.tex"))


# postas plots --------


sales_timeseries <- ggplot(data = postas_week %>% filter(week <= 18805),
                           mapping = aes(x = week,
                                         y = sales_per/10000,
                                         color = treat))  +
  xlab("Week") +
  ylab("Weekly sales per restaurant [10000 JPY]") +
  geom_point(size = 0.1) +
  geom_line(size = 0.5)  +
  scale_x_date(date_labels = "%Y-%m") +
  ggtitle("Time series of weekly sales per restaurant") +
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  annotate("text",
           x = as.Date("2020-07-17"),
           y = Inf,
           label = "The GZ certification system",
           size = 4,
           vjust=5)+
  scale_color_manual(name = " ",
                     values = c("#339900","#ff9900"),
                     labels = c("Yamanashi", "Neighboring \n Prefectures")) +
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "cm"))

sales_timeseries

ggsave("04_analyze/Postas/output/sales_plot.png", sales_timeseries, width = 10, height = 8, dpi = 300)

#customers
customers_timeseries <- ggplot(data = postas_week %>% filter(week <= 18805),
                               mapping = aes(x = week,
                                             y = customers_per,
                                             color = treat))  +
  xlab("Week") +
  ylab("Weekly customers per restaurant") +
  geom_point(size = 0.1) +
  geom_line(size = 0.5)  +
  scale_x_date(date_labels = "%Y-%m") +
  ggtitle("Time series of weekly customers per restaurant") +
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  annotate("text",
           x = as.Date("2020-07-17"),
           y = Inf,
           label = "The GZ certification system",
           size = 4,
           vjust=5)+
  scale_color_manual(name = " ",
                     values = c("#339900","#ff9900"),
                     labels = c("Yamanashi", "Neighboring \n Prefectures")) +
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "cm"))

customers_timeseries

ggsave("04_analyze/Postas/output/customers_plot.png", customers_timeseries, width = 10, height = 8, dpi = 300)


