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

table2 <- stargazer(dlmpostas1, dlmpostas2, dlmpostas3, dlmpostas4,
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
                    column.sep.width = "-15pt",
                    out = "04_analyze/Postas/output/postas_pref_week.tex")


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


