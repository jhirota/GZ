library(tidyverse)

# data load ------
wPrefCovid <- read_csv(here::here("03_build/weekSIR/output/cases_timeseries_plot.csv"))

# New infection cases in time series----------

COVID_caseplot <- ggplot(data = wPrefCovid,
                         mapping = aes(x = week,
                                       y = newcase_day_per,
                                       color = treat)) +
  geom_point(size = 0.1) +
  geom_line(size = 0.5)  +
  ggtitle("Time series of new infections by week") +
  xlab("Week") +
  ylab("New infections per 100000 people") +
  scale_x_date(date_labels = "%Y-%m") +
  geom_vline(xintercept = as.Date("2020-04-07"),
             linetype = "dotdash",
             color = "grey60") +
  annotate("text",
           x = as.Date("2020-04-07"),
           y = 27.5,
           label = "The first \n state of emergency",
           size = 4)+
  geom_vline(xintercept = as.Date("2020-07-22"),
             linetype = "dotdash",
             color = "grey60")+
  annotate("text",
           x = as.Date("2020-07-22"),
           y = 27.5,
           label = "Go To Campaign",
           size = 4)+
  geom_vline(xintercept = as.Date("2021-01-07"),
             linetype = "dotdash",
             color = "grey60")+
  annotate("text",
           x = as.Date("2021-01-07"),
           y = 27.5,
           label = "The second \n state of emergency",
           size = 4)+
  scale_color_manual(name = " ",
                     values = c("#339900","#ff9900"),
                     labels = c("Yamanashi", "Neighboring \n Prefectures")) +
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm"))

ggsave("04_analyze/Covid_cases_plot/output/Cases_by_week.png", COVID_caseplot, width = 10, height = 8, dpi = 300)
