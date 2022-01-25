

## Introduction -------

# Graph 1
source(here::here("04_analyze/Covid_cases_plot/code/analyze.R"))
COVID_timeseries <- ggplot(data = wPrefCovid,
                           mapping = aes(x = week,
                                         y = newcaseday_per,
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
  

ggsave(here::here("05_report/main/output/covid_week_timeseries.png"), COVID_timeseries, width = 10, height = 8, dpi = 300)

# Graph 2
source(here::here("04_analyze/Postas_plot/code/analyze.R"))
sales_timeseries <- ggplot(data = posplotdata %>% filter(week <= 18805),
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

ggsave(here::here("05_report/main/output/sales_week_timeseries.png"), sales_timeseries, width = 10, height = 8, dpi = 300)


# Graph 3
customers_timeseries <- ggplot(data = posplotdata %>% filter(week <= 18805),
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

ggsave(here::here("05_report/main/output/customers_week_timeseries.png"), customers_timeseries, width = 10, height = 8, dpi = 300)


# Graph 4
source(here::here("04_analyze/GZlist_cum/code/analyze.R"))
cumGZplot <- ggplot(data = GZcumplot,
                    mapping = aes(x = week,
                                  y = cumGZ))+
  xlab("Week") +
  ylab("Number of GZ-certified restaurants") +
  geom_point(size = 0.1, color = "forestgreen") +
  geom_line(size = 0.5, color = "forestgreen")  +
  scale_x_date(date_labels = "%Y-%m") +
  ggtitle("Time series of cumulative number of GZ-certified restaurants") +
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  annotate("text",
           x = as.Date("2020-07-17"),
           y = Inf,
           label = "The GZ certification system",
           size = 4,
           vjust=5)+
  geom_label(aes(x = as.Date("2020-09-28"), y = 2500, label = "The first expansion"), fill = "white",
             size = 4, vjust=5) +
  geom_label(aes(x = as.Date("2021-04-01"),y = 3000, label = "The second expansion"), fill = "white",
             size = 4, vjust=5) +
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16), 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16))

cumGZplot

ggsave(here::here("05_report/main/output/cumGZ_timeseries.png"), cumGZplot, width = 10, height = 5, dpi = 300)


## Results ----------

# Counterfactual (covid) and (economic)
reticulate::source_python(here::here("04_analyze/Counterfactual/code/analyze.py"))
file.copy(from = c("04_analyze/Counterfactual/output/cofa_covid.png",
                   "04_analyze/Counterfactual/output/cofa_sales.png"),
          to = "05_report/main/output", 
          overwrite = TRUE,
          recursive = FALSE, 
          copy.mode = TRUE)











