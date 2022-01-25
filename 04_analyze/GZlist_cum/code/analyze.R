library(tidyverse)


# data load 
GZcumplot <- read_csv(here::here("03_build/weekSIR/output/weekSIR.csv")) %>% 
  filter(pref == "Yamanashi") %>% 
  filter(week >= "2020-03-16")


# Cumulative number of GZ certified restaurants -------

gz_week_caseplot <- ggplot(data = GZcumplot,
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

gz_week_caseplot

ggsave(here::here("04_analyze/GZlist_cum/output/cumGZ_timeseries.png"), gz_week_caseplot, width = 10, height = 5, dpi = 300)

