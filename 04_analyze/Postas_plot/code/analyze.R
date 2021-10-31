library(tidyverse)


# data load------
weekSIR3 <- read_csv("03_build/Postas/output/weekSIR3.csv")
postas_new2 <- read_csv("03_build/Postas/output/postas_2019_2021.csv")

# postas plots --------

postasplot <- ggplot(data = weekSIR3,
                     mapping = aes(x = week,
                                   y = sales_per,
                                   color = pref)) +
  geom_point() +
  geom_line()

postasplot

ggsave("04_analyze/Postas_plot/output/sales_plot.png", postasplot, width = 10, height = 8, dpi = 300)


postas.cusplot <- ggplot(data = weekSIR3,
                         mapping = aes(x = week,
                                       y = customers_per,
                                       color = pref)) +
  geom_point() +
  geom_line()

postas.cusplot

ggsave("04_analyze/Postas_plot/output/customers_plot.png", postas.cusplot, width = 10, height = 8, dpi = 300)


# new postas plots --------

posplotdata <- postas_new2 %>% 
  group_by(treat, week) %>% 
  summarize_at(vars(which(names(postas_new2) == "sales_per")-2, which(names(postas_new2) == "customers_per")-2),
               funs(mean(., na.rm=TRUE))) %>%
  arrange(week) %>% 
  ungroup()



newpostasplot <- ggplot(data = posplotdata %>% filter(week <= 18805),
                        mapping = aes(x = week,
                                      y = sales_per/10000,
                                      color = treat))  +
  xlab("") +
  ylab("1店舗あたりの週別売上[万円]") +
  geom_point(size = 0.1) +
  geom_line(size = 0.5)  +
  scale_x_date(date_labels = "%Y-%m") +
  ggtitle("山梨県と近隣5県の1店舗あたりの週別売上の時系列推移") +
  theme_gray(base_family = "MS Mincho") +
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  annotate("text",
           x = as.Date("2020-07-17"),
           y = Inf,
           label = "GZ認証制度開始",
           color = "forestgreen",
           size = 4,
           fontface="italic",
           vjust=5,
           family = "MS Mincho")+
  scale_color_manual(name = " ", values = c("#339900","#ff9900")) +
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "cm"))

newpostasplot

ggsave("04_analyze/Postas_plot/output/sales_plot2.png", newpostasplot, width = 10, height = 8, dpi = 300)

#客数
newpostascusplot <- ggplot(data = posplotdata%>% filter(week <= 18805),
                           mapping = aes(x = week,
                                         y = customers_per,
                                         color = treat))  +
  xlab("") +
  ylab("1店舗あたりの客数") +
  geom_point(size = 0.1) +
  geom_line(size = 0.5)  +
  scale_x_date(date_labels = "%Y-%m") +
  ggtitle("山梨県と近隣5県の1店舗あたりの週別客数の時系列推移") +
  theme_gray(base_family = "MS Mincho") +
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  annotate("text",
           x = as.Date("2020-07-17"),
           y = Inf,
           label = "GZ認証制度開始",
           color = "forestgreen",
           size = 4,
           fontface="italic",
           vjust=5,
           family = "MS Mincho")+
  scale_color_manual(name = " ", values = c("#339900","#ff9900")) +
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "cm"))

newpostascusplot

ggsave("04_analyze/Postas_plot/output/customers_plot2.png", newpostascusplot, width = 10, height = 8, dpi = 300)


