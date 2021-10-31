library(tidyverse)

# 2.1. 図2 新規感染者推移のグラフ from pref_bet_day_analyizing.R----------

wPrefCovid <- read_csv("03_build/weekSIR/output/weekSIR2.csv")

COVID_caseplot2 <- ggplot(data = wPrefCovid,
                          mapping = aes(x = week,
                                        y = newcaseday,
                                        color = pref)) +
  geom_point(size = 0.1) +
  geom_line(size = 0.5)  +
  ggtitle("山梨県と近隣5県の週別新規感染者数の時系列推移") +
  xlab("") +
  ylab("週あたりの新規感染者数の合計")+
  scale_x_date(date_labels = "%Y-%m")+
  theme_gray (base_family = "MS Mincho")+
  geom_vline(xintercept = as.Date("2020-04-07"),
             linetype = "dotdash",
             color = "grey60")+
  annotate("text",
           x = as.Date("2020-04-07"),
           y = 880,
           label = "第一回\n緊急事態宣言",
           family = "MS Mincho")+
  geom_vline(xintercept = as.Date("2020-07-22"),
             linetype = "dotdash",
             color = "grey60")+
  annotate("text",
           x = as.Date("2020-07-22"),
           y = 880,
           label = "GoToキャンペーン\n開始",
           family = "MS Mincho")+
  geom_vline(xintercept = as.Date("2021-01-07"),
             linetype = "dotdash",
             color = "grey60")+
  annotate("text",
           x = as.Date("2021-01-07"),
           y = 880,
           label = "第二回\n緊急事態宣言",
           family = "MS Mincho")+
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm"))

COVID_caseplot2 ##感染者推移比較

ggsave("04_analyze/Covid_cases_plot/output/Cases_by_week.png", COVID_caseplot2, width = 10, height = 8, dpi = 300)

