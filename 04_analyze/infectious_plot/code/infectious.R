
library(tidyverse)

Agrgt <- read_csv("03_build/Controls/output/県別流入リスク.csv")

inflowrisk <- ggplot(data = Agrgt,
       mapping = aes(x = week,
                     y = agrgt潜在的感染者人流_lag1,
                     color = 都道府県)) +
  geom_point(size = 0.1) +
  geom_line(size = 0.5) +
  ggtitle("他県からの感染者流入リスクの推移(県別)") +
  theme_gray (base_family = "HiraKakuPro-W3") +
  scale_color_manual(values=c("red",rep("grey",5)))

# ggsave("04_analyze/infectious_plot/output/infectious_inflow_risk.png", inflowrisk, width = 6, height = 4, dpi = 300)

inflowpop <- ggplot(data = Agrgt,
       mapping = aes(x = week,
                     y = sum_pop,
                     color = 都道府県)) +
  geom_point(size = 0.1) +
  geom_line(size = 0.5) +
  ggtitle("他県からの人流流入の推移(県別)") +
  theme_gray (base_family = "HiraKakuPro-W3") +
  scale_color_brewer(palette = "Greys")

# ggsave("04_analyze/infectious_plot/output/mobility_inflow.png", inflowpop , width = 6, height = 4, dpi = 300)

