library(tidyverse)


# plot -------
cleancovid <- read_csv("03_build/Case_positive/output/市町村別感染者数.csv")
  
Chuhoku <- cleancovid %>% 
  filter(city == "甲府市" ||
           city == "甲斐市" || 
           city == "中央市" || 
           city == "昭和町" || 
           city == "韮崎市" || 
           city == "南アルプス市" || 
           city == "北杜市")

caseplot <- ggplot(data = Chuhoku,
                   mapping = aes(x = 公表日,
                                 y = Nofcases,
                                 color = city)) +
  geom_point(size = 0.1) +
  geom_line(size = 0.5) +
  ggtitle("中北地域の新規感染者数の推移(市町村別)") +
  theme_gray (base_family = "HiraKakuPro-W3") +
  scale_color_brewer(palette = "Set3")

# ggsave("04_analyze/Case_positive_plot/output/newcases_city.png", caseplot, width = 6, height = 4, dpi = 300)

cumplot <- cleancovid %>% 
  filter(公表日 == "2021-04-30") %>% 
  ggplot(mapping = aes(x = city,
                       y = cum_cases,
                       fill = city)) +
  geom_bar(stat='identity') +
  theme_gray (base_family = "HiraKakuPro-W3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# ggsave("04_analyze/Case_positive_plot/output/cumcases_city.png", cumplot, width = 6, height = 4, dpi = 300)

#Top 5 cities in number of cases--------
Top5 <- c("甲斐市", "笛吹市", "甲府市", "富士吉田市", "南アルプス市")

Top5plot <- cleancovid %>% 
  filter(city %in% Top5) %>% 
  ggplot(mapping = aes(x = 公表日,
                       y = Nofcases,
                       color = city)) +
  geom_point(size = 0.1) +
  geom_line(size = 0.5) +
  ggtitle("新規感染者数上位５市の推移(市町村別)") +
  theme_gray (base_family = "HiraKakuPro-W3") +
  scale_color_brewer(palette = "Set2")

# ggsave("04_analyze/Case_positive_plot/output/newcases_city_top5.png", Top5plot, width = 6, height = 4, dpi = 300)


# weekly plot -------

weekcovid <- read.csv("03_build/Case_positive/output/市町村別感染者数_週別.csv")
weekcovid$公表日 <- as.Date(weekcovid$公表日)

weekplot <- weekcovid %>% 
  filter(city %in% Top5) %>% 
  ggplot(mapping = aes(x = 公表日,
                       y = Nofcases,
                       color = city)) +
  geom_point(size = 0.1) +
  geom_line(size = 0.5) +
  ggtitle("新規感染者数上位５市の推移(市町村別・週別)") +
  labs(fill = "市町村名") +
  theme_gray (base_family = "HiraKakuPro-W3") +
  scale_color_brewer(palette = "Set2") +
  xlab("感染の公表日") +
  ylab("新規感染者数")

# ggsave("04_analyze/Case_positive_plot/output/newcases_city_top5_week.png", weekplot, width = 6, height = 4, dpi = 300)




