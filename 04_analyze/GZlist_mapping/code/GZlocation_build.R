library(tidyverse)
library(jpndistrict)

# GZ mapping -----
yamanashi <- read.csv("03_build/GZlist/output/GZ_mapping.csv")

GZmapping <- yamanashi %>% 
  ggplot() +
  geom_sf(aes(fill = Freq)) +
  geom_point(aes(x = Keido,
                   y = Ido),
               data = GZlist,
             size = 0.01,
             color = "red") +
  scale_fill_continuous(name = "# of shops",
                        low = "lightgreen",
                        high = "darkgreen",
                        limits = c(0, 1200))
GZmapping

# ggsave("04_analyze/GZlist_mapping/output/GZ_70th.png", GZmapping, width = 6, height = 4, dpi = 300)

# GZ40週時点(2020/10/04まで) mapping ---------

GZ_40th <- yamanashi %>% 
  ggplot() +
  geom_sf(aes(fill = Freq40th)) +
  geom_point(aes(x = Keido,
                 y = Ido),
             data = GZlist40th,
             size = 0.01,
             color = "red") +
  scale_fill_continuous(name = "# of shops",
                        low = "lightgreen",
                        high = "darkgreen",
                        limits = c(0, 1200))
GZ_40th

# ggsave("04_analyze/GZlist_mapping/output/GZ_40th.png", GZ_40th, width = 6, height = 4, dpi = 300)

# GZ52週時点(2020/12/27まで) mapping---------

GZ_52nd <- yamanashi %>% 
  ggplot() +
  geom_sf(aes(fill = Freq52nd)) +
  geom_point(aes(x = Keido,
                 y = Ido),
             data = GZlist52nd,
             size = 0.01,
             color = "red") +
  scale_fill_continuous(name = "# of shops",
                        low = "lightgreen",
                        high = "darkgreen",
                        limits = c(0, 1200))
GZ_52nd

# ggsave("04_analyze/GZlist_mapping/output/GZ_52nd.png", GZ_52nd, width = 6, height = 4, dpi = 300)


# Freq mapping (Freq >= 200)-------

Freq200_map <- yamanashi %>%
  ggplot() +
  geom_sf(aes(fill = (Freq >= 200)))

Freq200_map

# ggsave("04_analyze/GZlist_mapping/output/GZ_Freq200.png", Freq200_map, width = 6, height = 4, dpi = 300)


# Kansokujo plot ----

kansoku <- read.csv("02_bring/Weather/data/kansokujo1.csv")

kansokujo <- yamanashi %>% 
  ggplot() +
  geom_sf() +
  geom_point(aes(x = Keido,
                 y = Ido),
             data = kansoku,
             color = "red",
             show.legend = TRUE) +
  geom_text(aes(x = Keido,
                y = Ido,
                label = 観測所),
            data = kansoku,
            hjust = 0,
            vjust = 0,
            family = "HiraKakuPro-W3") +
  theme_gray (base_family = "HiraKakuPro-W3")

# ggsave("04_analyze/GZlist_mapping/output/kansokujo_plot.png", kansokujo, width = 6, height = 4, dpi = 300)

