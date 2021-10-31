
# GZ all plot -------
GZall_list3 <- read.csv("03_build/GZlist/output/GZalllist_week.csv")

png("04_analyze/GZlist_cum/output/FoodandHotel.png")
plot(y = GZall_list3$GZnew[GZall_list3$Type == "Food"], x = GZall_list3$GZnew[GZall_list3$Type == "Hotel"])
dev.off()

png("04_analyze/GZlist_cum/output/FoodandHotel2.png")
plot(y = GZall_list3$cumGZ[GZall_list3$Type == "Food"],
     x = GZall_list3$cumGZ[GZall_list3$Type == "Hotel"],
     xlim = c(0, 4500),
     ylim = c(0, 4500))+
  abline(1, 1)
dev.off()

a <- ggplot(data = GZall_list3,
       mapping = aes(x = week, y = cumGZ, color = Type)) +
  geom_point(size = 0.5) +
  geom_line()
# ggsave("04_analyze/GZlist_cum/output/GZall_cum.png", a, width = 6, height = 4, dpi = 300)

b <- ggplot(data = GZall_list3,
            mapping = aes(x = week, y = GZnew, color = Type)) +
  geom_point(size = 0.5) +
  geom_line()
# ggsave("04_analyze/GZlist_cum/output/GZall_new.png", b, width = 6, height = 4, dpi = 300)

png("04_analyze/GZlist_cum/output/cumGZ_week.png")
plot(y = GZall_list3$cumGZ,
     x = GZall_list3$week,
     color = GZall_list3$Type)
dev.off()

