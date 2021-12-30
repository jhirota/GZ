
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



# GZ累計のプロット --------
GZcumplot <- read.csv("03_build/weekSIR/output/weekSIR2.csv") %>% 
  filter(pref == "Yamanashi") %>% 
  filter(weeknum >= 12)
GZcumplot$week <- as.Date(GZcumplot$week)

gz_week_caseplot <- ggplot(data = GZcumplot,
                           mapping = aes(x = week,
                                         y = cumGZ))+
  xlab("") +
  ylab("グリーンゾーン認証累計店舗数(飲食カテゴリー限定)") +
  geom_point(size = 0.1, color = "forestgreen") +
  geom_line(size = 0.5, color = "forestgreen")  +
  scale_x_date(date_labels = "%Y-%m") +
  ggtitle("グリーンゾーン認証累計店舗数(飲食カテゴリー限定)　時系列推移") +
  theme_gray (base_family = "MS Mincho") +
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  geom_label(aes(x = as.Date("2020-07-17"), y = Inf, label="2020年7月第3週\nGZ認証制度開始"), fill = "white",
             size = 4, vjust=2, family = "MS Mincho") +
  geom_vline(xintercept = as.Date("2020-12-28"), linetype = "dotdash", color = "forestgreen")+
  geom_label(aes(x = as.Date("2020-09-28"), y = 2500, label = "第1フェーズ"), fill = "white",
             size = 4, vjust=5, family = "MS Mincho", color = "forestgreen") +
  geom_label(aes(x = as.Date("2020-12-28"), y = Inf, label = "2020年\n12月\n第5週"), fill = "white",
             size = 4, vjust=2, family = "MS Mincho") +
  geom_vline(xintercept = as.Date("2021-02-01"), linetype = "dotdash", color = "forestgreen")+
  geom_label(aes(x = as.Date("2021-04-01"),y = 3000, label = "第2フェーズ"), fill = "white",
             size = 4, vjust=5, family = "MS Mincho", color = "forestgreen") +
  geom_label(aes(x = as.Date("2021-02-01"), y = Inf, label = "2021年\n2月\n第1週"), fill = "white",
             size = 4, vjust=2, family = "MS Mincho") +
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16), 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16))



gz_week_caseplot


ggsave("04_analyze/GZlist_cum/output/GZ_week_plot.png", gz_week_caseplot, width = 10, height = 8, dpi = 300)


