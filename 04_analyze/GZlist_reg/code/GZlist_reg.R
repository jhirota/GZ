
GZall_list4 <- read.csv("03_build/GZlist/output/GZalllist_week_wide.csv")
GZall_list4$week <- as.Date(GZall_list4$week)

# Food と Hotelの相関 ------
lm <- lm(cumGZFood ~ cumGZHotel + factor(week), data = GZall_list4)
summary(lm)

# alias(lm)
# car::vif(lm)
# 
# cor(GZall_list4, method = "pearson")
