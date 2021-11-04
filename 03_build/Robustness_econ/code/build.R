library(tidyverse)

#data load ---
postas_day <- read_csv("03_build/Postas/output/postas_daily_data.csv")
selfrest <- read_csv("03_build/Selfrestraint/output/selfrestraint.csv")

#data clean ---
colnames(selfrest)[which(colnames(selfrest) == "prefecture")] <- "pref"
colnames(selfrest)[which(colnames(selfrest) == "Night-time self-restraint rate")] <- "Night_selfrest"

selfrest$date <- as.Date(selfrest$date)
selfrest$pref <- gsub("茨城県", "Ibaragi", selfrest$pref)
selfrest$pref <- gsub("静岡県", "Shizuoka", selfrest$pref)
selfrest$pref <- gsub("栃木県", "Tochigi", selfrest$pref)
selfrest$pref <- gsub("群馬県", "Gunma", selfrest$pref)
selfrest$pref <- gsub("山梨県", "Yamanashi", selfrest$pref)
selfrest$pref <- gsub("長野県", "Nagano", selfrest$pref)

# selfrest rate data is until 2021/03
postas_rob <- postas_day %>% 
  left_join(selfrest, by = c("date", "pref")) %>% 
  filter(date <= as.Date("2021-04-30"))


# data clean (add a variable which increases linear by random amount) --------

postas_rob$rand <- 0

set.seed(20211103)
for (i in unique(postas_rob$pref)) {
  GZup <- postas_rob$date[postas_rob$GZnew > 0]
  postas_rob$rand[postas_rob$pref == i] <- ifelse(postas_rob$date %in% GZup, runif(912, min = 1, max = 100), 0)
}
postas_rob <- postas_rob %>% 
  mutate(linear_rand = ave(rand, pref, FUN = cumsum))

postas_rob <- postas_rob %>% 
  mutate(linear_rand_ymns = ifelse(postas_rob$pref == "Yamanashi", postas_rob$linear_rand, 0))
    
# randplot <- ggplot(data = postas_rob,
#        mapping = aes(x = date, y = linear_rand, color = pref)) +
#   geom_line()
# randplot
# ggsave("03_build/Robustness_econ/output/rand_linear.png", randplot, width = 6, height = 4, dpi = 300)




# data save -----
write_csv(postas_rob, "03_build/Robustness_econ/output/postas_rob.csv")
