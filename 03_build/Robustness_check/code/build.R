library(readxl)
library(lubridate)
library(tidyverse)

# data load ----
weekSIR3 <- read_csv("03_build/Postas/output/weekSIR3.csv")
GZall_list4 <- read_csv("03_build/GZlist/output/GZalllist_week_wide.csv")

# data clean (add type of GZ) --------

weekSIR_rob <- weekSIR3 
weekSIR_rob$cumGZHotel <- 0
weekSIR_rob$cumGZTransition <- 0
weekSIR_rob$cumGZWinery <- 0
weekSIR_rob$cumGZShuzou <- 0
weekSIR_rob$cumGZFood <- 0

weekSIR_rob$cumGZHotel[weekSIR_rob$pref == "Yamanashi" & weekSIR_rob$week >= 18456] <- GZall_list4$cumGZHotel
weekSIR_rob$cumGZTransition[weekSIR_rob$pref == "Yamanashi" & weekSIR_rob$week >= 18456] <- GZall_list4$cumGZTransition
weekSIR_rob$cumGZWinery[weekSIR_rob$pref == "Yamanashi" & weekSIR_rob$week >= 18456] <- GZall_list4$cumGZWinery
weekSIR_rob$cumGZShuzou[weekSIR_rob$pref == "Yamanashi" & weekSIR_rob$week >= 18456] <- GZall_list4$cumGZShuzou
weekSIR_rob$cumGZFood[weekSIR_rob$pref == "Yamanashi" & weekSIR_rob$week >= 18456] <- GZall_list4$cumGZFood

weekSIR_rob$cumGZFoodHotel <- weekSIR_rob$cumGZFood + weekSIR_rob$cumGZHotel

# data clean (add a variable which increases linear by random amount) --------

weekSIR_rob$rand <- 0

set.seed(20211103)
for (i in unique(weekSIR_rob$pref)) {
  weekSIR_rob$rand[weekSIR_rob$pref == i] <- runif(68, min = 1, max = 100)
  beforeGZ <- "2020-07-17"
  weekSIR_rob$rand[weekSIR_rob$pref == i][weekSIR_rob$week < as.Date(beforeGZ)] <- 0
}
weekSIR_rob <- weekSIR_rob %>% 
  mutate(linear_rand = ave(rand, pref, FUN = cumsum)) %>% 
  mutate(linear_rand_ymns = ifelse(pref == "Yamanashi", ave(rand, pref, FUN = cumsum), 0))

# randplot <- ggplot(data = weekSIR_rob,
#        mapping = aes(x = week, y = linear_rand, color = pref)) +
#   geom_line()
# ggsave("03_build/Robustness_check/output/rand_linear.png", randplot, width = 6, height = 4, dpi = 300)



# data clean (add school disclosure and gathering-ban dummy vars) --------

#data load and clean
dummy <- read_excel("02_bring/Dummy_vars/data/Dummies.xlsx")
colnames(dummy)[which(colnames(dummy) == c("Pref", "day"))] <- c("pref", "date")
dummy$date <- as.Date(dummy$date)
dummy$pref <- gsub("Ibaraki", "Ibaragi", dummy$pref)
dummy$week <- floor_date(dummy$date,
                         "week",
                         week_start = getOption("lubridate.week.start", 1))

# if a week contains even a single day of restriction, the week has value 1.
dummy_w <- dummy %>% 
  group_by(week, pref) %>% 
  summarize(dummy_school_closure = max(dummy_school_closure),
            dummy_gathering_restriction = max(dummy_gathering_restriction)) %>% 
  ungroup()

# data merge

weekSIR_rob <- left_join(x = weekSIR_rob,
                         y = dummy_w, 
                         by = c("week", "pref"))


# data merge with self restraint rate --------
#data load and clean
selfrest <- read_csv("03_build/Selfrestraint/output/selfrestraint.csv")
colnames(selfrest)[which(colnames(selfrest) == "prefecture")] <- "pref"
selfrest$date <- as.Date(selfrest$date)
selfrest$week <- floor_date(selfrest$date,
                         "week",
                         week_start = getOption("lubridate.week.start", 1))
selfrest$pref <- gsub("茨城県", "Ibaragi", selfrest$pref)
selfrest$pref <- gsub("静岡県", "Shizuoka", selfrest$pref)
selfrest$pref <- gsub("栃木県", "Tochigi", selfrest$pref)
selfrest$pref <- gsub("群馬県", "Gunma", selfrest$pref)
selfrest$pref <- gsub("山梨県", "Yamanashi", selfrest$pref)
selfrest$pref <- gsub("長野県", "Nagano", selfrest$pref)


selfrest_w <- selfrest %>% 
  filter(pref %in% c("Ibaragi", "Shizuoka", "Tochigi", "Gunma", "Yamanashi", "Nagano")) %>%
  select(!date) %>% 
  group_by(pref, week) %>% 
  summarize_all(mean) %>% 
  ungroup()

weekSIR_rob <- left_join(x = weekSIR_rob,
                         y = selfrest_w, 
                         by = c("week", "pref"))

write_csv(weekSIR_rob, "03_build/Robustness_check/output/weekSIR_robustness.csv")
