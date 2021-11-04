
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

weekSIR_rob$randomnew <- 0

for (i in unique(weekSIR_rob$pref)) {
  weekSIR_rob$randomnew[weekSIR_rob$pref == i] <- rnorm(n = 68, mean = 10, sd = 5)
  beforeGZ <- "2020-07-17"
  weekSIR_rob$randomnew[weekSIR_rob$pref == i][weekSIR_rob$week < as.Date(beforeGZ)] <- 0
}
weekSIR_rob$randomnew <- ifelse(weekSIR_rob$randomnew < 0, 0, weekSIR_rob$randomnew)


weekSIR_rob <- weekSIR_rob %>% 
  mutate(cumrandomnew = ave(randomnew, pref, FUN = cumsum)) %>% 
  mutate(cumrandom_ymns = ifelse(pref == "Yamanashi", ave(randomnew, pref, FUN = cumsum), 0))



write.csv(weekSIR_rob, "03_build/Robustness_check/output/weekSIR_robustness.csv", row.names = FALSE)
