library(tidyverse)
library(lfe)
library(stargazer)

# data load--------

weekSIR2 <- read_csv("03_build/weekSIR/output/weekSIR2.csv")
ppDATA2 <- read_csv("03_build/Papilio_mobility/output/city_weekly2.csv")

colnames(ppDATA2)[which(colnames(ppDATA2) == "agrgt潜在的感染者人流_lag2")] <- "agrgt_potecovid_lag2"
colnames(ppDATA2)[which(colnames(ppDATA2) == "agrgt潜在的感染者人流_lag1")] <- "agrgt_potecovid_lag1"

# regressions to be compared -----

covid_res1.4 <- felm(log(newcaset.1 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag1 + 1) + log(avg_temp_q) + log(rain +1) + resview + emergency + log(noftestst.1 + 1)| pref_dummy + week_dummy | 0 | pref, data = weekSIR2)
covid_res2.4 <- felm(log(newcaset.2 + 1) ~ 1 + log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag2 + 1) + log(avg_temp_q) + log(rain + 1) + resview + emergency + log(noftestst.2 + 1)| pref_dummy + week_dummy | 0 | pref, data = weekSIR2)
FE_pp3.5 <- felm(log(infected_2w + 1) ~ log(cumGZ + 1) + log(agrgt_potecovid_lag2 + 1) + log(susceptible) + log(I(avg_temp^2)) + log(mobility_per) + log(infected + 1)| city+week | 0 |city, data = ppDATA2)
FE_pp4.5 <- felm(log(infected_1w + 1) ~ log(cumGZ + 1) + log(agrgt_potecovid_lag1 + 1) + log(susceptible) + log(I(avg_temp^2)) + log(mobility_per) + log(infected + 1)| city+week | 0 |city, data = ppDATA2)


# coef plot --------


coefdf <- data.frame(x = c("県間分析\n１週間の公表ラグ想定",
                           "県間分析\n２週間の公表ラグ想定",
                           "市町村分析\n２週間の公表ラグ想定",
                           "市町村分析\n１週間の公表ラグ想定"),
                     y = c(covid_res1.4$coefficients[1],#lag1 felm pref
                           covid_res2.4$coefficients[1],#lag2 felm pref
                           FE_pp3.5$coefficients[1][[1]],#lag2 felm city
                           FE_pp4.5$coefficients[1][[1]]),#lag1 felm city 
                     L = c(confint(covid_res1.4)[1,][[1]],
                           confint(covid_res2.4)[1,][[1]],
                           confint(FE_pp3.5)[1,][[1]],
                           confint(FE_pp4.5)[1,][[1]]),
                     U = c(confint(covid_res1.4)[1,][[2]],
                           confint(covid_res2.4)[1,][[2]],
                           confint(FE_pp3.5)[1,][[2]],
                           confint(FE_pp4.5)[1,][[2]]))


comparison <- ggplot(coefdf, aes(x = x, y = y)) +
  geom_errorbar(aes(ymax = U, ymin = L),
                width = 0.2,
                size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  labs(title = "GZ認証制度の効果の比較（市町村ー県間）",
       x = "分析範囲とラグ",
       y = "GZ認証制度の新規感染者数に与える影響（%）") +
  theme_gray (base_family = "HiraKakuPro-W3")


ggsave("04_analyze/Comparison_coef/output/comparison_city_pref.png", comparison, width = 6, height = 4, dpi = 300)

