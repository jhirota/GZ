library(lfe)
library(stargazer)


# 7.2.(i). GZ認証累計店舗数がレストラン閲覧増加率に与える影響 from weekSIR.R------------
weekSIR2 <- read.csv(here::here("03_build/weekSIR/output/weekSIR2.csv"))

resview1 <- felm(resview ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency| pref+week | 0 | pref, data = weekSIR2)
resview2 <- felm(resview ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp_q + rain| pref+week | 0 | pref, data = weekSIR2)
resview3 <- felm(resview ~ log(cumGZ + 1)  + log(newcaseday + 1) + emergency + avg_temp_q + rain + log(cumGZ + 1):log(newcaseday + 1)| pref+week | 0 | pref, data = weekSIR2)

resview_yama_mean <- mean(weekSIR2$resview[weekSIR2$pref == "Yamanashi"])
resview_nonyama_mean <- mean(weekSIR2$resview[weekSIR2$pref != "Yamanashi"])

# resviewhtml <- stargazer(resview1, resview2, resview3,
#                          dep.var.labels = "Restaurant View (change rate from the same week of 2019) ",
#                          title = "TABLE: Restaurant View and GreenZone certification",
#                          digits = 3,
#                          digits.extra = 0,
#                          type = "html",
#                          out = "04_analyze/Res_view/output/resview_pref_week.html",
#                          add.lines=list(c("Restraurant View Yamanashi mean", "", round(resview_yama_mean, digits = 3)),
#                                         c("Restraurant View Control mean", "",round(resview_nonyama_mean, digits = 3)),
#                                         c("Prefecture FE", "X", "X", "X", "X"), c("Week FE", "X", "X","X", "X")),
#                          omit.stat=c("f", "ser"))
