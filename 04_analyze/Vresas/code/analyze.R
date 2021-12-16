
library(tidyverse)
library(lfe)


# 7.2.(iii) 山梨県と周辺５都道府県における県外からの人流変化率　時系列推移 Vresus from weekSIR.R--------
mob_vresas <- read_csv("03_build/weekSIR/output/weekSIR2.csv")
incity_yama_mean <- mean(mob_vresas$in_city[mob_vresas$pref == "Yamanashi"])
incity_nonyama_mean <- mean(mob_vresas$in_city[mob_vresas$pref != "Yamanashi"])
inpref_yama_mean <- mean(mob_vresas$in_pref[mob_vresas$pref == "Yamanashi"])
inpref_nonyama_mean <- mean(mob_vresas$in_pref[mob_vresas$pref != "Yamanashi"])
outpref_yama_mean <- mean(mob_vresas$out_pref[mob_vresas$pref == "Yamanashi"])
outpref_nonyama_mean <- mean(mob_vresas$out_pref[mob_vresas$pref != "Yamanashi"])

#intercept出さなくても良さげなので、felmで回す。
mob1.0 <- felm(in_city ~ log(cumGZ + 1)| pref + week | 0 | pref, data = mob_vresas)
mob1.1 <- felm(in_city ~ log(cumGZ + 1) + log(newcaseday + 1) + emergency + avg_temp_q + rain| pref + week | 0 | pref, data = mob_vresas)
mob1.2 <- felm(in_city ~ log(cumGZ + 1) + log(newcaseday + 1) + emergency + avg_temp_q + rain + log(cumGZ + 1):log(newcaseday + 1)| pref + week | 0 | pref, data = mob_vresas)
mob2.0 <- felm(in_pref ~ log(cumGZ + 1)| pref + week | 0 | pref, data = mob_vresas)
mob2.1 <- felm(in_pref ~ log(cumGZ + 1) + log(newcaseday + 1) + emergency + avg_temp_q + rain| pref + week | 0 | pref, data = mob_vresas)
mob2.2 <- felm(in_pref ~ log(cumGZ + 1) + log(newcaseday + 1) + emergency + avg_temp_q + rain + log(cumGZ + 1):log(newcaseday + 1)| pref + week | 0 | pref, data = mob_vresas)
mob3.0 <- felm(out_pref ~ log(cumGZ + 1)| pref + week | 0 | pref, data = mob_vresas)
mob3.1 <- felm(out_pref ~ log(cumGZ + 1) + log(newcaseday + 1) + emergency + avg_temp_q + rain| pref + week | 0 | pref, data = mob_vresas)
mob3.2 <- felm(out_pref ~ log(cumGZ + 1) + log(newcaseday + 1) + emergency + avg_temp_q + rain + log(cumGZ + 1):log(newcaseday + 1)| pref + week | 0 | pref, data = mob_vresas)


mobplm2html <- stargazer(mob1.0, mob1.1, mob1.2, mob2.0, mob2.1, mob2.2, mob3.0, mob3.1, mob3.2,
                         dep.var.labels = c("incity","inpref","outpref"),
                         title = "TABLE: Mobility and GreenZone certification",
                         digits = 3,
                         digits.extra = 0,
                         type = "html",
                         out = "04_analyze/Vresas/output/mobility_pref_week.html",
                         add.lines=list(c("mobility Yamanashi mean(%)", "", round(incity_yama_mean, digits = 3),"","",round(inpref_yama_mean, digits = 3), "","",round(outpref_yama_mean, digits = 3), "" ),
                                        c("mobility Control mean(%)","", round(incity_nonyama_mean, digits = 3),"","",round(inpref_nonyama_mean, digits = 3),"","",round(outpref_nonyama_mean, digits = 3),""),
                                        c("Prefecture FE", rep("X",9)), c("Week FE", rep("X",9))),
                         omit.stat=c("f", "ser"))
