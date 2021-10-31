library(tidyverse)


# data load ----
# weekSIR2 <- read_csv("03_build/weekSIR/output/weekSIR2.csv")
# 
# # data make ------
# c_res_lag2 <- lm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag2 + 1) +
                   # log(avg_temp_q) + log(rain + 1) + resview + emergency + log(noftestst.2 + 1) + 
                   # factor(pref) + factor(week), data = weekSIR2)

# weekSIR2$pred_covid <- NA
# weekSIR2$pred_covid[1:396] <- exp(fitted(c_res_lag2))
# 
# # counterfactual ------
# Yamapred <- rep(NA, 68)
# j <- 1
# 
# for (i in 1:nrow(weekSIR2)){
#   if (weekSIR2$pref[i] == "Yamanashi"){
#     pred <- ggpredict(c_res_lag2,
#                       terms = "cumGZ",
#                       type = "fe",
#                       condition = c(week = as.character(weekSIR2$week[i]),
#                                     pref = weekSIR2$pref[i],
#                                     noftestst.2 = weekSIR2$noftestst.2[i],
#                                     susceptable = weekSIR2$susceptable[i],
#                                     agrgt_potecovid_lag2 = weekSIR2$agrgt_potecovid_lag2[i],
#                                     avg_temp_q = weekSIR2$avg_temp_q[i],
#                                     rain = weekSIR2$rain[i],
#                                     resview = weekSIR2$resview[i],
#                                     emergency = weekSIR2$emergency[i]),
#                       back.transform = TRUE)
#     GZ0value <- pred %>% filter(x == 0)
#     Yamapred[j+2] <- GZ0value$predicted[1]
#     j <- j + 1
#   }else{
#   }
# }
# 
# 
# YamanashiTrend <- data.frame(week = levels(factor(weekSIR2$week)),
#                              nofcases = weekSIR2$newcaseday[weekSIR2$pref == "Yamanashi"],
#                              counterfactual = Yamapred,
#                              fitted = c(rep(NA,2), weekSIR2$pred_covid[weekSIR2$pref == "Yamanashi"][1:66]))
# 
# 
# 
# Yamanashi1 <- YamanashiTrend[,1:2]
# Yamanashi2 <- YamanashiTrend[,c(1,3)]
# Yamanashi3 <- YamanashiTrend[,c(1,4)]
# Yamanashi1$type <-"actual"
# Yamanashi2$type <- "counterfactual"
# Yamanashi3$type <- "fitted"
# names(Yamanashi2)[2] <- "nofcases"
# names(Yamanashi3)[2] <- "nofcases"
# 
# newYamanashiTrend <- rbind(Yamanashi1, Yamanashi2, Yamanashi3) %>% 
#   arrange(week)
# newYamanashiTrend$week <- as.Date(newYamanashiTrend$week)
# 
# write.csv(newYamanashiTrend, "03_build/Counterfactual/output/cofa_covid.csv")


# Findings -------

# covidsinceGZ <- newYamanashiTrend %>% filter(week >= 18475) #8月以降4月まで
# c <- covidsinceGZ$nofcases[covidsinceGZ$type == "actual"] -
#   covidsinceGZ$nofcases[covidsinceGZ$type == "counterfactual"]
# sum(c, na.rm = TRUE) ##単純計算で調査期間の9ヶ月で1175人の感染を防いだ



# counterfactual (update)------

# covid counterfactual plot-----
weekSIR3 <- read_csv("03_build/Postas/output/weekSIR3.csv") %>% 
  arrange(week)

covidmodel <- lm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(agrgt_potecovid_lag2 + 1) + emergency +
                   log(noftestst.2 + 1) + log(customers_per) + log(avg_temp_q) + log(rain + 1) + 
                   factor(pref) + factor(week), data = weekSIR3)


weekSIR3$pred_covid <- NA
weekSIR3$pred_covid[1:nrow(weekSIR3)] <- exp(fitted(covidmodel))

# counterfactual
Yamapred <- rep(NA, 68)
j <- 1

for (i in 1:nrow(weekSIR3)){
  if (weekSIR3$pref[i] == "Yamanashi"){
    pred <- ggpredict(covidmodel,
                      terms = "cumGZ",
                      type = "fe",
                      condition = c(week = as.character(weekSIR3$week[i]),
                                    pref = weekSIR3$pref[i],
                                    noftestst.2 = weekSIR3$noftestst.2[i],
                                    agrgt_potecovid_lag2 = weekSIR3$agrgt_potecovid_lag2[i],
                                    avg_temp_q = weekSIR3$avg_temp_q[i],
                                    rain = weekSIR3$rain[i],
                                    customers_per = weekSIR3$customers_per[i],
                                    emergency = weekSIR3$emergency[i]),
                      back.transform = TRUE)
    GZ0value <- pred %>% filter(x == 0)
    Yamapred[j] <- GZ0value$predicted[1]
    j <- j + 1
  }else{
  }
}


YamanashiTrend2 <- data.frame(week = levels(factor(weekSIR3$week)),
                             nofcases = weekSIR3$newcaseday[weekSIR3$pref == "Yamanashi"],
                             counterfactual = c(rep(NA, 2), Yamapred[1:66]),
                             fitted = c(rep(NA,2), weekSIR3$pred_covid[weekSIR3$pref == "Yamanashi"][1:66]))



Yamanashi1 <- YamanashiTrend2[,1:2]
Yamanashi2 <- YamanashiTrend2[,c(1,3)]
Yamanashi3 <- YamanashiTrend2[,c(1,4)]
Yamanashi1$type <-"actual"
Yamanashi2$type <- "counterfactual"
Yamanashi3$type <- "fitted"
names(Yamanashi2)[2] <- "nofcases"
names(Yamanashi3)[2] <- "nofcases"

newYamanashiTrend2 <- rbind(Yamanashi1, Yamanashi2, Yamanashi3) %>% 
  arrange(week)
newYamanashiTrend2$week <- as.Date(newYamanashiTrend$week)

write.csv(newYamanashiTrend2, "03_build/Counterfactual/output/cofa_covid2.csv")




