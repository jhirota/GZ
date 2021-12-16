library(tidyverse)
library(ggeffects)

#data load -------
weekSIR2 <- read_csv("03_build/weekSIR/output/weekSIR2.csv")
newYamanashiTrend <- read_csv("/Users/jumpeihirota/Desktop/Cloned因果推論/ClonedGZpj/03_build/Counterfactual/output/cofa_covid.csv")


# covid counterfactual graph with no GZ certification (2 weeks lag)------------
# 
# c_res_lag2 <- lm(log(newcaset.2 + 1) ~ log(cumGZ + 1)  + log(susceptable) + log(agrgt_potecovid_lag2 + 1) +
#                    log(avg_temp_q) + log(rain + 1) + resview + emergency + log(noftestst.2 + 1) + 
#                    factor(pref) + factor(week), data = weekSIR2)
# #Plot
# CoFa <- ggplot(data = newYamanashiTrend,
#                mapping = aes(x = week, y = nofcases, color = type)) +
#   geom_point(size = 0.5) +
#   geom_line() + 
#   geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
#   annotate("text",
#            x = as.Date("2020-07-17"),
#            y = 250,
#            label = "GZ認証制度開始",
#            family = "MS Mincho",
#            color = "forestgreen")+
#   labs(title = "GZ認証がなかった場合の感染者数の推移との比較（週別-2週間ラグ）",
#        x = "",
#        y = "新規感染者数の週毎の合計") +
#   scale_color_discrete(name = " ",
#                        labels = c(actual = "実際の感染者数",
#                                   counterfactual = "GZ認証がない場合の\nモデルの推定値",
#                                   fitted = "モデルの推定値"))+
#   theme_gray(base_family = "MS Mincho")+
#   theme(title = element_text(size = 16),
#         axis.text.x = element_text(size = 12),
#         axis.title.x = element_text(size = 16),
#         axis.text.y = element_text(size = 12),
#         axis.title.y = element_text(size = 16),
#         legend.text = element_text(size = 14),
#         legend.key.size = unit(2, "cm"))
# 
# CoFa 
# 
# ggsave("04_analyze/Counterfactual/output/counterfactual_covid.png", CoFa, width = 10, height = 8, dpi = 300)


# Postas counterfactual (daily)---------

#data load
YamanashiTrend <- read.csv("03_build/Counterfactual/output/sales_daily_data_for_plot.csv")
YamanashiTrend.cus <- read.csv("03_build/Counterfactual/output/customers_daily_data_for_plot.csv")
YamanashiTrend$date <- as.Date(YamanashiTrend$date)
YamanashiTrend.cus$date <- as.Date(YamanashiTrend.cus$date)

# Plot
CoFaSales <- ggplot(data = YamanashiTrend %>% filter(date >= 18262),
                    mapping = aes(x = date, y = Sales, color = type)) +
  geom_point(size = 0.5) +
  geom_line() + 
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  annotate("text",
           x = as.Date("2020-07-17"),
           y = 250,
           label = "GZ認証制度開始",
           family = "HiraKakuProN-W3",
           color = "forestgreen")+
  labs(title = "GZ認証がなかった場合の店舗売上の推移との比較",
       x = "日",
       y = "日毎の売上合計") +
  scale_color_discrete(name = " ",
                       labels = c(actual = "実際の売上",
                                  counterfactual = "GZ認証がない場合の\n売上の推定値",
                                  fitted = "モデルの推定値"))+
  theme_gray(base_family = "HiraKakuPro-W3")+
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "cm"))

CoFaSales 


ggsave("04_analyze/Counterfactual/output/counterfactual_sales.png", CoFaSales, width = 10, height = 8, dpi = 300)



CoFacustomers <- ggplot(data = YamanashiTrend.cus,
                        mapping = aes(x = date, y = customers, color = type)) +
  geom_point(size = 0.5) +
  geom_line() +
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  annotate("text",
           x = as.Date("2020-07-17"),
           y = 150,
           label = "GZ認証制度開始",
           family = "HiraKakuProN-W3",
           color = "forestgreen")+
  labs(title = "GZ認証がなかった場合の店舗来客数の推移との比較",
       x = "日",
       y = "日毎の客数合計の推移") +
  scale_color_discrete(name = " ",
                       labels = c(actual = "実際の来客数",
                                  counterfactual = "GZ認証がない場合の\n来客数の推定値",
                                  fitted = "モデルの推定値"))+
  theme_gray(base_family = "HiraKakuPro-W3")+
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "cm"))


CoFacustomers

ggsave("04_analyze/Counterfactual/output/counterfactual_customers.png", CoFacustomers, width = 10, height = 8, dpi = 300)



# Postas counterfactual (weekly)---------
#data load
YamanashiTrend_w <- read_csv("03_build/Counterfactual/output/sales_weekly_data_for_plot.csv") 
YamanashiTrend.cus_w <- read_csv("03_build/Counterfactual/output/customers_weekly_data_for_plot.csv")

#plot
CoFaSales_w <- ggplot(data = YamanashiTrend_w %>% filter(week >= 18262 & week <= 18748),
                      mapping = aes(x = week, y = Sales/1000, color = type)) +
  geom_point(size = 0.5) +
  geom_line() + 
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  annotate("text",
           x = as.Date("2020-07-17"),
           y = 250,
           label = "GZ認証制度開始",
           family = "MS Mincho",
           color = "forestgreen")+
  labs(title = "GZ認証がなかった場合の店舗売上の推移との比較",
       x = "",
       y = "週毎の売上合計[単位：千円]") +
  scale_color_discrete(name = " ",
                       labels = c(actual = "実際の売上",
                                  counterfactual = "GZ認証がない場合の\n売上の推定値",
                                  fitted = "モデルの推定値"))+
  theme_gray(base_family = "MS Mincho")+
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "cm"))

CoFaSales_w  

ggsave("04_analyze/Counterfactual/output/counterfactual_sales_week.png", CoFaSales_w, width = 10, height = 8, dpi = 300)


CoFacustomers_w <- ggplot(data = YamanashiTrend.cus_w %>% filter(week >= 18262 & week <= 18748),
                          mapping = aes(x = week, y = customers, color = type)) +
  geom_point(size = 0.5) +
  geom_line() +
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  annotate("text",
           x = as.Date("2020-07-17"),
           y = 150,
           label = "GZ認証制度開始",
           family = "MS Mincho",
           color = "forestgreen")+
  labs(title = "GZ認証がなかった場合の店舗来客数の推移との比較",
       x = "",
       y = "週毎の来客数合計") +
  scale_color_discrete(name = " ",
                       labels = c(actual = "実際の来客数",
                                  counterfactual = "GZ認証がない場合の\n来客数の推定値",
                                  fitted = "モデルの推定値"))+
  theme_gray(base_family = "MS Mincho")+
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "cm"))

CoFacustomers_w

ggsave("04_analyze/Counterfactual/output/counterfactual_customers_week.png", CoFacustomers_w, width = 10, height = 8, dpi = 300)



# [UPDATE] covid counterfactual graph with no GZ certification (2 weeks lag)------------
newYamanashiTrend2 <- read_csv("03_build/Counterfactual/output/cofa_covid2.csv")[-1]

#Plot
CoFa2 <- ggplot(data = newYamanashiTrend2,
               mapping = aes(x = week, y = nofcases, color = type)) +
  geom_point(size = 0.5) +
  geom_line() + 
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  annotate("text",
           x = as.Date("2020-07-17"),
           y = 250,
           label = "GZ認証制度開始",
           family = "HiraKakuProN-W3",
           color = "forestgreen")+
  labs(title = "GZ認証がなかった場合の感染者数の推移との比較（週別-2週間ラグ）",
       x = "週",
       y = "新規感染者数の週毎の合計") +
  scale_color_discrete(name = " ",
                       labels = c(actual = "実際の感染者数",
                                  counterfactual = "GZ認証がない場合の\nモデルの推定値",
                                  fitted = "モデルの推定値"))+
  theme_gray(base_family = "HiraKakuPro-W3")+
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "cm"))

CoFa2

# covidsinceGZ <- newYamanashiTrend2 %>% filter(week >= 18475) #8月以降4月まで
# c <- covidsinceGZ$nofcases[covidsinceGZ$type == "actual"] -
#   covidsinceGZ$nofcases[covidsinceGZ$type == "counterfactual"]
# sum(c, na.rm = TRUE) ##単純計算で調査期間の9ヶ月で1122人の感染を防いだ
# 
# rate <- sum(covidsinceGZ$nofcases[covidsinceGZ$type == "actual"]) /
#   sum(covidsinceGZ$nofcases[covidsinceGZ$type == "counterfactual"])
# rate #調査期間の9ヶ月で49.7%減少させた。



ggsave("04_analyze/Counterfactual/output/counterfactual_covid2.png", CoFa2, width = 10, height = 8, dpi = 300)




