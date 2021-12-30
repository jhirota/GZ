
library(tidyverse)
library(lfe)


# 7.2.(iii) 山梨県と周辺５都道府県における県外からの人流変化率　時系列推移 --------
mob_vresas <- read_csv(here::here("03_build/weekSIR/output/weekSIR2.csv"))
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


# mobplm2html <- stargazer(mob1.0, mob1.1, mob1.2, mob2.0, mob2.1, mob2.2, mob3.0, mob3.1, mob3.2,
#                          dep.var.labels = c("incity","inpref","outpref"),
#                          title = "TABLE: Mobility and GreenZone certification",
#                          digits = 3,
#                          digits.extra = 0,
#                          type = "html",
#                          out = "04_analyze/Vresas/output/mobility_pref_week.html",
#                          add.lines=list(c("mobility Yamanashi mean(%)", "", round(incity_yama_mean, digits = 3),"","",round(inpref_yama_mean, digits = 3), "","",round(outpref_yama_mean, digits = 3), "" ),
#                                         c("mobility Control mean(%)","", round(incity_nonyama_mean, digits = 3),"","",round(inpref_nonyama_mean, digits = 3),"","",round(outpref_nonyama_mean, digits = 3),""),
#                                         c("Prefecture FE", rep("X",9)), c("Week FE", rep("X",9))),
#                          omit.stat=c("f", "ser"))



# 7.2.(i). 山梨県と周辺５都道府県のレストラン閲覧増加率　時系列推移 from plot.R-------
resplotdata <- read_csv("03_build/weekSIR/output/weekSIR2.csv") %>% 
  arrange(week)

resplotdata$treat <- ifelse(resplotdata$pref == "Yamanashi", "山梨県", "近隣5県")
resplotdata1 <- resplotdata %>% 
  group_by(treat, week) %>% 
  summarize(resview2 = mean(resview)) %>% 
  ungroup()
resplotdata1$week <- as.Date(resplotdata1$week)

res_week_caseplot <- ggplot(data = resplotdata1,
                            mapping = aes(x = week,
                                          y = resview2,
                                          color = treat))  +
  xlab("") +
  ylab("レストラン閲覧増加率(2019年同週比)") +
  geom_point(size = 0.1) +
  geom_line(size = 0.5)  +
  ggtitle("山梨県と近隣5県の週別レストラン閲覧増加率の時系列推移") +
  theme_gray(base_family = "MS Mincho") +
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  annotate("text",
           x = as.Date("2020-07-17"),
           y = Inf,
           label = "GZ認証制度開始",
           color = "forestgreen",
           size = 4,
           fontface="italic",
           vjust=5,
           family = "MS Mincho")+
  scale_color_manual(name = " ", values = c("#339900","#ff9900")) +
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "cm"))


res_week_caseplot

ggsave("04_analyze/Vresas/output/resview_plot.png", res_week_caseplot, width = 10, height = 8, dpi = 300)


# 7.2.(ii) 山梨県と周辺５都道府県の小売・娯楽施設における人流変化率　時系列推移 from pref_bet_day_analyzing.R----------


Gmobdata <- read.csv("03_build/Pref_covid/output/pref_bet_day_COVID_GZ.csv", header =TRUE) 

Gmobdata$treat <- ifelse(Gmobdata$pref == "Yamanashi", "山梨県", "近隣5県")
Gmobdata$date <- as.Date(Gmobdata$date) 

data_google <- Gmobdata %>% 
  group_by(treat,date) %>% 
  summarize(retail_and_recreation2 = mean(retail_and_recreation)) %>% 
  ungroup()

gmob_caseplot <- ggplot(data = data_google,
                        mapping = aes(x = date,
                                      y = retail_and_recreation2,
                                      color = treat)) +
  xlab("") +
  ylab("Google Mobility人流増加率（2020年基準値比）") +
  geom_point(size = 0.1) +
  geom_line(size = 0.5)  +
  ggtitle("山梨県と近隣5県の日別人流増加率の時系列推移") +
  theme_gray (base_family = "MS Mincho") +
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  annotate("text",
           x = as.Date("2020-07-17"),
           y = Inf,
           label = "GZ認証制度開始",
           color = "forestgreen",
           size = 4,
           fontface="italic",
           vjust=5,
           family = "MS Mincho")+
  scale_color_manual(name = " ", values = c("#339900", "#ff9900")) +
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "cm"))

gmob_caseplot

ggsave("04_analyze/Vresas/output/Google_mobility_plot.png", gmob_caseplot, width = 10, height = 8, dpi = 300)

# 7.2.(iii) 山梨県と周辺５都道府県における県外からの人流変化率　時系列推移 Vresus from plot.R---------
vresasdata <- read_csv("03_build/weekSIR/output/weekSIR2.csv") %>%
  mutate(treat = ifelse(pref == "Yamanashi", "山梨県", "近隣5県")) %>% 
  group_by(week, treat) %>% 
  summarize_at(c("in_pref", "out_pref", "in_city"), mean, na.rm=TRUE) %>% 
  ungroup()
vresasdata$week <- as.Date(vresasdata$week)

#(out_pref)
vrmob_outpref_caseplot <- ggplot(data = vresasdata,
                                 mapping = aes(x = week,
                                               y = out_pref,
                                               colour = treat))  +
  xlab("") +
  ylab("他都道府県からの流入増加率(2019年同週比)") +
  geom_point(size = 0.1) +
  geom_line(size = 0.5)  +
  ggtitle("山梨県と近隣5県の他都道府県からの週別流入量の時系列推移") +
  theme_gray (base_family = "MS Mincho") +
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  annotate("text",
           x = as.Date("2020-07-17"),
           y = Inf,
           label = "GZ認証制度開始",
           color = "forestgreen",
           size = 4,
           fontface="italic",
           vjust=5,
           family = "MS Mincho")+
  scale_color_manual(name = " ", values = c("#339900", "#ff9900")) +
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "cm"))

vrmob_outpref_caseplot

ggsave("04_analyze/Vresas/output/outpref_mob_plot.png", vrmob_outpref_caseplot, width = 10, height = 8, dpi = 300)

#(in_pref)

vrmob_inpref_caseplot <- ggplot(data = vresasdata,
                                mapping = aes(x = week,
                                              y = in_pref,
                                              color = treat))  +
  xlab("") +
  ylab("県内住民人流増加率(2019年同週比)") +
  geom_point(size = 0.1) +
  geom_line(size = 0.5)  +
  ggtitle("山梨県と近隣5県の県内住民人流の週別増加率の時系列推移") +
  theme_gray (base_family = "MS Mincho") +
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  annotate("text",
           x = as.Date("2020-07-17"),
           y = Inf,
           label = "GZ認証制度開始",
           color = "forestgreen",
           size = 4,
           fontface="italic",
           vjust=5,
           family = "MS Mincho")+
  scale_color_manual(name = " ", values = c("#339900", "#ff9900")) +
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "cm"))

vrmob_inpref_caseplot

ggsave("04_analyze/Vresas/output/inpref_mob_plot.png", vrmob_inpref_caseplot, width = 10, height = 8, dpi = 300)


#(in_city)
vrmob_incity_caseplot <- ggplot(data = vresasdata,
                                mapping = aes(x = week,
                                              y = in_city,
                                              color = treat))  +
  xlab("") +
  ylab("市内住民人流増加率(2019年同週比)") +
  geom_point(size = 0.1) +
  geom_line(size = 0.5)  +
  ggtitle("山梨県と近隣5県の市内住民人流の週別増加率の時系列推移") +
  theme_gray (base_family = "MS Mincho") +
  geom_vline(xintercept = as.Date("2020-07-17"), linetype = "dotdash", color = "forestgreen")+
  annotate("text",
           x = as.Date("2020-07-17"),
           y = Inf,
           label = "GZ認証制度開始",
           color = "forestgreen",
           size = 4,
           fontface="italic",
           vjust=5,
           family = "MS Mincho")+
  scale_color_manual(name = " ", values = c("#339900", "#ff9900")) +
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16), 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "cm"))

vrmob_incity_caseplot

ggsave("04_analyze/Vresas/output/incity_mob_plot.png", vrmob_incity_caseplot, width = 10, height = 8, dpi = 300)


