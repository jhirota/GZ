
library(tidyverse)
library(lfe)
library(stargazer)

# data load -----
weekSIR_rob <- read_csv(here::here("03_build/Robust_check/output/weekSIR_robustness.csv"))
vresasdata <- read_csv(here::here("03_build/GZ_covid/output/weekly_vresas_plot.csv"))
data_google <- read_csv(here::here("03_build/GZ_covid/output/Gmob_plot.csv")) 

# Vresas time series analysis --------------
incity_yama_mean <- mean(weekSIR_rob$in_city[weekSIR_rob$pref == "Yamanashi"])
incity_nonyama_mean <- mean(weekSIR_rob$in_city[weekSIR_rob$pref != "Yamanashi"])
inpref_yama_mean <- mean(weekSIR_rob$in_pref[weekSIR_rob$pref == "Yamanashi"])
inpref_nonyama_mean <- mean(weekSIR_rob$in_pref[weekSIR_rob$pref != "Yamanashi"])
outpref_yama_mean <- mean(weekSIR_rob$out_pref[weekSIR_rob$pref == "Yamanashi"])
outpref_nonyama_mean <- mean(weekSIR_rob$out_pref[weekSIR_rob$pref != "Yamanashi"])

mob1.0 <- felm(in_city ~ log(cumGZ + 1) + emergency| pref + week | 0 | pref, data = weekSIR_rob)
mob1.1 <- felm(in_city ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref + week | 0 | pref, data = weekSIR_rob)
mob1.2 <- felm(in_city ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref + week | 0 | pref, data = weekSIR_rob)
mob2.0 <- felm(in_pref ~ log(cumGZ + 1) + emergency| pref + week | 0 | pref, data = weekSIR_rob)
mob2.1 <- felm(in_pref ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref + week | 0 | pref, data = weekSIR_rob)
mob2.2 <- felm(in_pref ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref + week | 0 | pref, data = weekSIR_rob)
mob3.0 <- felm(out_pref ~ log(cumGZ + 1) + emergency| pref + week | 0 | pref, data = weekSIR_rob)
mob3.1 <- felm(out_pref ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref + week | 0 | pref, data = weekSIR_rob)
mob3.2 <- felm(out_pref ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref + week | 0 | pref, data = weekSIR_rob)

vresas_reg <- stargazer(mob1.0, mob1.1, mob1.2, mob2.0, mob2.1, mob2.2, mob3.0, mob3.1, mob3.2,
                    dep.var.labels = c("incity","inpref","outpref"),
                    title = "Interregional Mobility and the Green Zone certification",
                    digits = 3,
                    digits.extra = 0,
                    type = "latex",
                    add.lines=list(c("Mean of Mobility in Yamanashi", "",
                                     round(incity_yama_mean, digits = 3),"","",
                                     round(inpref_yama_mean, digits = 3), "","",
                                     round(outpref_yama_mean, digits = 3), ""),
                                   c("Mean of Mobility in Control","", 
                                     round(incity_nonyama_mean, digits = 3),"","",
                                     round(inpref_nonyama_mean, digits = 3),"","",
                                     round(outpref_nonyama_mean, digits = 3),""),
                                   c("Prefecture FE", rep("X",9)),
                                   c("Week FE", rep("X",9))),
                    omit.stat=c("f", "ser"),
                    header = FALSE,
                    float.env = "sidewaystable",
                    out = "04_analyze/Vresas/output/vresas_mob_reg.tex")

# Restaurant's view analysis------------

resview1 <- felm(resview ~ log(cumGZ + 1) + emergency| pref + week | 0 | pref, data = weekSIR_rob)
resview2 <- felm(resview ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref + week | 0 | pref, data = weekSIR_rob)
resview3 <- felm(resview ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref + week | 0 | pref, data = weekSIR_rob)

resview_yama_mean <- mean(weekSIR_rob$resview[weekSIR_rob$pref == "Yamanashi"])
resview_nonyama_mean <- mean(weekSIR_rob$resview[weekSIR_rob$pref != "Yamanashi"])

resview_reg <- stargazer(resview1, resview2, resview3,
                        dep.var.labels = "Restaurant View (percentage change) ",
                        title = "Restaurant View (percentage change) and the Green Zone certification",
                        digits = 3,
                        digits.extra = 0,
                        type = "latex",
                        add.lines=list(c("Restraurant View Yamanashi mean", "",
                                         round(resview_yama_mean, digits = 3)),
                                       c("Restraurant View Control mean", "",
                                         round(resview_nonyama_mean, digits = 3)),
                                       c("Prefecture FE", "X", "X", "X"),
                                       c("Week FE", "X", "X","X")),
                        omit.stat = c("f", "ser"),
                        header = FALSE,
                        column.sep.width = "-15pt",
                        out = "04_analyze/Vresas/output/resview_reg.tex")



# Restraunts' view time series -------

res_week_caseplot <- ggplot(data = vresasdata,
                            mapping = aes(x = week,
                                          y = resview,
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

# ggsave("04_analyze/Vresas/output/resview_plot.png", res_week_caseplot, width = 10, height = 8, dpi = 300)


# Google mobility time series ----------

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

# ggsave("04_analyze/Vresas/output/Google_mobility_plot.png", gmob_caseplot, width = 10, height = 8, dpi = 300)

# V-RESAS time series ---------

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

# ggsave("04_analyze/Vresas/output/outpref_mob_plot.png", vrmob_outpref_caseplot, width = 10, height = 8, dpi = 300)

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

# ggsave("04_analyze/Vresas/output/inpref_mob_plot.png", vrmob_inpref_caseplot, width = 10, height = 8, dpi = 300)


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

# ggsave("04_analyze/Vresas/output/incity_mob_plot.png", vrmob_incity_caseplot, width = 10, height = 8, dpi = 300)


