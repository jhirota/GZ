library(tidyverse)
library(lfe)
library(stargazer)


# Infection prevention effects --------
# source("04_analyze/Publication/code/analyze.R")
# prefweek50felmhtml
# exported to output folder as well

# Economic effects --------
source("04_analyze/Postas_reg/code/analyze.R")
# postashtml <- stargazer(lmpostas4, lmpostas5, lmpostas6, lmpostas.cus4, lmpostas.cus5, lmpostas.cus6,
#                         title = "Restaurants' sales and customers (POS) and the Green Zone certification",
#                         digits = 3,
#                         digits.extra = 0,
#                         out = "05_report/tables_tex/output/sales_cus_pref_week.tex",
#                         add.lines=list(c("Prefecture FE", rep("X",6)), c("Week FE", rep("X",6))),
#                         omit.stat=c("f", "ser"))
dpostashtml <- stargazer(dlmpostas1, dlmpostas2, dlmpostas3, dlmpostas.cus1, dlmpostas.cus2, dlmpostas.cus3,
                         title = "Restaurants' sales and customers (POS) and the Green Zone certification",
                         digits = 3,
                         digits.extra = 0,
                         type = "html",
                         out = "05_report/tables_tex/output/postas_pref_day.tex",
                         add.lines=list(c("Prefecture FE", rep("X",6)), c("Day FE", rep("X",6))),
                         omit.stat=c("f", "ser"))


# 1 week lag ------------

# source("04_analyze/Publication/code/analyze.R")
# prefweek51felmhtml
# exported to output folder as well


# Resview -----------

source("04_analyze/Res_view/code/analyze.R")
resviewhtml <- stargazer(resview1, resview2, resview3,
                         dep.var.labels = "Restaurant View (percentage change from the same week of 2019) ",
                         title = "Restaurant View (percentage change) and the Green Zone certification",
                         digits = 3,
                         digits.extra = 0,
                         out = "05_report/tables_tex/output/resview_pref_week.tex",
                         add.lines=list(c("Restraurant View Yamanashi mean", "", round(resview_yama_mean, digits = 3)),
                                        c("Restraurant View Control mean", "",round(resview_nonyama_mean, digits = 3)),
                                        c("Prefecture FE", "X", "X", "X", "X"), c("Week FE", "X", "X","X", "X")),
                         omit.stat=c("f", "ser"))

# Google Mobility data -----------
source("04_analyze/Google_mobility/code/analyze.R")
reg_table_html <- stargazer(retrec1, retrec2, gropha1, gropha2, park1, park2, station1, station2, work1, work2, residence1, residence2,
                            title = "Mobility type (Google Mobility) the Green Zone certification",
                            dep.var.labels = c("retail and recreation", "grocery and pharmacy", "parks", "transit stations", "workplaces", "residential"),
                            digits =3,
                            digits.extra = 0, 
                            out = "05_report/tables_tex/output/Google_Mobility.tex",
                            add.lines=list(c("Prefecture FE", rep("X",12)),
                                           c("Date FE", rep("X",12))),
                            omit.stat=c("f", "ser"))


# Self-restraint rate ---------
source("04_analyze/Robustness_econ/code/analyze.R")
nighthtml <- stargazer(nightself1, nightself2, nightself3,
                       title = "The night-time self-restraint rate and the Green Zone certification",
                       digits = 3,
                       digits.extra = 0,
                       out = "05_report/tables_tex/output/night_selfrest.tex",
                       add.lines=list(c("Prefecture FE", rep("X",3)),
                                      c("Day FE", rep("X",3))),
                       omit.stat=c("f", "ser"))

malehtml <- stargazer(male15, male20, male30, male40, male50, male60, male70,
                      title = "The self-restraint rate by male age group and the Green Zone certification",
                      digits = 3,
                      digits.extra = 0,
                      out = "05_report/tables_tex/output/selfrest_by_age_male.tex",
                      add.lines=list(c("Prefecture FE", rep("X",7)),
                                     c("Day FE", rep("X",7))),
                      omit.stat=c("f", "ser"))

femalehtml <- stargazer(female15, female20, female30, female40, female50, female60, female70,
                        title = "The self-restraint rate by female age group and the Green Zone certification",
                        digits = 3,
                        digits.extra = 0,
                        out = "05_report/tables_tex/output/selfrest_by_age_female.tex",
                        add.lines=list(c("Prefecture FE", rep("X",7)),
                                       c("Day FE", rep("X",7))),
                        omit.stat=c("f", "ser"))

# V-RESAS -----------
source("04_analyze/Vresas/code/analyze.R")
mobplm2html <- stargazer(mob1.0, mob1.1, mob1.2, mob2.0, mob2.1, mob2.2, mob3.0, mob3.1, mob3.2,
                         dep.var.labels = c("incity","inpref","outpref"),
                         title = "Interregional Mobility and the Green Zone certification",
                         digits = 3,
                         digits.extra = 0,
                         out = "05_report/tables_tex/output/mobility_pref_week.tex",
                         add.lines=list(c("mobility Yamanashi mean(%)", "", round(incity_yama_mean, digits = 3),"","",round(inpref_yama_mean, digits = 3), "","",round(outpref_yama_mean, digits = 3), "" ),
                                        c("mobility Control mean(%)","", round(incity_nonyama_mean, digits = 3),"","",round(inpref_nonyama_mean, digits = 3),"","",round(outpref_nonyama_mean, digits = 3),""),
                                        c("Prefecture FE", rep("X",9)), c("Week FE", rep("X",9))),
                         omit.stat=c("f", "ser"))














