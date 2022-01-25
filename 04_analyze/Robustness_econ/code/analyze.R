library(tidyverse)
library(stargazer)
library(lfe)

# reg night selfrest  ------

#data load 
postas_rob <- read_csv(here::here("03_build/Robust_check/output/postas_rob.csv"))

# reg Night_selfrest
NSHR1 <- felm(NSHR ~ log(cumGZ + 1) + emergency| pref+date | 0 | pref, data = postas_rob)
NSHR2 <- felm(NSHR ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1)| pref+date | 0 | pref, data = postas_rob)
NSHR3 <- felm(NSHR ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)

table6.1 <- stargazer(NSHR1, NSHR2, NSHR3,
                      title = "The night-time stay-home rate and the Green Zone certification",
                      digits = 3,
                      digits.extra = 0,
                      type = "latex",
                      add.lines=list(c("Prefecture FE", rep("X",3)),
                                     c("Day FE", rep("X",3))),
                      omit.stat=c("f", "ser"),
                      header = FALSE,
                      out = "04_analyze/Robustness_econ/output/night_stayhome.tex")


# reg selfrest by age (male)---------
male15 <- felm(M15 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
male20 <- felm(M20 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
male30 <- felm(M30 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
male40 <- felm(M40 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
male50 <- felm(M50 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
male60 <- felm(M60 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
male70 <- felm(M70 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)

table6.2 <- stargazer(male15, male20, male30, male40, male50, male60, male70,
                      title = "The self-restraint rate by male age group and the Green Zone certification",
                      digits = 3,
                      digits.extra = 0,
                      type = "latex",
                      add.lines=list(c("Prefecture FE", rep("X",7)),
                                     c("Day FE", rep("X",7))),
                      omit.stat=c("f", "ser"),
                      header = FALSE,
                      column.sep.width = "-15pt",
                      out = "04_analyze/Robustness_econ/output/SHR_by_age_male.tex")



# reg selfrest by age (female)---------
female15 <- felm(F15 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
female20 <- felm(F20 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
female30 <- felm(F30 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
female40 <- felm(F40 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
female50 <- felm(F50 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
female60 <- felm(F60 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)
female70 <- felm(F70 ~ log(cumGZ + 1) + emergency + log(newcase_day + 1) + log(avg_temp) + log(avg_rain + 1) + dummy_school_closure + dummy_gathering_restriction| pref+date | 0 | pref, data = postas_rob)

table6.3 <- stargazer(female15, female20, female30, female40, female50, female60, female70,
                      title = "The self-restraint rate by female age group and the Green Zone certification",
                      digits = 3,
                      digits.extra = 0,
                      type = "latex",
                      add.lines=list(c("Prefecture FE", rep("X",7)),
                                     c("Day FE", rep("X",7))),
                      omit.stat=c("f", "ser"),
                      header = FALSE,
                      column.sep.width = "-15pt",
                      out = "04_analyze/Robustness_econ/output/SHR_by_age_female.tex")
