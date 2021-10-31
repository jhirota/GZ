setwd("/Users/kazuyahirokawa/OneDrive - The University of Tokyo/東大/1S/事例研究/政策評価のための因果推論/分析")
library(tidyverse)
library(stargazer)

data <- read.csv("GZ_COVID.csv", header = TRUE)
View(data)

reg_COVID <- lm(data = data, newcaseday ~ Treat + Post +Treat:Post + emergency)
stargazer(reg_COVID, type ="text")

reg_mob <- lm(data = data, retail_and_recreation ~ Treat + Post + Treat:Post + emergency)
stargazer(reg_mob, type = "text")

data1 <- data%>%
  filter(Code != 13)%>%
  filter(Code != 14)%>%

View(data1)  
