library(tidyverse)
library(lfe)
library(stargazer)

weekSIR <- read.csv("03_build/Pref_covid/output/weekSIR.csv", header =TRUE)

# PCR merge --------

PCRmerge1 <- read.csv("03_build/Controls/output/PCRtests.csv") 

PCRmerge1$Pref <- gsub("群馬県", "Gunma", PCRmerge1$Pref)
PCRmerge1$Pref <- gsub("山梨県", "Yamanashi", PCRmerge1$Pref)
PCRmerge1$Pref <- gsub("栃木県", "Tochigi", PCRmerge1$Pref)
PCRmerge1$Pref <- gsub("茨城県", "Ibaragi", PCRmerge1$Pref)
PCRmerge1$Pref <- gsub("長野県", "Nagano", PCRmerge1$Pref)
PCRmerge1$Pref <- gsub("静岡県", "Shizuoka", PCRmerge1$Pref)

names(PCRmerge1)[2:3] <- c("pref", "week")
weekSIR$week <- as.Date(weekSIR$week)
PCRmerge1$week <- as.Date(PCRmerge1$week)

weekSIR <- left_join(x = weekSIR,
                     y = PCRmerge1,
                     by = c("week", "pref"))

weekSIR$noftests[is.na(weekSIR$noftests)] <- 0
weekSIR$noftestst.1 <- sapply(1:nrow(weekSIR), function(x) weekSIR$noftests[x+6])
weekSIR$noftestst.2 <- sapply(1:nrow(weekSIR), function(x) weekSIR$noftests[x+12])


weekSIR$emergency <- gsub(2, 1, weekSIR$emergency)
weekSIR$emergency <- gsub(5, 1, weekSIR$emergency)
weekSIR$emergency <- gsub(4, 1, weekSIR$emergency)
weekSIR$emergency <- gsub(7, 1, weekSIR$emergency)
weekSIR$emergency <- as.numeric(weekSIR$emergency)

# write.csv(weekSIR, "03_build/weekSIR/output/weekSIR2.csv", row.names = FALSE)


