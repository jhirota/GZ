
library(tidyverse)


# data load and merge-------

link_genderage <- "02_bring/Selfrestraint/data/self-restraint/gender_age_self-restraint_rate_202001_202103"
link_night <- "02_bring/Selfrestraint/data/self-restraint/night_self-restraint_rate_202001_202103"
lf_night <- list.files(path = link_night, full.names = T)
lf_genage <- list.files(path = link_genderage, full.names = T)

data_night <- lapply(lf_night, read_csv, locale = locale(encoding = "shift-jis"))
data_genage <- lapply(lf_genage, read.csv)

Night <- rbind(head(data_night[[1]], -6), head(data_night[[2]], -6), head(data_night[[3]], -6), head(data_night[[4]], -6),
               head(data_night[[5]], -6),head(data_night[[6]], -6),head(data_night[[7]], -6),head(data_night[[8]], -6),
               head(data_night[[9]], -6),head(data_night[[10]], -6),head(data_night[[11]], -6),head(data_night[[12]], -6),
               head(data_night[[13]], -6),head(data_night[[14]], -6),head(data_night[[15]], -6))
Genage <- rbind(head(data_genage[[1]], -6), head(data_genage[[2]], -6), head(data_genage[[3]], -6), head(data_genage[[4]], -6),
                head(data_genage[[5]], -6),head(data_genage[[6]], -6),head(data_genage[[7]], -6),head(data_genage[[8]], -6),
                head(data_genage[[9]], -6),head(data_genage[[10]], -6),head(data_genage[[11]], -6),head(data_genage[[12]], -6),
                head(data_genage[[13]], -6),head(data_genage[[14]], -6),head(data_genage[[15]], -6))


# data clean and merge------

colnames(Night)[2:3] <- c("prefecture", "Night-time self-restraint rate")
Night$date <- as.Date(Night$date)

Genage$date <- strptime(as.character(Genage$date), "%m/%d/%y") %>% 
  as.Date()

Selfrest <- left_join(x = Night, y = Genage, by = c("prefecture", "date"))


gg <- ggplot(data = Selfrest,
             aes(x = date,
                 y = ))




write.csv(Selfrest, "03_build/Selfrestraint/output/selfrestraint.csv", row.names=FALSE)








