library(wooldridge)
library(tidyverse)
library(stargazer)
library(modelsummary)

GZ <- read.csv("GZ_COVID2.csv", header = TRUE)

# Handling
# columns <- c("date", "prefcode", "pref", "infect_perday", "infect_total", "death_perday", "death_total", "PCR", "Kyugyo", "SoE")
# names(GZ)[0:10] <- columns
# for (i in nrow(GZ)){
#   if(GZ$date[i] <= 18438){
#     GZ$date[i] == 0
#   }else{
#     GZ$date[i] == 1
#   }
# }

names(GZ)[1] <- "date"
GZ <- GZ[-3769:-3774,-20:-25]
GZ[,1] <- as.Date(GZ[,1])
GZ <- GZ %>% 
  filter(date >= 18307 & date <= 18627)

GZ["Others"] <- for (i in nrow(GZ)) {
  GZ$Others[i] <- GZ
}
  
# Base
a <- lm(cases_day ~ date + emer_dec + Treat*Post + deaths_day + kyugyou + Rt + retail_and_recreation
        + grocery_and_pharmacy + parks + transit_stations + workplaces + residential, data = GZ)
summary(a)

#Analysis
lm <- lm(cases_day ~ Treat*Post + date + deaths_day + Rt + retail_and_recreation
         + grocery_and_pharmacy + transit_stations + workplaces + residential, data = GZ)
summary(lm)

#Analysis without Tokyo Kanagawa Saitama
GZ_rural <- GZ %>% 
  filter(code != 11) %>% 
  filter(code != 13) %>% 
  filter(code != 14) %>% 
  filter(code != 12)
lm2 <- lm(cases_day ~ Treat*Post + date + deaths_day + Rt + retail_and_recreation
         + grocery_and_pharmacy + transit_stations + workplaces + residential, data = GZ_rural)
summary(lm2)

stargazer(lm2, type = "text")

#Analysis3 @yamanashi
GZ_ymns <- GZ %>% 
  filter(code == 19)

lm3 <- lm(cases_day ~ date +  Post + deaths_day + Rt + retail_and_recreation
          + grocery_and_pharmacy + transit_stations + workplaces + residential, data = GZ_ymns)
summary(lm3)

#Analysis4
lm4 <- lm(infect_perday ~ Treat*Post + SoE, data = GZ)
stargazer(lm4, type = "text")

#graph
plot <- ggplot(data = GZ_rural,
               mapping = aes(x = date,
                             y = cases_day,
                             color = prefecture)) +
  geom_line(size = 0.4) +
  geom_abline(date = "2020-06-26")
plot



# GZ list Data
GZlist <- read.csv("../From Yamanashiken/GZlist.csv") 
df <- data.frame(table(GZlist[2]))
df[,1] <- as.Date(df[,1])
names(df)[1:2] <- c("date", "nshops")


GZshops <- ggplot(data = df,
                  mapping = aes(x = date,
                                y = nshops))+
  geom_col()
GZshops








