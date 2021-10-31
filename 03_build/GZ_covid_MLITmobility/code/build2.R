library(tidyverse)
library(lubridate)


# monthly data making-----

mondata <- DATAfinal[,-13]

mondata$date <- floor_date(mondata$date, "month") #date into month

mondata1 <- mondata %>% 
  group_by(date, city) %>% 
  summarize_at(vars(1, 2), funs(sum(., na.rm=TRUE))) 

mondata2 <- mondata %>% 
  group_by(date, city) %>% 
  summarize_at(vars(8, 9), funs(mean(., na.rm=TRUE)))

cleanmondata <- left_join(x = mondata1, y = mondata2, by = c("city", "date"))

cleanmondata$cumGZ <- ave(cleanmondata$GZapproved, cleanmondata$city, FUN = cumsum)
cleanmondata$lnGZ <- log(cleanmondata$cumGZ + 1)


join6 <- cleanmeshdata %>% 
  select(city, date, mobility)

cleanmondata <- cleanmondata %>% 
  left_join(y = join6, by = c("city", "date"))

cleanmondata$datedum <- factor(cleanmondata$date)
cleanmondata$nofcases_lm[28:nrow(cleanmondata)] <- cleanmondata$nofcases[1:(nrow(cleanmondata)-27)]
cleanmondata$nofcases_lm[1:27] <- 0

cleanmondata$lmobility <- log(cleanmondata$mobility)

write.csv(cleanmondata, "03_build/GZ_covid_MLITmobility/output/mondata.csv", row.names=FALSE)
