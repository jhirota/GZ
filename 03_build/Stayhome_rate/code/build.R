library(tidyverse)

# data load and merge-------

link_genderage <- "02_bring/Stayhome_rate/data/self-restraint/gender_age_self-restraint_rate_202001_202103"
link_night <- "02_bring/Stayhome_rate/data/self-restraint/night_self-restraint_rate_202001_202103"
lf_night <- list.files(path = here::here(link_night), full.names = T)
lf_genage <- list.files(path = here::here(link_genderage), full.names = T)

data_night <- lapply(lf_night, read_csv, locale = locale(encoding = "shift-jis"))
data_genage <- lapply(lf_genage, read.csv)

credit_delete <- function(data){
  if (any(colnames(data) %in% "state")){
    data <- data %>% 
      filter(is.na(state) == FALSE)
  }else{
    data <- data %>%
      filter(is.na(M70) == FALSE)
  }
    
  return(data)
}

Night <- tibble()
Genage <- tibble()

for (i in 1:length(data_night)){
  cleaned <- credit_delete(data_night[[i]])
  Night <- rbind(Night, cleaned)
}

for (i in 1:length(data_genage)){
  cleaned <- credit_delete(data_genage[[i]])
  Genage <- rbind(Genage, cleaned)
}

# data clean and merge------
Night <- Night %>% 
  rename(pref = 2,
         NSHR = 3) %>% 
  mutate(date = as.Date(date)) %>% 
  complete(date, pref)

Genage <- Genage %>% 
  mutate(date = strptime(as.character(date), "%m/%d/%y") %>% 
           as.Date()) %>% 
  rename(pref = 2)

SHR <- left_join(x = Night,
                 y = Genage,
                 by = c("pref", "date"))




# write_csv(SHR, here::here("03_build/Stayhome_rate/output/Stayhome_rate.csv"))








