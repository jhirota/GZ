library(tidyverse)

# data load -------

weekSIR_rob <- read_csv(here::here("03_build/Robustness_check/output/weekSIR_robustness.csv"))


# data making ------- 

summary_stat <- weekSIR_rob %>% 
  mutate(newcaseday_per = (newcaseday / population) *100000) %>%
  group_by(pref) %>% 
  summarise(Total_cases_per100000 = sum(newcaseday_per),
            Mean_cases_per100000 = mean(newcaseday_per),
            SD_cases_per100000 = sd(newcaseday_per),
            Total_customers_per_Restaurant = sum(customers_per),
            Mean_customers_per_Restaurant = mean(customers_per),
            SD_customers_per_Restaurant = sd(customers_per),
            Total_sales_per_Restaurant = sum(sales_per),
            Mean_sales_per_Restaurant = mean(sales_per),
            SD_sales_per_Restaurant = sd(sales_per)) %>% 
  ungroup() %>% 
  rename(Prefecture = pref)
  
  









