library(tidyverse)

#data load ---------
Yamanashi_res <- read_csv("02_bring/Vresas/data/山梨県の飲食店情報の閲覧数 – ジャンルごとの2019年同週比の推移.csv")
Yamanashi_mob <- read_csv("02_bring/Vresas/data/山梨県の滞在人口の動向 – 推定居住地ごとの2019年同週比の推移.csv")
Ibaraki_res <- read_csv("02_bring/Vresas/data/茨城県の飲食店情報の閲覧数 – ジャンルごとの2019年同週比の推移.csv")
Ibaraki_mob <- read_csv("02_bring/Vresas/data/茨城県の滞在人口の動向 – 推定居住地ごとの2019年同週比の推移.csv")
Tochigi_res <- read_csv("02_bring/Vresas/data/栃木県の飲食店情報の閲覧数 – ジャンルごとの2019年同週比の推移.csv")
Tochigi_mob <- read_csv("02_bring/Vresas/data/栃木県の滞在人口の動向 – 推定居住地ごとの2019年同週比の推移.csv")
Gunma_res <- read_csv("02_bring/Vresas/data/群馬県の飲食店情報の閲覧数 – ジャンルごとの2019年同週比の推移.csv")
Gunma_mob <- read_csv("02_bring/Vresas/data/群馬県の滞在人口の動向 – 推定居住地ごとの2019年同週比の推移.csv")
Nagano_res <- read_csv("02_bring/Vresas/data/長野県の飲食店情報の閲覧数 – ジャンルごとの2019年同週比の推移.csv")
Nagano_mob <- read_csv("02_bring/Vresas/data/長野県の滞在人口の動向 – 推定居住地ごとの2019年同週比の推移.csv")
Shizuoka_res <- read_csv("02_bring/Vresas/data/静岡県の飲食店情報の閲覧数 – ジャンルごとの2019年同週比の推移.csv")
Shizuoka_mob <- read_csv("02_bring/Vresas/data/静岡県の滞在人口の動向 – 推定居住地ごとの2019年同週比の推移.csv")

#data clean mobility ------
clean_mob <- function(data){
  data <- data %>% 
    select(-2) %>% 
    rename(pref = 1,
           week_JP = 2,
           kind = 3,
           ratio = 4) %>% 
    pivot_wider(names_from = "kind",
                values_from = "ratio") %>% 
    rename(in_city = 3,
           in_pref = 4,
           out_pref = 5)
  return(data)
}

VRmob <- rbind(clean_mob(Yamanashi_mob),
               clean_mob(Gunma_mob), 
               clean_mob(Ibaraki_mob), 
               clean_mob(Tochigi_mob), 
               clean_mob(Nagano_mob), 
               clean_mob(Shizuoka_mob)) %>% 
  mutate(pref = str_replace_all(pref,
                                c(山梨県全体 = "Yamanashi",
                                  茨城県全体 = "Ibaraki",
                                  栃木県全体 = "Tochigi",
                                  群馬県全体 = "Gunma",
                                  長野県全体 = "Nagano",
                                  静岡県全体 = "Shizuoka"))) %>% 
  mutate(weeknum = rep(1:93, 6))

write_csv(VRmob, "03_build/Vresas/output/VRESAS_mobility.csv")

#data clean restaurant ------

clean_res <- function(data){
  data <- data %>% 
    rename(pref = 1,
           week_JP = 2,
           kind = 3,
           ratio = 4) %>% 
    filter(kind == "すべて") %>% 
    select(-kind)

  return(data)
}

VRres <- rbind(clean_res(Yamanashi_res),
               clean_res(Gunma_res),
               clean_res(Ibaraki_res), 
               clean_res(Tochigi_res), 
               clean_res(Nagano_res), 
               clean_res(Shizuoka_res)) %>% 
  mutate(pref = str_replace_all(pref,
                                c(山梨県全体 = "Yamanashi",
                                  茨城県全体 = "Ibaraki",
                                  栃木県全体 = "Tochigi",
                                  群馬県全体 = "Gunma",
                                  長野県全体 = "Nagano",
                                  静岡県全体 = "Shizuoka"))) %>% 
  pivot_wider(names_from = "pref",
              values_from = "ratio")
  

write_csv(VRres, "03_build/Vresas/output/restaurant_all_genre.csv")


