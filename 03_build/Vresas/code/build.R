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
  data <- data[,-2]
  colnames(data) <- c("pref", "week", "kind", "ratio")
  newdata <- data %>% 
    pivot_wider(names_from = "kind",
                values_from = "ratio")
  colnames(newdata)[3:5] <- c("in_city", "in_pref", "out_pref")
  return(newdata)
}

Yamanashi_mob2 <- clean_mob(Yamanashi_mob)
Gunma_mob2 <- clean_mob(Gunma_mob)
Ibaraki_mob2 <- clean_mob(Ibaraki_mob)
Tochigi_mob2 <- clean_mob(Tochigi_mob)
Nagano_mob2 <- clean_mob(Nagano_mob)
Shizuoka_mob2 <- clean_mob(Shizuoka_mob)

VRmob <- rbind(Yamanashi_mob2, Gunma_mob2, Ibaraki_mob2, Tochigi_mob2, Nagano_mob2, Shizuoka_mob2)

VRmob$pref <- gsub("群馬県全体", "Gunma", VRmob$pref)
VRmob$pref <- gsub("山梨県全体", "Yamanashi", VRmob$pref)
VRmob$pref <- gsub("栃木県全体", "Tochigi", VRmob$pref)
VRmob$pref <- gsub("茨城県全体", "Ibaragi", VRmob$pref)
VRmob$pref <- gsub("長野県全体", "Nagano", VRmob$pref)
VRmob$pref <- gsub("静岡県全体", "Shizuoka", VRmob$pref)

# write.csv(VRmob, "03_build/Vresas/output/VRESAS_mobility.csv")

#data clean restaurant ------

clean_res <- function(data){
  colnames(data) <- c("pref", "week", "kind", "ratio")
  newdata <- data %>% 
    filter(kind == "すべて")
  newdata <- newdata[,-3]
  return(newdata)
}

Yamanashi_res2 <- clean_res(Yamanashi_res)
Gunma_res2 <- clean_res(Gunma_res)
Ibaraki_res2 <- clean_res(Ibaraki_res)
Tochigi_res2 <- clean_res(Tochigi_res)
Nagano_res2 <- clean_res(Nagano_res)
Shizuoka_res2 <- clean_res(Shizuoka_res)

VRres <- rbind(Yamanashi_res2, Gunma_res2, Ibaraki_res2, Tochigi_res2, Nagano_res2, Shizuoka_res2)

VRres$pref <- gsub("群馬県全体", "Gunma", VRres$pref)
VRres$pref <- gsub("山梨県全体", "Yamanashi", VRres$pref)
VRres$pref <- gsub("栃木県全体", "Tochigi", VRres$pref)
VRres$pref <- gsub("茨城県全体", "Ibaragi", VRres$pref)
VRres$pref <- gsub("長野県全体", "Nagano", VRres$pref)
VRres$pref <- gsub("静岡県全体", "Shizuoka", VRres$pref)

VRres <- VRres %>% 
  pivot_wider(names_from = "pref",
              values_from = "ratio")

write.csv(VRres, "03_build/Vresas/output/restraunt_all_genre.csv", row.names = FALSE)


