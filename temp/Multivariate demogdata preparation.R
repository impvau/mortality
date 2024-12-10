##################################
# Preparation of subnational data
##################################

# state level precise death counts

read.jpn_death <- function(region, label = region)
{
  deaths_path = paste("https://www.ipss.go.jp/p-toukei/JMD/", region, "/STATS/",   "Deaths_1x1.txt", sep = "")
  deaths_data = data.table::fread(deaths_path, skip = 2, header = TRUE, data.table = F, blank.lines.skip = T)
  
  exposure_path = paste("https://www.ipss.go.jp/p-toukei/JMD/", region, "/STATS/",   "Exposures_1x1.txt", sep = "")
  exposure_data = data.table::fread(exposure_path, skip = 2, header = TRUE, data.table = F, blank.lines.skip = T)
  
  return(list(Deaths = deaths_data, pop = exposure_data))
}


# Hokkaido

dum = read.jpn_death("01", "Hokkaido")$Deaths
Hokkaido_count_F = matrix(dum[2887:8436,3], nrow=111)
Hokkaido_count_M = matrix(dum[2887:8436,4], nrow=111)
Hokkaido_count_T = matrix(dum[2887:8436,5], nrow=111)

Hokkaido_count_female = rbind(Hokkaido_count_F[1:100,], colSums(Hokkaido_count_F[101:111,]))
Hokkaido_count_male   = rbind(Hokkaido_count_M[1:100,], colSums(Hokkaido_count_M[101:111,]))
Hokkaido_count_total  = rbind(Hokkaido_count_T[1:100,], colSums(Hokkaido_count_T[101:111,])) 

# Aomori

dum = read.jpn_death("02", "Aomori")$Deaths
Aomori_count_F = matrix(dum[2887:8436,3], nrow=111)
Aomori_count_M = matrix(dum[2887:8436,4], nrow=111)
Aomori_count_T = matrix(dum[2887:8436,5], nrow=111)

Aomori_count_female = rbind(Aomori_count_F[1:100,], colSums(Aomori_count_F[101:111,]))
Aomori_count_male   = rbind(Aomori_count_M[1:100,], colSums(Aomori_count_M[101:111,]))
Aomori_count_total  = rbind(Aomori_count_T[1:100,], colSums(Aomori_count_T[101:111,])) 

# Iwate

dum = read.jpn_death("03", "Iwate")$Deaths
Iwate_count_F = matrix(dum[2887:8436,3], nrow=111)
Iwate_count_M = matrix(dum[2887:8436,4], nrow=111)
Iwate_count_T = matrix(dum[2887:8436,5], nrow=111)

Iwate_count_female = rbind(Iwate_count_F[1:100,], colSums(Iwate_count_F[101:111,]))
Iwate_count_male   = rbind(Iwate_count_M[1:100,], colSums(Iwate_count_M[101:111,]))
Iwate_count_total  = rbind(Iwate_count_T[1:100,], colSums(Iwate_count_T[101:111,])) 

# Miyagi

dum = read.jpn_death("04", "Miyagi")$Deaths
Miyagi_count_F = matrix(dum[2887:8436,3], nrow=111)
Miyagi_count_M = matrix(dum[2887:8436,4], nrow=111)
Miyagi_count_T = matrix(dum[2887:8436,5], nrow=111)

Miyagi_count_female = rbind(Miyagi_count_F[1:100,], colSums(Miyagi_count_F[101:111,]))
Miyagi_count_male   = rbind(Miyagi_count_M[1:100,], colSums(Miyagi_count_M[101:111,]))
Miyagi_count_total  = rbind(Miyagi_count_T[1:100,], colSums(Miyagi_count_T[101:111,])) 

# Akita

dum = read.jpn_death("05", "Akita")$Deaths
Akita_count_F = matrix(dum[2887:8436,3], nrow=111)
Akita_count_M = matrix(dum[2887:8436,4], nrow=111)
Akita_count_T = matrix(dum[2887:8436,5], nrow=111)

Akita_count_female = rbind(Akita_count_F[1:100,], colSums(Akita_count_F[101:111,]))
Akita_count_male   = rbind(Akita_count_M[1:100,], colSums(Akita_count_M[101:111,]))
Akita_count_total  = rbind(Akita_count_T[1:100,], colSums(Akita_count_T[101:111,])) 

# Yamagata

dum = read.jpn_death("06", "Yamagata")$Deaths
Yamagata_count_F = matrix(dum[2887:8436,3], nrow=111)
Yamagata_count_M = matrix(dum[2887:8436,4], nrow=111)
Yamagata_count_T = matrix(dum[2887:8436,5], nrow=111)

Yamagata_count_female = rbind(Yamagata_count_F[1:100,], colSums(Yamagata_count_F[101:111,]))
Yamagata_count_male   = rbind(Yamagata_count_M[1:100,], colSums(Yamagata_count_M[101:111,]))
Yamagata_count_total  = rbind(Yamagata_count_T[1:100,], colSums(Yamagata_count_T[101:111,])) 

# Fukushima

dum = read.jpn_death("07", "Fukushima")$Deaths
Fukushima_count_F = matrix(dum[2887:8436,3], nrow=111)
Fukushima_count_M = matrix(dum[2887:8436,4], nrow=111)
Fukushima_count_T = matrix(dum[2887:8436,5], nrow=111)

Fukushima_count_female = rbind(Fukushima_count_F[1:100,], colSums(Fukushima_count_F[101:111,]))
Fukushima_count_male   = rbind(Fukushima_count_M[1:100,], colSums(Fukushima_count_M[101:111,]))
Fukushima_count_total  = rbind(Fukushima_count_T[1:100,], colSums(Fukushima_count_T[101:111,])) 

# Ibaraki

dum = read.jpn_death("08", "Ibaraki")$Deaths
Ibaraki_count_F = matrix(dum[2887:8436,3], nrow=111)
Ibaraki_count_M = matrix(dum[2887:8436,4], nrow=111)
Ibaraki_count_T = matrix(dum[2887:8436,5], nrow=111)

Ibaraki_count_female = rbind(Ibaraki_count_F[1:100,], colSums(Ibaraki_count_F[101:111,]))
Ibaraki_count_male   = rbind(Ibaraki_count_M[1:100,], colSums(Ibaraki_count_M[101:111,]))
Ibaraki_count_total  = rbind(Ibaraki_count_T[1:100,], colSums(Ibaraki_count_T[101:111,])) 

# Tochigi

dum = read.jpn_death("09", "Tochigi")$Deaths
Tochigi_count_F = matrix(dum[2887:8436,3], nrow=111)
Tochigi_count_M = matrix(dum[2887:8436,4], nrow=111)
Tochigi_count_T = matrix(dum[2887:8436,5], nrow=111)

Tochigi_count_female = rbind(Tochigi_count_F[1:100,], colSums(Tochigi_count_F[101:111,]))
Tochigi_count_male   = rbind(Tochigi_count_M[1:100,], colSums(Tochigi_count_M[101:111,]))
Tochigi_count_total  = rbind(Tochigi_count_T[1:100,], colSums(Tochigi_count_T[101:111,])) 

# Gunma

dum = read.jpn_death("10", "Gunma")$Deaths
Gunma_count_F = matrix(dum[2887:8436,3], nrow=111)
Gunma_count_M = matrix(dum[2887:8436,4], nrow=111)
Gunma_count_T = matrix(dum[2887:8436,5], nrow=111)

Gunma_count_female = rbind(Gunma_count_F[1:100,], colSums(Gunma_count_F[101:111,]))
Gunma_count_male   = rbind(Gunma_count_M[1:100,], colSums(Gunma_count_M[101:111,]))
Gunma_count_total  = rbind(Gunma_count_T[1:100,], colSums(Gunma_count_T[101:111,])) 

# Saitama

dum = read.jpn_death("11", "Saitama")$Deaths
Saitama_count_F = matrix(dum[2887:8436,3], nrow=111)
Saitama_count_M = matrix(dum[2887:8436,4], nrow=111)
Saitama_count_T = matrix(dum[2887:8436,5], nrow=111)

Saitama_count_female = rbind(Saitama_count_F[1:100,], colSums(Saitama_count_F[101:111,]))
Saitama_count_male   = rbind(Saitama_count_M[1:100,], colSums(Saitama_count_M[101:111,]))
Saitama_count_total  = rbind(Saitama_count_T[1:100,], colSums(Saitama_count_T[101:111,])) 

# Chiba

dum = read.jpn_death("12", "Chiba")$Deaths
Chiba_count_F = matrix(dum[2887:8436,3], nrow=111)
Chiba_count_M = matrix(dum[2887:8436,4], nrow=111)
Chiba_count_T = matrix(dum[2887:8436,5], nrow=111)

Chiba_count_female = rbind(Chiba_count_F[1:100,], colSums(Chiba_count_F[101:111,]))
Chiba_count_male   = rbind(Chiba_count_M[1:100,], colSums(Chiba_count_M[101:111,]))
Chiba_count_total  = rbind(Chiba_count_T[1:100,], colSums(Chiba_count_T[101:111,])) 

# Tokyo

dum = read.jpn_death("13", "Tokyo")$Deaths
Tokyo_count_F = matrix(dum[2887:8436,3], nrow=111)
Tokyo_count_M = matrix(dum[2887:8436,4], nrow=111)
Tokyo_count_T = matrix(dum[2887:8436,5], nrow=111)

Tokyo_count_female = rbind(Tokyo_count_F[1:100,], colSums(Tokyo_count_F[101:111,]))
Tokyo_count_male   = rbind(Tokyo_count_M[1:100,], colSums(Tokyo_count_M[101:111,]))
Tokyo_count_total  = rbind(Tokyo_count_T[1:100,], colSums(Tokyo_count_T[101:111,])) 

# Kanagawa

dum = read.jpn_death("14", "Kanagawa")$Deaths
Kanagawa_count_F = matrix(dum[2887:8436,3], nrow=111)
Kanagawa_count_M = matrix(dum[2887:8436,4], nrow=111)
Kanagawa_count_T = matrix(dum[2887:8436,5], nrow=111)

Kanagawa_count_female = rbind(Kanagawa_count_F[1:100,], colSums(Kanagawa_count_F[101:111,]))
Kanagawa_count_male   = rbind(Kanagawa_count_M[1:100,], colSums(Kanagawa_count_M[101:111,]))
Kanagawa_count_total  = rbind(Kanagawa_count_T[1:100,], colSums(Kanagawa_count_T[101:111,])) 

# Niigata

dum = read.jpn_death("15", "Niigata")$Deaths
Niigata_count_F = matrix(dum[2887:8436,3], nrow=111)
Niigata_count_M = matrix(dum[2887:8436,4], nrow=111)
Niigata_count_T = matrix(dum[2887:8436,5], nrow=111)

Niigata_count_female = rbind(Niigata_count_F[1:100,], colSums(Niigata_count_F[101:111,]))
Niigata_count_male   = rbind(Niigata_count_M[1:100,], colSums(Niigata_count_M[101:111,]))
Niigata_count_total  = rbind(Niigata_count_T[1:100,], colSums(Niigata_count_T[101:111,])) 

# Toyama

dum = read.jpn_death("16", "Toyama")$Deaths
Toyama_count_F = matrix(dum[2887:8436,3], nrow=111)
Toyama_count_M = matrix(dum[2887:8436,4], nrow=111)
Toyama_count_T = matrix(dum[2887:8436,5], nrow=111)

Toyama_count_female = rbind(Toyama_count_F[1:100,], colSums(Toyama_count_F[101:111,]))
Toyama_count_male   = rbind(Toyama_count_M[1:100,], colSums(Toyama_count_M[101:111,]))
Toyama_count_total  = rbind(Toyama_count_T[1:100,], colSums(Toyama_count_T[101:111,])) 

# Ishikawa

dum = read.jpn_death("17", "Ishikawa")$Deaths
Ishikawa_count_F = matrix(dum[2887:8436,3], nrow=111)
Ishikawa_count_M = matrix(dum[2887:8436,4], nrow=111)
Ishikawa_count_T = matrix(dum[2887:8436,5], nrow=111)

Ishikawa_count_female = rbind(Ishikawa_count_F[1:100,], colSums(Ishikawa_count_F[101:111,]))
Ishikawa_count_male   = rbind(Ishikawa_count_M[1:100,], colSums(Ishikawa_count_M[101:111,]))
Ishikawa_count_total  = rbind(Ishikawa_count_T[1:100,], colSums(Ishikawa_count_T[101:111,])) 

# Fukui

dum = read.jpn_death("18", "Fukui")$Deaths
Fukui_count_F = matrix(dum[2887:8436,3], nrow=111)
Fukui_count_M = matrix(dum[2887:8436,4], nrow=111)
Fukui_count_T = matrix(dum[2887:8436,5], nrow=111)

Fukui_count_female = rbind(Fukui_count_F[1:100,], colSums(Fukui_count_F[101:111,]))
Fukui_count_male   = rbind(Fukui_count_M[1:100,], colSums(Fukui_count_M[101:111,]))
Fukui_count_total  = rbind(Fukui_count_T[1:100,], colSums(Fukui_count_T[101:111,])) 

# Yamanashi

dum = read.jpn_death("19", "Yamanashi")$Deaths
Yamanashi_count_F = matrix(dum[2887:8436,3], nrow=111)
Yamanashi_count_M = matrix(dum[2887:8436,4], nrow=111)
Yamanashi_count_T = matrix(dum[2887:8436,5], nrow=111)

Yamanashi_count_female = rbind(Yamanashi_count_F[1:100,], colSums(Yamanashi_count_F[101:111,]))
Yamanashi_count_male   = rbind(Yamanashi_count_M[1:100,], colSums(Yamanashi_count_M[101:111,]))
Yamanashi_count_total  = rbind(Yamanashi_count_T[1:100,], colSums(Yamanashi_count_T[101:111,])) 

# Nagano

dum = read.jpn_death("20", "Nagano")$Deaths
Nagano_count_F = matrix(dum[2887:8436,3], nrow=111)
Nagano_count_M = matrix(dum[2887:8436,4], nrow=111)
Nagano_count_T = matrix(dum[2887:8436,5], nrow=111)

Nagano_count_female = rbind(Nagano_count_F[1:100,], colSums(Nagano_count_F[101:111,]))
Nagano_count_male   = rbind(Nagano_count_M[1:100,], colSums(Nagano_count_M[101:111,]))
Nagano_count_total  = rbind(Nagano_count_T[1:100,], colSums(Nagano_count_T[101:111,])) 

# Gifu

dum = read.jpn_death("21", "Gifu")$Deaths
Gifu_count_F = matrix(dum[2887:8436,3], nrow=111)
Gifu_count_M = matrix(dum[2887:8436,4], nrow=111)
Gifu_count_T = matrix(dum[2887:8436,5], nrow=111)

Gifu_count_female = rbind(Gifu_count_F[1:100,], colSums(Gifu_count_F[101:111,]))
Gifu_count_male   = rbind(Gifu_count_M[1:100,], colSums(Gifu_count_M[101:111,]))
Gifu_count_total  = rbind(Gifu_count_T[1:100,], colSums(Gifu_count_T[101:111,])) 

# Shizuoka

dum = read.jpn_death("22", "Shizuoka")$Deaths
Shizuoka_count_F = matrix(dum[2887:8436,3], nrow=111)
Shizuoka_count_M = matrix(dum[2887:8436,4], nrow=111)
Shizuoka_count_T = matrix(dum[2887:8436,5], nrow=111)

Shizuoka_count_female = rbind(Shizuoka_count_F[1:100,], colSums(Shizuoka_count_F[101:111,]))
Shizuoka_count_male   = rbind(Shizuoka_count_M[1:100,], colSums(Shizuoka_count_M[101:111,]))
Shizuoka_count_total  = rbind(Shizuoka_count_T[1:100,], colSums(Shizuoka_count_T[101:111,])) 

# Aichi

dum = read.jpn_death("23", "Aichi")$Deaths
Aichi_count_F = matrix(dum[2887:8436,3], nrow=111)
Aichi_count_M = matrix(dum[2887:8436,4], nrow=111)
Aichi_count_T = matrix(dum[2887:8436,5], nrow=111)

Aichi_count_female = rbind(Aichi_count_F[1:100,], colSums(Aichi_count_F[101:111,]))
Aichi_count_male   = rbind(Aichi_count_M[1:100,], colSums(Aichi_count_M[101:111,]))
Aichi_count_total  = rbind(Aichi_count_T[1:100,], colSums(Aichi_count_T[101:111,])) 

# Mie

dum = read.jpn_death("24", "Mie")$Deaths
Mie_count_F = matrix(dum[2887:8436,3], nrow=111)
Mie_count_M = matrix(dum[2887:8436,4], nrow=111)
Mie_count_T = matrix(dum[2887:8436,5], nrow=111)

Mie_count_female = rbind(Mie_count_F[1:100,], colSums(Mie_count_F[101:111,]))
Mie_count_male   = rbind(Mie_count_M[1:100,], colSums(Mie_count_M[101:111,]))
Mie_count_total  = rbind(Mie_count_T[1:100,], colSums(Mie_count_T[101:111,])) 

# Shiga

dum = read.jpn_death("25", "Shiga")$Deaths
Shiga_count_F = matrix(dum[2887:8436,3], nrow=111)
Shiga_count_M = matrix(dum[2887:8436,4], nrow=111)
Shiga_count_T = matrix(dum[2887:8436,5], nrow=111)

Shiga_count_female = rbind(Shiga_count_F[1:100,], colSums(Shiga_count_F[101:111,]))
Shiga_count_male   = rbind(Shiga_count_M[1:100,], colSums(Shiga_count_M[101:111,]))
Shiga_count_total  = rbind(Shiga_count_T[1:100,], colSums(Shiga_count_T[101:111,])) 

# Kyoto

dum = read.jpn_death("26", "Kyoto")$Deaths
Kyoto_count_F = matrix(dum[2887:8436,3], nrow=111)
Kyoto_count_M = matrix(dum[2887:8436,4], nrow=111)
Kyoto_count_T = matrix(dum[2887:8436,5], nrow=111)

Kyoto_count_female = rbind(Kyoto_count_F[1:100,], colSums(Kyoto_count_F[101:111,]))
Kyoto_count_male   = rbind(Kyoto_count_M[1:100,], colSums(Kyoto_count_M[101:111,]))
Kyoto_count_total  = rbind(Kyoto_count_T[1:100,], colSums(Kyoto_count_T[101:111,])) 

# Osaka

dum = read.jpn_death("27", "Osaka")$Deaths
Osaka_count_F = matrix(dum[2887:8436,3], nrow=111)
Osaka_count_M = matrix(dum[2887:8436,4], nrow=111)
Osaka_count_T = matrix(dum[2887:8436,5], nrow=111)

Osaka_count_female = rbind(Osaka_count_F[1:100,], colSums(Osaka_count_F[101:111,]))
Osaka_count_male   = rbind(Osaka_count_M[1:100,], colSums(Osaka_count_M[101:111,]))
Osaka_count_total  = rbind(Osaka_count_T[1:100,], colSums(Osaka_count_T[101:111,])) 

# Hyogo

dum = read.jpn_death("28", "Hyogo")$Deaths
Hyogo_count_F = matrix(dum[2887:8436,3], nrow=111)
Hyogo_count_M = matrix(dum[2887:8436,4], nrow=111)
Hyogo_count_T = matrix(dum[2887:8436,5], nrow=111)

Hyogo_count_female = rbind(Hyogo_count_F[1:100,], colSums(Hyogo_count_F[101:111,]))
Hyogo_count_male   = rbind(Hyogo_count_M[1:100,], colSums(Hyogo_count_M[101:111,]))
Hyogo_count_total  = rbind(Hyogo_count_T[1:100,], colSums(Hyogo_count_T[101:111,])) 

# Nara

dum = read.jpn_death("29", "Nara")$Deaths
Nara_count_F = matrix(dum[2887:8436,3], nrow=111)
Nara_count_M = matrix(dum[2887:8436,4], nrow=111)
Nara_count_T = matrix(dum[2887:8436,5], nrow=111)

Nara_count_female = rbind(Nara_count_F[1:100,], colSums(Nara_count_F[101:111,]))
Nara_count_male   = rbind(Nara_count_M[1:100,], colSums(Nara_count_M[101:111,]))
Nara_count_total  = rbind(Nara_count_T[1:100,], colSums(Nara_count_T[101:111,])) 

# Wakayama

dum = read.jpn_death("30", "Wakayama")$Deaths
Wakayama_count_F = matrix(dum[2887:8436,3], nrow=111)
Wakayama_count_M = matrix(dum[2887:8436,4], nrow=111)
Wakayama_count_T = matrix(dum[2887:8436,5], nrow=111)

Wakayama_count_female = rbind(Wakayama_count_F[1:100,], colSums(Wakayama_count_F[101:111,]))
Wakayama_count_male   = rbind(Wakayama_count_M[1:100,], colSums(Wakayama_count_M[101:111,]))
Wakayama_count_total  = rbind(Wakayama_count_T[1:100,], colSums(Wakayama_count_T[101:111,])) 

# Tottori

dum = read.jpn_death("31", "Tottori")$Deaths
Tottori_count_F = matrix(dum[2887:8436,3], nrow=111)
Tottori_count_M = matrix(dum[2887:8436,4], nrow=111)
Tottori_count_T = matrix(dum[2887:8436,5], nrow=111)

Tottori_count_female = rbind(Tottori_count_F[1:100,], colSums(Tottori_count_F[101:111,]))
Tottori_count_male   = rbind(Tottori_count_M[1:100,], colSums(Tottori_count_M[101:111,]))
Tottori_count_total  = rbind(Tottori_count_T[1:100,], colSums(Tottori_count_T[101:111,])) 

# Shimane

dum = read.jpn_death("32", "Shimane")$Deaths
Shimane_count_F = matrix(dum[2887:8436,3], nrow=111)
Shimane_count_M = matrix(dum[2887:8436,4], nrow=111)
Shimane_count_T = matrix(dum[2887:8436,5], nrow=111)

Shimane_count_female = rbind(Shimane_count_F[1:100,], colSums(Shimane_count_F[101:111,]))
Shimane_count_male   = rbind(Shimane_count_M[1:100,], colSums(Shimane_count_M[101:111,]))
Shimane_count_total  = rbind(Shimane_count_T[1:100,], colSums(Shimane_count_T[101:111,])) 

# Okayama

dum = read.jpn_death("33", "Okayama")$Deaths
Okayama_count_F = matrix(dum[2887:8436,3], nrow=111)
Okayama_count_M = matrix(dum[2887:8436,4], nrow=111)
Okayama_count_T = matrix(dum[2887:8436,5], nrow=111)

Okayama_count_female = rbind(Okayama_count_F[1:100,], colSums(Okayama_count_F[101:111,]))
Okayama_count_male   = rbind(Okayama_count_M[1:100,], colSums(Okayama_count_M[101:111,]))
Okayama_count_total  = rbind(Okayama_count_T[1:100,], colSums(Okayama_count_T[101:111,])) 

# Hiroshima

dum = read.jpn_death("34", "Hiroshima")$Deaths
Hiroshima_count_F = matrix(dum[2887:8436,3], nrow=111)
Hiroshima_count_M = matrix(dum[2887:8436,4], nrow=111)
Hiroshima_count_T = matrix(dum[2887:8436,5], nrow=111)

Hiroshima_count_female = rbind(Hiroshima_count_F[1:100,], colSums(Hiroshima_count_F[101:111,]))
Hiroshima_count_male   = rbind(Hiroshima_count_M[1:100,], colSums(Hiroshima_count_M[101:111,]))
Hiroshima_count_total  = rbind(Hiroshima_count_T[1:100,], colSums(Hiroshima_count_T[101:111,])) 

# Yamaguchi

dum = read.jpn_death("35", "Yamaguchi")$Deaths
Yamaguchi_count_F = matrix(dum[2887:8436,3], nrow=111)
Yamaguchi_count_M = matrix(dum[2887:8436,4], nrow=111)
Yamaguchi_count_T = matrix(dum[2887:8436,5], nrow=111)

Yamaguchi_count_female = rbind(Yamaguchi_count_F[1:100,], colSums(Yamaguchi_count_F[101:111,]))
Yamaguchi_count_male   = rbind(Yamaguchi_count_M[1:100,], colSums(Yamaguchi_count_M[101:111,]))
Yamaguchi_count_total  = rbind(Yamaguchi_count_T[1:100,], colSums(Yamaguchi_count_T[101:111,])) 

# Tokushima

dum = read.jpn_death("36", "Tokushima")$Deaths
Tokushima_count_F = matrix(dum[2887:8436,3], nrow=111)
Tokushima_count_M = matrix(dum[2887:8436,4], nrow=111)
Tokushima_count_T = matrix(dum[2887:8436,5], nrow=111)

Tokushima_count_female = rbind(Tokushima_count_F[1:100,], colSums(Tokushima_count_F[101:111,]))
Tokushima_count_male   = rbind(Tokushima_count_M[1:100,], colSums(Tokushima_count_M[101:111,]))
Tokushima_count_total  = rbind(Tokushima_count_T[1:100,], colSums(Tokushima_count_T[101:111,])) 

# Kagawa

dum = read.jpn_death("37", "Kagawa")$Deaths
Kagawa_count_F = matrix(dum[2887:8436,3], nrow=111)
Kagawa_count_M = matrix(dum[2887:8436,4], nrow=111)
Kagawa_count_T = matrix(dum[2887:8436,5], nrow=111)

Kagawa_count_female = rbind(Kagawa_count_F[1:100,], colSums(Kagawa_count_F[101:111,]))
Kagawa_count_male   = rbind(Kagawa_count_M[1:100,], colSums(Kagawa_count_M[101:111,]))
Kagawa_count_total  = rbind(Kagawa_count_T[1:100,], colSums(Kagawa_count_T[101:111,])) 

# Ehime

dum = read.jpn_death("38", "Ehime")$Deaths
Ehime_count_F = matrix(dum[2887:8436,3], nrow=111)
Ehime_count_M = matrix(dum[2887:8436,4], nrow=111)
Ehime_count_T = matrix(dum[2887:8436,5], nrow=111)

Ehime_count_female = rbind(Ehime_count_F[1:100,], colSums(Ehime_count_F[101:111,]))
Ehime_count_male   = rbind(Ehime_count_M[1:100,], colSums(Ehime_count_M[101:111,]))
Ehime_count_total  = rbind(Ehime_count_T[1:100,], colSums(Ehime_count_T[101:111,])) 

# Kochi

dum = read.jpn_death("39", "Kochi")$Deaths
Kochi_count_F = matrix(dum[2887:8436,3], nrow=111)
Kochi_count_M = matrix(dum[2887:8436,4], nrow=111)
Kochi_count_T = matrix(dum[2887:8436,5], nrow=111)

Kochi_count_female = rbind(Kochi_count_F[1:100,], colSums(Kochi_count_F[101:111,]))
Kochi_count_male   = rbind(Kochi_count_M[1:100,], colSums(Kochi_count_M[101:111,]))
Kochi_count_total  = rbind(Kochi_count_T[1:100,], colSums(Kochi_count_T[101:111,])) 

# Fukuoka

dum = read.jpn_death("40", "Fukuoka")$Deaths
Fukuoka_count_F = matrix(dum[2887:8436,3], nrow=111)
Fukuoka_count_M = matrix(dum[2887:8436,4], nrow=111)
Fukuoka_count_T = matrix(dum[2887:8436,5], nrow=111)

Fukuoka_count_female = rbind(Fukuoka_count_F[1:100,], colSums(Fukuoka_count_F[101:111,]))
Fukuoka_count_male   = rbind(Fukuoka_count_M[1:100,], colSums(Fukuoka_count_M[101:111,]))
Fukuoka_count_total  = rbind(Fukuoka_count_T[1:100,], colSums(Fukuoka_count_T[101:111,])) 

# Saga

dum = read.jpn_death("41", "Saga")$Deaths
Saga_count_F = matrix(dum[2887:8436,3], nrow=111)
Saga_count_M = matrix(dum[2887:8436,4], nrow=111)
Saga_count_T = matrix(dum[2887:8436,5], nrow=111)

Saga_count_female = rbind(Saga_count_F[1:100,], colSums(Saga_count_F[101:111,]))
Saga_count_male   = rbind(Saga_count_M[1:100,], colSums(Saga_count_M[101:111,]))
Saga_count_total  = rbind(Saga_count_T[1:100,], colSums(Saga_count_T[101:111,])) 

# Nagasaki

dum = read.jpn_death("42", "Nagasaki")$Deaths
Nagasaki_count_F = matrix(dum[2887:8436,3], nrow=111)
Nagasaki_count_M = matrix(dum[2887:8436,4], nrow=111)
Nagasaki_count_T = matrix(dum[2887:8436,5], nrow=111)

Nagasaki_count_female = rbind(Nagasaki_count_F[1:100,], colSums(Nagasaki_count_F[101:111,]))
Nagasaki_count_male   = rbind(Nagasaki_count_M[1:100,], colSums(Nagasaki_count_M[101:111,]))
Nagasaki_count_total  = rbind(Nagasaki_count_T[1:100,], colSums(Nagasaki_count_T[101:111,])) 

# Kumamoto

dum = read.jpn_death("43", "Kumamoto")$Deaths
Kumamoto_count_F = matrix(dum[2887:8436,3], nrow=111)
Kumamoto_count_M = matrix(dum[2887:8436,4], nrow=111)
Kumamoto_count_T = matrix(dum[2887:8436,5], nrow=111)

Kumamoto_count_female = rbind(Kumamoto_count_F[1:100,], colSums(Kumamoto_count_F[101:111,]))
Kumamoto_count_male   = rbind(Kumamoto_count_M[1:100,], colSums(Kumamoto_count_M[101:111,]))
Kumamoto_count_total  = rbind(Kumamoto_count_T[1:100,], colSums(Kumamoto_count_T[101:111,])) 

# Oita

dum = read.jpn_death("44", "Oita")$Deaths
Oita_count_F = matrix(dum[2887:8436,3], nrow=111)
Oita_count_M = matrix(dum[2887:8436,4], nrow=111)
Oita_count_T = matrix(dum[2887:8436,5], nrow=111)

Oita_count_female = rbind(Oita_count_F[1:100,], colSums(Oita_count_F[101:111,]))
Oita_count_male   = rbind(Oita_count_M[1:100,], colSums(Oita_count_M[101:111,]))
Oita_count_total  = rbind(Oita_count_T[1:100,], colSums(Oita_count_T[101:111,])) 

# Miyazaki

dum = read.jpn_death("45", "Miyazaki")$Deaths
Miyazaki_count_F = matrix(dum[2887:8436,3], nrow=111)
Miyazaki_count_M = matrix(dum[2887:8436,4], nrow=111)
Miyazaki_count_T = matrix(dum[2887:8436,5], nrow=111)

Miyazaki_count_female = rbind(Miyazaki_count_F[1:100,], colSums(Miyazaki_count_F[101:111,]))
Miyazaki_count_male   = rbind(Miyazaki_count_M[1:100,], colSums(Miyazaki_count_M[101:111,]))
Miyazaki_count_total  = rbind(Miyazaki_count_T[1:100,], colSums(Miyazaki_count_T[101:111,])) 

# Kagoshima

dum = read.jpn_death("46", "Kagoshima")$Deaths
Kagoshima_count_F = matrix(dum[2887:8436,3], nrow=111)
Kagoshima_count_M = matrix(dum[2887:8436,4], nrow=111)
Kagoshima_count_T = matrix(dum[2887:8436,5], nrow=111)

Kagoshima_count_female = rbind(Kagoshima_count_F[1:100,], colSums(Kagoshima_count_F[101:111,]))
Kagoshima_count_male   = rbind(Kagoshima_count_M[1:100,], colSums(Kagoshima_count_M[101:111,]))
Kagoshima_count_total  = rbind(Kagoshima_count_T[1:100,], colSums(Kagoshima_count_T[101:111,])) 

# Okinawa

dum = read.jpn_death("47", "Okinawa")$Deaths
Okinawa_count_F = matrix(dum[1:5550,3], nrow=111)
Okinawa_count_M = matrix(dum[1:5550,4], nrow=111)
Okinawa_count_T = matrix(dum[1:5550,5], nrow=111)

Okinawa_count_female = rbind(Okinawa_count_F[1:100,], colSums(Okinawa_count_F[101:111,]))
Okinawa_count_male   = rbind(Okinawa_count_M[1:100,], colSums(Okinawa_count_M[101:111,]))
Okinawa_count_total  = rbind(Okinawa_count_T[1:100,], colSums(Okinawa_count_T[101:111,])) 

########################
# clustering by regions
########################

# R1

mfts_R1 = Hokkaido
mfts_R1_smooth = Hokkaido_smooth

# R2

mfts_R2_female_count = Aomori$pop$female + Iwate$pop$female + Miyagi$pop$female + Akita$pop$female + 
  Yamagata$pop$female + Fukushima$pop$female
mfts_R2_female_rate = (Aomori_count_female + Iwate_count_female + Miyagi_count_female + Akita_count_female + 
                         Yamagata_count_female + Fukushima_count_female)/mfts_R2_female_count

mfts_R2_male_count = Aomori$pop$male + Iwate$pop$male + Miyagi$pop$male + Akita$pop$male + 
  Yamagata$pop$male + Fukushima$pop$male
mfts_R2_male_rate = (Aomori_count_male + Iwate_count_male + Miyagi_count_male + Akita_count_male + 
                       Yamagata_count_male + Fukushima_count_male)/mfts_R2_male_count

mfts_R2_total_count = Aomori$pop$total + Iwate$pop$total + Miyagi$pop$total + Akita$pop$total + 
  Yamagata$pop$total + Fukushima$pop$total
mfts_R2_total_rate = (Aomori_count_total + Iwate_count_total + Miyagi_count_total + Akita_count_total + 
                        Yamagata_count_total + Fukushima_count_total)/mfts_R2_total_count


mfts_R2_rate_summary = cbind(rep(1973:2022,101), rep(0:100,50), as.numeric(mfts_R2_female_rate), as.numeric(mfts_R2_male_rate), as.numeric(mfts_R2_total_rate))
colnames(mfts_R2_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(mfts_R2_rate_summary, "mfts_R2_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

mfts_R2_count_summary = cbind(rep(1973:2022,101), rep(0:100,50), as.numeric(mfts_R2_female_count), as.numeric(mfts_R2_male_count), as.numeric(mfts_R2_total_count))
colnames(mfts_R2_count_summary) = c("Year","Age","Female","Male","Total")
write.table(mfts_R2_count_summary, "mfts_R2_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

mfts_R2 = read.demogdata("mfts_R2_rate.txt", "mfts_R2_count.txt", type="mortality", label = "R2", skip=0)
mfts_R2_smooth = smooth.demogdata(mfts_R2)

# R3

mfts_R3_female_count = Ibaraki$pop$female + Tochigi$pop$female + Gunma$pop$female + Saitama$pop$female + 
  Chiba$pop$female + Tokyo$pop$female + Kanagawa$pop$female
mfts_R3_female_rate = (Ibaraki_count_female + Tochigi_count_female + Gunma_count_female + Saitama_count_female + 
                         Chiba_count_female + Tokyo_count_female + Kanagawa_count_female)/mfts_R3_female_count

mfts_R3_male_count = Ibaraki$pop$male + Tochigi$pop$male + Gunma$pop$male + Saitama$pop$male + 
  Chiba$pop$male + Tokyo$pop$male + Kanagawa$pop$male
mfts_R3_male_rate = (Ibaraki_count_male + Tochigi_count_male + Gunma_count_male + Saitama_count_male + 
                       Chiba_count_male + Tokyo_count_male + Kanagawa_count_male)/mfts_R3_male_count

mfts_R3_total_count = Ibaraki$pop$total + Tochigi$pop$total + Gunma$pop$total + Saitama$pop$total + 
  Chiba$pop$total + Tokyo$pop$total + Kanagawa$pop$total
mfts_R3_total_rate = (Ibaraki_count_total + Tochigi_count_total + Gunma_count_total + Saitama_count_total + 
                        Chiba_count_total + Tokyo_count_total + Kanagawa_count_total)/mfts_R3_total_count

mfts_R3_rate_summary = cbind(rep(1973:2022,101), rep(0:100,50), as.numeric(mfts_R3_female_rate), as.numeric(mfts_R3_male_rate), as.numeric(mfts_R3_total_rate))
colnames(mfts_R3_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(mfts_R3_rate_summary, "mfts_R3_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

mfts_R3_count_summary = cbind(rep(1973:2022,101), rep(0:100,50), as.numeric(mfts_R3_female_count), as.numeric(mfts_R3_male_count), as.numeric(mfts_R3_total_count))
colnames(mfts_R3_count_summary) = c("Year","Age","Female","Male","Total")
write.table(mfts_R3_count_summary, "mfts_R3_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

mfts_R3 = read.demogdata("mfts_R3_rate.txt", "mfts_R3_count.txt", type="mortality", label = "R3", skip=0)
mfts_R3_smooth = smooth.demogdata(mfts_R3)

# R4

mfts_R4_female_count = Niigata$pop$female + Toyama$pop$female + Ishikawa$pop$female + Fukui$pop$female + 
  Yamanashi$pop$female + Nagano$pop$female + Gifu$pop$female + Shizuoka$pop$female + Aichi$pop$female
mfts_R4_female_rate = (Niigata_count_female + Toyama_count_female + Ishikawa_count_female + Fukui_count_female + 
                         Yamanashi_count_female + Nagano_count_female + Gifu_count_female + Shizuoka_count_female + Aichi_count_female)/mfts_R4_female_count

mfts_R4_male_count = Niigata$pop$male + Toyama$pop$male + Ishikawa$pop$male + Fukui$pop$male + 
  Yamanashi$pop$male + Nagano$pop$male + Gifu$pop$male + Shizuoka$pop$male + Aichi$pop$male
mfts_R4_male_rate = (Niigata_count_male + Toyama_count_male + Ishikawa_count_male + Fukui_count_male + 
                       Yamanashi_count_male + Nagano_count_male + Gifu_count_male + Shizuoka_count_male + Aichi_count_male)/mfts_R4_male_count

mfts_R4_total_count = Niigata$pop$total + Toyama$pop$total + Ishikawa$pop$total + Fukui$pop$total + 
  Yamanashi$pop$total + Nagano$pop$total + Gifu$pop$total + Shizuoka$pop$total + Aichi$pop$total
mfts_R4_total_rate = (Niigata_count_total + Toyama_count_total + Ishikawa_count_total + Fukui_count_total + 
                        Yamanashi_count_total + Nagano_count_total + Gifu_count_total + Shizuoka_count_total + Aichi_count_total)/mfts_R4_total_count

mfts_R4_rate_summary = cbind(rep(1973:2022,101), rep(0:100,50), as.numeric(mfts_R4_female_rate), as.numeric(mfts_R4_male_rate), as.numeric(mfts_R4_total_rate))
colnames(mfts_R4_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(mfts_R4_rate_summary, "mfts_R4_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

mfts_R4_count_summary = cbind(rep(1973:2022,101), rep(0:100,50), as.numeric(mfts_R4_female_count), as.numeric(mfts_R4_male_count), as.numeric(mfts_R4_total_count))
colnames(mfts_R4_count_summary) = c("Year","Age","Female","Male","Total")
write.table(mfts_R4_count_summary, "mfts_R4_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

mfts_R4 = read.demogdata("mfts_R4_rate.txt", "mfts_R4_count.txt", type="mortality", label = "R4", skip=0)
mfts_R4_smooth = smooth.demogdata(mfts_R4)

# R5

mfts_R5_female_count = Mie$pop$female + Shiga$pop$female + Kyoto$pop$female + Osaka$pop$female + 
  Hyogo$pop$female + Nara$pop$female + Wakayama$pop$female
mfts_R5_female_rate = (Mie_count_female + Shiga_count_female + Kyoto_count_female + Osaka_count_female +
                         Hyogo_count_female + Nara_count_female + Wakayama_count_female)/mfts_R5_female_count

mfts_R5_male_count = Mie$pop$male + Shiga$pop$male + Kyoto$pop$male + Osaka$pop$male + 
  Hyogo$pop$male + Nara$pop$male + Wakayama$pop$male
mfts_R5_male_rate = (Mie_count_male + Shiga_count_male + Kyoto_count_male + Osaka_count_male +
                       Hyogo_count_male + Nara_count_male + Wakayama_count_male)/mfts_R5_male_count

mfts_R5_total_count = Mie$pop$total + Shiga$pop$total + Kyoto$pop$total + Osaka$pop$total + 
  Hyogo$pop$total + Nara$pop$total + Wakayama$pop$total
mfts_R5_total_rate = (Mie_count_total + Shiga_count_total + Kyoto_count_total + Osaka_count_total +
                        Hyogo_count_total + Nara_count_total + Wakayama_count_total)/mfts_R5_total_count

mfts_R5_rate_summary = cbind(rep(1973:2022,101), rep(0:100,50), as.numeric(mfts_R5_female_rate), as.numeric(mfts_R5_male_rate), as.numeric(mfts_R5_total_rate))
colnames(mfts_R5_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(mfts_R5_rate_summary, "mfts_R5_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

mfts_R5_count_summary = cbind(rep(1973:2022,101), rep(0:100,50), as.numeric(mfts_R5_female_count), as.numeric(mfts_R5_male_count), as.numeric(mfts_R5_total_count))
colnames(mfts_R5_count_summary) = c("Year","Age","Female","Male","Total")
write.table(mfts_R5_count_summary, "mfts_R5_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

mfts_R5 = read.demogdata("mfts_R5_rate.txt", "mfts_R5_count.txt", type="mortality", label = "R5", skip=0)
mfts_R5_smooth = smooth.demogdata(mfts_R5)

# R6 (5 prefectures)

mfts_R6_female_count = Tottori$pop$female + Shimane$pop$female + Okayama$pop$female + Hiroshima$pop$female + Yamaguchi$pop$female
mfts_R6_female_rate = (Tottori_count_female + Shimane_count_female + Okayama_count_female + Hiroshima_count_female + Yamaguchi_count_female)/mfts_R6_female_count

mfts_R6_male_count = Tottori$pop$male + Shimane$pop$male + Okayama$pop$male + Hiroshima$pop$male + Yamaguchi$pop$male
mfts_R6_male_rate  = (Tottori_count_male + Shimane_count_male + Okayama_count_male + Hiroshima_count_male + Yamaguchi_count_male)/mfts_R6_male_count

mfts_R6_total_count = Tottori$pop$total + Shimane$pop$total + Okayama$pop$total + Hiroshima$pop$total + Yamaguchi$pop$total
mfts_R6_total_rate  = (Tottori_count_total + Shimane_count_total + Okayama_count_total + Hiroshima_count_total + Yamaguchi_count_total)/mfts_R6_total_count

mfts_R6_rate_summary = cbind(rep(1973:2022,101), rep(0:100,50), as.numeric(mfts_R6_female_rate), as.numeric(mfts_R6_male_rate), as.numeric(mfts_R6_total_rate))
colnames(mfts_R6_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(mfts_R6_rate_summary, "mfts_R6_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

mfts_R6_count_summary = cbind(rep(1973:2022,101), rep(0:100,50), as.numeric(mfts_R6_female_count), as.numeric(mfts_R6_male_count), as.numeric(mfts_R6_total_count))
colnames(mfts_R6_count_summary) = c("Year","Age","Female","Male","Total")
write.table(mfts_R6_count_summary, "mfts_R6_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

mfts_R6 = read.demogdata("mfts_R6_rate.txt", "mfts_R6_count.txt", type="mortality", label = "R6", skip=0)
mfts_R6_smooth = smooth.demogdata(mfts_R6)

# R7

mfts_R7_female_count = Tokushima$pop$female + Kagawa$pop$female + Ehime$pop$female + Kochi$pop$female
mfts_R7_female_rate  = (Tokushima_count_female + Kagawa_count_female + Ehime_count_female + Kochi_count_female)/mfts_R7_female_count

mfts_R7_male_count = Tokushima$pop$male + Kagawa$pop$male + Ehime$pop$male + Kochi$pop$male
mfts_R7_male_rate  = (Tokushima_count_male + Kagawa_count_male + Ehime_count_male + Kochi_count_male)/mfts_R7_male_count

mfts_R7_total_count = Tokushima$pop$total + Kagawa$pop$total + Ehime$pop$total + Kochi$pop$total
mfts_R7_total_rate  = (Tokushima_count_total + Kagawa_count_total + Ehime_count_total + Kochi_count_total)/mfts_R7_total_count

mfts_R7_rate_summary = cbind(rep(1973:2022,101), rep(0:100,50), as.numeric(mfts_R7_female_rate), as.numeric(mfts_R7_male_rate), as.numeric(mfts_R7_total_rate))
colnames(mfts_R7_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(mfts_R7_rate_summary, "mfts_R7_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

mfts_R7_count_summary = cbind(rep(1973:2022,101), rep(0:100,50), as.numeric(mfts_R7_female_count), as.numeric(mfts_R7_male_count), as.numeric(mfts_R7_total_count))
colnames(mfts_R7_count_summary) = c("Year","Age","Female","Male","Total")
write.table(mfts_R7_count_summary, "mfts_R7_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

mfts_R7 = read.demogdata("mfts_R7_rate.txt", "mfts_R7_count.txt", type="mortality", label = "R7", skip=0)
mfts_R7_smooth = smooth.demogdata(mfts_R7)

# R8

mfts_R8_female_count = Fukuoka$pop$female + Saga$pop$female + Nagasaki$pop$female + Kumamoto$pop$female + 
  Oita$pop$female + Miyazaki$pop$female + Kagoshima$pop$female + Okinawa$pop$female
mfts_R8_female_rate = (Fukuoka_count_female + Saga_count_female + Nagasaki_count_female + Kumamoto_count_female + Oita_count_female + Miyazaki_count_female + Kagoshima_count_female + Okinawa_count_female)/mfts_R8_female_count

mfts_R8_male_count = Fukuoka$pop$male + Saga$pop$male + Nagasaki$pop$male + Kumamoto$pop$male + 
  Oita$pop$male + Miyazaki$pop$male + Kagoshima$pop$male + Okinawa$pop$male
mfts_R8_male_rate = (Fukuoka_count_male + Saga_count_male + Nagasaki_count_male + Kumamoto_count_male + 
                       Oita_count_male + Miyazaki_count_male + Kagoshima_count_male + Okinawa_count_male)/mfts_R8_male_count

mfts_R8_total_count = Fukuoka$pop$total + Saga$pop$total + Nagasaki$pop$total + Kumamoto$pop$total + 
  Oita$pop$total + Miyazaki$pop$total + Kagoshima$pop$total + Okinawa$pop$total
mfts_R8_total_rate = (Fukuoka_count_total + Saga_count_total + Nagasaki_count_total + Kumamoto_count_total + 
                        Oita_count_total + Miyazaki_count_total + Kagoshima_count_total + Okinawa_count_total)/mfts_R8_total_count

mfts_R8_rate_summary = cbind(rep(1973:2022,101), rep(0:100,50), as.numeric(mfts_R8_female_rate), as.numeric(mfts_R8_male_rate), as.numeric(mfts_R8_total_rate))
colnames(mfts_R8_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(mfts_R8_rate_summary, "mfts_R8_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

mfts_R8_count_summary = cbind(rep(1973:2022,101), rep(0:100,50), as.numeric(mfts_R8_female_count), as.numeric(mfts_R8_male_count), as.numeric(mfts_R8_total_count))
colnames(mfts_R8_count_summary) = c("Year","Age","Female","Male","Total")
write.table(mfts_R8_count_summary, "mfts_R8_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

mfts_R8 = read.demogdata("mfts_R8_rate.txt", "mfts_R8_count.txt", type="mortality", label = "R8", skip=0)
mfts_R8_smooth = smooth.demogdata(mfts_R8)

########################
# collection of regions
########################

region = c("mfts_R1", "mfts_R2", "mfts_R3", "mfts_R4", "mfts_R5", "mfts_R6", "mfts_R7", "mfts_R8")

region_smooth = c("mfts_R1_smooth", "mfts_R2_smooth", "mfts_R3_smooth", "mfts_R4_smooth", "mfts_R5_smooth", 
                  "mfts_R6_smooth", "mfts_R7_smooth", "mfts_R8_smooth")

###################################################################################
# Combine state level total series (unsmoothed) into a demography time series file
###################################################################################

total_comb = total_comb_pop =  matrix(NA, 101*50, 47)
for(iw in 2:48)
{
  total_comb[,iw-1] = as.numeric(get(state[iw])$rate$total)
  total_comb_pop[,iw-1] = as.numeric(get(state[iw])$pop$total)
}
total_comb_v2 = cbind(rep(1973:2022, each=101), rep(0:100, 50), total_comb)
total_comb_pop_v2 = cbind(rep(1973:2022, each=101), rep(0:100, 50), total_comb_pop)
colnames(total_comb_v2) = colnames(total_comb_pop_v2) = c("Year", "Age", state[2:48])

###################################################################################
# Combine region level total series (unsmoothed) into a demography time series file
###################################################################################

total_region_comb = total_region_comb_pop = matrix(NA, 101*50, 8)
for(iw in 1:8)
{
  total_region_comb[,iw] = as.numeric(get(region[iw])$rate$total)
  total_region_comb_pop[,iw] = as.numeric(get(region[iw])$pop$total)
}
total_region_comb_v2 = cbind(rep(1973:2022, each=101), rep(0:100, 50), total_region_comb)
total_region_comb_pop_v2 = cbind(rep(1973:2022, each=101), rep(0:100, 50), total_region_comb_pop)
colnames(total_region_comb_v2) = colnames(total_region_comb_pop_v2) = c("Year", "Age", region)

