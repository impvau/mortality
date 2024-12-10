# Shared settings for Japanese mortality data processing

# Prefecture names and their corresponding indices
prefectures <- c("Japan", "Hokkaido", "Aomori", "Iwate", "Miyagi", "Akita", "Yamagata", "Fukushima",
                 "Ibaraki", "Tochigi", "Gunma", "Saitama", "Chiba", "Tokyo", "Kanagawa", "Niigata",
                 "Toyama", "Ishikawa", "Fukui", "Yamanashi", "Nagano", "Gifu", "Shizuoka", "Aichi",
                 "Mie", "Shiga", "Kyoto", "Osaka", "Hyogo", "Nara", "Wakayama", "Tottori", "Shimane",
                 "Okayama", "Hiroshima", "Yamaguchi", "Tokushima", "Kagawa", "Ehime", "Kochi",
                 "Fukuoka", "Saga", "Nagasaki", "Kumamoto", "Oita", "Miyazaki", "Kagoshima", "Okinawa")

num_prefs <- length(prefectures)

ind_prefs <- sprintf("%02d", 0:(length(prefectures) - 1))  # Indices: "00", "01", "02", ...

prefectures_smooth <- paste(prefectures, "_smooth", sep = "")


# Year range for data processing
year_range <- 1973:2022

model_names <- c("lc_sum", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", 
                 "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "M_fdm", "pr")
                 