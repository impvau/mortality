###############################################
# Benchmark method of Shang and Hyndamn 2017
##############################################

library(ftsa)
library(demography)

# state level forecasts

ind_state_forc_female = c("ind_Japan_forc_female", "ind_Hokkaido_forc_female", "ind_Aomori_forc_female", 
                          "ind_Iwate_forc_female", "ind_Miyagi_forc_female", "ind_Akita_forc_female", 
                          "ind_Yamagata_forc_female", "ind_Fukushima_forc_female", "ind_Ibaraki_forc_female", 
                          "ind_Tochigi_forc_female", "ind_Gunma_forc_female", "ind_Saitama_forc_female", 
                          "ind_Chiba_forc_female", "ind_Tokyo_forc_female", "ind_Kanagawa_forc_female",
                          "ind_Niigata_forc_female", "ind_Toyama_forc_female", "ind_Ishikawa_forc_female", 
                          "ind_Fukui_forc_female", "ind_Yamanashi_forc_female", "ind_Nagano_forc_female", 
                          "ind_Gifu_forc_female", "ind_Shizuoka_forc_female", "ind_Aichi_forc_female",
                          "ind_Mie_forc_female", "ind_Shiga_forc_female", "ind_Kyoto_forc_female", 
                          "ind_Osaka_forc_female", "ind_Hyogo_forc_female", "ind_Nara_forc_female", 
                          "ind_Wakayama_forc_female", "ind_Tottori_forc_female", "ind_Shimane_forc_female",
                          "ind_Okayama_forc_female", "ind_Hiroshima_forc_female", "ind_Yamaguchi_forc_female", 
                          "ind_Tokushima_forc_female", "ind_Kagawa_forc_female", "ind_Ehime_forc_female", 
                          "ind_Kochi_forc_female", "ind_Fukuoka_forc_female", "ind_Saga_forc_female", 
                          "ind_Nagasaki_forc_female", "ind_Kumamoto_forc_female", "ind_Oita_forc_female", 
                          "ind_Miyazaki_forc_female", "ind_Kagoshima_forc_female", "ind_Okinawa_forc_female")

ind_state_forc_male = c("ind_Japan_forc_male", "ind_Hokkaido_forc_male", "ind_Aomori_forc_male", 
                        "ind_Iwate_forc_male", "ind_Miyagi_forc_male", "ind_Akita_forc_male", 
                        "ind_Yamagata_forc_male", "ind_Fukushima_forc_male", "ind_Ibaraki_forc_male", 
                        "ind_Tochigi_forc_male", "ind_Gunma_forc_male", "ind_Saitama_forc_male", 
                        "ind_Chiba_forc_male", "ind_Tokyo_forc_male", "ind_Kanagawa_forc_male",
                        "ind_Niigata_forc_male", "ind_Toyama_forc_male", "ind_Ishikawa_forc_male", 
                        "ind_Fukui_forc_male", "ind_Yamanashi_forc_male", "ind_Nagano_forc_male", 
                        "ind_Gifu_forc_male", "ind_Shizuoka_forc_male", "ind_Aichi_forc_male",
                        "ind_Mie_forc_male", "ind_Shiga_forc_male", "ind_Kyoto_forc_male", 
                        "ind_Osaka_forc_male", "ind_Hyogo_forc_male", "ind_Nara_forc_male", 
                        "ind_Wakayama_forc_male", "ind_Tottori_forc_male", "ind_Shimane_forc_male",
                        "ind_Okayama_forc_male", "ind_Hiroshima_forc_male", "ind_Yamaguchi_forc_male", 
                        "ind_Tokushima_forc_male", "ind_Kagawa_forc_male", "ind_Ehime_forc_male", 
                        "ind_Kochi_forc_male", "ind_Fukuoka_forc_male", "ind_Saga_forc_male", 
                        "ind_Nagasaki_forc_male", "ind_Kumamoto_forc_male", "ind_Oita_forc_male", 
                        "ind_Miyazaki_forc_male", "ind_Kagoshima_forc_male", "ind_Okinawa_forc_male")

ind_state_forc_total = c("ind_Japan_forc_total", "ind_Hokkaido_forc_total", "ind_Aomori_forc_total", 
                         "ind_Iwate_forc_total", "ind_Miyagi_forc_total", "ind_Akita_forc_total", 
                         "ind_Yamagata_forc_total", "ind_Fukushima_forc_total", "ind_Ibaraki_forc_total", 
                         "ind_Tochigi_forc_total", "ind_Gunma_forc_total", "ind_Saitama_forc_total", 
                         "ind_Chiba_forc_total", "ind_Tokyo_forc_total", "ind_Kanagawa_forc_total",
                         "ind_Niigata_forc_total", "ind_Toyama_forc_total", "ind_Ishikawa_forc_total", 
                         "ind_Fukui_forc_total", "ind_Yamanashi_forc_total", "ind_Nagano_forc_total", 
                         "ind_Gifu_forc_total", "ind_Shizuoka_forc_total", "ind_Aichi_forc_total",
                         "ind_Mie_forc_total", "ind_Shiga_forc_total", "ind_Kyoto_forc_total", 
                         "ind_Osaka_forc_total", "ind_Hyogo_forc_total", "ind_Nara_forc_total", 
                         "ind_Wakayama_forc_total", "ind_Tottori_forc_total", "ind_Shimane_forc_total",
                         "ind_Okayama_forc_total", "ind_Hiroshima_forc_total", "ind_Yamaguchi_forc_total", 
                         "ind_Tokushima_forc_total", "ind_Kagawa_forc_total", "ind_Ehime_forc_total", 
                         "ind_Kochi_forc_total", "ind_Fukuoka_forc_total", "ind_Saga_forc_total", 
                         "ind_Nagasaki_forc_total", "ind_Kumamoto_forc_total", "ind_Oita_forc_total", 
                         "ind_Miyazaki_forc_total", "ind_Kagoshima_forc_total", "ind_Okinawa_forc_total")

# Residual 

ind_state_train_residual_female = c("ind_Japan_train_residual_female", "ind_Hokkaido_train_residual_female", "ind_Aomori_train_residual_female", 
                                    "ind_Iwate_train_residual_female", "ind_Miyagi_train_residual_female", "ind_Akita_train_residual_female", 
                                    "ind_Yamagata_train_residual_female", "ind_Fukushima_train_residual_female", "ind_Ibaraki_train_residual_female", 
                                    "ind_Tochigi_train_residual_female", "ind_Gunma_train_residual_female", "ind_Saitama_train_residual_female", 
                                    "ind_Chiba_train_residual_female", "ind_Tokyo_train_residual_female", "ind_Kanagawa_train_residual_female",
                                    "ind_Niigata_train_residual_female", "ind_Toyama_train_residual_female", "ind_Ishikawa_train_residual_female", 
                                    "ind_Fukui_train_residual_female", "ind_Yamanashi_train_residual_female", "ind_Nagano_train_residual_female", 
                                    "ind_Gifu_train_residual_female", "ind_Shizuoka_train_residual_female", "ind_Aichi_train_residual_female",
                                    "ind_Mie_train_residual_female", "ind_Shiga_train_residual_female", "ind_Kyoto_train_residual_female", 
                                    "ind_Osaka_train_residual_female", "ind_Hyogo_train_residual_female", "ind_Nara_train_residual_female", 
                                    "ind_Wakayama_train_residual_female", "ind_Tottori_train_residual_female", "ind_Shimane_train_residual_female",
                                    "ind_Okayama_train_residual_female", "ind_Hiroshima_train_residual_female", "ind_Yamaguchi_train_residual_female", 
                                    "ind_Tokushima_train_residual_female", "ind_Kagawa_train_residual_female", "ind_Ehime_train_residual_female", 
                                    "ind_Kochi_train_residual_female", "ind_Fukuoka_train_residual_female", "ind_Saga_train_residual_female", 
                                    "ind_Nagasaki_train_residual_female", "ind_Kumamoto_train_residual_female", "ind_Oita_train_residual_female", 
                                    "ind_Miyazaki_train_residual_female", "ind_Kagoshima_train_residual_female", "ind_Okinawa_train_residual_female")

ind_state_train_residual_male = c("ind_Japan_train_residual_male", "ind_Hokkaido_train_residual_male", "ind_Aomori_train_residual_male", 
                                  "ind_Iwate_train_residual_male", "ind_Miyagi_train_residual_male", "ind_Akita_train_residual_male", 
                                  "ind_Yamagata_train_residual_male", "ind_Fukushima_train_residual_male", "ind_Ibaraki_train_residual_male", 
                                  "ind_Tochigi_train_residual_male", "ind_Gunma_train_residual_male", "ind_Saitama_train_residual_male", 
                                  "ind_Chiba_train_residual_male", "ind_Tokyo_train_residual_male", "ind_Kanagawa_train_residual_male",
                                  "ind_Niigata_train_residual_male", "ind_Toyama_train_residual_male", "ind_Ishikawa_train_residual_male", 
                                  "ind_Fukui_train_residual_male", "ind_Yamanashi_train_residual_male", "ind_Nagano_train_residual_male", 
                                  "ind_Gifu_train_residual_male", "ind_Shizuoka_train_residual_male", "ind_Aichi_train_residual_male",
                                  "ind_Mie_train_residual_male", "ind_Shiga_train_residual_male", "ind_Kyoto_train_residual_male", 
                                  "ind_Osaka_train_residual_male", "ind_Hyogo_train_residual_male", "ind_Nara_train_residual_male", 
                                  "ind_Wakayama_train_residual_male", "ind_Tottori_train_residual_male", "ind_Shimane_train_residual_male",
                                  "ind_Okayama_train_residual_male", "ind_Hiroshima_train_residual_male", "ind_Yamaguchi_train_residual_male", 
                                  "ind_Tokushima_train_residual_male", "ind_Kagawa_train_residual_male", "ind_Ehime_train_residual_male", 
                                  "ind_Kochi_train_residual_male", "ind_Fukuoka_train_residual_male", "ind_Saga_train_residual_male", 
                                  "ind_Nagasaki_train_residual_male", "ind_Kumamoto_train_residual_male", "ind_Oita_train_residual_male", 
                                  "ind_Miyazaki_train_residual_male", "ind_Kagoshima_train_residual_male", "ind_Okinawa_train_residual_male")

ind_state_train_residual_total = c("ind_Japan_train_residual_total", "ind_Hokkaido_train_residual_total", "ind_Aomori_train_residual_total", 
                                   "ind_Iwate_train_residual_total", "ind_Miyagi_train_residual_total", "ind_Akita_train_residual_total", 
                                   "ind_Yamagata_train_residual_total", "ind_Fukushima_train_residual_total", "ind_Ibaraki_train_residual_total", 
                                   "ind_Tochigi_train_residual_total", "ind_Gunma_train_residual_total", "ind_Saitama_train_residual_total", 
                                   "ind_Chiba_train_residual_total", "ind_Tokyo_train_residual_total", "ind_Kanagawa_train_residual_total",
                                   "ind_Niigata_train_residual_total", "ind_Toyama_train_residual_total", "ind_Ishikawa_train_residual_total", 
                                   "ind_Fukui_train_residual_total", "ind_Yamanashi_train_residual_total", "ind_Nagano_train_residual_total", 
                                   "ind_Gifu_train_residual_total", "ind_Shizuoka_train_residual_total", "ind_Aichi_train_residual_total",
                                   "ind_Mie_train_residual_total", "ind_Shiga_train_residual_total", "ind_Kyoto_train_residual_total", 
                                   "ind_Osaka_train_residual_total", "ind_Hyogo_train_residual_total", "ind_Nara_train_residual_total", 
                                   "ind_Wakayama_train_residual_total", "ind_Tottori_train_residual_total", "ind_Shimane_train_residual_total",
                                   "ind_Okayama_train_residual_total", "ind_Hiroshima_train_residual_total", "ind_Yamaguchi_train_residual_total", 
                                   "ind_Tokushima_train_residual_total", "ind_Kagawa_train_residual_total", "ind_Ehime_train_residual_total", 
                                   "ind_Kochi_train_residual_total", "ind_Fukuoka_train_residual_total", "ind_Saga_train_residual_total", 
                                   "ind_Nagasaki_train_residual_total", "ind_Kumamoto_train_residual_total", "ind_Oita_train_residual_total", 
                                   "ind_Miyazaki_train_residual_total", "ind_Kagoshima_train_residual_total", "ind_Okinawa_train_residual_total")


# Mean Errors at state level

ind_state_me_female = c("ind_Japan_me_female", "ind_Hokkaido_me_female", "ind_Aomori_me_female", 
                        "ind_Iwate_me_female", "ind_Miyagi_me_female", "ind_Akita_me_female", 
                        "ind_Yamagata_me_female", "ind_Fukushima_me_female", "ind_Ibaraki_me_female", 
                        "ind_Tochigi_me_female", "ind_Gunma_me_female", "ind_Saitama_me_female", 
                        "ind_Chiba_me_female", "ind_Tokyo_me_female", "ind_Kanagawa_me_female",
                        "ind_Niigata_me_female", "ind_Toyama_me_female", "ind_Ishikawa_me_female", 
                        "ind_Fukui_me_female", "ind_Yamanashi_me_female", "ind_Nagano_me_female", 
                        "ind_Gifu_me_female", "ind_Shizuoka_me_female", "ind_Aichi_me_female",
                        "ind_Mie_me_female", "ind_Shiga_me_female", "ind_Kyoto_me_female", 
                        "ind_Osaka_me_female", "ind_Hyogo_me_female", "ind_Nara_me_female", 
                        "ind_Wakayama_me_female", "ind_Tottori_me_female", "ind_Shimane_me_female",
                        "ind_Okayama_me_female", "ind_Hiroshima_me_female", "ind_Yamaguchi_me_female", 
                        "ind_Tokushima_me_female", "ind_Kagawa_me_female", "ind_Ehime_me_female", 
                        "ind_Kochi_me_female", "ind_Fukuoka_me_female", "ind_Saga_me_female", 
                        "ind_Nagasaki_me_female", "ind_Kumamoto_me_female", "ind_Oita_me_female", 
                        "ind_Miyazaki_me_female", "ind_Kagoshima_me_female", "ind_Okinawa_me_female")

ind_state_me_male = c("ind_Japan_me_male", "ind_Hokkaido_me_male", "ind_Aomori_me_male", 
                      "ind_Iwate_me_male", "ind_Miyagi_me_male", "ind_Akita_me_male", 
                      "ind_Yamagata_me_male", "ind_Fukushima_me_male", "ind_Ibaraki_me_male", 
                      "ind_Tochigi_me_male", "ind_Gunma_me_male", "ind_Saitama_me_male", 
                      "ind_Chiba_me_male", "ind_Tokyo_me_male", "ind_Kanagawa_me_male",
                      "ind_Niigata_me_male", "ind_Toyama_me_male", "ind_Ishikawa_me_male", 
                      "ind_Fukui_me_male", "ind_Yamanashi_me_male", "ind_Nagano_me_male", 
                      "ind_Gifu_me_male", "ind_Shizuoka_me_male", "ind_Aichi_me_male",
                      "ind_Mie_me_male", "ind_Shiga_me_male", "ind_Kyoto_me_male", 
                      "ind_Osaka_me_male", "ind_Hyogo_me_male", "ind_Nara_me_male", 
                      "ind_Wakayama_me_male", "ind_Tottori_me_male", "ind_Shimane_me_male",
                      "ind_Okayama_me_male", "ind_Hiroshima_me_male", "ind_Yamaguchi_me_male", 
                      "ind_Tokushima_me_male", "ind_Kagawa_me_male", "ind_Ehime_me_male", 
                      "ind_Kochi_me_male", "ind_Fukuoka_me_male", "ind_Saga_me_male", 
                      "ind_Nagasaki_me_male", "ind_Kumamoto_me_male", "ind_Oita_me_male", 
                      "ind_Miyazaki_me_male", "ind_Kagoshima_me_male", "ind_Okinawa_me_male")


ind_state_me_total = c("ind_Japan_me_total", "ind_Hokkaido_me_total", "ind_Aomori_me_total", 
                       "ind_Iwate_me_total", "ind_Miyagi_me_total", "ind_Akita_me_total", 
                       "ind_Yamagata_me_total", "ind_Fukushima_me_total", "ind_Ibaraki_me_total", 
                       "ind_Tochigi_me_total", "ind_Gunma_me_total", "ind_Saitama_me_total", 
                       "ind_Chiba_me_total", "ind_Tokyo_me_total", "ind_Kanagawa_me_total",
                       "ind_Niigata_me_total", "ind_Toyama_me_total", "ind_Ishikawa_me_total", 
                       "ind_Fukui_me_total", "ind_Yamanashi_me_total", "ind_Nagano_me_total", 
                       "ind_Gifu_me_total", "ind_Shizuoka_me_total", "ind_Aichi_me_total",
                       "ind_Mie_me_total", "ind_Shiga_me_total", "ind_Kyoto_me_total", 
                       "ind_Osaka_me_total", "ind_Hyogo_me_total", "ind_Nara_me_total", 
                       "ind_Wakayama_me_total", "ind_Tottori_me_total", "ind_Shimane_me_total",
                       "ind_Okayama_me_total", "ind_Hiroshima_me_total", "ind_Yamaguchi_me_total", 
                       "ind_Tokushima_me_total", "ind_Kagawa_me_total", "ind_Ehime_me_total", 
                       "ind_Kochi_me_total", "ind_Fukuoka_me_total", "ind_Saga_me_total", 
                       "ind_Nagasaki_me_total", "ind_Kumamoto_me_total", "ind_Oita_me_total", 
                       "ind_Miyazaki_me_total", "ind_Kagoshima_me_total", "ind_Okinawa_me_total")



# Mean Absolute Errors at state level 

ind_state_mae_female = c("ind_Japan_mae_female", "ind_Hokkaido_mae_female", "ind_Aomori_mae_female", 
                         "ind_Iwate_mae_female", "ind_Miyagi_mae_female", "ind_Akita_mae_female", 
                         "ind_Yamagata_mae_female", "ind_Fukushima_mae_female", "ind_Ibaraki_mae_female", 
                         "ind_Tochigi_mae_female", "ind_Gunma_mae_female", "ind_Saitama_mae_female", 
                         "ind_Chiba_mae_female", "ind_Tokyo_mae_female", "ind_Kanagawa_mae_female",
                         "ind_Niigata_mae_female", "ind_Toyama_mae_female", "ind_Ishikawa_mae_female", 
                         "ind_Fukui_mae_female", "ind_Yamanashi_mae_female", "ind_Nagano_mae_female", 
                         "ind_Gifu_mae_female", "ind_Shizuoka_mae_female", "ind_Aichi_mae_female",
                         "ind_Mie_mae_female", "ind_Shiga_mae_female", "ind_Kyoto_mae_female", 
                         "ind_Osaka_mae_female", "ind_Hyogo_mae_female", "ind_Nara_mae_female", 
                         "ind_Wakayama_mae_female", "ind_Tottori_mae_female", "ind_Shimane_mae_female",
                         "ind_Okayama_mae_female", "ind_Hiroshima_mae_female", "ind_Yamaguchi_mae_female", 
                         "ind_Tokushima_mae_female", "ind_Kagawa_mae_female", "ind_Ehime_mae_female", 
                         "ind_Kochi_mae_female", "ind_Fukuoka_mae_female", "ind_Saga_mae_female", 
                         "ind_Nagasaki_mae_female", "ind_Kumamoto_mae_female", "ind_Oita_mae_female", 
                         "ind_Miyazaki_mae_female", "ind_Kagoshima_mae_female", "ind_Okinawa_mae_female")

ind_state_mae_male = c("ind_Japan_mae_male", "ind_Hokkaido_mae_male", "ind_Aomori_mae_male", 
                       "ind_Iwate_mae_male", "ind_Miyagi_mae_male", "ind_Akita_mae_male", 
                       "ind_Yamagata_mae_male", "ind_Fukushima_mae_male", "ind_Ibaraki_mae_male", 
                       "ind_Tochigi_mae_male", "ind_Gunma_mae_male", "ind_Saitama_mae_male", 
                       "ind_Chiba_mae_male", "ind_Tokyo_mae_male", "ind_Kanagawa_mae_male",
                       "ind_Niigata_mae_male", "ind_Toyama_mae_male", "ind_Ishikawa_mae_male", 
                       "ind_Fukui_mae_male", "ind_Yamanashi_mae_male", "ind_Nagano_mae_male", 
                       "ind_Gifu_mae_male", "ind_Shizuoka_mae_male", "ind_Aichi_mae_male",
                       "ind_Mie_mae_male", "ind_Shiga_mae_male", "ind_Kyoto_mae_male", 
                       "ind_Osaka_mae_male", "ind_Hyogo_mae_male", "ind_Nara_mae_male", 
                       "ind_Wakayama_mae_male", "ind_Tottori_mae_male", "ind_Shimane_mae_male",
                       "ind_Okayama_mae_male", "ind_Hiroshima_mae_male", "ind_Yamaguchi_mae_male", 
                       "ind_Tokushima_mae_male", "ind_Kagawa_mae_male", "ind_Ehime_mae_male", 
                       "ind_Kochi_mae_male", "ind_Fukuoka_mae_male", "ind_Saga_mae_male", 
                       "ind_Nagasaki_mae_male", "ind_Kumamoto_mae_male", "ind_Oita_mae_male", 
                       "ind_Miyazaki_mae_male", "ind_Kagoshima_mae_male", "ind_Okinawa_mae_male")


ind_state_mae_total = c("ind_Japan_mae_total", "ind_Hokkaido_mae_total", "ind_Aomori_mae_total", 
                        "ind_Iwate_mae_total", "ind_Miyagi_mae_total", "ind_Akita_mae_total", 
                        "ind_Yamagata_mae_total", "ind_Fukushima_mae_total", "ind_Ibaraki_mae_total", 
                        "ind_Tochigi_mae_total", "ind_Gunma_mae_total", "ind_Saitama_mae_total", 
                        "ind_Chiba_mae_total", "ind_Tokyo_mae_total", "ind_Kanagawa_mae_total",
                        "ind_Niigata_mae_total", "ind_Toyama_mae_total", "ind_Ishikawa_mae_total", 
                        "ind_Fukui_mae_total", "ind_Yamanashi_mae_total", "ind_Nagano_mae_total", 
                        "ind_Gifu_mae_total", "ind_Shizuoka_mae_total", "ind_Aichi_mae_total",
                        "ind_Mie_mae_total", "ind_Shiga_mae_total", "ind_Kyoto_mae_total", 
                        "ind_Osaka_mae_total", "ind_Hyogo_mae_total", "ind_Nara_mae_total", 
                        "ind_Wakayama_mae_total", "ind_Tottori_mae_total", "ind_Shimane_mae_total",
                        "ind_Okayama_mae_total", "ind_Hiroshima_mae_total", "ind_Yamaguchi_mae_total", 
                        "ind_Tokushima_mae_total", "ind_Kagawa_mae_total", "ind_Ehime_mae_total", 
                        "ind_Kochi_mae_total", "ind_Fukuoka_mae_total", "ind_Saga_mae_total", 
                        "ind_Nagasaki_mae_total", "ind_Kumamoto_mae_total", "ind_Oita_mae_total", 
                        "ind_Miyazaki_mae_total", "ind_Kagoshima_mae_total", "ind_Okinawa_mae_total")


# Root Mean Square Errors at state level

ind_state_rmse_female = c("ind_Japan_rmse_female", "ind_Hokkaido_rmse_female", "ind_Aomori_rmse_female", 
                          "ind_Iwate_rmse_female", "ind_Miyagi_rmse_female", "ind_Akita_rmse_female", 
                          "ind_Yamagata_rmse_female", "ind_Fukushima_rmse_female", "ind_Ibaraki_rmse_female", 
                          "ind_Tochigi_rmse_female", "ind_Gunma_rmse_female", "ind_Saitama_rmse_female", 
                          "ind_Chiba_rmse_female", "ind_Tokyo_rmse_female", "ind_Kanagawa_rmse_female",
                          "ind_Niigata_rmse_female", "ind_Toyama_rmse_female", "ind_Ishikawa_rmse_female", 
                          "ind_Fukui_rmse_female", "ind_Yamanashi_rmse_female", "ind_Nagano_rmse_female", 
                          "ind_Gifu_rmse_female", "ind_Shizuoka_rmse_female", "ind_Aichi_rmse_female",
                          "ind_Mie_rmse_female", "ind_Shiga_rmse_female", "ind_Kyoto_rmse_female", 
                          "ind_Osaka_rmse_female", "ind_Hyogo_rmse_female", "ind_Nara_rmse_female", 
                          "ind_Wakayama_rmse_female", "ind_Tottori_rmse_female", "ind_Shimane_rmse_female",
                          "ind_Okayama_rmse_female", "ind_Hiroshima_rmse_female", "ind_Yamaguchi_rmse_female", 
                          "ind_Tokushima_rmse_female", "ind_Kagawa_rmse_female", "ind_Ehime_rmse_female", 
                          "ind_Kochi_rmse_female", "ind_Fukuoka_rmse_female", "ind_Saga_rmse_female", 
                          "ind_Nagasaki_rmse_female", "ind_Kumamoto_rmse_female", "ind_Oita_rmse_female", 
                          "ind_Miyazaki_rmse_female", "ind_Kagoshima_rmse_female", "ind_Okinawa_rmse_female")

ind_state_rmse_male = c("ind_Japan_rmse_male", "ind_Hokkaido_rmse_male", "ind_Aomori_rmse_male", 
                        "ind_Iwate_rmse_male", "ind_Miyagi_rmse_male", "ind_Akita_rmse_male", 
                        "ind_Yamagata_rmse_male", "ind_Fukushima_rmse_male", "ind_Ibaraki_rmse_male", 
                        "ind_Tochigi_rmse_male", "ind_Gunma_rmse_male", "ind_Saitama_rmse_male", 
                        "ind_Chiba_rmse_male", "ind_Tokyo_rmse_male", "ind_Kanagawa_rmse_male",
                        "ind_Niigata_rmse_male", "ind_Toyama_rmse_male", "ind_Ishikawa_rmse_male", 
                        "ind_Fukui_rmse_male", "ind_Yamanashi_rmse_male", "ind_Nagano_rmse_male", 
                        "ind_Gifu_rmse_male", "ind_Shizuoka_rmse_male", "ind_Aichi_rmse_male",
                        "ind_Mie_rmse_male", "ind_Shiga_rmse_male", "ind_Kyoto_rmse_male", 
                        "ind_Osaka_rmse_male", "ind_Hyogo_rmse_male", "ind_Nara_rmse_male", 
                        "ind_Wakayama_rmse_male", "ind_Tottori_rmse_male", "ind_Shimane_rmse_male",
                        "ind_Okayama_rmse_male", "ind_Hiroshima_rmse_male", "ind_Yamaguchi_rmse_male", 
                        "ind_Tokushima_rmse_male", "ind_Kagawa_rmse_male", "ind_Ehime_rmse_male", 
                        "ind_Kochi_rmse_male", "ind_Fukuoka_rmse_male", "ind_Saga_rmse_male", 
                        "ind_Nagasaki_rmse_male", "ind_Kumamoto_rmse_male", "ind_Oita_rmse_male", 
                        "ind_Miyazaki_rmse_male", "ind_Kagoshima_rmse_male", "ind_Okinawa_rmse_male")

ind_state_rmse_total = c("ind_Japan_rmse_total", "ind_Hokkaido_rmse_total", "ind_Aomori_rmse_total", 
                         "ind_Iwate_rmse_total", "ind_Miyagi_rmse_total", "ind_Akita_rmse_total", 
                         "ind_Yamagata_rmse_total", "ind_Fukushima_rmse_total", "ind_Ibaraki_rmse_total", 
                         "ind_Tochigi_rmse_total", "ind_Gunma_rmse_total", "ind_Saitama_rmse_total", 
                         "ind_Chiba_rmse_total", "ind_Tokyo_rmse_total", "ind_Kanagawa_rmse_total",
                         "ind_Niigata_rmse_total", "ind_Toyama_rmse_total", "ind_Ishikawa_rmse_total", 
                         "ind_Fukui_rmse_total", "ind_Yamanashi_rmse_total", "ind_Nagano_rmse_total", 
                         "ind_Gifu_rmse_total", "ind_Shizuoka_rmse_total", "ind_Aichi_rmse_total",
                         "ind_Mie_rmse_total", "ind_Shiga_rmse_total", "ind_Kyoto_rmse_total", 
                         "ind_Osaka_rmse_total", "ind_Hyogo_rmse_total", "ind_Nara_rmse_total", 
                         "ind_Wakayama_rmse_total", "ind_Tottori_rmse_total", "ind_Shimane_rmse_total",
                         "ind_Okayama_rmse_total", "ind_Hiroshima_rmse_total", "ind_Yamaguchi_rmse_total", 
                         "ind_Tokushima_rmse_total", "ind_Kagawa_rmse_total", "ind_Ehime_rmse_total", 
                         "ind_Kochi_rmse_total", "ind_Fukuoka_rmse_total", "ind_Saga_rmse_total", 
                         "ind_Nagasaki_rmse_total", "ind_Kumamoto_rmse_total", "ind_Oita_rmse_total", 
                         "ind_Miyazaki_rmse_total", "ind_Kagoshima_rmse_total", "ind_Okinawa_rmse_total")


# region level forecasts

ind_region_forc_female = c("ind_region_R1_forc_female", "ind_region_R2_forc_female", "ind_region_R3_forc_female", 
                           "ind_region_R4_forc_female", "ind_region_R5_forc_female", "ind_region_R6_forc_female", 
                           "ind_region_R7_forc_female", "ind_region_R8_forc_female")

ind_region_forc_male = c("ind_region_R1_forc_male", "ind_region_R2_forc_male", "ind_region_R3_forc_male", 
                         "ind_region_R4_forc_male", "ind_region_R5_forc_male", "ind_region_R6_forc_male", 
                         "ind_region_R7_forc_male", "ind_region_R8_forc_male")

ind_region_forc_total = c("ind_region_R1_forc_total", "ind_region_R2_forc_total", "ind_region_R3_forc_total", 
                          "ind_region_R4_forc_total", "ind_region_R5_forc_total", "ind_region_R6_forc_total", 
                          "ind_region_R7_forc_total", "ind_region_R8_forc_total")


# Mean Errors at region level

ind_region_me_female = c("ind_region_R1_me_female", "ind_region_R2_me_female", "ind_region_R3_me_female", 
                         "ind_region_R4_me_female", "ind_region_R5_me_female", "ind_region_R6_me_female", 
                         "ind_region_R7_me_female", "ind_region_R8_me_female")

ind_region_me_male = c("ind_region_R1_me_male", "ind_region_R2_me_male", "ind_region_R3_me_male", 
                       "ind_region_R4_me_male", "ind_region_R5_me_male", "ind_region_R6_me_male", 
                       "ind_region_R7_me_male", "ind_region_R8_me_male")

ind_region_me_total = c("ind_region_R1_me_total", "ind_region_R2_me_total", "ind_region_R3_me_total", 
                        "ind_region_R4_me_total", "ind_region_R5_me_total", "ind_region_R6_me_total", 
                        "ind_region_R7_me_total", "ind_region_R8_me_total")


# Mean Absolute Errors at region level

ind_region_mae_female = c("ind_region_R1_mae_female", "ind_region_R2_mae_female", "ind_region_R3_mae_female", 
                          "ind_region_R4_mae_female", "ind_region_R5_mae_female", "ind_region_R6_mae_female", 
                          "ind_region_R7_mae_female", "ind_region_R8_mae_female")

ind_region_mae_male = c("ind_region_R1_mae_male", "ind_region_R2_mae_male", "ind_region_R3_mae_male", 
                        "ind_region_R4_mae_male", "ind_region_R5_mae_male", "ind_region_R6_mae_male", 
                        "ind_region_R7_mae_male", "ind_region_R8_mae_male")

ind_region_mae_total = c("ind_region_R1_mae_total", "ind_region_R2_mae_total", "ind_region_R3_mae_total", 
                         "ind_region_R4_mae_total", "ind_region_R5_mae_total", "ind_region_R6_mae_total", 
                         "ind_region_R7_mae_total", "ind_region_R8_mae_total")


# Root Mean Square Errors at region level

ind_region_rmse_female = c("ind_region_R1_rmse_female", "ind_region_R2_rmse_female", "ind_region_R3_rmse_female", 
                           "ind_region_R4_rmse_female", "ind_region_R5_rmse_female", "ind_region_R6_rmse_female", 
                           "ind_region_R7_rmse_female", "ind_region_R8_rmse_female")

ind_region_rmse_male = c("ind_region_R1_rmse_male", "ind_region_R2_rmse_male", "ind_region_R3_rmse_male", 
                         "ind_region_R4_rmse_male", "ind_region_R5_rmse_male", "ind_region_R6_rmse_male", 
                         "ind_region_R7_rmse_male", "ind_region_R8_rmse_male")

ind_region_rmse_total = c("ind_region_R1_rmse_total", "ind_region_R2_rmse_total", "ind_region_R3_rmse_total", 
                          "ind_region_R4_rmse_total", "ind_region_R5_rmse_total", "ind_region_R6_rmse_total", 
                          "ind_region_R7_rmse_total", "ind_region_R8_rmse_total")

# region level training residuals

ind_region_train_residual_female = c("ind_region_R1_train_residual_female", "ind_region_R2_train_residual_female", "ind_region_R3_train_residual_female", 
                                     "ind_region_R4_train_residual_female", "ind_region_R5_train_residual_female", "ind_region_R6_train_residual_female", 
                                     "ind_region_R7_train_residual_female", "ind_region_R8_train_residual_female")

ind_region_train_residual_male = c("ind_region_R1_train_residual_male", "ind_region_R2_train_residual_male", "ind_region_R3_train_residual_male", 
                                   "ind_region_R4_train_residual_male", "ind_region_R5_train_residual_male", "ind_region_R6_train_residual_male", 
                                   "ind_region_R7_train_residual_male", "ind_region_R8_train_residual_male")

ind_region_train_residual_total = c("ind_region_R1_train_residual_total", "ind_region_R2_train_residual_total", "ind_region_R3_train_residual_total", 
                                    "ind_region_R4_train_residual_total", "ind_region_R5_train_residual_total", "ind_region_R6_train_residual_total", 
                                    "ind_region_R7_train_residual_total", "ind_region_R8_train_residual_total")



#########
# Errors 
#########

me   = ftsa:::me
mae  = ftsa:::mae
rmse = ftsa:::rmse


# iw: region index
# year_horizon: forecast horizon
# fmethod: "classical" or "M" (standard vs robust functional principal component analysis)
# alpha: 1 - nominal coverage probability

# function for region forecast

ind_back_test_region <- function(iw, year_horizon, fmethod = c("classical", "M"),  pcamethod = c("static", "dynamic"), alpha = 0.2)
{
  fmethod = match.arg(fmethod)
  pcamethod = match.arg(pcamethod)
  
  train_residual_female = train_residual_male = train_residual_total = list()
  
  res_male = res_male_lb = res_male_ub = 
    res_female = res_female_lb = res_female_ub = 
    res_total = res_total_lb = res_total_ub = array(NA, dim = c(year_horizon, 101, year_horizon))
  
  if (pcamethod == "static")
  {
    for(j in 1:year_horizon)
    {
      ind_dat = extract.years(get(region_smooth[iw]), years = 1973:(2011+j))
      
      fdm_female_order = head(which(round(cumsum(fdm(ind_dat, series = "female", method = fmethod, lambda = 2.33, order = length(ind_dat$year))$varprop),3)>=0.95),1)
      fdm_female = fdm(ind_dat, series = "female", order = fdm_female_order, method = fmethod, lambda = 2.33)
      train_residual_female[[j]] = ind_dat$rate$female - exp(fdm_female$fitted$y)
      fun_forc_female  = forecast(fdm_female, h = year_horizon)        
      
      fdm_male_order = head(which(round(cumsum(fdm(ind_dat, series = "male", method = fmethod, lambda = 2.33, order = length(ind_dat$year))$varprop),3)>=0.95),1)
      fdm_male = fdm(ind_dat, series = "male", order = fdm_male_order, method = fmethod, lambda = 2.33)
      train_residual_male[[j]] = ind_dat$rate$male - exp(fdm_male$fitted$y)
      fun_forc_male  = forecast(fdm_male, h = year_horizon)
      
      fdm_total_order = head(which(round(cumsum(fdm(ind_dat, series = "total", method = fmethod, lambda = 2.33, order = length(ind_dat$year))$varprop),3)>=0.95),1)
      fdm_total = fdm(ind_dat, series = "total", order = fdm_total_order, method = fmethod, lambda = 2.33)
      train_residual_total[[j]] = ind_dat$rate$total - exp(fdm_total$fitted$y)
      fun_forc_total  = forecast(fdm_total, h = year_horizon)
      
      res_female[,,j]    = t(fun_forc_female$rate$female)
      res_male[,,j]    = t(fun_forc_male$rate$male)
      res_total[,,j]    = t(fun_forc_total$rate$total)
    }
  }
  
  if (pcamethod == "dynamic")
  {
    for(j in 1:year_horizon)
    {
      ind_dat = extract.years(get(region_smooth[iw]), years = 1973:(2011+j))
      
      # female
      data_dum_mean_female = rowMeans(log(ind_dat$rate$female), na.rm = TRUE)
      data_dum_sd_female = apply(log(ind_dat$rate$female), 1, sd, na.rm = TRUE)
      data_dum_female = t(scale(t(log(ind_dat$rate$female)), center = TRUE, scale = TRUE))
      
      C_0_female = long_run_covariance_estimation(data_dum_female, H = 3, C0 = 3)
      eigen_decomp_female = eigen(C_0_female)
      dynamic_order_female = head(which(cumsum(eigen_decomp_female$values)/sum(eigen_decomp_female$values) >= 0.95),1)
      dynamic_basis_female = as.matrix(eigen_decomp_female$vectors[,1:dynamic_order_female])
      dynamic_scores_female = t(dynamic_basis_female) %*% data_dum_female
      
      train_residual_female[[j]] = ind_dat$rate$female - exp(dynamic_basis_female %*% dynamic_scores_female*data_dum_sd_female + data_dum_mean_female)
      
      scores_fit_female = scores_fore_female = list()
      fore_ftsm_dyn_female = matrix(NA, nrow = nrow(data_dum_female), ncol = year_horizon)
      
      for(ik in 1:dynamic_order_female)
      {
        scores_fit_female[[ik]] = auto.arima(dynamic_scores_female[ik,])
        scores_fore_female[[ik]] = forecast(scores_fit_female[[ik]], h = year_horizon)$mean
      }
      
      for(ih in 1:year_horizon)
      {
        fore_ftsm_dyn_female[,ih] = dynamic_basis_female%*% unlist(lapply(scores_fore_female,`[[`,ih))
      }
      
      fore_res_female = exp(fore_ftsm_dyn_female * data_dum_sd_female + data_dum_mean_female)
      res_female[,,j]    = t(fore_res_female)
      
      # male
      data_dum_mean_male = rowMeans(log(ind_dat$rate$male), na.rm = TRUE)
      data_dum_sd_male = apply(log(ind_dat$rate$male), 1, sd, na.rm = TRUE)
      data_dum_male = t(scale(t(log(ind_dat$rate$male)), center = TRUE, scale = TRUE))
      
      C_0_male = long_run_covariance_estimation(data_dum_male, H = 3, C0 = 3)
      eigen_decomp_male = eigen(C_0_male)
      dynamic_order_male = head(which(cumsum(eigen_decomp_male$values)/sum(eigen_decomp_male$values) >= 0.95),1)
      dynamic_basis_male = as.matrix(eigen_decomp_male$vectors[,1:dynamic_order_male])
      dynamic_scores_male = t(dynamic_basis_male) %*% data_dum_male
      
      train_residual_male[[j]] = ind_dat$rate$male - exp(dynamic_basis_male %*% dynamic_scores_male*data_dum_sd_male + data_dum_mean_male)
      
      scores_fit_male = scores_fore_male = list()
      fore_ftsm_dyn_male = matrix(NA, nrow = nrow(data_dum_male), ncol = year_horizon)
      
      for(ik in 1:dynamic_order_male)
      {
        scores_fit_male[[ik]] = auto.arima(dynamic_scores_male[ik,])
        scores_fore_male[[ik]] = forecast(scores_fit_male[[ik]], h = year_horizon)$mean
      }
      
      for(ih in 1:year_horizon)
      {
        fore_ftsm_dyn_male[,ih] = dynamic_basis_male%*% unlist(lapply(scores_fore_male,`[[`,ih))
      }
      
      fore_res_male = exp(fore_ftsm_dyn_male * data_dum_sd_male + data_dum_mean_male)
      res_male[,,j]    = t(fore_res_male)
      
      # total
      data_dum_mean_total = rowMeans(log(ind_dat$rate$total), na.rm = TRUE)
      data_dum_sd_total = apply(log(ind_dat$rate$total), 1, sd, na.rm = TRUE)
      data_dum_total = t(scale(t(log(ind_dat$rate$total)), center = TRUE, scale = TRUE))
      
      C_0_total = long_run_covariance_estimation(data_dum_total, H = 3, C0 = 3)
      eigen_decomp_total = eigen(C_0_total)
      dynamic_order_total = head(which(cumsum(eigen_decomp_total$values)/sum(eigen_decomp_total$values) >= 0.95),1)
      dynamic_basis_total = as.matrix(eigen_decomp_total$vectors[,1:dynamic_order_total])
      dynamic_scores_total = t(dynamic_basis_total) %*% data_dum_total
      
      train_residual_total[[j]] = ind_dat$rate$total - exp(dynamic_basis_total %*% dynamic_scores_total*data_dum_sd_total + data_dum_mean_total)
      
      scores_fit_total = scores_fore_total = list()
      fore_ftsm_dyn_total = matrix(NA, nrow = nrow(data_dum_total), ncol = year_horizon)
      
      for(ik in 1:dynamic_order_total)
      {
        scores_fit_total[[ik]] = auto.arima(dynamic_scores_total[ik,])
        scores_fore_total[[ik]] = forecast(scores_fit_total[[ik]], h = year_horizon)$mean
      }
      
      for(ih in 1:year_horizon)
      {
        fore_ftsm_dyn_total[,ih] = dynamic_basis_total%*% unlist(lapply(scores_fore_total,`[[`,ih))
      }
      
      fore_res_total = exp(fore_ftsm_dyn_total * data_dum_sd_total + data_dum_mean_total)
      res_total[,,j]    = t(fore_res_total)
    }
  }
  
  # Errors
  female_me = male_me = total_me = female_mae = male_mae = total_mae = female_rmse = male_rmse = total_rmse = vector("numeric",year_horizon)
  for(k in 1:year_horizon)
  {
    female_me[k]  = me(res_female[k,,1:(11-k)],  extract.years(get(region[iw]), years = (2012+k):2022)$rate$female)
    male_me[k]    = me(res_male[k,,1:(11-k)],    extract.years(get(region[iw]), years = (2012+k):2022)$rate$male)
    total_me[k]   = me(res_total[k,,1:(11-k)],   extract.years(get(region[iw]), years = (2012+k):2022)$rate$total)
    
    female_mae[k]  = mae(res_female[k,,1:(11-k)],  extract.years(get(region[iw]), years = (2012+k):2022)$rate$female)
    male_mae[k]    = mae(res_male[k,,1:(11-k)],    extract.years(get(region[iw]), years = (2012+k):2022)$rate$male)
    total_mae[k]   = mae(res_total[k,,1:(11-k)],   extract.years(get(region[iw]), years = (2012+k):2022)$rate$total)
    
    female_rmse[k]  = rmse(res_female[k,,1:(11-k)],  extract.years(get(region[iw]), years = (2012+k):2022)$rate$female)
    male_rmse[k]    = rmse(res_male[k,,1:(11-k)],  extract.years(get(region[iw]), years = (2012+k):2022)$rate$male)
    total_rmse[k]   = rmse(res_total[k,,1:(11-k)], extract.years(get(region[iw]), years = (2012+k):2022)$rate$total)
  }
  
  return(list(res_female = res_female, res_male = res_male, res_total = res_total,
              train_residual_female = train_residual_female, 
              train_residual_male = train_residual_male, train_residual_total = train_residual_total,
              female_me = female_me, male_me = male_me, total_me = total_me,
              female_mae = female_mae, male_mae = male_mae, total_mae = total_mae,
              female_rmse = female_rmse, male_rmse = male_rmse, total_rmse = total_rmse))
}

# function for prefecture forecast

ind_back_test <- function(iw, year_horizon, fmethod = c("classical", "M"), pcamethod = c("static", "dynamic"), alpha = 0.2)
{
  fmethod = match.arg(fmethod)
  pcamethod = match.arg(pcamethod)
  
  train_residual_female = train_residual_male = train_residual_total = list()
  
  res_male  = res_female = res_total = array(NA, dim = c(year_horizon, 101, year_horizon))
  
  if (pcamethod == "static")
  {
    for(j in 1:year_horizon)
    {
      ind_dat = extract.years(get(state_smooth[iw]), years = 1973:(2011+j))
      
      fdm_female_order = head(which(round(cumsum(fdm(ind_dat, series = "female", method = fmethod, lambda = 2.33, order = length(ind_dat$year))$varprop),3)>=0.95),1)
      fdm_female = fdm(ind_dat, series = "female", order = fdm_female_order, method = fmethod, lambda = 2.33)
      train_residual_female[[j]] = log(ind_dat$rate$female) - (fdm_female$fitted$y)
      fun_forc_female  = forecast(fdm_female, h = year_horizon)   
      
      fdm_male_order = head(which(round(cumsum(fdm(ind_dat, series = "male", method = fmethod, lambda = 2.33, order = length(ind_dat$year))$varprop),3)>=0.95),1)
      fdm_male = fdm(ind_dat, series = "male", order = fdm_male_order, method = fmethod, lambda = 2.33)
      train_residual_male[[j]] = log(ind_dat$rate$male) - (fdm_male$fitted$y)
      fun_forc_male  = forecast(fdm_male, h = year_horizon)
      
      fdm_total_order = head(which(round(cumsum(fdm(ind_dat, series = "total", method = fmethod, lambda = 2.33, order = length(ind_dat$year))$varprop),3)>=0.95),1)
      fdm_total = fdm(ind_dat, series = "total", order = fdm_total_order, method = fmethod, lambda = 2.33)
      train_residual_total[[j]] = log(ind_dat$rate$total) - (fdm_total$fitted$y)
      fun_forc_total  = forecast(fdm_total, h = year_horizon)
      
      res_female[,,j] = t(fun_forc_female$rate$female)
      res_male[,,j] = t(fun_forc_male$rate$male)
      res_total[,,j] = t(fun_forc_total$rate$total)
    }
  }
  
  if (pcamethod == "dynamic")
  {
    for(j in 1:year_horizon)
    {
      ind_dat = extract.years(get(state_smooth[iw]), years = 1973:(2011+j))
      # female
      data_dum_mean_female = rowMeans(log(ind_dat$rate$female), na.rm = TRUE)
      data_dum_sd_female = apply(log(ind_dat$rate$female), 1, sd, na.rm = TRUE)
      data_dum_female = t(scale(t(log(ind_dat$rate$female)), center = TRUE, scale = TRUE))
      
      C_0_female = long_run_covariance_estimation(data_dum_female, H = 3, C0 = 3)
      eigen_decomp_female = eigen(C_0_female)
      dynamic_order_female = head(which(cumsum(eigen_decomp_female$values)/sum(eigen_decomp_female$values) >= 0.95),1)
      dynamic_basis_female = as.matrix(eigen_decomp_female$vectors[,1:dynamic_order_female])
      dynamic_scores_female = t(dynamic_basis_female) %*% data_dum_female
      
      train_residual_female[[j]] = ind_dat$rate$female - exp(dynamic_basis_female %*% dynamic_scores_female*data_dum_sd_female + data_dum_mean_female)
      
      scores_fit_female = scores_fore_female = list()
      fore_ftsm_dyn_female = matrix(NA, nrow = nrow(data_dum_female), ncol = year_horizon)
      
      for(ik in 1:dynamic_order_female)
      {
        scores_fit_female[[ik]] = auto.arima(dynamic_scores_female[ik,])
        scores_fore_female[[ik]] = forecast(scores_fit_female[[ik]], h = year_horizon)$mean
      }
      
      for(ih in 1:year_horizon)
      {
        fore_ftsm_dyn_female[,ih] = dynamic_basis_female%*% unlist(lapply(scores_fore_female,`[[`,ih))
      }
      
      fore_res_female = exp(fore_ftsm_dyn_female * data_dum_sd_female + data_dum_mean_female)
      res_female[,,j]    = t(fore_res_female)
      
      # male
      data_dum_mean_male = rowMeans(log(ind_dat$rate$male), na.rm = TRUE)
      data_dum_sd_male = apply(log(ind_dat$rate$male), 1, sd, na.rm = TRUE)
      data_dum_male = t(scale(t(log(ind_dat$rate$male)), center = TRUE, scale = TRUE))
      
      C_0_male = long_run_covariance_estimation(data_dum_male, H = 3, C0 = 3)
      eigen_decomp_male = eigen(C_0_male)
      dynamic_order_male = head(which(cumsum(eigen_decomp_male$values)/sum(eigen_decomp_male$values) >= 0.95),1)
      dynamic_basis_male = as.matrix(eigen_decomp_male$vectors[,1:dynamic_order_male])
      dynamic_scores_male = t(dynamic_basis_male) %*% data_dum_male
      
      train_residual_male[[j]] = ind_dat$rate$male - exp(dynamic_basis_male %*% dynamic_scores_male*data_dum_sd_male + data_dum_mean_male)
      
      scores_fit_male = scores_fore_male = list()
      fore_ftsm_dyn_male = matrix(NA, nrow = nrow(data_dum_male), ncol = year_horizon)
      
      for(ik in 1:dynamic_order_male)
      {
        scores_fit_male[[ik]] = auto.arima(dynamic_scores_male[ik,])
        scores_fore_male[[ik]] = forecast(scores_fit_male[[ik]], h = year_horizon)$mean
      }
      
      for(ih in 1:year_horizon)
      {
        fore_ftsm_dyn_male[,ih] = dynamic_basis_male%*% unlist(lapply(scores_fore_male,`[[`,ih))
      }
      
      fore_res_male = exp(fore_ftsm_dyn_male * data_dum_sd_male + data_dum_mean_male)
      res_male[,,j]    = t(fore_res_male)
      
      # total
      data_dum_mean_total = rowMeans(log(ind_dat$rate$total), na.rm = TRUE)
      data_dum_sd_total = apply(log(ind_dat$rate$total), 1, sd, na.rm = TRUE)
      data_dum_total = t(scale(t(log(ind_dat$rate$total)), center = TRUE, scale = TRUE))
      
      C_0_total = long_run_covariance_estimation(data_dum_total, H = 3, C0 = 3)
      eigen_decomp_total = eigen(C_0_total)
      dynamic_order_total = head(which(cumsum(eigen_decomp_total$values)/sum(eigen_decomp_total$values) >= 0.95),1)
      dynamic_basis_total = as.matrix(eigen_decomp_total$vectors[,1:dynamic_order_total])
      dynamic_scores_total = t(dynamic_basis_total) %*% data_dum_total
      
      train_residual_total[[j]] = ind_dat$rate$total - exp(dynamic_basis_total %*% dynamic_scores_total*data_dum_sd_total + data_dum_mean_total)
      
      scores_fit_total = scores_fore_total = list()
      fore_ftsm_dyn_total = matrix(NA, nrow = nrow(data_dum_total), ncol = year_horizon)
      
      for(ik in 1:dynamic_order_total)
      {
        scores_fit_total[[ik]] = auto.arima(dynamic_scores_total[ik,])
        scores_fore_total[[ik]] = forecast(scores_fit_total[[ik]], h = year_horizon)$mean
      }
      
      for(ih in 1:year_horizon)
      {
        fore_ftsm_dyn_total[,ih] = dynamic_basis_total%*% unlist(lapply(scores_fore_total,`[[`,ih))
      }
      
      fore_res_total = exp(fore_ftsm_dyn_total * data_dum_sd_total + data_dum_mean_total)
      res_total[,,j] = t(fore_res_total)
    }
  }
  
  # Errors
  female_me = male_me = total_me = female_mae = male_mae = total_mae = female_rmse = male_rmse = total_rmse = vector("numeric", year_horizon)
  for(k in 1:year_horizon)
  {    
    female_me[k]  = me(res_female[k, , 1:(11-k)],  extract.years(get(state[iw]), years = (2012+k):2022)$rate$female)
    male_me[k]    = me(res_male[k, , 1:(11-k)],  extract.years(get(state[iw]), years = (2012+k):2022)$rate$male)
    total_me[k]   = me(res_total[k, , 1:(11-k)], extract.years(get(state[iw]), years = (2012+k):2022)$rate$total)
    
    female_mae[k]  = mae(res_female[k, , 1:(11-k)],  extract.years(get(state[iw]), years = (2012+k):2022)$rate$female)
    male_mae[k]    = mae(res_male[k, , 1:(11-k)],  extract.years(get(state[iw]), years = (2012+k):2022)$rate$male)
    total_mae[k]   = mae(res_total[k, , 1:(11-k)], extract.years(get(state[iw]), years = (2012+k):2022)$rate$total)
    
    female_rmse[k]  = rmse(res_female[k, , 1:(11-k)],  extract.years(get(state[iw]), years = (2012+k):2022)$rate$female)
    male_rmse[k]    = rmse(res_male[k, , 1:(11-k)],  extract.years(get(state[iw]), years = (2012+k):2022)$rate$male)
    total_rmse[k]   = rmse(res_total[k, , 1:(11-k)], extract.years(get(state[iw]), years = (2012+k):2022)$rate$total)
  }	
  
  
  return(list(res_female = res_female, res_male = res_male, res_total = res_total,
              train_residual_female = train_residual_female, 
              train_residual_male = train_residual_male, train_residual_total = train_residual_total,
              female_me = female_me, male_me = male_me, total_me = total_me,
              female_mae = female_mae, male_mae = male_mae, total_mae = total_mae,
              female_rmse = female_rmse, male_rmse = male_rmse, total_rmse = total_rmse))
}

#####################################################################
# Univariate forecast of age-specific mortality rates by prefectures
#####################################################################

for(ik in 1:48)
{
  dum = ind_back_test(iw = ik,  fmethod = "classical", pcamethod = "dynamic", year_horizon = 10)
  assign(ind_state_forc_female[ik], dum$res_female)
  assign(ind_state_forc_male[ik],   dum$res_male)
  assign(ind_state_forc_total[ik],  dum$res_total)
  
  assign(ind_state_train_residual_female[ik], dum$train_residual_female)
  assign(ind_state_train_residual_male[ik], dum$train_residual_male)
  assign(ind_state_train_residual_total[ik], dum$train_residual_total)
  
  assign(ind_state_me_female[ik], dum$female_me)
  assign(ind_state_me_male[ik],   dum$male_me)
  assign(ind_state_me_total[ik],  dum$total_me) 
  
  assign(ind_state_mae_female[ik], dum$female_mae)
  assign(ind_state_mae_male[ik],   dum$male_mae)
  assign(ind_state_mae_total[ik],  dum$total_mae)   
  
  assign(ind_state_rmse_female[ik], dum$female_rmse)
  assign(ind_state_rmse_male[ik],   dum$male_rmse)
  assign(ind_state_rmse_total[ik],  dum$total_rmse)   
  
  print(ik)
}

###########################################################
# Univariate forecast of age-specific mortality by regions
###########################################################

for(ik in 1:8)
{
  dum = ind_back_test_region(iw = ik, fmethod = "classic", pcamethod = "dynamic", year_horizon = 10)
  assign(ind_region_forc_female[ik], dum$res_female)
  assign(ind_region_forc_male[ik], dum$res_male)
  assign(ind_region_forc_total[ik], dum$res_total)
  
  assign(ind_region_train_residual_female[ik], dum$train_residual_female)
  assign(ind_region_train_residual_male[ik], dum$train_residual_male)
  assign(ind_region_train_residual_total[ik], dum$train_residual_total)
  
  assign(ind_region_me_female[ik], dum$female_me)
  assign(ind_region_me_male[ik],   dum$male_me)
  assign(ind_region_me_total[ik],  dum$total_me)
  
  assign(ind_region_mae_female[ik], dum$female_mae)
  assign(ind_region_mae_male[ik],   dum$male_mae)
  assign(ind_region_mae_total[ik],  dum$total_mae)
  
  assign(ind_region_rmse_female[ik], dum$female_rmse)
  assign(ind_region_rmse_male[ik],   dum$male_rmse)
  assign(ind_region_rmse_total[ik],  dum$total_rmse)
  
  print(ik)
}


#####################
# Summary of results
#####################

# Region + Sex

ind_region_me_female_mean_overall = ind_region_me_male_mean_overall = ind_region_me_total_mean_overall =
  ind_region_mae_female_mean_overall = ind_region_mae_male_mean_overall = ind_region_mae_total_mean_overall = 
  ind_region_rmse_female_mean_overall = ind_region_rmse_male_mean_overall = ind_region_rmse_total_mean_overall = NULL

for(ik in 1:8)  
{
  ind_region_me_female_mean_overall = cbind(ind_region_me_female_mean_overall, get(ind_region_me_female[ik]))
  ind_region_me_male_mean_overall   = cbind(ind_region_me_male_mean_overall,   get(ind_region_me_male[ik]))
  ind_region_me_total_mean_overall  = cbind(ind_region_me_total_mean_overall,   get(ind_region_me_total[ik]))
  
  ind_region_mae_female_mean_overall = cbind(ind_region_mae_female_mean_overall, get(ind_region_mae_female[ik]))
  ind_region_mae_male_mean_overall   = cbind(ind_region_mae_male_mean_overall,   get(ind_region_mae_male[ik]))
  ind_region_mae_total_mean_overall  = cbind(ind_region_mae_total_mean_overall,   get(ind_region_mae_total[ik]))
  
  ind_region_rmse_female_mean_overall = cbind(ind_region_rmse_female_mean_overall, get(ind_region_rmse_female[ik]))
  ind_region_rmse_male_mean_overall   = cbind(ind_region_rmse_male_mean_overall,   get(ind_region_rmse_male[ik]))
  ind_region_rmse_total_mean_overall  = cbind(ind_region_rmse_total_mean_overall,   get(ind_region_rmse_total[ik]))
}

# Prefecture + Sex

ind_me_female_mean_overall = ind_me_male_mean_overall = ind_me_total_mean_overall =
  ind_mae_female_mean_overall = ind_mae_male_mean_overall = ind_mae_total_mean_overall = 
  ind_rmse_female_mean_overall = ind_rmse_male_mean_overall = ind_rmse_total_mean_overall = NULL

for(ik in 1:48)
{
  ind_me_female_mean_overall  = cbind(ind_me_female_mean_overall, get(ind_state_me_female[ik]))
  ind_me_male_mean_overall    = cbind(ind_me_male_mean_overall,   get(ind_state_me_male[ik]))
  ind_me_total_mean_overall   = cbind(ind_me_total_mean_overall,   get(ind_state_me_total[ik]))
  
  ind_mae_female_mean_overall  = cbind(ind_mae_female_mean_overall, get(ind_state_mae_female[ik]))
  ind_mae_male_mean_overall    = cbind(ind_mae_male_mean_overall,   get(ind_state_mae_male[ik]))
  ind_mae_total_mean_overall   = cbind(ind_mae_total_mean_overall,   get(ind_state_mae_total[ik]))
  
  ind_rmse_female_mean_overall  = cbind(ind_rmse_female_mean_overall, get(ind_state_rmse_female[ik]))
  ind_rmse_male_mean_overall    = cbind(ind_rmse_male_mean_overall,   get(ind_state_rmse_male[ik]))
  ind_rmse_total_mean_overall   = cbind(ind_rmse_total_mean_overall,   get(ind_state_rmse_total[ik]))
}

################################
# Averaging over 47 prefectures
################################

rowmeans_ind_me_female_mean_overall = apply(ind_me_female_mean_overall[,2:48], 1, mean)
rowmeans_ind_me_male_mean_overall   = apply(ind_me_male_mean_overall[,2:48], 1, mean)
rowmeans_ind_me_total_mean_overall  = apply(ind_me_total_mean_overall[,2:48], 1, mean)

rowmeans_ind_mae_female_mean_overall = apply(ind_mae_female_mean_overall[,2:48], 1, mean)
rowmeans_ind_mae_male_mean_overall   = apply(ind_mae_male_mean_overall[,2:48], 1, mean)
rowmeans_ind_mae_total_mean_overall  = apply(ind_mae_total_mean_overall[,2:48], 1, mean)

rowmeans_ind_rmse_female_mean_overall = apply(ind_rmse_female_mean_overall[,2:48], 1, mean)
rowmeans_ind_rmse_male_mean_overall   = apply(ind_rmse_male_mean_overall[,2:48], 1, mean)
rowmeans_ind_rmse_total_mean_overall  = apply(ind_rmse_total_mean_overall[,2:48], 1, mean)

#####################
# Summary of results
#####################

ind_all_level_err_me = cbind(ind_me_total_mean_overall[,1], apply(cbind(ind_me_female_mean_overall[,1], ind_me_male_mean_overall[,1]), 1,mean),
                                     apply(ind_region_me_total_mean_overall, 1, mean), apply(cbind(ind_region_me_female_mean_overall, ind_region_me_male_mean_overall),1,mean),
                                     rowmeans_ind_me_total_mean_overall, apply(cbind(rowmeans_ind_me_female_mean_overall, rowmeans_ind_me_male_mean_overall),1,mean))
ind_all_level_err_me_all = rbind(ind_all_level_err_me, colMeans(ind_all_level_err_me), apply(ind_all_level_err_me, 2, median))


ind_all_level_err_mae = cbind(ind_mae_total_mean_overall[,1], apply(cbind(ind_mae_female_mean_overall[,1], ind_mae_male_mean_overall[,1]), 1,mean),
                                      apply(ind_region_mae_total_mean_overall, 1, mean), apply(cbind(ind_region_mae_female_mean_overall, ind_region_mae_male_mean_overall),1,mean),
                                      rowmeans_ind_mae_total_mean_overall, apply(cbind(rowmeans_ind_mae_female_mean_overall, rowmeans_ind_mae_male_mean_overall),1,mean))
ind_all_level_err_mae_all = rbind(ind_all_level_err_mae, colMeans(ind_all_level_err_mae), apply(ind_all_level_err_mae, 2, median))


ind_all_level_err_rmse = cbind(ind_rmse_total_mean_overall[,1], apply(cbind(ind_rmse_female_mean_overall[,1], ind_rmse_male_mean_overall[,1]), 1,mean),
                                       apply(ind_region_rmse_total_mean_overall, 1, mean), apply(cbind(ind_region_rmse_female_mean_overall, ind_region_rmse_male_mean_overall),1,mean),
                                       rowmeans_ind_rmse_total_mean_overall, apply(cbind(rowmeans_ind_rmse_female_mean_overall, rowmeans_ind_rmse_male_mean_overall),1,mean))
ind_all_level_err_rmse_all = rbind(ind_all_level_err_rmse, colMeans(ind_all_level_err_rmse), apply(ind_all_level_err_rmse, 2, median))

colnames(ind_all_level_err_rmse) = colnames(ind_all_level_err_mae) = colnames(ind_all_level_err_me)  =  c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

colnames(ind_all_level_err_rmse_all) = colnames(ind_all_level_err_mae_all) = colnames(ind_all_level_err_me_all)  =  c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")
rownames(ind_all_level_err_me_all) = rownames(ind_all_level_err_mae_all) = rownames(ind_all_level_err_rmse_all) = c(1:10, "Mean", "Median")




#################################
# point forecast errors function 
#################################

BU_optim_hier_mfts <- function(total_data_raw_rate_region, female_data_raw_rate_region,
                               male_data_raw_rate_region, female_data_raw_rate, male_data_raw_rate,
                               total_data_raw_rate, age, kj, hier_method = c("BU","Ind", "comb_OLS", "comb_GLS", "mint"))
{
  hier_method = match.arg(hier_method)
  
  hier_rate = matrix(NA,168,(11-kj)) # in total 168 series
  # Level_0 (Total)
  hier_rate[1,] = get(total_data_raw_rate[1])[kj,age,1:(11-kj)] # first arg: horizon; # third arg: number of forecasting years 
  # Level_1 (Sex)
  hier_rate[2,] = get(female_data_raw_rate[1])[kj,age,1:(11-kj)]
  hier_rate[3,] = get(male_data_raw_rate[1])[kj,age,1:(11-kj)]
  
  # Level_2 (Region + Total, Female, Male)
  for(iw in 1:8)
  {
    hier_rate[iw+3,]  = get(total_data_raw_rate_region[iw])[kj,age,1:(11-kj)]  
    hier_rate[iw+11,] = get(female_data_raw_rate_region[iw])[kj,age,1:(11-kj)]
    hier_rate[iw+19,] = get(male_data_raw_rate_region[iw])[kj,age,1:(11-kj)]
  }
  
  # Level_3 (Prefecture + Total, Female, Male)
  
  for(iw in 2:48)
  {
    hier_rate[iw+26,] = get(total_data_raw_rate[iw])[kj,age,1:(11-kj)]
  }  
  for(iw in 2:48)
  {
    hier_rate[(2*iw+71),] = get(female_data_raw_rate[iw])[kj,age,1:(11-kj)]
    hier_rate[(2*iw+72),] = get(male_data_raw_rate[iw])[kj,age,1:(11-kj)]   
  }
  
  # forecast reconciliation via bottom-up, optimal combination or MinT methods
  hier_fore   = matrix(NA,168,(11-kj))
  summing_mat = Smat_fun(kj = kj, age = age)
  for(ik in 1:(11-kj))
  {
    hier = summing_mat[,,ik]
    if(hier_method == "Ind")
    {
      hier_fore[,ik] = hier_rate[,ik]
    }
    if(hier_method == "BU")
    {
      hier_fore[,ik] = (hier %*% hier_rate[75:168,ik])
    }
    if(hier_method == "comb_OLS")
    {
      hier_fore[,ik] = hier %*% ginv(t(hier) %*% hier) %*% t(hier) %*% hier_rate[,ik]
    }
    if(hier_method == "comb_GLS")
    {
      hier_fore[,ik] = hier %*% ginv(t(hier) %*% ginv(diag(rowSums(hier^2))) %*% hier) %*% t(hier) %*% ginv(diag(rowSums(hier^2))) %*% hier_rate[,ik]
    }
    if(hier_method == "mint")
    {
      wh = wh_fun(kj = kj, age = age)
      hier_fore[,ik] = hier %*% solve(t(hier) %*% solve(wh) %*% hier) %*% t(hier) %*% solve(wh) %*% hier_rate[,ik]
    }
  }
  return(hier_fore)
}




BU_optim_err <- function(ik, hier_method, total_data_raw_rate_region, female_data_raw_rate_region,
                         male_data_raw_rate_region, female_data_raw_rate, 
                         male_data_raw_rate, total_data_raw_rate)
{
  me = ftsa:::me; mae = ftsa:::mae; rmse = ftsa:::rmse
  BU_optim_hier_comb = array(0, dim = c(101,168,(11-ik)))
  for(ik_age in 1:101)
  {
    BU_optim_hier_comb[ik_age,,] = BU_optim_hier_mfts(total_data_raw_rate_region, female_data_raw_rate_region,
                                                      male_data_raw_rate_region,
                                                      female_data_raw_rate, male_data_raw_rate, total_data_raw_rate,      
                                                      age = ik_age, kj = ik, hier_method = hier_method)
  }
  
  #####################################
  # Errors, including ME, MAE and RMSE
  #####################################
  
  # Level 0 (Total)
  me_total_err    = me(BU_optim_hier_comb[,1,],   extract.years(get(state[1]), years = (2012+ik):2022)$rate$total)
  mae_total_err   = mae(BU_optim_hier_comb[,1,],   extract.years(get(state[1]), years = (2012+ik):2022)$rate$total)
  rmse_total_err  = rmse(BU_optim_hier_comb[,1,],  extract.years(get(state[1]), years = (2012+ik):2022)$rate$total)
  
  # Level 1 (Sex)
  me_female_err  = me(BU_optim_hier_comb[,2,],  extract.years(get(state[1]), years = (2012+ik):2022)$rate$female)
  me_male_err    = me(BU_optim_hier_comb[,3,],  extract.years(get(state[1]), years = (2012+ik):2022)$rate$male)
  
  mae_female_err = mae(BU_optim_hier_comb[,2,],  extract.years(get(state[1]), years = (2012+ik):2022)$rate$female)
  mae_male_err   = mae(BU_optim_hier_comb[,3,],  extract.years(get(state[1]), years = (2012+ik):2022)$rate$male)
  
  rmse_female_err = rmse(BU_optim_hier_comb[,2,],  extract.years(get(state[1]), years = (2012+ik):2022)$rate$female)
  rmse_male_err   = rmse(BU_optim_hier_comb[,3,],  extract.years(get(state[1]), years = (2012+ik):2022)$rate$male)
  
  # Level 2 (Region + Total, Female and Male)
  me_total_R_err = me_female_R_err = me_male_R_err = mae_total_R_err = mae_female_R_err = mae_male_R_err = rmse_total_R_err = rmse_female_R_err = rmse_male_R_err = vector("numeric",8)
  for(iw in 1:8)
  {
    me_total_R_err[iw]  = me(BU_optim_hier_comb[,(iw+3),],  extract.years(get(region[iw]), years = (2012+ik):2022)$rate$total)
    me_female_R_err[iw] = me(BU_optim_hier_comb[,(iw+11),], extract.years(get(region[iw]), years = (2012+ik):2022)$rate$female)
    me_male_R_err[iw]   = me(BU_optim_hier_comb[,(iw+19),], extract.years(get(region[iw]), years = (2012+ik):2022)$rate$male)
    
    mae_total_R_err[iw]  = mae(BU_optim_hier_comb[,(iw+3),],  extract.years(get(region[iw]), years = (2012+ik):2022)$rate$total)
    mae_female_R_err[iw] = mae(BU_optim_hier_comb[,(iw+11),], extract.years(get(region[iw]), years = (2012+ik):2022)$rate$female)
    mae_male_R_err[iw]   = mae(BU_optim_hier_comb[,(iw+19),], extract.years(get(region[iw]), years = (2012+ik):2022)$rate$male)
    
    rmse_total_R_err[iw]  = rmse(BU_optim_hier_comb[,(iw+3),],  extract.years(get(region[iw]), years = (2012+ik):2022)$rate$total)
    rmse_female_R_err[iw] = rmse(BU_optim_hier_comb[,(iw+11),], extract.years(get(region[iw]), years = (2012+ik):2022)$rate$female)
    rmse_male_R_err[iw]   = rmse(BU_optim_hier_comb[,(iw+19),], extract.years(get(region[iw]), years = (2012+ik):2022)$rate$male)
  }
  
  # Level 3 (Prefecture + Total)
  
  me_state_err = bottom_female_me =  bottom_male_me = mae_state_err = bottom_female_mae =  bottom_male_mae = rmse_state_err = bottom_female_rmse = bottom_male_rmse = vector("numeric",47)
  
  for(iwk in 2:48)
  {
    me_state_err[iwk-1]    = me(BU_optim_hier_comb[,(iwk+26),],    extract.years(get(state[iwk]), years = (2012+ik):2022)$rate$total)
    mae_state_err[iwk-1]   = mae(BU_optim_hier_comb[,(iwk+26),],   extract.years(get(state[iwk]), years = (2012+ik):2022)$rate$total)
    rmse_state_err[iwk-1]  = rmse(BU_optim_hier_comb[,(iwk+26),],  extract.years(get(state[iwk]), years = (2012+ik):2022)$rate$total)
  }
  
  for(iwk in 2:48)
  {
    bottom_female_me[iwk-1]   = me(BU_optim_hier_comb[,(71+2*iwk),],    extract.years(get(state[iwk]), years = (2012+ik):2022)$rate$female)
    bottom_female_mae[iwk-1]  = mae(BU_optim_hier_comb[,(71+2*iwk),],   extract.years(get(state[iwk]), years = (2012+ik):2022)$rate$female)
    bottom_female_rmse[iwk-1] = rmse(BU_optim_hier_comb[,(71+2*iwk),],  extract.years(get(state[iwk]), years = (2012+ik):2022)$rate$female)
    
    bottom_male_me[iwk-1]     = me(BU_optim_hier_comb[,(72+2*iwk),],    extract.years(get(state[iwk]), years = (2012+ik):2022)$rate$male)
    bottom_male_mae[iwk-1]    = mae(BU_optim_hier_comb[,(72+2*iwk),],   extract.years(get(state[iwk]), years = (2012+ik):2022)$rate$male)
    bottom_male_rmse[iwk-1]   = rmse(BU_optim_hier_comb[,(72+2*iwk),],  extract.years(get(state[iwk]), years = (2012+ik):2022)$rate$male)
  }      
  
  return(list(me_total_err = me_total_err, me_female_err = me_female_err, 
              me_male_err  = me_male_err,  me_total_R_err = me_total_R_err,
              me_female_R_err = me_female_R_err, me_male_R_err = me_male_R_err,
              me_state_err  = me_state_err, me_bottom_female_err = bottom_female_me, 
              me_bottom_male_err = bottom_male_me, 
              
              mae_total_err = mae_total_err, mae_female_err = mae_female_err, 
              mae_male_err  = mae_male_err,  mae_total_R_err = mae_total_R_err,
              mae_female_R_err = mae_female_R_err, mae_male_R_err = mae_male_R_err,
              mae_state_err  = mae_state_err, mae_bottom_female_err = bottom_female_mae, 
              mae_bottom_male_err = bottom_male_mae, 
              
              rmse_total_err = rmse_total_err, rmse_female_err = rmse_female_err, 
              rmse_male_err  = rmse_male_err,  
              rmse_total_R_err = rmse_total_R_err,
              rmse_female_R_err = rmse_female_R_err,
              rmse_male_R_err = rmse_male_R_err,
              rmse_state_err  = rmse_state_err,
              rmse_bottom_female_err = bottom_female_rmse, rmse_bottom_male_err = bottom_male_rmse))
}


########################################################################################
# Reconciliation of univariate point forecasts using the optimal reconciliation methods
########################################################################################

# female_data_raw: Female age-specific exposure to risk
# male_data_raw: Male age-specific exposure to risk 
# female_data_raw_rate: Female age-specific mortality rates (ind)
# male_data_raw_rate: Male age-specific mortality rates (ind)
# age: Age variable
# kj: forecast horizon
# hier_method: bottom-up or optimal combination method

# create storing objects:

optim_Level_0_err     = optim_Level_F_err       = optim_Level_M_err       = matrix(0,10,3)
optim_Level_T_R_err   = optim_Level_F_R_err     = optim_Level_M_R_err     = array(0, dim = c(10,3,8))
optim_Level_State_err = optim_Level_State_F_err = optim_Level_State_M_err = array(0, dim = c(47,10,3))

for(ikw in 1:10)
{
  dum = BU_optim_err(ik = ikw, hier_method = "comb_OLS", 
                     total_data_raw_rate_region = ind_region_forc_total,
                     female_data_raw_rate_region = ind_region_forc_female,
                     male_data_raw_rate_region = ind_region_forc_male,
                     female_data_raw_rate = ind_state_forc_female, 
                     male_data_raw_rate = ind_state_forc_male, 
                     total_data_raw_rate = ind_state_forc_total)
  
  # Total + Sex
  
  optim_Level_0_err[ikw,1] = dum$me_total_err
  optim_Level_F_err[ikw,1] = dum$me_female_err
  optim_Level_M_err[ikw,1] = dum$me_male_err
  
  optim_Level_0_err[ikw,2] = dum$mae_total_err
  optim_Level_F_err[ikw,2] = dum$mae_female_err
  optim_Level_M_err[ikw,2] = dum$mae_male_err
  
  optim_Level_0_err[ikw,3] = dum$rmse_total_err
  optim_Level_F_err[ikw,3] = dum$rmse_female_err
  optim_Level_M_err[ikw,3] = dum$rmse_male_err
  
  # Region + Sex
  
  optim_Level_T_R_err[ikw,1,] = dum$me_total_R_err
  optim_Level_F_R_err[ikw,1,] = dum$me_female_R_err
  optim_Level_M_R_err[ikw,1,] = dum$me_male_R_err
  
  optim_Level_T_R_err[ikw,2,] = dum$mae_total_R_err
  optim_Level_F_R_err[ikw,2,] = dum$mae_female_R_err
  optim_Level_M_R_err[ikw,2,] = dum$mae_male_R_err
  
  optim_Level_T_R_err[ikw,3,] = dum$rmse_total_R_err
  optim_Level_F_R_err[ikw,3,] = dum$rmse_female_R_err
  optim_Level_M_R_err[ikw,3,] = dum$rmse_male_R_err
  
  # Prefecture + Sex
  
  optim_Level_State_err[,ikw,1]   = dum$me_state_err
  optim_Level_State_F_err[,ikw,1] = dum$me_bottom_female_err
  optim_Level_State_M_err[,ikw,1] = dum$me_bottom_male_err
  
  optim_Level_State_err[,ikw,2]   = dum$mae_state_err
  optim_Level_State_F_err[,ikw,2] = dum$mae_bottom_female_err
  optim_Level_State_M_err[,ikw,2] = dum$mae_bottom_male_err
  
  optim_Level_State_err[,ikw,3]   = dum$rmse_state_err
  optim_Level_State_F_err[,ikw,3] = dum$rmse_bottom_female_err
  optim_Level_State_M_err[,ikw,3] = dum$rmse_bottom_male_err
}

# summary of results

optim_all_level_err_me = cbind(optim_Level_0_err[,1], rowMeans(cbind(optim_Level_F_err[,1], optim_Level_M_err[,1])),
                               rowMeans(optim_Level_T_R_err[,1,]), rowMeans(cbind(rowMeans(optim_Level_F_R_err[,1,]), rowMeans(optim_Level_M_R_err[,1,]))),
                               colMeans(optim_Level_State_err[,,1]), colMeans(rbind(optim_Level_State_F_err[,,1], optim_Level_State_M_err[,,1])))

optim_all_level_err_mae = cbind(optim_Level_0_err[,2], rowMeans(cbind(optim_Level_F_err[,2], optim_Level_M_err[,2])),
                                rowMeans(optim_Level_T_R_err[,2,]), rowMeans(cbind(rowMeans(optim_Level_F_R_err[,2,]), rowMeans(optim_Level_M_R_err[,2,]))),
                                colMeans(optim_Level_State_err[,,2]), colMeans(rbind(optim_Level_State_F_err[,,2], optim_Level_State_M_err[,,2])))

optim_all_level_err_rmse = cbind(optim_Level_0_err[,3], rowMeans(cbind(optim_Level_F_err[,3], optim_Level_M_err[,3])),
                                 rowMeans(optim_Level_T_R_err[,3,]), rowMeans(cbind(rowMeans(optim_Level_F_R_err[,3,]), rowMeans(optim_Level_M_R_err[,3,]))),
                                 colMeans(optim_Level_State_err[,,3]), colMeans(rbind(optim_Level_State_F_err[,,3], optim_Level_State_M_err[,,3])))
colnames(optim_all_level_err_me) = colnames(optim_all_level_err_mae) = colnames(optim_all_level_err_rmse) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

optim_all_level_err_rmse_all = rbind(optim_all_level_err_rmse, apply(optim_all_level_err_rmse, 2, mean))
rownames(optim_all_level_err_rmse_all) = c(1:10, "Mean")


#########################################################################################
# Reconciliation of univariate point forecasts using the bottom-up reconciliation method
#########################################################################################

# female_data_raw: Female age-specific exposure to risk
# male_data_raw: Male age-specific exposure to risk 
# female_data_raw_rate: Female age-specific mortality rates (ind)
# male_data_raw_rate: Male age-specific mortality rates (ind)
# age: Age variable
# kj: forecast horizon
# hier_method: bottom-up or optimal combination method


# create storing objects:

Level_0_err_ind     = Level_F_err_ind       = Level_M_err_ind       = matrix(NA,10,3)
Level_T_R_err_ind   = Level_F_R_err_ind     = Level_M_R_err_ind     = array(NA, dim = c(10,3,8))
Level_State_err_ind = Level_State_F_err_ind = Level_State_M_err_ind = array(NA, dim = c(47,10,3))

for(ikw in 1:10)
{
  dum = BU_optim_err(ik = ikw, hier_method = "BU", 
                     total_data_raw_rate_region = ind_region_forc_total,
                     female_data_raw_rate_region = ind_region_forc_female,
                     male_data_raw_rate_region = ind_region_forc_male,
                     female_data_raw_rate = ind_state_forc_female, 
                     male_data_raw_rate = ind_state_forc_male, 
                     total_data_raw_rate = ind_state_forc_total)
  
  # Total + Sex
  
  Level_0_err_ind[ikw,1] = dum$me_total_err
  Level_F_err_ind[ikw,1] = dum$me_female_err
  Level_M_err_ind[ikw,1] = dum$me_male_err
  
  Level_0_err_ind[ikw,2] = dum$mae_total_err
  Level_F_err_ind[ikw,2] = dum$mae_female_err
  Level_M_err_ind[ikw,2] = dum$mae_male_err
  
  Level_0_err_ind[ikw,3] = dum$rmse_total_err
  Level_F_err_ind[ikw,3] = dum$rmse_female_err
  Level_M_err_ind[ikw,3] = dum$rmse_male_err
  
  # Region + Sex
  
  Level_T_R_err_ind[ikw,1,] = dum$me_total_R_err
  Level_F_R_err_ind[ikw,1,] = dum$me_female_R_err
  Level_M_R_err_ind[ikw,1,] = dum$me_male_R_err
  
  Level_T_R_err_ind[ikw,2,] = dum$mae_total_R_err
  Level_F_R_err_ind[ikw,2,] = dum$mae_female_R_err
  Level_M_R_err_ind[ikw,2,] = dum$mae_male_R_err
  
  Level_T_R_err_ind[ikw,3,] = dum$rmse_total_R_err
  Level_F_R_err_ind[ikw,3,] = dum$rmse_female_R_err
  Level_M_R_err_ind[ikw,3,] = dum$rmse_male_R_err
  
  # Prefecture + Sex
  
  Level_State_err_ind[,ikw,1]   = dum$me_state_err
  Level_State_F_err_ind[,ikw,1] = dum$me_bottom_female_err
  Level_State_M_err_ind[,ikw,1] = dum$me_bottom_male_err
  
  Level_State_err_ind[,ikw,2]   = dum$mae_state_err
  Level_State_F_err_ind[,ikw,2] = dum$mae_bottom_female_err
  Level_State_M_err_ind[,ikw,2] = dum$mae_bottom_male_err
  
  Level_State_err_ind[,ikw,3]   = dum$rmse_state_err
  Level_State_F_err_ind[,ikw,3] = dum$rmse_bottom_female_err
  Level_State_M_err_ind[,ikw,3] = dum$rmse_bottom_male_err
}

# summary of results

BU_all_level_err_me_ind = cbind(Level_0_err_ind[,1], rowMeans(cbind(Level_F_err_ind[,1], Level_M_err_ind[,1])),
                                rowMeans(Level_T_R_err_ind[,1,]), rowMeans(cbind(rowMeans(Level_F_R_err_ind[,1,]), rowMeans(Level_M_R_err_ind[,1,]))),
                                colMeans(Level_State_err_ind[,,1]), colMeans(rbind(Level_State_F_err_ind[,,1], Level_State_M_err_ind[,,1])))

BU_all_level_err_mae_ind = cbind(Level_0_err_ind[,2], rowMeans(cbind(Level_F_err_ind[,2], Level_M_err_ind[,2])),
                                 rowMeans(Level_T_R_err_ind[,2,]), rowMeans(cbind(rowMeans(Level_F_R_err_ind[,2,]), rowMeans(Level_M_R_err_ind[,2,]))),
                                 colMeans(Level_State_err_ind[,,2]), colMeans(rbind(Level_State_F_err_ind[,,2], Level_State_M_err_ind[,,2])))

BU_all_level_err_rmse_ind = cbind(Level_0_err_ind[,3], rowMeans(cbind(Level_F_err_ind[,3], Level_M_err_ind[,3])),
                                  rowMeans(Level_T_R_err_ind[,3,]), rowMeans(cbind(rowMeans(Level_F_R_err_ind[,3,]), rowMeans(Level_M_R_err_ind[,3,]))),
                                  colMeans(Level_State_err_ind[,,3]), colMeans(rbind(Level_State_F_err_ind[,,3], Level_State_M_err_ind[,,3])))

colnames(BU_all_level_err_me_ind) = colnames(BU_all_level_err_mae_ind) = colnames(BU_all_level_err_rmse_ind) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

BU_all_level_err_rmse_ind_all = rbind(BU_all_level_err_rmse_ind, apply(BU_all_level_err_rmse_ind, 2, mean))
rownames(BU_all_level_err_rmse_ind_all) = c(1:10, "Mean")

####################################################################################
# Reconciliation of univariate point forecasts using the MinT reconciliation method
####################################################################################

## Reference: SL Wickramasuriya, G Athanasopoulos, RJ Hyndman (2019), Optimal forecast reconciliation for hierarchical and grouped time series through trace minimization, JASA



# create storing objects:
optim_mint_Level_0_err_ind = optim_mint_Level_F_err_ind = optim_mint_Level_M_err_ind  = matrix(NA,10,3)
optim_mint_Level_T_R_err_ind = optim_mint_Level_F_R_err_ind = optim_mint_Level_M_R_err_ind = array(NA, dim = c(10,3,8))
optim_mint_Level_State_err_ind = optim_mint_Level_State_F_err_ind = optim_mint_Level_State_M_err_ind = array(NA, dim = c(47,10,3))

for(ikw in 1:10)
{
  dum = BU_optim_err(ik = ikw, hier_method = "mint", 
                     total_data_raw_rate_region = ind_region_forc_total,
                     female_data_raw_rate_region = ind_region_forc_female,
                     male_data_raw_rate_region = ind_region_forc_male,
                     female_data_raw_rate = ind_state_forc_female, 
                     male_data_raw_rate = ind_state_forc_male,
                     total_data_raw_rate = ind_state_forc_total)
  
  # Total + Sex
  
  optim_mint_Level_0_err_ind[ikw,1] = dum$me_total_err
  optim_mint_Level_F_err_ind[ikw,1] = dum$me_female_err
  optim_mint_Level_M_err_ind[ikw,1] = dum$me_male_err
  
  optim_mint_Level_0_err_ind[ikw,2] = dum$mae_total_err
  optim_mint_Level_F_err_ind[ikw,2] = dum$mae_female_err
  optim_mint_Level_M_err_ind[ikw,2] = dum$mae_male_err
  
  optim_mint_Level_0_err_ind[ikw,3] = dum$rmse_total_err
  optim_mint_Level_F_err_ind[ikw,3] = dum$rmse_female_err
  optim_mint_Level_M_err_ind[ikw,3] = dum$rmse_male_err
  
  # Region + Sex
  
  optim_mint_Level_T_R_err_ind[ikw,1,] = dum$me_total_R_err
  optim_mint_Level_F_R_err_ind[ikw,1,] = dum$me_female_R_err
  optim_mint_Level_M_R_err_ind[ikw,1,] = dum$me_male_R_err
  
  optim_mint_Level_T_R_err_ind[ikw,2,] = dum$mae_total_R_err
  optim_mint_Level_F_R_err_ind[ikw,2,] = dum$mae_female_R_err
  optim_mint_Level_M_R_err_ind[ikw,2,] = dum$mae_male_R_err
  
  optim_mint_Level_T_R_err_ind[ikw,3,] = dum$rmse_total_R_err
  optim_mint_Level_F_R_err_ind[ikw,3,] = dum$rmse_female_R_err
  optim_mint_Level_M_R_err_ind[ikw,3,] = dum$rmse_male_R_err
  
  # Prefecture + Sex
  
  optim_mint_Level_State_err_ind[,ikw,1]   = dum$me_state_err
  optim_mint_Level_State_F_err_ind[,ikw,1] = dum$me_bottom_female_err
  optim_mint_Level_State_M_err_ind[,ikw,1] = dum$me_bottom_male_err
  
  optim_mint_Level_State_err_ind[,ikw,2]   = dum$mae_state_err
  optim_mint_Level_State_F_err_ind[,ikw,2] = dum$mae_bottom_female_err
  optim_mint_Level_State_M_err_ind[,ikw,2] = dum$mae_bottom_male_err
  
  optim_mint_Level_State_err_ind[,ikw,3]   = dum$rmse_state_err
  optim_mint_Level_State_F_err_ind[,ikw,3] = dum$rmse_bottom_female_err
  optim_mint_Level_State_M_err_ind[,ikw,3] = dum$rmse_bottom_male_err
}

# summary of results

optim_mint_all_level_err_me_ind = cbind(optim_mint_Level_0_err_ind[,1], rowMeans(cbind(optim_mint_Level_F_err_ind[,1], optim_mint_Level_M_err_ind[,1])),
                                        rowMeans(optim_mint_Level_T_R_err_ind[,1,]), rowMeans(cbind(rowMeans(optim_mint_Level_F_R_err_ind[,1,]), rowMeans(optim_mint_Level_M_R_err_ind[,1,]))),
                                        colMeans(optim_mint_Level_State_err_ind[,,1]), colMeans(rbind(optim_mint_Level_State_F_err_ind[,,1], optim_mint_Level_State_M_err_ind[,,1])))

optim_mint_all_level_err_mae_ind = cbind(optim_mint_Level_0_err_ind[,2], rowMeans(cbind(optim_mint_Level_F_err_ind[,2], optim_mint_Level_M_err_ind[,2])),
                                         rowMeans(optim_mint_Level_T_R_err_ind[,2,]), rowMeans(cbind(rowMeans(optim_mint_Level_F_R_err_ind[,2,]), rowMeans(optim_mint_Level_M_R_err_ind[,2,]))),
                                         colMeans(optim_mint_Level_State_err_ind[,,2]), colMeans(rbind(optim_mint_Level_State_F_err_ind[,,2], optim_mint_Level_State_M_err_ind[,,2])))

optim_mint_all_level_err_rmse_ind = cbind(optim_mint_Level_0_err_ind[,3], rowMeans(cbind(optim_mint_Level_F_err_ind[,3], optim_mint_Level_M_err_ind[,3])),
                                          rowMeans(optim_mint_Level_T_R_err_ind[,3,]), rowMeans(cbind(rowMeans(optim_mint_Level_F_R_err_ind[,3,]), rowMeans(optim_mint_Level_M_R_err_ind[,3,]))),
                                          colMeans(optim_mint_Level_State_err_ind[,,3]), colMeans(rbind(optim_mint_Level_State_F_err_ind[,,3], optim_mint_Level_State_M_err_ind[,,3])))
colnames(optim_mint_all_level_err_me_ind) = colnames(optim_mint_all_level_err_mae_ind) = colnames(optim_mint_all_level_err_rmse_ind) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

optim_mint_all_level_err_rmse_ind_all = rbind(optim_mint_all_level_err_rmse_ind, apply(optim_mint_all_level_err_rmse_ind, 2, mean))
rownames(optim_mint_all_level_err_rmse_ind_all) = c(1:10, "Mean")

####################
# Output of results
###################

library(xtable)

dir.create("out", recursive = TRUE, showWarnings = FALSE)

# base forecasts
#xtable(ind_all_level_err_rmse_all, digits = 4)

# bottom-up reconciliation
#xtable(BU_all_level_err_rmse_ind_all, digits = 4)

# optimal combination reconciliation
#xtable(optim_all_level_err_rmse_all, digits = 4)

# MinT reconciliation
#xtable(optim_mint_all_level_err_rmse_ind_all, digits = 4)

# Write the data as CSV files in the "out/" directory
write.csv(ind_all_level_err_rmse_all, file = "out/ind_all_level_err_rmse_all.csv", row.names = FALSE)
write.csv(BU_all_level_err_rmse_ind_all, file = "out/BU_all_level_err_rmse_ind_all.csv", row.names = FALSE)
write.csv(optim_all_level_err_rmse_all, file = "out/optim_all_level_err_rmse_all.csv", row.names = FALSE)
write.csv(optim_mint_all_level_err_rmse_ind_all, file = "out/optim_mint_all_level_err_rmse_ind_all.csv", row.names = FALSE)

#save.image("27_Nov.RData")
