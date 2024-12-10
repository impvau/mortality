#######################################
# Japanese subnational mortality rates
#######################################

library(demography)
library(xlsx)
library(dplyr) 
library(magrittr)

dir <- "temp"

read.jpn <- function(region, label = region, save_path = file.path(dir, "cache")) {
  # Ensure save_path directory exists
  dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
  
  # Define file paths for saving the downloaded data
  death_rate_file <- file.path(save_path, paste0(region, "_Mx_1x1.csv"))
  exposure_file <- file.path(save_path, paste0(region, "_Exposures_1x1.csv"))
  
  # Download or load the death rate data
  if (!file.exists(death_rate_file)) {
    death_rate_url <- paste0("https://www.ipss.go.jp/p-toukei/JMD/", region, "/STATS/", "Mx_1x1.txt")
    death_rate_data <- data.table::fread(death_rate_url, skip = 2, header = TRUE, data.table = FALSE, blank.lines.skip = TRUE)
    write.csv(death_rate_data, death_rate_file, row.names = FALSE)
  } else {
    death_rate_data <- read.csv(death_rate_file)
  }
  
  # Download or load the exposure data
  if (!file.exists(exposure_file)) {
    exposure_url <- paste0("https://www.ipss.go.jp/p-toukei/JMD/", region, "/STATS/", "Exposures_1x1.txt")
    exposure_data <- data.table::fread(exposure_url, skip = 2, header = TRUE, data.table = FALSE, blank.lines.skip = TRUE)
    write.csv(exposure_data, exposure_file, row.names = FALSE)
  } else {
    exposure_data <- read.csv(exposure_file)
  }
  
  # Process the data into a 'demogdata' object
  obj <- list(type = "mortality", label = label, lambda = 0)
  obj$year <- sort(unique(death_rate_data[, 1]))
  n <- length(obj$year)
  m <- length(unique(death_rate_data[, 2]))
  obj$age <- death_rate_data[1:m, 2]
  mnames <- names(death_rate_data)[-c(1, 2)]
  n.mort <- length(mnames)
  obj$rate <- obj$pop <- list()
  
  for (i in 1:n.mort) {
    obj$rate[[i]] <- matrix(as.numeric(death_rate_data[, i + 2]), nrow = m, ncol = n)
    obj$rate[[i]][obj$rate[[i]] < 0] <- NA
    obj$pop[[i]] <- matrix(as.numeric(exposure_data[, i + 2]), nrow = m, ncol = n)
    obj$pop[[i]][obj$pop[[i]] < 0] <- NA
    dimnames(obj$rate[[i]]) <- dimnames(obj$pop[[i]]) <- list(obj$age, obj$year)
  }
  
  names(obj$pop) <- names(obj$rate) <- tolower(mnames)
  obj$age <- as.numeric(obj$age)
  if (is.na(obj$age[m])) 
    obj$age[m] <- 2 * obj$age[m - 1] - obj$age[m - 2]
  
  return(structure(obj, class = "demogdata"))
}

state = c("Japan", "Hokkaido", "Aomori", "Iwate", "Miyagi", "Akita", "Yamagata", "Fukushima",
          "Ibaraki", "Tochigi", "Gunma", "Saitama", "Chiba", "Tokyo", "Kanagawa", "Niigata",
          "Toyama", "Ishikawa", "Fukui", "Yamanashi", "Nagano", "Gifu", "Shizuoka", "Aichi",
          "Mie", "Shiga", "Kyoto", "Osaka", "Hyogo", "Nara", "Wakayama", "Tottori", "Shimane",
          "Okayama", "Hiroshima", "Yamaguchi", "Tokushima", "Kagawa", "Ehime", "Kochi",
          "Fukuoka", "Saga", "Nagasaki", "Kumamoto", "Oita", "Miyazaki", "Kagoshima", "Okinawa")

state_smooth = paste(state, "_smooth", sep = "")

Japan = extract.ages(read.jpn("00", "Japan"), 0:100)
Hokkaido = extract.ages(read.jpn("01", "Hokkaido"), 0:100)
Aomori = extract.ages(read.jpn("02", "Aomori"), 0:100)
Iwate = extract.ages(read.jpn("03", "Iwate"), 0:100)
Miyagi = extract.ages(read.jpn("04", "Miyagi"), 0:100)
Akita = extract.ages(read.jpn("05", "Akita"), 0:100)
Yamagata = extract.ages(read.jpn("06", "Yamagata"), 0:100)
Fukushima = extract.ages(read.jpn("07", "Fukushima"), 0:100)
Ibaraki = extract.ages(read.jpn("08", "Ibaraki"), 0:100)
Tochigi = extract.ages(read.jpn("09", "Tochigi"), 0:100)
Gunma = extract.ages(read.jpn("10", "Gunma"), 0:100)
Saitama = extract.ages(read.jpn("11", "Saitama"), 0:100)
Chiba = extract.ages(read.jpn("12", "Chiba"), 0:100)
Tokyo = extract.ages(read.jpn("13", "Tokyo"), 0:100)
Kanagawa = extract.ages(read.jpn("14", "Kanagawa"), 0:100)
Niigata = extract.ages(read.jpn("15", "Niigata"), 0:100)
Toyama = extract.ages(read.jpn("16", "Toyama"), 0:100)
Ishikawa = extract.ages(read.jpn("17", "Ishikawa"), 0:100)
Fukui = extract.ages(read.jpn("18", "Fukui"), 0:100)
Yamanashi = extract.ages(read.jpn("19", "Yamanashi"), 0:100)
Nagano = extract.ages(read.jpn("20", "Nagano"), 0:100)
Gifu = extract.ages(read.jpn("21", "Gifu"), 0:100)
Shizuoka = extract.ages(read.jpn("22", "Shizuoka"), 0:100)
Aichi = extract.ages(read.jpn("23", "Aichi"), 0:100)
Mie = extract.ages(read.jpn("24", "Mie"), 0:100)
Shiga = extract.ages(read.jpn("25", "Shiga"), 0:100)
Kyoto = extract.ages(read.jpn("26", "Kyoto"), 0:100)
Osaka = extract.ages(read.jpn("27", "Osaka"), 0:100)
Hyogo = extract.ages(read.jpn("28", "Hyogo"), 0:100)
Nara = extract.ages(read.jpn("29", "Nara"), 0:100)
Wakayama = extract.ages(read.jpn("30", "Wakayama"), 0:100)
Tottori = extract.ages(read.jpn("31", "Tottori"), 0:100)
Shimane = extract.ages(read.jpn("32", "Shimane"), 0:100)
Okayama = extract.ages(read.jpn("33", "Okayama"), 0:100)
Hiroshima = extract.ages(read.jpn("34", "Hiroshima"), 0:100)
Yamaguchi = extract.ages(read.jpn("35", "Yamaguchi"), 0:100)
Tokushima = extract.ages(read.jpn("36", "Tokushima"), 0:100)
Kagawa    = extract.ages(read.jpn("37", "Kagawa"), 0:100)
Ehime     = extract.ages(read.jpn("38", "Ehime"), 0:100)
Kochi     = extract.ages(read.jpn("39", "Kochi"), 0:100)
Fukuoka   = extract.ages(read.jpn("40", "Fukuoka"), 0:100)
Saga      = extract.ages(read.jpn("41", "Saga"), 0:100)
Nagasaki  = extract.ages(read.jpn("42", "Nagasaki"), 0:100)
Kumamoto  = extract.ages(read.jpn("43", "Kumamoto"), 0:100)
Oita      = extract.ages(read.jpn("44", "Oita"), 0:100)
Miyazaki  = extract.ages(read.jpn("45", "Miyazaki"), 0:100)
Kagoshima = extract.ages(read.jpn("46", "Kagoshima"), 0:100)
Okinawa   = extract.ages(read.jpn("47", "Okinawa"), 0:100)

num_prefs <- length(state)

## check if all prefectures have the same length
for(ij in 1:num_prefs)
{
  print(paste0("Prefecture ", ij))
  print(c(head(get(state[ij])$year, 1), tail(get(state[ij])$year, 1)))
}

## Okinawa only covers 1973 to 2022
## select the data range 1973:2022 for all prefectures

for(ij in 1:(num_prefs-1))
{
  temp_dat = get(state[ij])
  temp_dat_truncated = extract.years(temp_dat, 1973:2022)
  assign(state[ij], temp_dat_truncated)
  
  rm(temp_dat, temp_dat_truncated)
}


################################
# Imputation for missing values
################################

state_impute_female = paste(state, "_impute_female", sep = "")
state_impute_male = paste(state, "_impute_male", sep = "")

for(ij in 1:num_prefs)
{
  # female
  data_female = log(get(state[ij])$rate$female)
  data_female_impute = apply(ifelse(is.finite(data_female), data_female, NA), 2, na.interp)
  assign(state_impute_female[[ij]], data_female_impute)
  
  # male
  data_male = log(get(state[ij])$rate$male)
  data_male_impute = apply(ifelse(is.finite(data_male), data_male, NA), 2, na.interp)
  assign(state_impute_male[[ij]], data_male_impute)
}

# check
for(ij in 1:num_prefs)
{
  print(sum(is.infinite(get(state_impute_female[[ij]]))))
  print(sum(is.infinite(get(state_impute_male[[ij]]))))
}



## imputation for missing values using linear interpolation

# female
data_female = log(get(state[1])$rate$female)
data_female_impute = apply(ifelse(is.finite(data_female), data_female, NA), 2, na.interp)
rownames(data_female_impute) = paste("A", 0:100, sep = "")
colnames(data_female_impute) = get(state[1])$year 
#output_path = paste("D:/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Raw Data/Female Data/Japan_National_female.csv", sep = "")
output_path <- "data/Female Data/Japan_National_female.csv"; 
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

write.csv(data_female_impute, output_path, row.names = TRUE)


for(ij in 2:num_prefs)
{
  data_female = log(get(state[ij])$rate$female)
  data_female_impute = apply(ifelse(is.finite(data_female), data_female, NA), 2, na.interp)
  rownames(data_female_impute) = paste("A", 0:100, sep = "")
  colnames(data_female_impute) = get(state[ij])$year 
  #output_path = paste("D:/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Raw Data/Female Data/", 
  #                    state[ij], "_female.csv", sep = "")

output_path <- paste("data/Female Data/", state[ij], "_female.csv", sep = "")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
                      
  write.csv(data_female_impute, output_path, row.names = TRUE)
}

# male
data_male = log(get(state[1])$rate$male)
data_male_impute = apply(ifelse(is.finite(data_male), data_male, NA), 2, na.interp)
rownames(data_male_impute) = paste("A", 0:100, sep = "")
colnames(data_male_impute) = get(state[1])$year 
#output_path = paste("D:/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Raw Data/Male Data/Japan_National_male.csv",  sep = "")
output_path <- paste("data/Male Data/Japan_National_male.csv", sep = "")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

write.csv(data_male_impute, output_path, row.names = TRUE)

for(ij in 2:num_prefs)
{
  data_male = log(get(state[ij])$rate$male)
  data_male_impute = apply(ifelse(is.finite(data_male), data_male, NA), 2, na.interp)
  rownames(data_male_impute) = paste("A", 0:100, sep = "")
  colnames(data_male_impute) = get(state[ij])$year 
  #output_path = paste("D:/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Raw Data/Male Data/", 
  #                    state[ij], "_male.csv", sep = "")
  output_path <- paste("data/Male Data/", state[ij], "_male.csv", sep = "")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(data_male_impute, output_path, row.names = TRUE)
}

# total
data_total = log(get(state[1])$rate$total)
data_total_impute = apply(ifelse(is.finite(data_total), data_total, NA), 2, na.interp)
rownames(data_total_impute) = paste("A", 0:100, sep = "")
colnames(data_total_impute) = get(state[1])$year 
#output_path = paste("D:/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Raw Data/Total Data/Japan_National_total.csv",  sep = "")
output_path <- paste("data/Total Data/Japan_National_total.csv", sep = "")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

write.csv(data_total_impute, output_path, row.names = TRUE)

for(ij in 2:num_prefs)
{
  data_female = log(get(state[ij])$rate$female)
  data_female_impute = apply(ifelse(is.finite(data_female), data_female, NA), 2, na.interp)
  rownames(data_female_impute) = paste("A", 0:100, sep = "")
  colnames(data_female_impute) = get(state[ij])$year 
  #output_path = paste("D:/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Raw Data/Female Data/", 
  #                    state[ij], "_female.csv", sep = "")
  output_path <- paste("data/Female Data/", state[ij], "_female.csv", sep = "")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(data_female_impute, output_path, row.names = TRUE)
}

for(ij in 2:num_prefs)
{
  data_total = log(get(state[ij])$rate$total)
  data_total_impute = apply(ifelse(is.finite(data_total), data_total, NA), 2, na.interp)
  rownames(data_total_impute) = paste("A", 0:100, sep = "")
  colnames(data_total_impute) = get(state[ij])$year 
  #output_path = paste("D:/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Raw Data/Total Data/", 
  #                    state[ij], "_total.csv", sep = "")
  output_path <- paste("data/Total Data/", state[ij], "_total.csv", sep = "")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(data_total_impute, output_path, row.names = TRUE)
}

#################################################################################################
# smoothing: weighted penalized regression splines with a monotonic constraint for ages above 65
#################################################################################################

for(ij in 1:num_prefs)
{
  assign(state_smooth[ij], smooth.demogdata(get(state[ij])))
}


na_counter_female = na_counter_male = na_counter_total = rep(0, num_prefs)
for(i in 1:num_prefs)
{
  na_counter_female[i] = sum(is.infinite(log(get(state_smooth[i])$rate$female)))
  na_counter_male[i] = sum(is.infinite(log(get(state_smooth[i])$rate$male)))
  na_counter_total[i] = sum(is.infinite(log(get(state_smooth[i])$rate$total)))
}

all(na_counter_female == 0)
all(na_counter_male == 0)
all(na_counter_total == 0)

# female
data_female = get(state_smooth[1])$rate$female
rownames(data_female) = paste("A", 0:100, sep = "")
colnames(data_female) = get(state_smooth[1])$year 
#output_path = paste("D:/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Smooth Data/Female Data/Japan_National_smooth_female.csv",  
#                    sep = "")
output_path <- paste("data/Smooth Data/Female Data/Japan_National_smooth_female.csv", sep = "")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

write.csv(data_female, output_path, row.names = TRUE)

for(ij in 2:num_prefs)
{
  data_female = get(state_smooth[ij])$rate$female
  rownames(data_female) = paste("A", 0:100, sep = "")
  colnames(data_female) = get(state_smooth[ij])$year 
  #output_path = paste("D:/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Smooth Data/Female Data/", 
  #                    state_smooth[ij], "_female.csv", sep = "")
  output_path <- paste("data/Smooth Data/Female Data/", state_smooth[ij], "_female.csv", sep = "")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(data_female_impute, output_path, row.names = TRUE)
}

# male
data_male = get(state_smooth[1])$rate$male
rownames(data_male) = paste("A", 0:100, sep = "")
colnames(data_male) = get(state_smooth[1])$year 
#output_path = paste("D:/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Smooth Data/Male Data/Japan_National_smooth_male.csv",  
#                    sep = "")
output_path <- paste("data/Smooth Data/Male Data/Japan_National_smooth_male.csv", sep = "")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

write.csv(data_male, output_path, row.names = TRUE)

for(ij in 2:num_prefs)
{
  data_male = get(state_smooth[ij])$rate$male
  rownames(data_male) = paste("A", 0:100, sep = "")
  colnames(data_male) = get(state_smooth[ij])$year 
  #output_path = paste("D:/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Smooth Data/Male Data/", 
  #                    state_smooth[ij], "_male.csv", sep = "")
  output_path <- paste("data/Smooth Data/Male Data/", state_smooth[ij], "_male.csv", sep = "")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(data_male_impute, output_path, row.names = TRUE)
}

# total
data_total = get(state_smooth[1])$rate$total
rownames(data_total) = paste("A", 0:100, sep = "")
colnames(data_total) = get(state_smooth[1])$year 
#output_path = paste("D:/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Smooth Data/total Data/Japan_National_smooth_total.csv",  
#                    sep = "")
output_path <- paste("data/Smooth Data/Total Data/Japan_National_smooth_total.csv", sep = "")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write.csv(data_total, output_path, row.names = TRUE)

for(ij in 2:num_prefs)
{
  data_total = get(state_smooth[ij])$rate$total
  rownames(data_total) = paste("A", 0:100, sep = "")
  colnames(data_total) = get(state_smooth[ij])$year 
  #output_path = paste("D:/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Smooth Data/total Data/", 
  #                    state_smooth[ij], "_total.csv", sep = "")
  output_path <- paste("data/Smooth Data/Total Data/", state_smooth[ij], "_total.csv", sep = "")
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  write.csv(data_total_impute, output_path, row.names = TRUE)
}



########
# Plots
########

#output_path = paste("D:/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/plots/")
output_path <- "data/plots/"
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# Raw data
for(ij in 1:num_prefs)
{
  # female
  data_female = log(get(state[ij])$rate$female)
  data_female_impute = apply(ifelse(is.finite(data_female), data_female, NA), 2, na.interp)
  data_demog_female = demogdata(data_female_impute, pop = get(state[ij])$pop$female, ages = get(state[ij])$age, 
                              years = get(state[ij])$year, type = get(state[ij])$type, label = get(state[ij])$label, name = "female")
  savepdf(paste(output_path, "State_female_", ij, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
  plot(data_demog_female, transform = FALSE)
  dev.off()
  
  # male
  data_male = log(get(state[ij])$rate$male)
  data_male_impute = apply(ifelse(is.finite(data_male), data_male, NA), 2, na.interp)
  data_demog_male = demogdata(data_male_impute, pop = get(state[ij])$pop$male, ages = get(state[ij])$age, 
                                years = get(state[ij])$year, type = get(state[ij])$type, label = get(state[ij])$label, name = "male")
  savepdf(paste(output_path, "State_male_", ij, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
  plot(data_demog_male, transform = FALSE)
  dev.off()
  
  
  # total
  data_total = log(get(state[ij])$rate$total)
  data_total_impute = apply(ifelse(is.finite(data_total), data_total, NA), 2, na.interp)
  data_demog_total = demogdata(data_total_impute, pop = get(state[ij])$pop$total, ages = get(state[ij])$age, 
                              years = get(state[ij])$year, type = get(state[ij])$type, label = get(state[ij])$label, name = "total")
  savepdf(paste(output_path, "State_total_", ij, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
  plot(data_demog_total, transform = FALSE)
  dev.off()
}

# Smoothed data

for(ij in 1:num_prefs)
{
  # female
  savepdf(paste(output_path, "State_female_smooth", ij, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
  plot(get(state_smooth[ij]), series = "female")
  dev.off()
  
  # male
  savepdf(paste(output_path, "State_male_smooth", ij, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
  plot(get(state_smooth[ij]), series = "total")
  dev.off()
  
  # total
  savepdf(paste(output_path, "State_total_smooth", ij, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
  plot(get(state_smooth[ij]), series = "male")
  dev.off()
}


##########

savepdf("Japan_female_over_time", width = 14, height = 10, toplines = 0.9, pointsize = 10)
plot(get(state_smooth[1]))
dev.off()

savepdf("Japan_female_over_time_local", width = 14, height = 10, toplines = 0.9, pointsize = 10)
plot(get(state_smooth[1]), ages = 3:30, plotlegend = TRUE, legendpos = "bottomright")
dev.off()

savepdf("Japan_female_over_age", width = 14, height = 10, toplines = 0.9, pointsize = 10)
plot(get(state_smooth[1]), plot.type = "time", xlab = "Year")
dev.off()

savepdf("Japan_female_over_age_local", width = 14, height = 10, toplines = 0.9, pointsize = 10)
plot(get(state_smooth[1]), ages = 3:30, plot.type = "time", xlab = "Year")
col_select = rainbow(min(1024, 1.25*28))
legend("topright", lty = c(1,1,1), col = col_select[c(1, 16, 28)], c("3 years old", "18 years old", "30 years old"))
dev.off()


#############
# FPC Output
#############


select_K <- function(tau, eigenvalue)
{
  k_max = length(eigenvalue)
  k_all = rep(0, k_max-1)
  for(k in 1:(k_max-1))
  {
    k_all[k] = (eigenvalue[k+1]/eigenvalue[k])*ifelse(eigenvalue[k]/eigenvalue[1] > tau, 1, 0) + ifelse(eigenvalue[k]/eigenvalue[1] < tau, 1, 0)
  }
  K_hat = which.min(k_all)
  return(K_hat)
}


fpc_output <- function(ij, years)
{
  dat_female = log(extract.years(get(state_smooth[ij]), years)$rate$female)
  dat_male = log(extract.years(get(state_smooth[ij]), years)$rate$male)
  
  # compute long-run covariance
  LRC_est_female = ftsa::long_run_covariance_estimation(dat = dat_female)
  LRC_est_male = ftsa::long_run_covariance_estimation(dat = dat_male)
  
  eigen_decomp_female = eigen(LRC_est_female, symmetric = TRUE)
  eigen_decomp_male = eigen(LRC_est_male, symmetric = TRUE)
  
  lambda_val_female = eigen_decomp_female$values[which(eigen_decomp_female$values > 0)]
  ncomp_female = select_K(tau = 1/log(max(lambda_val_female[1], length(lambda_val_female))), eigenvalue = lambda_val_female)
  basis_female = matrix(eigen_decomp_female$vectors[,1:6], ncol = 6)
  eigen_female = eigen_decomp_female$values[1:6]
  
  lambda_val_male = eigen_decomp_male$values[which(eigen_decomp_male$values > 0)]
  ncomp_male = select_K(tau = 1/log(max(lambda_val_male[1], length(lambda_val_male))), eigenvalue = lambda_val_male)
  basis_male = matrix(eigen_decomp_male$vectors[,1:6], ncol = 6)
  eigen_male = eigen_decomp_male$values[1:6]
  
  return(list(basis_female = basis_female, eigen_female = eigen_female, ncomp_female = ncomp_female,
              basis_male = basis_male, eigen_male = eigen_male, ncomp_male = ncomp_male))
}

####################
# years = 1973:2013
###################


Japan_basis_2013 = fpc_output(ij = 1, years = 1973:2013)
Fukushima_basis_2013 = fpc_output(ij = 8, years = 1973:2013)
Hokkaido_basis_2013 = fpc_output(ij = 2, years = 1973:2013)
Okinawa_basis_2013 = fpc_output(ij = num_prefs, years = 1973:2013)

female_basis_2013 = data.frame(cbind(0:100, Japan_basis_2013$basis_female, Fukushima_basis_2013$basis_female, Hokkaido_basis_2013$basis_female, Okinawa_basis_2013$basis_female))
colnames(female_basis_2013) = c("Age", paste0("Japan_basis_", 1:6), paste0("Fukushima_basis_", 1:6), paste0("Hokkaido_basis_", 1:6), paste0("Okinawa_basis_", 1:6))

male_basis_2013 = data.frame(cbind(0:100, Japan_basis_2013$basis_male, Fukushima_basis_2013$basis_male, Hokkaido_basis_2013$basis_male, Okinawa_basis_2013$basis_male))
colnames(male_basis_2013) = c("Age", paste0("Japan_basis_", 1:6), paste0("Fukushima_basis_", 1:6), paste0("Hokkaido_basis_", 1:6), paste0("Okinawa_basis_", 1:6))

write.csv(female_basis_2013, file = "female_basis_2013.csv")
write.csv(male_basis_2013, file = "male_basis_2013.csv")

savepdf("FPC_functions_female_2013", width = 20, height = 15)
female_basis_2013 %>% select(contains(c("Age", "basis_1", "basis_2"))) %>% 
  pivot_longer(c(paste0("Japan_basis_", 1:2), paste0("Fukushima_basis_", 1:2), paste0("Hokkaido_basis_", 1:2), paste0("Okinawa_basis_", 1:2))) %>%
  mutate(region = case_when(name %in% c("Japan_basis_1", "Japan_basis_2") ~ "Japan",
                            name %in% c("Fukushima_basis_1", "Fukushima_basis_2") ~ "Fukushima",
                            name %in% c("Hokkaido_basis_1", "Hokkaido_basis_2") ~ "Hokkaido",
                            name %in% c("Okinawa_basis_1", "Okinawa_basis_2") ~ "Okinawa"),
         color = case_when(str_ends(name, "basis_1") ~ "1st FPC", TRUE ~ "2nd FPC")
  ) %>% 
  ggplot(aes(x = Age, y = value, color = color)) +
  geom_line() + 
  facet_wrap(~ factor(region, levels = c("Japan", "Fukushima", "Hokkaido", "Okinawa"))) +
  xlab('Age') +
  ylab('FPC Value') +
  labs(title = 'First and Second Empirical Dynamic FPC Functions, 1973:2013') 
dev.off()

savepdf("FPC_functions_male_2013", width = 20, height = 15)
male_basis_2013 %>% select(contains(c("Age", "basis_1", "basis_2"))) %>% 
  pivot_longer(c(paste0("Japan_basis_", 1:2), paste0("Fukushima_basis_", 1:2), paste0("Hokkaido_basis_", 1:2), paste0("Okinawa_basis_", 1:2))) %>%
  mutate(region = case_when(name %in% c("Japan_basis_1", "Japan_basis_2") ~ "Japan",
                            name %in% c("Fukushima_basis_1", "Fukushima_basis_2") ~ "Fukushima",
                            name %in% c("Hokkaido_basis_1", "Hokkaido_basis_2") ~ "Hokkaido",
                            name %in% c("Okinawa_basis_1", "Okinawa_basis_2") ~ "Okinawa"),
         color = case_when(str_ends(name, "basis_1") ~ "1st FPC", TRUE ~ "2nd FPC")
  ) %>% 
  ggplot(aes(x = Age, y = value, color = color)) +
  geom_line() + 
  facet_wrap(~ factor(region, levels = c("Japan", "Fukushima", "Hokkaido", "Okinawa"))) +
  xlab('Age') +
  ylab('FPC Value') +
  labs(title = 'First and Second Empirical Dynamic FPC Functions, 1973:2013')
dev.off()

####################
# years = 1973:2003
###################

Japan_basis_2003 = fpc_output(ij = 1, years = 1973:2003)
Fukushima_basis_2003 = fpc_output(ij = 8, years = 1973:2003)
Hokkaido_basis_2003 = fpc_output(ij = 2, years = 1973:2003)
Okinawa_basis_2003 = fpc_output(ij = num_prefs, years = 1973:2003)

female_basis_2003 = data.frame(cbind(0:100, Japan_basis_2003$basis_female, Fukushima_basis_2003$basis_female, Hokkaido_basis_2003$basis_female, Okinawa_basis_2003$basis_female))
colnames(female_basis_2003) = c("Age", paste0("Japan_basis_", 1:6), paste0("Fukushima_basis_", 1:6), paste0("Hokkaido_basis_", 1:6), paste0("Okinawa_basis_", 1:6))

male_basis_2003 = data.frame(cbind(0:100, Japan_basis_2003$basis_male, Fukushima_basis_2003$basis_male, Hokkaido_basis_2003$basis_male, Okinawa_basis_2003$basis_male))
colnames(male_basis_2003) = c("Age", paste0("Japan_basis_", 1:6), paste0("Fukushima_basis_", 1:6), paste0("Hokkaido_basis_", 1:6), paste0("Okinawa_basis_", 1:6))

write.csv(female_basis_2003, file = "female_basis_2003.csv")
write.csv(male_basis_2003, file = "male_basis_2003.csv")

savepdf("FPC_functions_female_2003", width = 20, height = 15)
female_basis_2003 %>% select(contains(c("Age", "basis_1", "basis_2"))) %>% 
  pivot_longer(c(paste0("Japan_basis_", 1:2), paste0("Fukushima_basis_", 1:2), paste0("Hokkaido_basis_", 1:2), paste0("Okinawa_basis_", 1:2))) %>%
  mutate(region = case_when(name %in% c("Japan_basis_1", "Japan_basis_2") ~ "Japan",
                            name %in% c("Fukushima_basis_1", "Fukushima_basis_2") ~ "Fukushima",
                            name %in% c("Hokkaido_basis_1", "Hokkaido_basis_2") ~ "Hokkaido",
                            name %in% c("Okinawa_basis_1", "Okinawa_basis_2") ~ "Okinawa"),
         color = case_when(str_ends(name, "basis_1") ~ "1st FPC", TRUE ~ "2nd FPC")
  ) %>% 
  ggplot(aes(x = Age, y = value, color = color)) +
  geom_line() + 
  facet_wrap(~ factor(region, levels = c("Japan", "Fukushima", "Hokkaido", "Okinawa"))) +
  xlab('Age') +
  ylab('FPC Value') +
  labs(title = 'First and Second Empirical Dynamic FPC Functions, 1973:2003') 
dev.off()

savepdf("FPC_functions_male_2003", width = 20, height = 15)
male_basis_2003 %>% select(contains(c("Age", "basis_1", "basis_2"))) %>% 
  pivot_longer(c(paste0("Japan_basis_", 1:2), paste0("Fukushima_basis_", 1:2), paste0("Hokkaido_basis_", 1:2), paste0("Okinawa_basis_", 1:2))) %>%
  mutate(region = case_when(name %in% c("Japan_basis_1", "Japan_basis_2") ~ "Japan",
                            name %in% c("Fukushima_basis_1", "Fukushima_basis_2") ~ "Fukushima",
                            name %in% c("Hokkaido_basis_1", "Hokkaido_basis_2") ~ "Hokkaido",
                            name %in% c("Okinawa_basis_1", "Okinawa_basis_2") ~ "Okinawa"),
         color = case_when(str_ends(name, "basis_1") ~ "1st FPC", TRUE ~ "2nd FPC")
  ) %>% 
  ggplot(aes(x = Age, y = value, color = color)) +
  geom_line() + 
  facet_wrap(~ factor(region, levels = c("Japan", "Fukushima", "Hokkaido", "Okinawa"))) +
  xlab('Age') +
  ylab('FPC Value') +
  labs(title = 'First and Second Empirical Dynamic FPC Functions, 1973:2003')
dev.off()

####################
# years = 1973:1993
###################

Japan_basis_1993 = fpc_output(ij = 1, years = 1973:1993)
Fukushima_basis_1993 = fpc_output(ij = 8, years = 1973:1993)
Hokkaido_basis_1993 = fpc_output(ij = 2, years = 1973:1993)
Okinawa_basis_1993 = fpc_output(ij = num_prefs, years = 1973:1993)

female_basis_1993 = data.frame(cbind(0:100, Japan_basis_1993$basis_female, Fukushima_basis_1993$basis_female, Hokkaido_basis_1993$basis_female, Okinawa_basis_1993$basis_female))
colnames(female_basis_1993) = c("Age", paste0("Japan_basis_", 1:6), paste0("Fukushima_basis_", 1:6), paste0("Hokkaido_basis_", 1:6), paste0("Okinawa_basis_", 1:6))

male_basis_1993 = data.frame(cbind(0:100, Japan_basis_1993$basis_male, Fukushima_basis_1993$basis_male, Hokkaido_basis_1993$basis_male, Okinawa_basis_1993$basis_male))
colnames(male_basis_1993) = c("Age", paste0("Japan_basis_", 1:6), paste0("Fukushima_basis_", 1:6), paste0("Hokkaido_basis_", 1:6), paste0("Okinawa_basis_", 1:6))

write.csv(female_basis_1993, file = "female_basis_1993.csv")
write.csv(male_basis_1993, file = "male_basis_1993.csv")

savepdf("FPC_functions_female_1993", width = 20, height = 15)
female_basis_1993 %>% select(contains(c("Age", "basis_1", "basis_2"))) %>% 
  pivot_longer(c(paste0("Japan_basis_", 1:2), paste0("Fukushima_basis_", 1:2), paste0("Hokkaido_basis_", 1:2), paste0("Okinawa_basis_", 1:2))) %>%
  mutate(region = case_when(name %in% c("Japan_basis_1", "Japan_basis_2") ~ "Japan",
                            name %in% c("Fukushima_basis_1", "Fukushima_basis_2") ~ "Fukushima",
                            name %in% c("Hokkaido_basis_1", "Hokkaido_basis_2") ~ "Hokkaido",
                            name %in% c("Okinawa_basis_1", "Okinawa_basis_2") ~ "Okinawa"),
         color = case_when(str_ends(name, "basis_1") ~ "1st FPC", TRUE ~ "2nd FPC")
  ) %>% 
  ggplot(aes(x = Age, y = value, color = color)) +
  geom_line() + 
  facet_wrap(~ factor(region, levels = c("Japan", "Fukushima", "Hokkaido", "Okinawa"))) +
  xlab('Age') +
  ylab('FPC Value') +
  labs(title = 'First and Second Empirical Dynamic FPC Functions, 1973:1993') 
dev.off()

savepdf("FPC_functions_male_1993", width = 20, height = 15)
male_basis_1993 %>% select(contains(c("Age", "basis_1", "basis_2"))) %>% 
  pivot_longer(c(paste0("Japan_basis_", 1:2), paste0("Fukushima_basis_", 1:2), paste0("Hokkaido_basis_", 1:2), paste0("Okinawa_basis_", 1:2))) %>%
  mutate(region = case_when(name %in% c("Japan_basis_1", "Japan_basis_2") ~ "Japan",
                            name %in% c("Fukushima_basis_1", "Fukushima_basis_2") ~ "Fukushima",
                            name %in% c("Hokkaido_basis_1", "Hokkaido_basis_2") ~ "Hokkaido",
                            name %in% c("Okinawa_basis_1", "Okinawa_basis_2") ~ "Okinawa"),
         color = case_when(str_ends(name, "basis_1") ~ "1st FPC", TRUE ~ "2nd FPC")
  ) %>% 
  ggplot(aes(x = Age, y = value, color = color)) +
  geom_line() + 
  facet_wrap(~ factor(region, levels = c("Japan", "Fukushima", "Hokkaido", "Okinawa"))) +
  xlab('Age') +
  ylab('FPC Value') +
  labs(title = 'First and Second Empirical Dynamic FPC Functions, 1973:1993')
dev.off()


plot(Japan_basis_1993$basis_female[,1], type = "l", ylim = c(-0.16, -0.04), ylab = "1st FPC of Japan Female Series", xlab = "Age", lwd = 2)
lines(Japan_basis_2003$basis_female[,1], col = 2, lwd = 2)
lines(Japan_basis_2013$basis_female[,1], col = 3, lwd = 2)
legend("bottomright", legend =  c(1993,2003,2013), col = 1:3, lty = rep(1,3), lwd = rep(2,3))


#############
# Japan all
#############

Japan_basis_all = fpc_output(ij = 1, years = 1973:2022)
Japan_basis_female_all = data.frame(cbind(0:100, Japan_basis_all$basis_female))
colnames(Japan_basis_female_all) = c("Age", paste0("Japan_basis_", 1:6))

Japan_basis_female_all[,1:5] %>% pivot_longer(paste0("Japan_basis_", 1:4)) %>% 
  mutate(FPC = case_when(str_ends(name, "basis_1") ~ "1st FPC",
                         str_ends(name, "basis_2") ~ "2nd FPC",
                         str_ends(name, "basis_3") ~ "3rd FPC",
                         str_ends(name, "basis_4") ~ "4th FPC",
                         str_ends(name, "basis_5") ~ "5th FPC",
                         str_ends(name, "basis_6") ~ "6th FPC")) %>% 
  ggplot(aes(x = Age, y = value)) +
  geom_line() + 
  facet_wrap(~ factor(FPC)) +
  xlab('Age') +
  ylab('FPC Value') +
  labs(title = 'Empirical Dynamic FPC Functions for Japanese Female, 1973:2022')
