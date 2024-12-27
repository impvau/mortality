######################################################################
#
# Download Japanese mortality data
# - sources data from ipss.go.jp
# - Caches if already downloaded (clear cache to force download)
# - Config values in settings.R
#
######################################################################

library(demography)
library(data.table)
library(dplyr)

# Requires execution in source directory or use launch.json in VSC
dir <- getwd()
source(file.path(dir, "settings.R"))

read.jpn <- function(prefecture_index, label, save_path = file.path(dir, "cache")) {
  # Ensure save_path directory exists
  dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
  
  # Define file paths for saving the downloaded data
  death_rate_file <- file.path(save_path, paste0(prefecture_index, "_Mx_1x1.csv"))
  exposure_file <- file.path(save_path, paste0(prefecture_index, "_Exposures_1x1.csv"))
  
  # Download or load the death rate data
  if (!file.exists(death_rate_file)) {
    death_rate_url <- paste0("https://www.ipss.go.jp/p-toukei/JMD/", prefecture_index, "/STATS/Mx_1x1.txt")
    death_rate_data <- data.table::fread(death_rate_url, skip = 2, header = TRUE, data.table = FALSE, blank.lines.skip = TRUE)
    write.csv(death_rate_data, death_rate_file, row.names = FALSE)
  } else {
    death_rate_data <- read.csv(death_rate_file)
  }
  
  # Download or load the exposure data
  if (!file.exists(exposure_file)) {
    exposure_url <- paste0("https://www.ipss.go.jp/p-toukei/JMD/", prefecture_index, "/STATS/Exposures_1x1.txt")
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

# Download and process data for all prefectures
for (i in seq_along(prefectures)) {
  prefecture_index <- ind_prefs[i]
  label <- prefectures[i]
  message("Processing prefecture: ", label, " (", prefecture_index, ")")
  assign(prefectures[i], read.jpn(prefecture_index, label))
}
