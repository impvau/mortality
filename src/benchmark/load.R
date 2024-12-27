#######################################
# Load Japanese subnational mortality rates
# - loads data from download/cache
# - saves environmented for smoothed data to reduce runtime
# - performs smoothing, imputation, etc.
#######################################

library(demography)
library(xlsx)
library(dplyr) 
library(magrittr)

# Requires execution in source directory or use launch.json in VSC
dir <- getwd()

state_file <- file.path(dir, "load_state.Rdata")

source(file.path(dir, "settings.R"))

# Check if the saved state exists
if (file.exists(state_file)) {
    # Load the saved state
    load(state_file)
    cat("Loaded saved state from", state_file, "\n")
} else {

    # Loop through all prefectures and assign the extracted ages to corresponding variables
    # e.g. Prefecture "Japan" will be accessible in variable Japan
    for (i in seq_along(prefectures)) {
        assign(prefectures[i], extract.ages(read.jpn(ind_prefs[i], prefectures[i]), 0:100))
    }
    ## check if all prefectures have the same length
    #for(ij in 1:num_prefs)
    #{
    #    print(paste0("Prefecture ", ij))
    #    print(c(head(get(prefectures[ij])$year, 1), tail(get(prefectures[ij])$year, 1)))
    #}

    # Only consider year_range data
    # Note: Okinawa only covers 1973 to 2022
    for (ij in 1:(num_prefs - 1)) {
        temp_dat <- get(prefectures[ij])
        temp_dat_truncated <- extract.years(temp_dat, year_range)
        assign(prefectures[ij], temp_dat_truncated)
        rm(temp_dat, temp_dat_truncated)
    }

    # ################################
    # # Imputation for missing values
    # ################################
    prefectures_impute_female = paste(prefectures, "_impute_female", sep = "")
    prefectures_impute_male = paste(prefectures, "_impute_male", sep = "")

    for(ij in 1:num_prefs)
    {
    # female
    data_female = log(get(prefectures[ij])$rate$female)
    data_female_impute = apply(ifelse(is.finite(data_female), data_female, NA), 2, na.interp)
    assign(prefectures_impute_female[[ij]], data_female_impute)
    
    # male
    data_male = log(get(prefectures[ij])$rate$male)
    data_male_impute = apply(ifelse(is.finite(data_male), data_male, NA), 2, na.interp)
    assign(prefectures_impute_male[[ij]], data_male_impute)
    }
    # Check
    #for(ij in 1:num_prefs)
    #{
    #    print(sum(is.infinite(get(prefectures_impute_female[[ij]]))))
    #    print(sum(is.infinite(get(prefectures_impute_male[[ij]]))))
    #}

    ## imputation for missing values using linear interpolation
    # female
    data_female = log(get(prefectures[1])$rate$female)
    data_female_impute = apply(ifelse(is.finite(data_female), data_female, NA), 2, na.interp)
    rownames(data_female_impute) = paste("A", 0:100, sep = "")
    colnames(data_female_impute) = get(prefectures[1])$year 
    output_path <- file.path(dir, "data/female/Japan_female.csv")
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    write.csv(data_female_impute, output_path, row.names = TRUE)

    for(ij in 2:num_prefs)
    {
        data_female = log(get(prefectures[ij])$rate$female)
        data_female_impute = apply(ifelse(is.finite(data_female), data_female, NA), 2, na.interp)
        rownames(data_female_impute) = paste("A", 0:100, sep = "")
        colnames(data_female_impute) = get(prefectures[ij])$year 
        output_path <- file.path(dir, paste("data/female/", prefectures[ij], "_female.csv", sep = ""))
        dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
        write.csv(data_female_impute, output_path, row.names = TRUE)
    }

    # male
    data_male = log(get(prefectures[1])$rate$male)
    data_male_impute = apply(ifelse(is.finite(data_male), data_male, NA), 2, na.interp)
    rownames(data_male_impute) = paste("A", 0:100, sep = "")
    colnames(data_male_impute) = get(prefectures[1])$year 
    output_path <- output_path <- file.path(dir, paste("data/male/Japan_male.csv", sep = ""))
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

    write.csv(data_male_impute, output_path, row.names = TRUE)

    for(ij in 2:num_prefs)
    {
        data_male = log(get(prefectures[ij])$rate$male)
        data_male_impute = apply(ifelse(is.finite(data_male), data_male, NA), 2, na.interp)
        rownames(data_male_impute) = paste("A", 0:100, sep = "")
        colnames(data_male_impute) = get(prefectures[ij])$year 
        output_path <- file.path(dir, paste("data/male/", prefectures[ij], "_male.csv", sep = ""))
        dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
        write.csv(data_male_impute, output_path, row.names = TRUE)
    }

    # total
    data_total = log(get(prefectures[1])$rate$total)
    data_total_impute = apply(ifelse(is.finite(data_total), data_total, NA), 2, na.interp)
    rownames(data_total_impute) = paste("A", 0:100, sep = "")
    colnames(data_total_impute) = get(prefectures[1])$year 
    output_path <- file.path(dir, paste("data/total/Japan_total.csv", sep = ""))
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    write.csv(data_total_impute, output_path, row.names = TRUE)

    for(ij in 2:num_prefs)
    {
        data_female = log(get(prefectures[ij])$rate$female)
        data_female_impute = apply(ifelse(is.finite(data_female), data_female, NA), 2, na.interp)
        rownames(data_female_impute) = paste("A", 0:100, sep = "")
        colnames(data_female_impute) = get(prefectures[ij])$year 
        output_path <- file.path(dir, paste("data/female/", prefectures[ij], "_female.csv", sep = ""))
        dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
        write.csv(data_female_impute, output_path, row.names = TRUE)
    }

    for(ij in 2:num_prefs)
    {
        data_total = log(get(prefectures[ij])$rate$total)
        data_total_impute = apply(ifelse(is.finite(data_total), data_total, NA), 2, na.interp)
        rownames(data_total_impute) = paste("A", 0:100, sep = "")
        colnames(data_total_impute) = get(prefectures[ij])$year 
        output_path <- file.path(dir, paste("data/total/", prefectures[ij], "_total.csv", sep = ""))
        dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
        write.csv(data_total_impute, output_path, row.names = TRUE)
    }

    # #################################################################################################
    # # smoothing: weighted penalized regression splines with a monotonic constraint for ages above 65
    # #################################################################################################

    output_path <- file.path(dir, "data/female/Japan_smooth_female.csv")

    for(ij in 1:num_prefs)
    {
        assign(prefectures_smooth[ij], smooth.demogdata(get(prefectures[ij])))
    }

    na_counter_female = na_counter_male = na_counter_total = rep(0, num_prefs)
    for(i in 1:num_prefs)
    {
        na_counter_female[i] = sum(is.infinite(log(get(prefectures_smooth[i])$rate$female)))
        na_counter_male[i] = sum(is.infinite(log(get(prefectures_smooth[i])$rate$male)))
        na_counter_total[i] = sum(is.infinite(log(get(prefectures_smooth[i])$rate$total)))
    }

    all(na_counter_female == 0)
    all(na_counter_male == 0)
    all(na_counter_total == 0)

    # female
    data_female = log(get(prefectures_smooth[1])$rate$female)
    rownames(data_female) = paste("A", 0:100, sep = "")
    colnames(data_female) = get(prefectures_smooth[1])$year 
    output_path <- file.path(dir, paste("data/female/Japan_smooth_female.csv", sep = ""))
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    write.csv(data_female, output_path, row.names = TRUE)

    for(ij in 2:num_prefs)
    {
        data_female = log(get(prefectures_smooth[ij])$rate$female)
        rownames(data_female) = paste("A", 0:100, sep = "")
        colnames(data_female) = get(prefectures_smooth[ij])$year 
        output_path <- file.path(dir, paste("data/female/", prefectures_smooth[ij], "_female.csv", sep = ""))
        dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
        write.csv(data_female_impute, output_path, row.names = TRUE)
    }

    # male
    data_male = log(get(prefectures_smooth[1])$rate$male)
    rownames(data_male) = paste("A", 0:100, sep = "")
    colnames(data_male) = get(prefectures_smooth[1])$year 
    output_path <- file.path(dir, paste("data/male/Japan_smooth_male.csv", sep = ""))
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    write.csv(data_male, output_path, row.names = TRUE)

    for(ij in 2:num_prefs)
    {
        data_male = log(get(prefectures_smooth[ij])$rate$male)
        rownames(data_male) = paste("A", 0:100, sep = "")
        colnames(data_male) = get(prefectures_smooth[ij])$year 
        output_path <- file.path(dir, paste("data/male/", prefectures_smooth[ij], "_male.csv", sep = ""))
        dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
        write.csv(data_male_impute, output_path, row.names = TRUE)
    }

    # total
    data_total = log(get(prefectures_smooth[1])$rate$total)
    rownames(data_total) = paste("A", 0:100, sep = "")
    colnames(data_total) = get(prefectures_smooth[1])$year 
    output_path <- file.path(dir, paste("data/total/Japan_smooth_total.csv", sep = ""))
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    write.csv(data_total, output_path, row.names = TRUE)

    for(ij in 2:num_prefs)
    {
        data_total = log(get(prefectures_smooth[ij])$rate$total)
        rownames(data_total) = paste("A", 0:100, sep = "")
        colnames(data_total) = get(prefectures_smooth[ij])$year 
        output_path <- file.path(dir, paste("data/total/", prefectures_smooth[ij], "_total.csv", sep = ""))
        dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
        write.csv(data_total_impute, output_path, row.names = TRUE)
    }

    # Save the state for future use
    save(list = ls(), file = state_file)
    cat("Processed and saved state to", state_file, "\n")

}