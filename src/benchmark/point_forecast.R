#######################################
# Benchmark all models
# - common functions like AIC/BIC, MSE, and processing data
#######################################

require(demography)
require(StMoMo)

dir <- "/workspaces/mortality/src/benchmark"

source(file.path(dir, "settings.R"))
source(file.path(dir, "download.R"))
source(file.path(dir, "load.R"))
source(file.path(dir, "point_forecast_helpers.R"))
source(file.path(dir, "point_forecast_methods.R"))


# This helper function does all the work for a single gender and a given method
run_forecast_for_gender <- function(gender, data_obj, test, n_splits, year_range, method) {
    n_year <- length(year_range)
    last_year <- tail(year_range, 1)

    train_res_array <- array(NA, dim = c(101, n_splits, n_splits))
    AIC_vals <- BIC_vals <- numeric(n_splits)

    # Select the forecast function based on the method
    if (method == "lc_sum") forecast_fun <- forecast_lc_sum
    else if (method == "cfr") forecast_fun <- forecast_cfr
    else if (method == "rh") forecast_fun <- forecast_rh
    else if (method == "apc") forecast_fun <- forecast_apc
    else if (method == "cbd") forecast_fun <- forecast_cbd
    else if (method == "m6") forecast_fun <- forecast_m6
    else if (method == "m7") forecast_fun <- forecast_m7
    else if (method == "m8") forecast_fun <- forecast_m8
    else if (method == "plat") forecast_fun <- forecast_plat
    else if (method == "lca_dt") forecast_fun <- forecast_lca_dt
    else if (method == "lca_dxt") forecast_fun <- forecast_lca_dxt
    else if (method == "lca_e0") forecast_fun <- forecast_lca_e0
    else if (method == "lca_none") forecast_fun <- forecast_lca_none
    else if (method == "fdm") forecast_fun <- forecast_fdm
    else if (method == "M_fdm") forecast_fun <- forecast_M_fdm
    else if (method == "pr") forecast_fun <- forecast_pr
    else if (method == "thiele") forecast_fun <- forecast_thiele
    else if (method == "thiele_sm") forecast_fun <- forecast_thiele_sm
    else stop(paste("No forecast function implemented for method:", method))

    # Extract actual values once outside the loop
    actual_data <- extract.years(test, years = (test$year[n_year-10] + 1):last_year)$rate[[gender]]

    # Create and write the actual data frame once, outside of the loop
    actual_df <- data.frame(
        age = rep(0:100, 10),  # Assuming 10 years total for simplicity
        year = rep((test$year[n_year-10] + 1):last_year, each = 101),
        actual = as.vector(log(actual_data))
        #actual_ln = as.vector(log(actual_data))
    )

    # Add forecast columns
    for (i in 1:n_splits) {
        actual_df[[paste0("fc", i)]] <- NA
    }

    # Forecast and store results for each split
    for (ik in seq_len(n_splits)) {

        start_year <- data_obj$year[1]
        end_year <- data_obj$year[n_year - 11 + ik]

        #cat("ik:", ik, "start_year:", start_year, "end_year:", end_year, "\n")

        # Prepare data
        obs <- prepare_data(data_obj, start_year, end_year)

        # Forecast using the chosen method
        res <- forecast_fun(obs)

        # Dynamically pick the correct forecast and IC elements for the chosen gender
        forecast_name <- paste0(gender, "_forecast")
        AIC_name <- paste0("AIC_", gender)
        BIC_name <- paste0("BIC_", gender)
        train_res_array[,,ik] <- res[[forecast_name]]
        AIC_vals[ik] <- res[[AIC_name]]
        BIC_vals[ik] <- res[[BIC_name]]
    }

    # Compute MSE
    mse_vals <- numeric(n_splits)
    for (ik in seq_len(n_splits)) {
        actual <- extract.years(test, years = (test$year[n_year-10] + ik):last_year)$rate[[gender]]
        mse_vals[ik] <- mse(log(train_res_array[,ik,1:(11-ik)]), log(actual))

        # Determine the number of prediction years for this split
        num_years <- 11 - ik
    
        # Fill the forecast columns diagonally for each year
        for (year_offset in 0:(num_years - 1)) {
            column_to_fill <- paste0("fc", year_offset + 1)  # Start from fc1 for each new year
            column_to_fill_ln <- paste0("fc_ln", year_offset + 1)  # Start from fc1 for each new year
            if (column_to_fill %in% names(actual_df)) {
                years_for_this_column <- (test$year[n_year-10] + ik + year_offset)
                rows_to_fill <- which(actual_df$year == years_for_this_column)
                actual_df[rows_to_fill, column_to_fill] <- log(train_res_array[,ik,year_offset + 1])
                #actual_df[rows_to_fill, column_to_fill_ln] <- log(train_res_array[,ik,year_offset + 1])
            }
        }

        #write.csv(actual_df, file = paste0("actual_data_", gender, "_", method, ".csv"), row.names = FALSE)

    }
    
    write.csv(actual_df, file = paste0("actual_data_", gender, "_", method, ".csv"), row.names = FALSE)

    # Structure results
    train_mse <- cbind(mse_vals)
    colnames(train_mse) <- method
    rownames(train_mse) <- 1:n_splits

    train_AIC <- cbind(AIC_vals)
    train_BIC <- cbind(BIC_vals)
    colnames(train_AIC) <- colnames(train_BIC) <- method

    train_fore <- list()
    train_fore[[method]] <- train_res_array

    list(
        train_fore = train_fore,
        train_AIC = train_AIC,
        train_BIC = train_BIC,
        train_mse = train_mse
    )
}

point_forecast <- function(index, state_select, state_select_smooth, output_dir = file.path(dir, "results"), method = "lc_sum") {
    dir.create(output_dir, showWarnings = FALSE)
    n_splits <- 10

    # Select the appropriate data object based on the method
    if (method %in% c("fdm", "M_fdm", "pr")) {
        data_obj <- get(state_select_smooth[index])
    } else {
        data_obj <- get(state_select[index])
    }
    
    test <- get(state_select[index])

    # Run separately for female and male with the chosen method
    male_res   <- run_forecast_for_gender("male", data_obj, test, n_splits, year_range, method)
    female_res <- run_forecast_for_gender("female", data_obj, test, n_splits, year_range, method)

    # Combine results into a single return structure
    return(list(
        train_female_fore = female_res$train_fore,
        train_male_fore   = male_res$train_fore,
        train_female_AIC  = female_res$train_AIC,
        train_female_BIC  = female_res$train_BIC,
        train_male_AIC    = male_res$train_AIC,
        train_male_BIC    = male_res$train_BIC,
        train_female_mse  = female_res$train_mse,
        train_male_mse    = male_res$train_mse
    ))
}

# Example usage
state = c("Japan")
state_smooth = c("Japan_smooth")

library(xtable)

# Define methods
all_methods <- c("lc_sum", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "M_fdm", "pr")

# Initialize lists to store results
results_female <- list()
results_male <- list()

# Evaluate point_forecast for OK methods
for (method in all_methods) {
    cat("Computing forecast for method:", method, "...\n")
    forecast_result <- point_forecast(index = 1, state_select = state, state_select_smooth = state_smooth, method = method)
    results_female[[method]] <- sqrt(forecast_result$train_female_mse)
    results_male[[method]] <- sqrt(forecast_result$train_male_mse)
}

# Combine results into tables
Japan_female_rmse_all <- do.call(cbind, results_female)
rownames(Japan_female_rmse_all) <- 1:10
Japan_female_rmse_all <- rbind(Japan_female_rmse_all, Mean = colMeans(Japan_female_rmse_all))
colnames(Japan_female_rmse_all) <- all_methods

Japan_male_rmse_all <- do.call(cbind, results_male)
rownames(Japan_male_rmse_all) <- 1:10
Japan_male_rmse_all <- rbind(Japan_male_rmse_all, Mean = colMeans(Japan_male_rmse_all))
colnames(Japan_male_rmse_all) <- all_methods

# Print tables
print(xtable(Japan_female_rmse_all, digits = 4))
print(xtable(Japan_male_rmse_all, digits = 4))
