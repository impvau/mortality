

standardise = function(x){(x-min(x))/(max(x)-min(x))}

Information_Criteria <- function(residuals_mat, K)
{
  n = length(as.numeric(residuals_mat))
  RSS = sum(residuals_mat^2, na.rm = TRUE)
  
  AIC_out = n*log(RSS/n) + 2*K
  BIC_out = n*log(RSS/n) + log(n)*K
  
  return(list(AIC = AIC_out, BIC = BIC_out))
}

mse <- function(forecast, true)
{
  if (length(forecast) != length(true)) 
    stop("MSE: the lengths of input vectors must be the same.")
  
  # ignore inf values (useful for log mortality)
  
  if(sum(is.infinite(true)) > 0)
  {
    err = mean((true[!is.infinite(true)] - forecast[!is.infinite(true)])^2)
  } else {
    err = mean((na.omit(as.numeric(true - forecast)))^2)
  }
  
  return(round(err, 6))
}

f2 <- function(x, ages) mean(ages) - x
f3 <- function(x, ages) pmax(mean(ages)-x,0)
constPlat <- function(ax, bx, kt, b0x, gc, wxt, ages)
{
  nYears <- dim(wxt)[2]
  x <- ages
  t <- 1:nYears
  c <- (1 - tail(ages, 1)):(nYears - ages[1])
  xbar <- mean(x)
  #\sum g(c)=0, \sum cg(c)=0, \sum c^2g(c)=0
  phiReg <- lm(gc ~ 1 + c + I(c^2), na.action = na.omit)
  phi <- coef(phiReg)
  gc <- gc - phi[1] - phi[2] * c - phi[3] * c^2
  kt[2, ] <- kt[2, ] + 2 * phi[3] * t
  kt[1, ] <- kt[1, ] + phi[2] * t + phi[3] * (t^2 - 2 * xbar * t)
  ax <- ax + phi[1] - phi[2] * x + phi[3] * x^2
  #\sum kt[i, ] = 0
  ci <- rowMeans(kt, na.rm = TRUE)
  ax <- ax + ci[1] + ci[2] * (xbar - x) + ci[3] * pmax(xbar - x, 0)
  kt[1, ] <- kt[1, ] - ci[1]
  kt[2, ] <- kt[2, ] - ci[2]
  kt[3, ] <- kt[3, ] - ci[3]
  list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
}

forecast_m_manual = function(obs, gender = "female", h)
{
  fdm_obs = fdm(obs, series = gender, method = "M")
  
  beta_forecast = matrix(NA, nrow = h, ncol = ncol(fdm_obs$coeff))
  for(ij in 1:ncol(fdm_obs$coeff))
  {
    beta_forecast[,ij] = forecast(auto.arima(fdm_obs$coeff[, ij]), h = h)$mean
  }
  
  forecast_rate = exp(fdm_obs$basis %*% t(beta_forecast)) 
  
  return(forecast_rate)
}

# Function to load data from CSV
load_model_data <- function(output_dir, gender, model_names) {
  result <- list(mse = list(), AIC = list(), BIC = list(), forecast = list())
  
  for (model in model_names) {
    # Load mse, AIC, and BIC
    result$mse[[model]] <- as.matrix(read.csv(paste0(output_dir, "mse_", gender, "_japan_", model, ".csv"), row.names = 1))
    result$AIC[[model]] <- as.matrix(read.csv(paste0(output_dir, "AIC_", gender, "_japan_", model, ".csv"), row.names = 1))
    result$BIC[[model]] <- as.matrix(read.csv(paste0(output_dir, "BIC_", gender, "_japan_", model, ".csv"), row.names = 1))
    # Load forecast
    result$forecast[[model]] <- as.matrix(read.csv(paste0(output_dir, "forecast_", gender, "_japan_", model, ".csv"), row.names = 1))
  }
  
  # Combine lists back into matrices and lists
  return(list(
    mse = do.call(cbind, result$mse),
    AIC = do.call(cbind, result$AIC),
    BIC = do.call(cbind, result$BIC),
    forecast = result$forecast
  ))
}

# Function to save data to CSV
save_model_data <- function(output_dir, gender, data_list, model_names) {
  for (data_type in names(data_list)) {
    data_matrix <- data_list[[data_type]]
    if (data_type == "forecast") {
      # Save forecasts as separate files
      for (i in seq_along(model_names)) {
        write.csv(data_matrix[[i]], paste0(output_dir, data_type, "_", gender, "_japan_", model_names[i], ".csv"), row.names = TRUE)
      }
    } else {
      # Save mse, AIC, BIC as column-separated files
      for (i in seq_along(model_names)) {
        write.csv(data_matrix[, i, drop = FALSE], paste0(output_dir, data_type, "_", gender, "_japan_", model_names[i], ".csv"), row.names = TRUE)
      }
    }
  }
}

prepare_data <- function(data_obj, start_year, end_year) {
    obs <- extract.years(data_obj, start_year:end_year)
    # Replace mortality > 1 with NA
    obs$rate$female <- replace(obs$rate$female, obs$rate$female > 1, NA)
    obs$rate$male   <- replace(obs$rate$male, obs$rate$male > 1, NA)

    # Interpolate missing data
    if (any(!is.finite(obs$rate$female))) {
        ind <- which(!is.finite(obs$rate$female))
        obs$rate$female <- matrix(na.interp(replace(as.numeric(obs$rate$female), ind, NA)), 101, ncol(obs$rate$female))
    }

    if (any(!is.finite(obs$rate$male))) {
        ind <- which(!is.finite(obs$rate$male))
        obs$rate$male <- matrix(na.interp(replace(as.numeric(obs$rate$male), ind, NA)), 101, ncol(obs$rate$male))
    }

    return(obs)
}