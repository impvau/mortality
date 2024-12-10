##########################
# Compute point forecasts
##########################

require(demography)
require(StMoMo)

######################################
# Compute forecasts of various models
######################################

dir <- "temp"
source(file.path(dir, "download_preproc.R"))

# useful functions

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

err_fun_forecast_modified <- function(index, state_select, state_select_smooth)
{
  n_year = length(get(state_select[index])$year)
  last_year = get(state_select[index])$year[n_year]
  
  ################################################
  # Lee-Carter, Renshaw-Haberman, APC, M6, M7, M8
  ################################################
  
  train_lc_sum_female   = train_lc_sum_male = 
    train_rh_female  = train_rh_male = 
    train_apc_female = train_apc_male =
    train_cbd_female = train_cbd_male = 
    train_m6_female  = train_m6_male = 
    train_m7_female  = train_m7_male = 
    train_m8_female  = train_m8_male = 
    train_plat_female = train_plat_male = array(NA, dim = c(101,10,10))
  
  
  AIC_lc_sum_female   = AIC_lc_sum_male = 
    AIC_rh_female  = AIC_rh_male = 
    AIC_apc_female = AIC_apc_male =
    AIC_cbd_female = AIC_cbd_male = 
    AIC_m6_female  = AIC_m6_male = 
    AIC_m7_female  = AIC_m7_male = 
    AIC_m8_female  = AIC_m8_male = 
    AIC_plat_female = AIC_plat_male = rep(0, 10)
  
  BIC_lc_sum_female   = BIC_lc_sum_male = 
    BIC_rh_female  = BIC_rh_male = 
    BIC_apc_female = BIC_apc_male =
    BIC_cbd_female = BIC_cbd_male = 
    BIC_m6_female  = BIC_m6_male = 
    BIC_m7_female  = BIC_m7_male = 
    BIC_m8_female  = BIC_m8_male = 
    BIC_plat_female = BIC_plat_male = rep(0, 10)
  
  
  for(ik in 1:10)
  {
    obs = extract.years(get(state_select[index]), (get(state_select[index])$year[1]):(get(state_select[index])$year[n_year-11]+ik))
    
    # replace mortality rate greater than 1 by NA
    
    obs$rate$female = replace(obs$rate$female, which(obs$rate$female > 1), NA)
    obs$rate$male   = replace(obs$rate$male,   which(obs$rate$male > 1),   NA)
    
    if(any(!is.finite(obs$rate$female)))
    {
      ind = which(!is.finite(obs$rate$female))
      obs$rate$female = matrix(na.interp(replace(as.numeric(obs$rate$female), ind, NA)), 101, ncol(obs$rate$female))
    }
    
    if(any(!is.finite(obs$rate$male)))
    {
      ind = which(!is.finite(obs$rate$male))
      obs$rate$male = matrix(na.interp(replace(as.numeric(obs$rate$male), ind, NA)), 101, ncol(obs$rate$male))
    }
    
    ### Lee-Carter ###
    
    wxt <- genWeightMat(ages = 0:100, years = obs$year, clip = 3)
    
    ## female
    
    # sum(kt) = sum
    
    LC1 = lc(const = "sum")
    LCfit1_female = fit(LC1, data = StMoMoData(obs, series = "female", type = "central"),
                        wxt = wxt, verbose = FALSE)
    train_lc_sum_female[,,ik] = forecast(LCfit1_female, h = 10)$rate
    
    IC_temp = Information_Criteria(residuals_mat = LCfit1_female$fittingModel$residuals, K = 3)
    AIC_lc_sum_female[ik] = IC_temp$AIC
    BIC_lc_sum_female[ik] = IC_temp$BIC
    rm(LCfit1_female); rm(IC_temp)
    
    ## male         
    
    # sum(kt) = sum
    
    LCfit1_male = fit(LC1, data = StMoMoData(obs, series = "male", type = "central"),
                      wxt = wxt, verbose = FALSE)
    train_lc_sum_male[,,ik] = forecast(LCfit1_male, h = 10)$rate
    IC_temp = Information_Criteria(residuals_mat = LCfit1_male$fittingModel$residuals, K = 3)
    AIC_lc_sum_male[ik] = IC_temp$AIC
    BIC_lc_sum_male[ik] = IC_temp$BIC
    rm(LCfit1_male)
    
    ### Renshaw-Haberman ###
    
    # female
    
    LCfit_female <-  fit(lc(), data = StMoMoData(obs, series = "female", type = "central"), verbose = FALSE)
    RHfit_female <- fit(rh(), data = StMoMoData(obs, series = "female", type = "central"), 
                        wxt = wxt, start.ax = LCfit_female$ax,
                        start.bx = LCfit_female$bx, start.kt = LCfit_female$kt, verbose = FALSE)
    dum_val = try(forecast(RHfit_female, h = 10)$rate, silent = TRUE)
    if(class(dum_val)[1] == "try-error")
    {
      train_rh_female[,,ik] = rep(NA, 101)
      AIC_rh_female[ik] = NA
      BIC_rh_female[ik] = NA
      
    } else
    {
      train_rh_female[,,ik] = dum_val
      IC_temp = Information_Criteria(residuals_mat = RHfit_female$fittingModel$residuals, K = 5)
      AIC_rh_female[ik] = IC_temp$AIC
      BIC_rh_female[ik] = IC_temp$BIC
    }
    rm(LCfit_female); rm(RHfit_female); rm(dum_val); rm(IC_temp)
    
    # male
    
    LCfit_male = fit(lc(), data = StMoMoData(obs, series = "male", type = "central"), verbose = FALSE)
    RHfit_male = fit(rh(), data = StMoMoData(obs, series = "male", type = "central"), 
                     wxt = wxt, start.ax = LCfit_male$ax,
                     start.bx = LCfit_male$bx, start.kt = LCfit_male$kt, verbose = FALSE)
    dum_val = try(forecast(RHfit_male, h = 10)$rate, silent = TRUE)
    if(class(dum_val)[1] == "try-error")
    {
      train_rh_male[,,ik] = rep(NA, 101)
      AIC_rh_male[ik] = NA
      BIC_rh_male[ik] = NA
    } else
    {
      train_rh_male[,,ik] = dum_val
      IC_temp = Information_Criteria(residuals_mat = RHfit_male$fittingModel$residualss, K = 5)
      AIC_rh_male[ik] = IC_temp$AIC
      BIC_rh_male[ik] = IC_temp$BIC
    }
    rm(LCfit_male); rm(RHfit_male); rm(IC_temp)
    
    ### Age-Period-Cohort ###
    
    # female
    
    APCfit_female <- fit(apc(), data = StMoMoData(obs, series = "female", type = "central"), 
                         wxt = wxt, verbose = FALSE)
    train_apc_female[,,ik] = forecast(APCfit_female, h = 10)$rate
    IC_temp = Information_Criteria(residuals_mat = APCfit_female$fittingModel$residuals, K = 3)
    AIC_apc_female[ik] = IC_temp$AIC
    BIC_apc_female[ik] = IC_temp$BIC
    rm(APCfit_female); rm(IC_temp)
    
    # male
    
    APCfit_male = fit(apc(), data = StMoMoData(obs, series = "male", type = "central"), 
                      wxt = wxt, verbose = FALSE)
    train_apc_male[,,ik] = forecast(APCfit_male, h = 10)$rate
    IC_temp = Information_Criteria(residuals_mat = APCfit_male$fittingModel$residuals, K = 3)
    AIC_apc_male[ik] = IC_temp$AIC
    BIC_apc_male[ik] = IC_temp$BIC
    rm(APCfit_male); rm(IC_temp)
    
    ### CBD ###
    
    # female
    
    CBDfit_female = fit(cbd(link = "log"), data = StMoMoData(obs, series = "female", type = "initial"), 
                        wxt = wxt, verbose = FALSE)
    train_cbd_female[,,ik] = forecast(CBDfit_female, h = 10)$rate
    IC_temp = Information_Criteria(residuals_mat = CBDfit_female$fittingModel$residuals, K = 2)
    AIC_cbd_female[ik] = IC_temp$AIC
    BIC_cbd_female[ik] = IC_temp$BIC
    rm(CBDfit_female); rm(IC_temp)
    
    # male
    
    CBDfit_male = fit(cbd(link = "log"), data = StMoMoData(obs, series = "male", type = "initial"), 
                      wxt = wxt, verbose = FALSE)
    train_cbd_male[,,ik] = forecast(CBDfit_male, h = 10)$rate
    IC_temp = Information_Criteria(residuals_mat = CBDfit_male$fittingModel$residuals, K = 2)
    AIC_cbd_male[ik] = IC_temp$AIC
    BIC_cbd_male[ik] = IC_temp$BIC
    rm(CBDfit_male); rm(IC_temp)
    
    ### M6 ###
    
    # female
    
    M6fit_female <- fit(m6(link = "log"), data = StMoMoData(obs, series = "female", type = "initial"), 
                        wxt = wxt, verbose = FALSE)
    dum_val = try(forecast(M6fit_female, h = 10)$rate, silent = TRUE)
    if(class(dum_val)[1] == "try-error")
    {
      train_m6_female[,,ik] = rep(NA, 101)
      AIC_m6_female[ik] = NA
      BIC_m6_female[ik] = NA
    } else
    {
      train_m6_female[,,ik] = dum_val
      
      IC_temp = Information_Criteria(residuals_mat = M6fit_female$fittingModel$residuals, K = 3)
      AIC_m6_female[ik] = IC_temp$AIC
      BIC_m6_female[ik] = IC_temp$BIC
    }
    rm(M6fit_female); rm(dum_val); rm(IC_temp)
    
    # male
    
    M6fit_male = fit(m6(link = "log"), data = StMoMoData(obs, series = "male", type = "initial"), 
                     wxt = wxt, verbose = FALSE)
    dum_val = try(forecast(M6fit_male, h = 10)$rate, silent = TRUE)
    if(class(dum_val)[1] == "try-error")
    {
      train_m6_male[,,ik] = rep(NA, 101)
      AIC_m6_male[ik] = NA
      BIC_m6_male[ik] = NA
    } else
    {
      train_m6_male[,,ik] = dum_val
      IC_temp = Information_Criteria(residuals_mat = M6fit_male$fittingModel$residuals, K = 3)
      AIC_m6_male[ik] = IC_temp$AIC
      BIC_m6_male[ik] = IC_temp$BIC
    }
    rm(M6fit_male); rm(dum_val); rm(IC_temp)
    
    ### M7 ###
    
    # female
    
    M7fit_female <- fit(m7(link = "log"), data = StMoMoData(obs, series = "female", type = "initial"), 
                        wxt = wxt, verbose = FALSE)
    dum_val = try(forecast(M7fit_female, h = 10)$rate, silent = TRUE)
    if(class(dum_val)[1] == "try-error")
    {
      train_m7_female[,,ik] = rep(NA, 101)
      AIC_m7_female[ik] = NA
      BIC_m7_female[ik] = NA
    } else
    {
      train_m7_female[,,ik] = dum_val
      IC_temp = Information_Criteria(residuals_mat = M7fit_female$fittingModel$residuals, K = 4)
      AIC_m7_female[ik] = IC_temp$AIC
      BIC_m7_female[ik] = IC_temp$BIC
    }
    rm(M7fit_female); rm(dum_val); rm(IC_temp)
    
    # male
    
    M7fit_male = fit(m7(link = "log"), data = StMoMoData(obs, series = "male", type = "initial"), 
                     wxt = wxt, verbose = FALSE)
    dum_val = try(forecast(M7fit_male, h = 10)$rate, silent = TRUE)
    if(class(dum_val)[1] == "try-error")
    {
      train_m7_male[,,ik] = rep(NA, 101)
      AIC_m7_male[ik] = NA
      BIC_m7_male[ik] = NA
    } else
    {
      train_m7_male[,,ik] = dum_val
      IC_temp = Information_Criteria(residuals_mat = M7fit_male$fittingModel$residuals, K = 4)
      AIC_m7_male[ik] = IC_temp$AIC
      BIC_m7_male[ik] = IC_temp$BIC
    }
    rm(M7fit_male); rm(dum_val); rm(IC_temp)
    
    ### M8 ###
    
    # female
    
    M8fit_female = fit(m8(link = "log", xc = 110), data = StMoMoData(obs, series = "female", type = "initial"), 
                       wxt = wxt, verbose = FALSE)
    dum_val = try(forecast(M8fit_female, h = 10)$rate, silent = TRUE)        
    if(class(dum_val)[1] == "try-error")
    {
      train_m8_female[,,ik] = rep(NA, 101)
      AIC_m8_female[ik] = NA
      BIC_m8_female[ik] = NA
    } else
    {
      train_m8_female[,,ik] = dum_val
      IC_temp = Information_Criteria(residuals_mat = M8fit_female$fittingModel$residuals, K = 3)
      AIC_m8_female[ik] = IC_temp$AIC
      BIC_m8_female[ik] = IC_temp$BIC
    }
    rm(M8fit_female); rm(dum_val); rm(IC_temp)
    
    # male
    
    M8fit_male = fit(m8(link = "log", xc = 110), data = StMoMoData(obs, series = "male", type = "initial"), 
                     wxt = wxt, verbose = FALSE)
    dum_val = try(forecast(M8fit_male, h = 10)$rate, silent = TRUE)        
    if(class(dum_val)[1] == "try-error")
    {
      train_m8_male[,,ik] = rep(NA, 101)
      AIC_m8_male[ik] = NA
      BIC_m8_male[ik] = NA
    } else
    {
      train_m8_male[,,ik] = dum_val
      IC_temp = Information_Criteria(residuals_mat = M8fit_male$fittingModel$residuals, K = 3)
      AIC_m8_male[ik] = IC_temp$AIC
      BIC_m8_male[ik] = IC_temp$BIC
    }
    
    rm(M8fit_male); rm(dum_val); rm(IC_temp)
    
    ### Plat ###
    
    # female
    
    PLAT <- StMoMo(link = "log", staticAgeFun = TRUE,
                   periodAgeFun = c("1", f2, f3), cohortAgeFun = "1",
                   constFun = constPlat)
    PLATfit_female <- fit(PLAT, data = StMoMoData(obs, series = "female", type = "central"), 
                          wxt = wxt, verbose = FALSE)
    train_plat_female[,,ik] = forecast(PLATfit_female, h = 10)$rate
    IC_temp = Information_Criteria(residuals_mat = PLATfit_female$fittingModel$residuals, K = 4)
    AIC_plat_female[ik] = IC_temp$AIC
    BIC_plat_female[ik] = IC_temp$BIC
    rm(PLATfit_female); rm(IC_temp)
    
    # male
    
    PLATfit_male = fit(PLAT, data = StMoMoData(obs, series = "male", type = "central"),
                       wxt = wxt, verbose = FALSE)
    train_plat_male[,,ik] = forecast(PLATfit_male, h = 10)$rate
    IC_temp = Information_Criteria(residuals_mat = PLATfit_male$fittingModel$residuals, K = 4)
    AIC_plat_male[ik] = IC_temp$AIC
    BIC_plat_male[ik] = IC_temp$BIC
    rm(PLATfit_male); rm(PLAT); rm(IC_temp)
    
    print(paste("LC&RH&M", ik, sep = " "))
    rm(ik)
  }
  
  ###############
  # Compute MSE
  ###############
  
  train_lc_sum_female_mse   = train_lc_sum_male_mse   = 
    train_rh_female_mse       = train_rh_male_mse =     
    train_apc_female_mse      = train_apc_male_mse = 
    train_cbd_female_mse      = train_cbd_male_mse = 
    train_m6_female_mse       = train_m6_male_mse = 
    train_m7_female_mse       = train_m7_male_mse = 
    train_m8_female_mse       = train_m8_male_mse = 
    train_plat_female_mse     = train_plat_male_mse = vector("numeric", 10)
  for(ik in 1:10)
  {
    # female
    
    train_lc_sum_female_mse[ik]   = mse(log(train_lc_sum_female[,ik,1:(11-ik)]),  log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_rh_female_mse[ik]       = mse(log(train_rh_female[,ik,1:(11-ik)]),      log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_apc_female_mse[ik]      = mse(log(train_apc_female[,ik,1:(11-ik)]),     log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_cbd_female_mse[ik]      = mse(log(train_cbd_female[,ik,1:(11-ik)]),     log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_m6_female_mse[ik]       = mse(log(train_m6_female[,ik,1:(11-ik)]),      log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_m7_female_mse[ik]       = mse(log(train_m7_female[,ik,1:(11-ik)]),      log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_m8_female_mse[ik]       = mse(log(train_m8_female[,ik,1:(11-ik)]),      log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_plat_female_mse[ik]     = mse(log(train_plat_female[,ik,1:(11-ik)]),    log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    
    # male
    
    train_lc_sum_male_mse[ik]   = mse(log(train_lc_sum_male[,ik,1:(11-ik)]),   log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_rh_male_mse[ik]       = mse(log(train_rh_male[,ik,1:(11-ik)]),       log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_apc_male_mse[ik]      = mse(log(train_apc_male[,ik,1:(11-ik)]),      log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_cbd_male_mse[ik]      = mse(log(train_cbd_male[,ik,1:(11-ik)]),      log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_m6_male_mse[ik]       = mse(log(train_m6_male[,ik,1:(11-ik)]),       log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_m7_male_mse[ik]       = mse(log(train_m7_male[,ik,1:(11-ik)]),       log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_m8_male_mse[ik]       = mse(log(train_m8_male[,ik,1:(11-ik)]),       log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_plat_male_mse[ik]     = mse(log(train_plat_male[,ik,1:(11-ik)]),     log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
  }        
  
  ######################################
  # Lee-Carter using demography package
  ######################################
  
  train_lca_dt_female = train_lca_dxt_female = train_lca_e0_female = train_lca_none_female = 
    train_lca_dt_male   = train_lca_dxt_male   = train_lca_e0_male   = train_lca_none_male   = array(NA, dim = c(101,10,10))
  
  AIC_lca_dt_female = AIC_lca_dxt_female = AIC_lca_e0_female = AIC_lca_none_female = 
    AIC_lca_dt_male = AIC_lca_dxt_male   = AIC_lca_e0_male   = AIC_lca_none_male = 
    BIC_lca_dt_female = BIC_lca_dxt_female = BIC_lca_e0_female = BIC_lca_none_female = 
    BIC_lca_dt_male   = BIC_lca_dxt_male   = BIC_lca_e0_male   = BIC_lca_none_male = rep(0, 10)
  
  for(ik in 1:10)
  {
    obs = extract.years(get(state_select[index]), (get(state_select[index])$year[1]):(get(state_select[index])$year[n_year-11]+ik))
    
    # replace mortality rate greater than 1 by NA
    
    obs$rate$female = replace(obs$rate$female, which(obs$rate$female > 1), NA)
    obs$rate$male   = replace(obs$rate$male,   which(obs$rate$male > 1),   NA)
    
    if(any(!is.finite(obs$rate$female)))
    {
      ind = which(!is.finite(obs$rate$female))
      obs$rate$female = matrix(na.interp(replace(as.numeric(obs$rate$female), ind, NA)), 101, ncol(obs$rate$female))
    }
    
    if(any(!is.finite(obs$rate$male)))
    {
      ind = which(!is.finite(obs$rate$male))
      obs$rate$male = matrix(na.interp(replace(as.numeric(obs$rate$male), ind, NA)), 101, ncol(obs$rate$male))
    }
    
    
    # female
    
    ## lca_dt
    lca_dt_female = lca(obs, series = "female", adjust = "dt", interpolate = TRUE)
    train_lca_dt_female[,,ik]   = forecast(lca_dt_female,  h = 10)$rate$female
    IC_lca_dt = Information_Criteria(residuals_mat = lca_dt_female$residuals$y, K = 3)
    AIC_lca_dt_female[ik] = IC_lca_dt$AIC
    BIC_lca_dt_female[ik] = IC_lca_dt$BIC
    
    ## lca_dxt
    lca_dxt_female = lca(obs, series = "female", adjust = "dxt", interpolate = TRUE)
    train_lca_dxt_female[,,ik]  = forecast(lca_dxt_female, h = 10)$rate$female
    IC_lca_dxt = Information_Criteria(residuals_mat = lca_dxt_female$residuals$y, K = 3)
    AIC_lca_dxt_female[ik] = IC_lca_dxt$AIC
    BIC_lca_dxt_female[ik] = IC_lca_dxt$BIC
    
    ## lca_e0
    lca_e0_female = lca(obs, series = "female", adjust = "e0", interpolate = TRUE)
    train_lca_e0_female[,,ik]   = forecast(lca_e0_female, h = 10)$rate$female
    IC_lca_e0 = Information_Criteria(residuals_mat = lca_e0_female$residuals$y, K = 3)
    AIC_lca_e0_female[ik] = IC_lca_e0$AIC
    BIC_lca_e0_female[ik] = IC_lca_e0$BIC
    
    ## lca_none
    lca_none_female = lca(obs, series = "female", adjust = "none", interpolate = TRUE)
    train_lca_none_female[,,ik] = forecast(lca_none_female, h = 10)$rate$female
    IC_lca_none = Information_Criteria(residuals_mat = lca_none_female$residuals$y, K = 3)
    AIC_lca_none_female[ik] = IC_lca_none$AIC
    BIC_lca_none_female[ik] = IC_lca_none$BIC
    
    
    # male
    
    ## lca_dt
    lca_dt_male = lca(obs, series = "male", adjust = "dt", interpolate = TRUE)
    train_lca_dt_male[,,ik]   = forecast(lca_dt_male,  h = 10)$rate$male
    IC_lca_dt = Information_Criteria(residuals_mat = lca_dt_male$residuals$y, K = 3)
    AIC_lca_dt_male[ik] = IC_lca_dt$AIC
    BIC_lca_dt_male[ik] = IC_lca_dt$BIC
    
    ## lca_dxt
    lca_dxt_male = lca(obs, series = "male", adjust = "dxt", interpolate = TRUE)
    train_lca_dxt_male[,,ik]  = forecast(lca_dxt_male, h = 10)$rate$male
    IC_lca_dxt = Information_Criteria(residuals_mat = lca_dxt_male$residuals$y, K = 3)
    AIC_lca_dxt_male[ik] = IC_lca_dxt$AIC
    BIC_lca_dxt_male[ik] = IC_lca_dxt$BIC
    
    ## lca_e0
    lca_e0_male = lca(obs, series = "male", adjust = "e0", interpolate = TRUE)
    train_lca_e0_male[,,ik]   = forecast(lca_e0_male, h = 10)$rate$male
    IC_lca_e0 = Information_Criteria(residuals_mat = lca_e0_male$residuals$y, K = 3)
    AIC_lca_e0_male[ik] = IC_lca_e0$AIC
    BIC_lca_e0_male[ik] = IC_lca_e0$BIC
    
    ## lca_none
    lca_none_male = lca(obs, series = "male", adjust = "none", interpolate = TRUE)
    train_lca_none_male[,,ik] = forecast(lca_none_male, h = 10)$rate$male
    IC_lca_none = Information_Criteria(residuals_mat = lca_none_male$residuals$y, K = 3)
    AIC_lca_none_male[ik] = IC_lca_none$AIC
    BIC_lca_none_male[ik] = IC_lca_none$BIC
    
    print(paste("LC", ik, sep = " "))
    rm(ik); rm(obs)
  }  
  
  train_lca_dt_female_mse = train_lca_dxt_female_mse = 
    train_lca_e0_female_mse = train_lca_none_female_mse = 
    train_lca_dt_male_mse = train_lca_dxt_male_mse = 
    train_lca_e0_male_mse = train_lca_none_male_mse = vector("numeric", 10)
  for(ik in 1:10)
  {
    # female
    
    train_lca_dt_female_mse[ik]   = mse(log(train_lca_dt_female[,ik,1:(11-ik)]),   log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_lca_dxt_female_mse[ik]  = mse(log(train_lca_dxt_female[,ik,1:(11-ik)]),  log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_lca_e0_female_mse[ik]   = mse(log(train_lca_e0_female[,ik,1:(11-ik)]),   log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_lca_none_female_mse[ik] = mse(log(train_lca_none_female[,ik,1:(11-ik)]), log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    
    # male
    
    train_lca_dt_male_mse[ik]   = mse(log(train_lca_dt_male[,ik,1:(11-ik)]),   log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_lca_dxt_male_mse[ik]  = mse(log(train_lca_dxt_male[,ik,1:(11-ik)]),  log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_lca_e0_male_mse[ik]   = mse(log(train_lca_e0_male[,ik,1:(11-ik)]),   log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_lca_none_male_mse[ik] = mse(log(train_lca_none_male[,ik,1:(11-ik)]), log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
  }
  
  ########################
  # Functional data model
  ########################
  
  train_fdm_female = train_fdm_male = train_robust_fdm_female = train_robust_fdm_male = 
    train_pr_female = train_pr_male = array(NA, dim = c(101,10,10))
  
  
  AIC_fdm_female = AIC_fdm_male = AIC_robust_fdm_female = AIC_robust_fdm_male = AIC_pr_female = AIC_pr_male =
    BIC_fdm_female = BIC_fdm_male = BIC_robust_fdm_female = BIC_robust_fdm_male = BIC_pr_female = BIC_pr_male = rep(0, 10)
  
  for(ik in 1:10)
  {
    obs = extract.years(get(state_select_smooth[index]), (get(state_select_smooth[index])$year[1]):(get(state_select[index])$year[n_year-11]+ik))
    
    # replace mortality rate greater than 1 by NA
    
    obs$rate$female = replace(obs$rate$female, which(obs$rate$female > 1), NA)
    obs$rate$male   = replace(obs$rate$male,   which(obs$rate$male > 1),   NA)
    
    if(any(!is.finite(obs$rate$female)))
    {
      ind = which(!is.finite(obs$rate$female))
      obs$rate$female = matrix(na.interp(replace(as.numeric(obs$rate$female), ind, NA)), 101, ncol(obs$rate$female))
    }
    
    if(any(!is.finite(obs$rate$male)))
    {
      ind = which(!is.finite(obs$rate$male))
      obs$rate$male = matrix(na.interp(replace(as.numeric(obs$rate$male), ind, NA)), 101, ncol(obs$rate$male))
    }
    
    ## fdm
    
    # female
    fdm_female = fdm(obs, series = "female", method = "classical")
    train_fdm_female[,,ik] = forecast(fdm_female, h = 10)$rate$female
    IC_fdm = Information_Criteria(residuals_mat = fdm_female$residuals$y, K = 6)
    AIC_fdm_female[ik] = IC_fdm$AIC
    BIC_fdm_female[ik] = IC_fdm$BIC
    
    # male
    fdm_male = fdm(obs, series = "male", method = "classical")
    train_fdm_male[,,ik] = forecast(fdm_male, h = 10)$rate$male
    IC_fdm = Information_Criteria(residuals_mat = fdm_male$residuals$y, K = 6)
    AIC_fdm_male[ik] = IC_fdm$AIC
    BIC_fdm_male[ik] = IC_fdm$BIC
    
    ## robust_fdm
    
    # female
    robust_fdm_female = fdm(obs, series = "female", method = "M")
    train_robust_fdm_female[,,ik] = forecast_m_manual(obs = obs, "female", h = 10)
    IC_robust_fdm = Information_Criteria(residuals_mat = robust_fdm_female$residuals$y, K = 6)
    AIC_robust_fdm_female[ik] = IC_robust_fdm$AIC
    BIC_robust_fdm_female[ik] = IC_robust_fdm$BIC
    
    # male
    robust_fdm_male = fdm(obs, series = "male", method = "M")
    train_robust_fdm_male[,,ik] = forecast_m_manual(obs = obs, "male", h = 10)
    IC_robust_fdm = Information_Criteria(residuals_mat = robust_fdm_male$residuals$y, K = 6)
    AIC_robust_fdm_male[ik] = IC_robust_fdm$AIC
    BIC_robust_fdm_male[ik] = IC_robust_fdm$BIC
    
    
    ## product-ratio
    
    pr_fdm = coherentfdm(obs)
    train_coherent_fdm = forecast(pr_fdm, h = 10)
    train_pr_female[,,ik] = train_coherent_fdm$female$rate$female
    train_pr_male[,,ik] = train_coherent_fdm$male$rate$male
    
    
    residuals_pr_female = train_coherent_fdm$female$model$female - train_coherent_fdm$female$fitted$y
    IC_pr = Information_Criteria(residuals_mat = residuals_pr_female, K = 12)
    AIC_pr_female[ik] = IC_pr$AIC
    BIC_pr_female[ik] = IC_pr$BIC
    
    residuals_pr_male = train_coherent_fdm$male$model$male - train_coherent_fdm$male$fitted$y
    IC_pr = Information_Criteria(residuals_mat = residuals_pr_male, K = 12)
    AIC_pr_male[ik] = IC_pr$AIC
    BIC_pr_male[ik] = IC_pr$BIC
    
    
    print(paste("Functional", ik, sep = " "))
    rm(ik); rm(obs)
  }    
  
  train_fdm_female_mse = train_fdm_male_mse = 
    train_robust_fdm_female_mse = train_robust_fdm_male_mse = 
    train_pr_female_mse = train_pr_male_mse = vector("numeric", 10)
  for(ik in 1:10)
  {
    # classical
    
    train_fdm_female_mse[ik] = mse(log(train_fdm_female[,ik,1:(11-ik)]), log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_fdm_male_mse[ik] = mse(log(train_fdm_male[,ik,1:(11-ik)]),   log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    
    # M
    
    train_robust_fdm_female_mse[ik] = mse(log(train_robust_fdm_female[,ik,1:(11-ik)]), log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_robust_fdm_male_mse[ik] = mse(log(train_robust_fdm_male[,ik,1:(11-ik)]),   log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    
    # product-ratio
    
    train_pr_female_mse[ik] = mse(log(train_pr_female[,ik,1:(11-ik)]), log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_pr_male_mse[ik] = mse(log(train_pr_male[,ik,1:(11-ik)]), log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
  }
  
  ##########
  # summary
  ##########
  
  # female (errors & forecasts & AIC & BIC)
  
  train_female_mse = cbind(train_lc_sum_female_mse,     train_rh_female_mse,
                           train_apc_female_mse,        train_cbd_female_mse,
                           train_m6_female_mse,         train_m7_female_mse,
                           train_m8_female_mse,         train_plat_female_mse,
                           train_lca_dt_female_mse,     train_lca_dxt_female_mse,    
                           train_lca_e0_female_mse,     train_lca_none_female_mse,   
                           train_fdm_female_mse,        train_robust_fdm_female_mse, 
                           train_pr_female_mse) 
  
  train_female_AIC = cbind(AIC_lc_sum_female,     AIC_rh_female,
                           AIC_apc_female,        AIC_cbd_female,
                           AIC_m6_female,         AIC_m7_female,
                           AIC_m8_female,         AIC_plat_female,
                           AIC_lca_dt_female,     AIC_lca_dxt_female,    
                           AIC_lca_e0_female,     AIC_lca_none_female,   
                           AIC_fdm_female,        AIC_robust_fdm_female, 
                           AIC_pr_female) 
  
  train_female_BIC = cbind(BIC_lc_sum_female,     BIC_rh_female,
                           BIC_apc_female,        BIC_cbd_female,
                           BIC_m6_female,         BIC_m7_female,
                           BIC_m8_female,         BIC_plat_female,
                           BIC_lca_dt_female,     BIC_lca_dxt_female,    
                           BIC_lca_e0_female,     BIC_lca_none_female,   
                           BIC_fdm_female,        BIC_robust_fdm_female, 
                           BIC_pr_female) 
  
  
  train_female_fore = list()
  train_female_fore[[1]] = train_lc_sum_female
  train_female_fore[[2]] = train_rh_female
  train_female_fore[[3]] = train_apc_female
  train_female_fore[[4]] = train_cbd_female
  train_female_fore[[5]] = train_m6_female
  train_female_fore[[6]] = train_m7_female
  train_female_fore[[7]] = train_m8_female
  train_female_fore[[8]] = train_plat_female
  train_female_fore[[9]] = train_lca_dt_female
  train_female_fore[[10]] = train_lca_dxt_female
  train_female_fore[[11]] = train_lca_e0_female
  train_female_fore[[12]] = train_lca_none_female
  train_female_fore[[13]] = train_fdm_female 
  train_female_fore[[14]] = train_robust_fdm_female
  train_female_fore[[15]] = train_pr_female
  
  # male (errors & forecasts & AIC & BIC)
  
  train_male_mse = cbind(train_lc_sum_male_mse,     train_rh_male_mse,
                         train_apc_male_mse,        train_cbd_male_mse,
                         train_m6_male_mse,         train_m7_male_mse,
                         train_m8_male_mse,         train_plat_male_mse,
                         train_lca_dt_male_mse,     train_lca_dxt_male_mse,    
                         train_lca_e0_male_mse,     train_lca_none_male_mse,   
                         train_fdm_male_mse,        train_robust_fdm_male_mse, 
                         train_pr_male_mse) 
  
  train_male_AIC = cbind(AIC_lc_sum_male,     AIC_rh_male,
                         AIC_apc_male,        AIC_cbd_male,
                         AIC_m6_male,         AIC_m7_male,
                         AIC_m8_male,         AIC_plat_male,
                         AIC_lca_dt_male,     AIC_lca_dxt_male,    
                         AIC_lca_e0_male,     AIC_lca_none_male,   
                         AIC_fdm_male,        AIC_robust_fdm_male, 
                         AIC_pr_male) 
  
  train_male_BIC = cbind(BIC_lc_sum_male,     BIC_rh_male,
                         BIC_apc_male,        BIC_cbd_male,
                         BIC_m6_male,         BIC_m7_male,
                         BIC_m8_male,         BIC_plat_male,
                         BIC_lca_dt_male,     BIC_lca_dxt_male,    
                         BIC_lca_e0_male,     BIC_lca_none_male,   
                         BIC_fdm_male,        BIC_robust_fdm_male, 
                         BIC_pr_male) 
  
  train_male_fore = list()
  train_male_fore[[1]] = train_lc_sum_male
  train_male_fore[[2]] = train_rh_male
  train_male_fore[[3]] = train_apc_male
  train_male_fore[[4]] = train_cbd_male
  train_male_fore[[5]] = train_m6_male
  train_male_fore[[6]] = train_m7_male
  train_male_fore[[7]] = train_m8_male
  train_male_fore[[8]] = train_plat_male
  train_male_fore[[9]] = train_lca_dt_male
  train_male_fore[[10]] = train_lca_dxt_male
  train_male_fore[[11]] = train_lca_e0_male
  train_male_fore[[12]] = train_lca_none_male
  train_male_fore[[13]] = train_fdm_male 
  train_male_fore[[14]] = train_robust_fdm_male
  train_male_fore[[15]] = train_pr_male
  
  colnames(train_male_mse) = colnames(train_female_mse) = c("lc_sum", "rh", "apc", "cbd",
                                                            "m6", "m7", "m8", "plat", "dt", "dxt", "e0", "none", 
                                                            "fdm", "M_fdm", "pr")
  rownames(train_male_mse) = rownames(train_female_mse) = 1:10
  return(list(train_female_fore = train_female_fore, train_male_fore = train_male_fore,
              train_female_AIC = train_female_AIC, train_female_BIC = train_female_BIC,
              train_male_AIC = train_male_AIC, train_male_BIC = train_male_BIC,
              train_female_mse = train_female_mse, train_male_mse = train_male_mse))
}


# Compute point forecasts

## JPN ALL 1973~2022: training 1973~2012; testing 2013~2022


state = c("Japan")
state_smooth = c("Japan_smooth")

Japan_fore = err_fun_forecast_modified(index = 1, state_select = state, state_select_smooth = state_smooth)


####################
# Output of results
####################

library(xtable)

# female
Japan_female_rmse_all = rbind(sqrt(Japan_fore$train_female_mse), apply(sqrt(Japan_fore$train_female_mse), 2, mean))
rownames(Japan_female_rmse_all) = c(1:10, "Mean")

xtable(Japan_female_rmse_all, digits = 4)

# male
Japan_male_rmse_all = rbind(sqrt(Japan_fore$train_male_mse), apply(sqrt(Japan_fore$train_male_mse), 2, mean))
rownames(Japan_male_rmse_all) = c(1:10, "Mean")

xtable(Japan_male_rmse_all, digits = 4)

save(Japan_fore, file = "15_Model_forecasts.RData")



