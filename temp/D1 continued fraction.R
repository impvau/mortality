###########################################
# Experiments of continues fraction models
###########################################

# female series only

library(tidyverse)

# Forecasting functions

cf_forecast = function(index, coef_g0, coef_h0, coef_g1, last_train_year = 2016, fore_horizon = 5, include_constant = FALSE)
{
  if(!all.equal(length(coef_g0), length(coef_h0), length(coef_g1)))
  {
    warning("Uncomfortable input data matrix and coefficient matrices.")
  }
  
  output_array = target_mat = matrix(0, nrow = 100, ncol = fore_horizon)

  for(ih in 1:fore_horizon)
  {
    if(ih == 1)
    {
      year_fore = last_train_year + ih 
      column_fore = which(colnames(get(state_smooth[index])$rate$female) == year_fore)
      
      for(ik in 1:100)
      {
        row_selected = which(rownames(get(state_smooth[index])$rate$female) == ik)
        target_mat[ik, ih] = log(get(state[index])$rate$female[row_selected, column_fore])
        
        y_tm1_x = log(get(state_smooth[index])$rate$female[row_selected, column_fore-1])
        y_tm2_x = log(get(state_smooth[index])$rate$female[row_selected, column_fore-2])
        y_tm3_x = log(get(state_smooth[index])$rate$female[row_selected, column_fore-3])
        
        y_tm1_xm1 = log(get(state_smooth[index])$rate$female[row_selected-1, column_fore-1])
        y_tm2_xm1 = log(get(state_smooth[index])$rate$female[row_selected-1, column_fore-2])
        y_tm3_xm1 = log(get(state_smooth[index])$rate$female[row_selected-1, column_fore-3])
        input_temp = as.matrix(data.frame(cbind(y_tm1_x = y_tm1_x, y_tm2_x = y_tm2_x, y_tm3_x = y_tm3_x, 
                                                y_tm1_xm1 = y_tm1_xm1, y_tm2_xm1 = y_tm2_xm1, y_tm3_xm1 = y_tm3_xm1, 
                                                Age = ik , Year = year_fore)))
        
        if(include_constant)
        {
          g0_temp = cbind(input_temp, 1) %*% as.matrix(coef_g0, nrow = 1)
          g1_temp = cbind(input_temp, 1) %*% as.matrix(coef_g1, nrow = 1)
          h0_temp = cbind(input_temp, 1) %*% as.matrix(coef_h0, nrow = 1)
          
          output_array[ik,ih] = g0_temp + h0_temp/g1_temp
        } else{
          g0_temp = input_temp %*% as.matrix(coef_g0[-9], nrow = 1)
          g1_temp = input_temp %*% as.matrix(coef_g1[-9], nrow = 1)
          h0_temp = input_temp %*% as.matrix(coef_h0[-9], nrow = 1)
          
          output_array[ik,ih] = g0_temp + h0_temp/g1_temp
        }
        
      }
    }
    
    
    if(ih == 2)
    {
      year_fore = last_train_year + ih 
      column_fore = which(colnames(get(state_smooth[index])$rate$female) == year_fore)
      
      for(ik in 1:100)
      {
        row_selected = which(rownames(get(state_smooth[index])$rate$female) == ik)
        target_mat[ik, ih] = log(get(state[index])$rate$female[row_selected, column_fore])
        
        y_tm1_x =  output_array[ik,ih-1] # use previous steps' forecasts
        y_tm2_x = log(get(state_smooth[index])$rate$female[row_selected, column_fore-2])
        y_tm3_x = log(get(state_smooth[index])$rate$female[row_selected, column_fore-3])
        
        if(ik == 1)
        {
          y_tm1_xm1 =  log(get(state_smooth[index])$rate$female[row_selected-1, column_fore-1]) # use previous steps' forecasts
        } else {
          y_tm1_xm1 = output_array[ik,ih-1] # use previous steps' forecasts
        }
        
        y_tm2_xm1 = log(get(state_smooth[index])$rate$female[row_selected-1, column_fore-2])
        y_tm3_xm1 = log(get(state_smooth[index])$rate$female[row_selected-1, column_fore-3])
        input_temp = as.matrix(data.frame(cbind(y_tm1_x = y_tm1_x, y_tm2_x = y_tm2_x, y_tm3_x = y_tm3_x, 
                                                y_tm1_xm1 = y_tm1_xm1, y_tm2_xm1 = y_tm2_xm1, y_tm3_xm1 = y_tm3_xm1, 
                                                Age = ik , Year = year_fore)))
        
        if(include_constant)
        {
          g0_temp = cbind(input_temp, 1) %*% as.matrix(coef_g0, nrow = 1)
          g1_temp = cbind(input_temp, 1) %*% as.matrix(coef_g1, nrow = 1)
          h0_temp = cbind(input_temp, 1) %*% as.matrix(coef_h0, nrow = 1)
          
          output_array[ik,ih] = g0_temp + h0_temp/g1_temp
        } else{
          g0_temp = input_temp %*% as.matrix(coef_g0[-9], nrow = 1)
          g1_temp = input_temp %*% as.matrix(coef_g1[-9], nrow = 1)
          h0_temp = input_temp %*% as.matrix(coef_h0[-9], nrow = 1)
          
          output_array[ik,ih] = g0_temp + h0_temp/g1_temp
        }
      }
    }
    
    if(ih == 3)
    {
      year_fore = last_train_year + ih 
      column_fore = which(colnames(get(state_smooth[index])$rate$female) == year_fore)
      
      for(ik in 1:100)
      {
        row_selected = which(rownames(get(state_smooth[index])$rate$female) == ik)
        target_mat[ik, ih] = log(get(state[index])$rate$female[row_selected, column_fore])
        
        y_tm1_x = output_array[ik,ih-1] # use previous steps' forecasts
        y_tm2_x = output_array[ik,ih-2] # use previous steps' forecasts
        y_tm3_x = log(get(state_smooth[index])$rate$female[row_selected, column_fore-3])
        
        if(ik == 1)
        {
          y_tm1_xm1 =  log(get(state_smooth[index])$rate$female[row_selected-1, column_fore-1]) # use previous steps' forecasts
          y_tm2_xm1 =  log(get(state_smooth[index])$rate$female[row_selected-1, column_fore-2]) # use previous steps' forecasts
        } else {
          y_tm1_xm1 = output_array[ik,ih-1] # use previous steps' forecasts
          y_tm2_xm1 = output_array[ik,ih-2] # use previous steps' forecasts
        }
        y_tm3_xm1 = log(get(state_smooth[index])$rate$female[row_selected-1, column_fore-3])
        
        input_temp = as.matrix(data.frame(cbind(y_tm1_x = y_tm1_x, y_tm2_x = y_tm2_x, y_tm3_x = y_tm3_x, 
                                                y_tm1_xm1 = y_tm1_xm1, y_tm2_xm1 = y_tm2_xm1, y_tm3_xm1 = y_tm3_xm1, 
                                                Age = ik , Year = year_fore)))
        
        if(include_constant)
        {
          g0_temp = cbind(input_temp, 1) %*% as.matrix(coef_g0, nrow = 1)
          g1_temp = cbind(input_temp, 1) %*% as.matrix(coef_g1, nrow = 1)
          h0_temp = cbind(input_temp, 1) %*% as.matrix(coef_h0, nrow = 1)
          
          output_array[ik,ih] = g0_temp + h0_temp/g1_temp
        } else{
          g0_temp = input_temp %*% as.matrix(coef_g0[-9], nrow = 1)
          g1_temp = input_temp %*% as.matrix(coef_g1[-9], nrow = 1)
          h0_temp = input_temp %*% as.matrix(coef_h0[-9], nrow = 1)
          
          output_array[ik,ih] = g0_temp + h0_temp/g1_temp
        }
      }
    }
    
    
    if(ih %in% c(4,5))
    {
      year_fore = last_train_year + ih 
      column_fore = which(colnames(get(state_smooth[index])$rate$female) == year_fore)
      
      for(ik in 1:100)
      {
        row_selected = which(rownames(get(state_smooth[index])$rate$female) == ik)
        target_mat[ik, ih] = log(get(state[index])$rate$female[row_selected, column_fore])
        
        y_tm1_x = output_array[ik,ih-1] # use previous steps' forecasts
        y_tm2_x = output_array[ik,ih-2] # use previous steps' forecasts
        y_tm3_x = output_array[ik,ih-3] # use previous steps' forecasts
        
        if(ik == 1)
        {
          y_tm1_xm1 =  log(get(state_smooth[index])$rate$female[row_selected-1, column_fore-1]) # use previous steps' forecasts
          y_tm2_xm1 =  log(get(state_smooth[index])$rate$female[row_selected-1, column_fore-2]) # use previous steps' forecasts
          y_tm3_xm1 =  log(get(state_smooth[index])$rate$female[row_selected-1, column_fore-3]) # use previous steps' forecasts
        } else {
          y_tm1_xm1 = output_array[ik,ih-1] # use previous steps' forecasts
          y_tm2_xm1 = output_array[ik,ih-2] # use previous steps' forecasts
          y_tm3_xm1 = output_array[ik,ih-3] # use previous steps' forecasts
        }
        
        input_temp = as.matrix(data.frame(cbind(y_tm1_x = y_tm1_x, y_tm2_x = y_tm2_x, y_tm3_x = y_tm3_x, 
                                                y_tm1_xm1 = y_tm1_xm1, y_tm2_xm1 = y_tm2_xm1, y_tm3_xm1 = y_tm3_xm1, 
                                                Age = ik , Year = year_fore)))
        
        if(include_constant)
        {
          g0_temp = cbind(input_temp, 1) %*% as.matrix(coef_g0, nrow = 1)
          g1_temp = cbind(input_temp, 1) %*% as.matrix(coef_g1, nrow = 1)
          h0_temp = cbind(input_temp, 1) %*% as.matrix(coef_h0, nrow = 1)
          
          output_array[ik,ih] = g0_temp + h0_temp/g1_temp
        } else{
          g0_temp = input_temp %*% as.matrix(coef_g0[-9], nrow = 1)
          g1_temp = input_temp %*% as.matrix(coef_g1[-9], nrow = 1)
          h0_temp = input_temp %*% as.matrix(coef_h0[-9], nrow = 1)
          
          output_array[ik,ih] = g0_temp + h0_temp/g1_temp
        }
      }
    }
  }

  return(list(forecast_result = output_array, forecast_target = target_mat))
}


D1_continued_fraction = function(index, coef_g0, coef_h0, coef_g1, test_year = 2017:2021, horizon = 5, include_constant = FALSE)
{
  if(!all.equal(length(coef_g0), length(coef_h0), length(coef_g1)))
  {
    warning("Uncomfortable input data matrix and coefficient matrices.")
  }
  
  fore_output = fore_target = array(0, dim = c(length(test_year), 100, length(test_year)))
  for(ij in 1:length(test_year))
  {
    fore_temp = cf_forecast(index = index, coef_g0 = coef_g0, coef_h0 = coef_h0, coef_g1 = coef_g1, last_train_year = test_year[1]+ij-2, fore_horizon = (horizon+1-ij), include_constant = include_constant)
    
  fore_output[(1:(horizon+1-ij)),,ij] = fore_temp$forecast_result
  fore_target[(1:(horizon+1-ij)),,ij] = fore_temp$forecast_target
  }
  
  return(list(prefecture = paste("Prefecture", index, sep = "_"), log_forecasts = fore_output, log_obs = fore_target))
}

########
## test
test = cf_forecast(index = 3, coef_g0 = m1_coef_g0, coef_h0 = m1_coef_h0, coef_g1 = m1_coef_g1, last_train_year = 2016, fore_horizon = 5)
test_2 = cf_forecast(index = 3, coef_g0 = m2_coef_g0, coef_h0 = m2_coef_h0, coef_g1 = m2_coef_g1, last_train_year = 2016, fore_horizon = 5, include_constant = TRUE)

test_fore_1 = D1_continued_fraction(index = 3, coef_g0 = m1_coef_g0, coef_h0 = m1_coef_h0, coef_g1 = m1_coef_g1, test_year = 2017:2021, horizon = 5, include_constant = FALSE)
test_fore_2 = D1_continued_fraction(index = 3, coef_g0 = m2_coef_g0, coef_h0 = m2_coef_h0, coef_g1 = m2_coef_g1, test_year = 2017:2021, horizon = 5, include_constant = TRUE)
########

# Forecasts evaluation functions
fore_eval = function(cf_result, bench_result)
{
  ME_bench = MAE_bench = RMSE_bench = ME_cf = MAE_cf = RMSE_cf = rep(0, 5)
  for(ij in 1:5)
  {
    target = cf_result$log_obs[ij, , (1:(6-ij))]
    cf_forecasts = cf_result$log_forecasts[ij, , (1:(6-ij))]
    bench_forecasts = log(bench_result[ij, 2:101, (1:(6-ij))])
    
    ME_bench[ij] = ftsa:::me(target[!is.infinite(target)], bench_forecasts[!is.infinite(target)])
    MAE_bench[ij] = ftsa:::mae(target[!is.infinite(target)], bench_forecasts[!is.infinite(target)])
    RMSE_bench[ij] = ftsa:::rmse(target[!is.infinite(target)], bench_forecasts[!is.infinite(target)])
    
    ME_cf[ij] = ftsa:::me(target[!is.infinite(target)], cf_forecasts[!is.infinite(target)])
    MAE_cf[ij] = ftsa:::mae(target[!is.infinite(target)], cf_forecasts[!is.infinite(target)])
    RMSE_cf[ij] = ftsa:::rmse(target[!is.infinite(target)], cf_forecasts[!is.infinite(target)])
  }
  
  return(list(ME_bench = ME_bench, MAE_bench = MAE_bench, RMSE_bench = RMSE_bench, 
              ME_cf = ME_cf, MAE_cf = MAE_cf, RMSE_cf = RMSE_cf))
}


####################################
# Depth 0 Continued Fraction Models
####################################

# coefficients: y_tm1_x, y_tm2_x, y_tm3_x, y_tm1_xm1, y_tm2_xm1, y_tm3_xm1, Age, Year, constant

# Model 1 by Pablo
m1_coef_g0 = c(-0.06331, 0.597683, 0.294266, -0.14478, 0.14252, 0.095565, 0, 0.000257, 0) 
m1_coef_h0 = c(-1.94511, 0.753612, 0.017573, 0.012615, 0.432184, 0.337738, 0, -0.00016, 0)
m1_coef_g1 = c(0.002373, 1.172641, 0.278134, -0.90857, -0.03565, 0.036219, 0, -0.02383, 0)

# Model 2 by Andy
m2_coef_g0 = c(0, 0.144855157616217228, -0.0550797249505651509, -0.16972964003749802, 0.186973938894168634, 0, 0, 0, 0.013952132299403534)
m2_coef_h0 = c(0.0865704973735543781, 0, 0.0588434575299097151, 0, 0, 0, 0.000838109925526536021, 0, -0.0876766848266946341)
m2_coef_g1 = c(0, -0.00300577352609899531, 0, -0.000304812780295019524, 0.0020751650115393306, 0, 0, 0, 0.162012194873397186)

coef_matrix = rbind(m1_coef_g0, m1_coef_h0, m1_coef_g1, m2_coef_g0, m2_coef_h0, m2_coef_g1)
library(xtable)
xtable(coef_matrix, digits = 6)

# All prefectures including national level
m1_forecast_all = m2_forecast_all = list()
for(ij in 1:48)
{
  m1_forecast_all[[ij]] = D1_continued_fraction(index = ij, coef_g0 = m1_coef_g0, coef_h0 = m1_coef_h0, coef_g1 = m1_coef_g1, test_year = 2017:2021, horizon = 5, include_constant = FALSE)
  m2_forecast_all[[ij]] = D1_continued_fraction(index = ij, coef_g0 = m2_coef_g0, coef_h0 = m2_coef_h0, coef_g1 = m2_coef_g1, test_year = 2017:2021, horizon = 5, include_constant = TRUE)
}

m1_error_all = m2_error_all = list()
for(ij in 1:48)
{
  m1_error_all[[ij]] = fore_eval(cf_result = m1_forecast_all[[ij]], bench_result = get(ind_state_forc_female[ij]))
  m2_error_all[[ij]] = fore_eval(cf_result = m2_forecast_all[[ij]], bench_result = get(ind_state_forc_female[ij]))
}

# Summary of result
m1_mae = m1_rmse = m2_mae = m2_rmse = bench_mae = bench_rmse = matrix(0, nrow = 5, ncol = 48)
for(ij in 1:48)
{
  # MAE
  bench_mae[,ij] = m1_error_all[[ij]]$MAE_bench
  m1_mae[,ij] = m1_error_all[[ij]]$MAE_cf
  m2_mae[,ij] = m2_error_all[[ij]]$MAE_cf
  
  # RMSE
  bench_rmse[,ij] = m1_error_all[[ij]]$RMSE_bench
  m1_rmse[,ij] = m1_error_all[[ij]]$RMSE_cf
  m2_rmse[,ij] = m2_error_all[[ij]]$RMSE_cf
}

colnames(m1_mae) = colnames(m1_rmse) = colnames(m2_mae) = 
  colnames(m2_rmse) = colnames(bench_mae) = colnames(bench_rmse) = c("Japan", paste("Prefecture", 1:47, sep = "_"))

average_mae = cbind(apply(bench_mae[,-1], 1, mean), apply(m1_mae[,-1], 1, mean), apply(m2_mae[,-1], 1, mean))
average_rmse = cbind(apply(bench_rmse[,-1], 1, mean), apply(m1_rmse[,-1], 1, mean), apply(m2_rmse[,-1], 1, mean))
xtable(rbind(average_mae, average_rmse), digits = 4)




## Make some plots
output_path = paste("/Users/yang/Library/CloudStorage/OneDrive-TheUniversityofNewcastle/Data/Japanese Mortality Data/R Working files/plots/")
# National Level
savepdf(paste(output_path, "MAE", 1, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
plot(bench_mae[,1], type = "b", ylim = c(0, 3.5), xlab = "Forecasting Horizon", ylab = "MAE (logarithm scale)")
lines(m1_mae[,1], col = 2, type = "b")
lines(m2_mae[,1], col = 4, type = "b")
legend("topleft", lty = c(1, 1, 1), col = c(1,2,4), c("Benchmark by Hyndman", "M1 by Pablo", "M2 by Andy"))
title(main = "Japanese Female Mortality, 2017:2021")
dev.off()

savepdf(paste(output_path, "RMSE", 1, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
plot(bench_rmse[,1], type = "b", ylim = c(0, 4.5), xlab = "Forecasting Horizon", ylab = "RASE (logarithm scale)")
lines(m1_rmse[,1], col = 2, type = "b")
lines(m2_rmse[,1], col = 4, type = "b")
legend("topleft", lty = c(1, 1, 1), col = c(1,2,4), c("Benchmark by Hyndman", "M1 by Pablo", "M2 by Andy"))
title(main = "Japanese Female Mortality, 2017:2021")
dev.off()

# Prefecture Level

for(ij in 2:48)
{
  # MAE
  savepdf(paste(output_path, "MAE", ij, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
  plot(bench_mae[,ij], type = "b", ylim = range(c(bench_mae[,ij], m1_mae[,ij], m2_mae[,ij])), xlab = "Forecasting Horizon", ylab = "MAE (logarithm scale)")
  lines(m1_mae[,ij], col = 2, type = "b")
  lines(m2_mae[,ij], col = 4, type = "b")
  title(main = paste(state[ij], "Female Mortality, 2017:2021"))
  dev.off()
  
  # RMSE
  savepdf(paste(output_path, "RMSE", ij, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
  plot(bench_rmse[,ij], type = "b", ylim = range(c(bench_rmse[,ij], m1_rmse[,ij], m2_rmse[,ij])), xlab = "Forecasting Horizon", ylab = "RMSE (logarithm scale)")
  lines(m1_rmse[,ij], col = 2, type = "b")
  lines(m2_rmse[,ij], col = 4, type = "b")
  title(main = paste(state[ij], "Female Mortality, 2017:2021"))
  dev.off()
}
