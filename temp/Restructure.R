###################
# Edit data format
###################

select_data <- function(selected_age)
{
  year_start = 1978
  year_end = 2021
  
  row_selected = which(rownames(get(state_smooth[1])$rate$female) == selected_age)
  column_start = which(colnames(get(state_smooth[1])$rate$female) == year_start)
  column_end = which(colnames(get(state_smooth[1])$rate$female) == year_end)
  
  df_export_list = list()
  for(ij in 2:48)
  {
    y_t_x = get(state_smooth[ij])$rate$female[row_selected, column_start:column_end]
    y_tm1_x = get(state_smooth[ij])$rate$female[row_selected, (column_start-1):(column_end-1)]
    y_tm2_x = get(state_smooth[ij])$rate$female[row_selected, (column_start-2):(column_end-2)]
    y_tm3_x = get(state_smooth[ij])$rate$female[row_selected, (column_start-3):(column_end-3)]
    
    y_tm1_xm1 = get(state_smooth[ij])$rate$female[row_selected-1, (column_start-1):(column_end-1)]
    y_tm2_xm1 = get(state_smooth[ij])$rate$female[row_selected-1, (column_start-2):(column_end-2)]
    y_tm3_xm1 = get(state_smooth[ij])$rate$female[row_selected-1, (column_start-3):(column_end-3)]
    
    temp_df = data.frame(cbind(y_t_x =  log(y_t_x), y_tm1_x = log(y_tm1_x), y_tm2_x = log(y_tm2_x), y_tm3_x = log(y_tm3_x), y_tm1_xm1 = log(y_tm1_xm1), y_tm2_xm1 = log(y_tm2_xm1), y_tm3_xm1 = log(y_tm3_xm1), Age = rep(selected_age, length(year_start:year_end)) , Year = as.numeric(year_start:year_end), Prefecture = rep(state[ij], length(year_start:year_end))) )
    
    df_export_list[[ij]] = temp_df
  }
  
  return(df_export_list = df_export_list)
}


out_all_list = list()
for(ik in 1:100)
{
  out_all_list[[ik]] = do.call("rbind", select_data(selected_age = ik))
}

out_all_df =  do.call("rbind", out_all_list)

output_path = paste("/Users/yang/Library/CloudStorage/OneDrive-TheUniversityofNewcastle/Data/Japanese Mortality Data/Restructured Data/Smooth_data_1978_2021.csv", sep = "")

write.csv(out_all_df, output_path, row.names = FALSE)


#########################################
# Raw mortality data for male and female
#########################################

select_data_raw <- function(selected_age, series = c("female", "male"), training_range = 1975:2011, lag_year = c(0,3))
{
  year_start = head(training_range, 1)
  year_end = tail(training_range, 1)
  
  row_selected = which((0:100) == selected_age)
  column_start = which((1975:2021) == year_start)
  column_end = which((1975:2021) == year_end)
  
  n_year = length(column_start:column_end)

  df_export_list = list()
  for(ij in 2:48)
  {
    if(series == "female")
    {
      data_df = get(state_impute_female[[ij]])
    }
    
    if(series == "male")
    {
      data_df = get(state_impute_male[[ij]])
    }
    
    
    if(lag_year == 0)
    {
      y_t_x = data_df[row_selected, column_start:column_end]
      
      y_t_xm1 = if(row_selected-1 < 1){rep(0, n_year)}else{data_df[max(row_selected-1, 0), column_start:column_end]}
      y_t_xm2 = if(row_selected-2 < 1){rep(0, n_year)}else{data_df[max(row_selected-2, 0), column_start:column_end]}
      y_t_xm3 = if(row_selected-3 < 1){rep(0, n_year)}else{data_df[max(row_selected-3, 0), column_start:column_end]}
      
      y_t_xp1 = if(row_selected+1 > 101){rep(0, n_year)}else{data_df[max(row_selected+1, 0), column_start:column_end]}
      y_t_xp2 = if(row_selected+2 > 101){rep(0, n_year)}else{data_df[max(row_selected+2, 0), column_start:column_end]}
      y_t_xp3 = if(row_selected+3 > 101){rep(0, n_year)}else{data_df[max(row_selected+3, 0), column_start:column_end]}
      
      df_export_list[[ij]] = data.frame(cbind(y_t_x =  y_t_x, y_t_xm1 = y_t_xm1, y_t_xm2 = y_t_xm2, y_t_xm3 = y_t_xm3,
                                 y_t_xp1 = y_t_xp1, y_t_xp2 = y_t_xp2, y_t_xp3 = y_t_xp3,
                                 Age = rep(selected_age, n_year), 
                                 Year = as.numeric(year_start:year_end),
                                 Sex = rep(series, n_year),
                                 Prefecture = rep(state[ij], n_year)))
    }
    
    if(lag_year == 3)
    {
      y_t_x = data_df[row_selected, column_start:column_end]
      
      y_tm1_x = data_df[row_selected, (column_start-1):(column_end-1)]
      y_tm2_x = data_df[row_selected, (column_start-2):(column_end-2)]
      y_tm3_x = data_df[row_selected, (column_start-3):(column_end-3)]
      
      y_tm1_xm1 = data_df[row_selected-1, (column_start-1):(column_end-1)]
      y_tm2_xm1 = data_df[row_selected-1, (column_start-2):(column_end-2)]
      y_tm3_xm1 = data_df[row_selected-1, (column_start-3):(column_end-3)]
      
      df_export_list[[ij]] = data.frame(cbind(y_t_x =  y_t_x, y_tm1_x = y_tm1_x, y_tm2_x = y_tm2_x, y_tm3_x = y_tm3_x, 
                                 y_tm1_xm1 = y_tm1_xm1, y_tm2_xm1 = y_tm2_xm1, y_tm3_xm1 = y_tm3_xm1, 
                                 Age = rep(selected_age, n_year), 
                                 Year = as.numeric(year_start:year_end), 
                                 Prefecture = rep(state[ij], n_year)))
    }
  }
  
  return(df_export_list = df_export_list)
}


# Imputed data without lagged observations

out_nolag_female = out_nolag_male = list()
for(ik in 0:100)
{
  out_nolag_female[[ik+1]] = do.call("rbind", select_data_raw(selected_age = ik, series = "female", training_range = 1975:2011, lag_year = 0))
  out_nolag_male[[ik+1]] = do.call("rbind", select_data_raw(selected_age = ik, series = "male", training_range = 1975:2011, lag_year = 0))
}

out_nolag_female_df =  do.call("rbind", out_nolag_female)
out_nolag_male_df =  do.call("rbind", out_nolag_male)


output_path = paste("/Users/yang/Library/CloudStorage/OneDrive-TheUniversityofNewcastle/Data/Japanese Mortality Data/Restructured Data/Smooth_data_1978_2021.csv", sep = "")

output_path_female = paste("C:/Users/rapha.THREADRIPPER/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Restructured Data/Imputed_data_female_no_lag.csv", sep = "")
output_path_male = paste("C:/Users/rapha.THREADRIPPER/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Restructured Data/Imputed_data_male_no_lag.csv", sep = "")

write.csv(out_nolag_female_df, output_path_female, row.names = FALSE)
write.csv(out_nolag_male_df, output_path_male, row.names = FALSE)

# Imputed data with 3 years of lagged observations

out_lag_female = out_lag_male = list()
for(ik in 1:100)
{
  out_lag_female[[ik]] = do.call("rbind", select_data_raw(selected_age = ik, series = "female", training_range = 1978:2011, lag_year = 3))
  out_lag_male[[ik]] = do.call("rbind", select_data_raw(selected_age = ik, series = "male", training_range = 1978:2011, lag_year = 3))
}

out_lag_female_df =  do.call("rbind", out_lag_female)
out_lag_male_df =  do.call("rbind", out_lag_male)

output_path_female = paste("C:/Users/rapha.THREADRIPPER/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Restructured Data/Imputed_data_female_with_lag.csv", sep = "")
output_path_male = paste("C:/Users/rapha.THREADRIPPER/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/Restructured Data/Imputed_data_male_with_lag.csv", sep = "")

write.csv(out_lag_female_df, output_path_female, row.names = FALSE)
write.csv(out_lag_male_df, output_path_male, row.names = FALSE)