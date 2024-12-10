##########################
# Compute point forecasts
##########################

require(demography)
require(StMoMo)

dir <- "src/benchmark"
source(file.path(dir, "settings.R"))
source(file.path(dir, "download.R"))
source(file.path(dir, "load.R"))


source(file.path(dir, "point_forecasts_helpers.R"))

err_fun_forecast_modified <- function(index, state_select, state_select_smooth, output_dir = file.path(dir, "results"))
{
    dir.create(output_dir, showWarnings = FALSE) # Ensure output directory exists

    n_year <- length(year_range)
    last_year <- tail(year_range, 1)

    train_lc_sum_female   = train_lc_sum_male = array(NA, dim = c(101,10,10))
    AIC_lc_sum_female   = AIC_lc_sum_male =rep(0, 10)
    BIC_lc_sum_female   = BIC_lc_sum_male =rep(0, 10)

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

    }

}

state = c("Japan")
state_smooth = c("Japan_smooth")
Japan_fore = err_fun_forecast_modified(index = 1, state_select = state, state_select_smooth = state_smooth)
