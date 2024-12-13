#######################################
# Forecast models
# - implementation of all forecasting models
#######################################

forecast_lc_sum <- function(obs) {
    wxt <- genWeightMat(ages = 0:100, years = obs$year, clip = 3)
    LC1 <- lc(const = "sum")

    # Female
    LCfit1_female <- fit(LC1, data = StMoMoData(obs, series = "female", type = "central"),
                         wxt = wxt, verbose = FALSE)
    female_forecast <- forecast(LCfit1_female, h = 10)$rate
    IC_female <- Information_Criteria(residuals_mat = LCfit1_female$fittingModel$residuals, K = 3)

    # Male
    LCfit1_male <- fit(LC1, data = StMoMoData(obs, series = "male", type = "central"),
                       wxt = wxt, verbose = FALSE)
    male_forecast <- forecast(LCfit1_male, h = 10)$rate
    IC_male <- Information_Criteria(residuals_mat = LCfit1_male$fittingModel$residuals, K = 3)

    list(
        female_forecast = female_forecast,
        male_forecast = male_forecast,
        AIC_female = IC_female$AIC,
        BIC_female = IC_female$BIC,
        AIC_male = IC_male$AIC,
        BIC_male = IC_male$BIC
    )
}

### Renshaw-Haberman (rh)
forecast_rh <- function(obs) {
    wxt <- genWeightMat(ages = 0:100, years = obs$year, clip = 3)

    # Female
    LCfit_female <- fit(lc(), data = StMoMoData(obs, series = "female", type = "central"), verbose = FALSE)
    RHfit_female <- fit(rh(), data = StMoMoData(obs, series = "female", type = "central"), 
                        wxt = wxt, start.ax = LCfit_female$ax, start.bx = LCfit_female$bx, start.kt = LCfit_female$kt, 
                        verbose = FALSE)
    dum_val_female <- try(forecast(RHfit_female, h = 10)$rate, silent = TRUE)
    if (inherits(dum_val_female, "try-error")) {
        female_forecast <- matrix(NA, nrow = 101, ncol = 10)
        AIC_female <- NA
        BIC_female <- NA
    } else {
        female_forecast <- dum_val_female
        IC_female <- Information_Criteria(residuals_mat = RHfit_female$fittingModel$residuals, K = 5)
        AIC_female <- IC_female$AIC
        BIC_female <- IC_female$BIC
    }

    # Male
    LCfit_male <- fit(lc(), data = StMoMoData(obs, series = "male", type = "central"), verbose = FALSE)
    RHfit_male <- fit(rh(), data = StMoMoData(obs, series = "male", type = "central"), 
                      wxt = wxt, start.ax = LCfit_male$ax, start.bx = LCfit_male$bx, start.kt = LCfit_male$kt, 
                      verbose = FALSE)
    dum_val_male <- try(forecast(RHfit_male, h = 10)$rate, silent = TRUE)
    if (inherits(dum_val_male, "try-error")) {
        male_forecast <- matrix(NA, nrow = 101, ncol = 10)
        AIC_male <- NA
        BIC_male <- NA
    } else {
        male_forecast <- dum_val_male
        IC_male <- Information_Criteria(residuals_mat = RHfit_male$fittingModel$residuals, K = 5)
        AIC_male <- IC_male$AIC
        BIC_male <- IC_male$BIC
    }

    list(
        female_forecast = female_forecast,
        male_forecast = male_forecast,
        AIC_female = AIC_female,
        BIC_female = BIC_female,
        AIC_male = AIC_male,
        BIC_male = BIC_male
    )
}

### Age-Period-Cohort (apc)
forecast_apc <- function(obs) {
    wxt <- genWeightMat(ages = 0:100, years = obs$year, clip = 3)

    # Female
    APCfit_female <- fit(apc(), data = StMoMoData(obs, series = "female", type = "central"), wxt = wxt, verbose = FALSE)
    female_forecast <- forecast(APCfit_female, h = 10)$rate
    IC_female <- Information_Criteria(residuals_mat = APCfit_female$fittingModel$residuals, K = 3)
    AIC_female <- IC_female$AIC
    BIC_female <- IC_female$BIC

    # Male
    APCfit_male <- fit(apc(), data = StMoMoData(obs, series = "male", type = "central"), wxt = wxt, verbose = FALSE)
    male_forecast <- forecast(APCfit_male, h = 10)$rate
    IC_male <- Information_Criteria(residuals_mat = APCfit_male$fittingModel$residuals, K = 3)
    AIC_male <- IC_male$AIC
    BIC_male <- IC_male$BIC

    list(
        female_forecast = female_forecast,
        male_forecast = male_forecast,
        AIC_female = AIC_female,
        BIC_female = BIC_female,
        AIC_male = AIC_male,
        BIC_male = BIC_male
    )
}

### CBD
forecast_cbd <- function(obs) {
    wxt <- genWeightMat(ages = 0:100, years = obs$year, clip = 3)

    # Female
    CBDfit_female <- fit(cbd(link = "log"), data = StMoMoData(obs, series = "female", type = "initial"), 
                         wxt = wxt, verbose = FALSE)
    female_forecast <- forecast(CBDfit_female, h = 10)$rate
    IC_female <- Information_Criteria(residuals_mat = CBDfit_female$fittingModel$residuals, K = 2)
    AIC_female <- IC_female$AIC
    BIC_female <- IC_female$BIC

    # Male
    CBDfit_male <- fit(cbd(link = "log"), data = StMoMoData(obs, series = "male", type = "initial"), 
                       wxt = wxt, verbose = FALSE)
    male_forecast <- forecast(CBDfit_male, h = 10)$rate
    IC_male <- Information_Criteria(residuals_mat = CBDfit_male$fittingModel$residuals, K = 2)
    AIC_male <- IC_male$AIC
    BIC_male <- IC_male$BIC

    list(
        female_forecast = female_forecast,
        male_forecast = male_forecast,
        AIC_female = AIC_female,
        BIC_female = BIC_female,
        AIC_male = AIC_male,
        BIC_male = BIC_male
    )
}

### M6
forecast_m6 <- function(obs) {
    wxt <- genWeightMat(ages = 0:100, years = obs$year, clip = 3)

    # Female
    M6fit_female <- fit(m6(link = "log"), data = StMoMoData(obs, series = "female", type = "initial"), 
                        wxt = wxt, verbose = FALSE)
    dum_val_female <- try(forecast(M6fit_female, h = 10)$rate, silent = TRUE)
    if (inherits(dum_val_female, "try-error")) {
        female_forecast <- matrix(NA, 101, 10)
        AIC_female <- NA
        BIC_female <- NA
    } else {
        female_forecast <- dum_val_female
        IC_female <- Information_Criteria(residuals_mat = M6fit_female$fittingModel$residuals, K = 3)
        AIC_female <- IC_female$AIC
        BIC_female <- IC_female$BIC
    }

    # Male
    M6fit_male <- fit(m6(link = "log"), data = StMoMoData(obs, series = "male", type = "initial"), 
                      wxt = wxt, verbose = FALSE)
    dum_val_male <- try(forecast(M6fit_male, h = 10)$rate, silent = TRUE)
    if (inherits(dum_val_male, "try-error")) {
        male_forecast <- matrix(NA, 101, 10)
        AIC_male <- NA
        BIC_male <- NA
    } else {
        male_forecast <- dum_val_male
        IC_male <- Information_Criteria(residuals_mat = M6fit_male$fittingModel$residuals, K = 3)
        AIC_male <- IC_male$AIC
        BIC_male <- IC_male$BIC
    }

    list(
        female_forecast = female_forecast,
        male_forecast = male_forecast,
        AIC_female = AIC_female,
        BIC_female = BIC_female,
        AIC_male = AIC_male,
        BIC_male = BIC_male
    )
}

### M7
forecast_m7 <- function(obs) {
    wxt <- genWeightMat(ages = 0:100, years = obs$year, clip = 3)

    # Female
    M7fit_female <- fit(m7(link = "log"), data = StMoMoData(obs, series = "female", type = "initial"), 
                        wxt = wxt, verbose = FALSE)
    dum_val_female <- try(forecast(M7fit_female, h = 10)$rate, silent = TRUE)
    if (inherits(dum_val_female, "try-error")) {
        female_forecast <- matrix(NA, 101, 10)
        AIC_female <- NA
        BIC_female <- NA
    } else {
        female_forecast <- dum_val_female
        IC_female <- Information_Criteria(residuals_mat = M7fit_female$fittingModel$residuals, K = 4)
        AIC_female <- IC_female$AIC
        BIC_female <- IC_female$BIC
    }

    # Male
    M7fit_male <- fit(m7(link = "log"), data = StMoMoData(obs, series = "male", type = "initial"), 
                      wxt = wxt, verbose = FALSE)
    dum_val_male <- try(forecast(M7fit_male, h = 10)$rate, silent = TRUE)
    if (inherits(dum_val_male, "try-error")) {
        male_forecast <- matrix(NA, 101, 10)
        AIC_male <- NA
        BIC_male <- NA
    } else {
        male_forecast <- dum_val_male
        IC_male <- Information_Criteria(residuals_mat = M7fit_male$fittingModel$residuals, K = 4)
        AIC_male <- IC_male$AIC
        BIC_male <- IC_male$BIC
    }

    list(
        female_forecast = female_forecast,
        male_forecast = male_forecast,
        AIC_female = AIC_female,
        BIC_female = BIC_female,
        AIC_male = AIC_male,
        BIC_male = BIC_male
    )
}

### M8
forecast_m8 <- function(obs) {
    wxt <- genWeightMat(ages = 0:100, years = obs$year, clip = 3)

    # Female
    M8fit_female <- fit(m8(link = "log", xc = 110), data = StMoMoData(obs, series = "female", type = "initial"), 
                        wxt = wxt, verbose = FALSE)
    dum_val_female <- try(forecast(M8fit_female, h = 10)$rate, silent = TRUE)
    if (inherits(dum_val_female, "try-error")) {
        female_forecast <- matrix(NA, 101, 10)
        AIC_female <- NA
        BIC_female <- NA
    } else {
        female_forecast <- dum_val_female
        IC_female <- Information_Criteria(residuals_mat = M8fit_female$fittingModel$residuals, K = 3)
        AIC_female <- IC_female$AIC
        BIC_female <- IC_female$BIC
    }

    # Male
    M8fit_male <- fit(m8(link = "log", xc = 110), data = StMoMoData(obs, series = "male", type = "initial"), 
                      wxt = wxt, verbose = FALSE)
    dum_val_male <- try(forecast(M8fit_male, h = 10)$rate, silent = TRUE)
    if (inherits(dum_val_male, "try-error")) {
        male_forecast <- matrix(NA, 101, 10)
        AIC_male <- NA
        BIC_male <- NA
    } else {
        male_forecast <- dum_val_male
        IC_male <- Information_Criteria(residuals_mat = M8fit_male$fittingModel$residuals, K = 3)
        AIC_male <- IC_male$AIC
        BIC_male <- IC_male$BIC
    }

    list(
        female_forecast = female_forecast,
        male_forecast = male_forecast,
        AIC_female = AIC_female,
        BIC_female = BIC_female,
        AIC_male = AIC_male,
        BIC_male = BIC_male
    )
}

### Plat
forecast_plat <- function(obs) {
    wxt <- genWeightMat(ages = 0:100, years = obs$year, clip = 3)

    # Define the Plat model as given
    PLAT <- StMoMo(link = "log", staticAgeFun = TRUE,
                   periodAgeFun = c("1", f2, f3), cohortAgeFun = "1",
                   constFun = constPlat)

    # Female
    PLATfit_female <- fit(PLAT, data = StMoMoData(obs, series = "female", type = "central"), 
                          wxt = wxt, verbose = FALSE)
    female_forecast <- forecast(PLATfit_female, h = 10)$rate
    IC_female <- Information_Criteria(residuals_mat = PLATfit_female$fittingModel$residuals, K = 4)
    AIC_female <- IC_female$AIC
    BIC_female <- IC_female$BIC

    # Male
    PLATfit_male <- fit(PLAT, data = StMoMoData(obs, series = "male", type = "central"),
                        wxt = wxt, verbose = FALSE)
    male_forecast <- forecast(PLATfit_male, h = 10)$rate
    IC_male <- Information_Criteria(residuals_mat = PLATfit_male$fittingModel$residuals, K = 4)
    AIC_male <- IC_male$AIC
    BIC_male <- IC_male$BIC

    list(
        female_forecast = female_forecast,
        male_forecast = male_forecast,
        AIC_female = AIC_female,
        BIC_female = BIC_female,
        AIC_male = AIC_male,
        BIC_male = BIC_male
    )
}

forecast_lca_dt <- function(obs) {
    # Female
    lca_dt_female = lca(obs, series = "female", adjust = "dt", interpolate = TRUE)
    female_forecast = forecast(lca_dt_female, h = 10)$rate$female
    IC_female = Information_Criteria(residuals_mat = lca_dt_female$residuals$y, K = 3)
    AIC_female = IC_female$AIC
    BIC_female = IC_female$BIC

    # Male
    lca_dt_male = lca(obs, series = "male", adjust = "dt", interpolate = TRUE)
    male_forecast = forecast(lca_dt_male, h = 10)$rate$male
    IC_male = Information_Criteria(residuals_mat = lca_dt_male$residuals$y, K = 3)
    AIC_male = IC_male$AIC
    BIC_male = IC_male$BIC

    list(
        female_forecast = female_forecast,
        male_forecast = male_forecast,
        AIC_female = AIC_female,
        BIC_female = BIC_female,
        AIC_male = AIC_male,
        BIC_male = BIC_male
    )
}

forecast_lca_dxt <- function(obs) {
    # Female
    lca_dxt_female = lca(obs, series = "female", adjust = "dxt", interpolate = TRUE)
    female_forecast = forecast(lca_dxt_female, h = 10)$rate$female
    IC_female = Information_Criteria(residuals_mat = lca_dxt_female$residuals$y, K = 3)
    AIC_female = IC_female$AIC
    BIC_female = IC_female$BIC

    # Male
    lca_dxt_male = lca(obs, series = "male", adjust = "dxt", interpolate = TRUE)
    male_forecast = forecast(lca_dxt_male, h = 10)$rate$male
    IC_male = Information_Criteria(residuals_mat = lca_dxt_male$residuals$y, K = 3)
    AIC_male = IC_male$AIC
    BIC_male = IC_male$BIC

    list(
        female_forecast = female_forecast,
        male_forecast = male_forecast,
        AIC_female = AIC_female,
        BIC_female = BIC_female,
        AIC_male = AIC_male,
        BIC_male = BIC_male
    )
}

forecast_lca_e0 <- function(obs) {
    # Female
    lca_e0_female = lca(obs, series = "female", adjust = "e0", interpolate = TRUE)
    female_forecast = forecast(lca_e0_female, h = 10)$rate$female
    IC_female = Information_Criteria(residuals_mat = lca_e0_female$residuals$y, K = 3)
    AIC_female = IC_female$AIC
    BIC_female = IC_female$BIC

    # Male
    lca_e0_male = lca(obs, series = "male", adjust = "e0", interpolate = TRUE)
    male_forecast = forecast(lca_e0_male, h = 10)$rate$male
    IC_male = Information_Criteria(residuals_mat = lca_e0_male$residuals$y, K = 3)
    AIC_male = IC_male$AIC
    BIC_male = IC_male$BIC

    list(
        female_forecast = female_forecast,
        male_forecast = male_forecast,
        AIC_female = AIC_female,
        BIC_female = BIC_female,
        AIC_male = AIC_male,
        BIC_male = BIC_male
    )
}

forecast_lca_none <- function(obs) {
    # Female
    lca_none_female = lca(obs, series = "female", adjust = "none", interpolate = TRUE)
    female_forecast = forecast(lca_none_female, h = 10)$rate$female
    IC_female = Information_Criteria(residuals_mat = lca_none_female$residuals$y, K = 3)
    AIC_female = IC_female$AIC
    BIC_female = IC_female$BIC

    # Male
    lca_none_male = lca(obs, series = "male", adjust = "none", interpolate = TRUE)
    male_forecast = forecast(lca_none_male, h = 10)$rate$male
    IC_male = Information_Criteria(residuals_mat = lca_none_male$residuals$y, K = 3)
    AIC_male = IC_male$AIC
    BIC_male = IC_male$BIC

    list(
        female_forecast = female_forecast,
        male_forecast = male_forecast,
        AIC_female = AIC_female,
        BIC_female = BIC_female,
        AIC_male = AIC_male,
        BIC_male = BIC_male
    )
}

forecast_fdm <- function(obs) {
    # Female
    fdm_female = fdm(obs, series = "female", method = "classical")
    female_forecast = forecast(fdm_female, h = 10)$rate$female
    IC_female = Information_Criteria(residuals_mat = fdm_female$residuals$y, K = 6)
    AIC_female = IC_female$AIC
    BIC_female = IC_female$BIC

    # Male
    fdm_male = fdm(obs, series = "male", method = "classical")
    male_forecast = forecast(fdm_male, h = 10)$rate$male
    IC_male = Information_Criteria(residuals_mat = fdm_male$residuals$y, K = 6)
    AIC_male = IC_male$AIC
    BIC_male = IC_male$BIC

    list(
        female_forecast = female_forecast,
        male_forecast = male_forecast,
        AIC_female = AIC_female,
        BIC_female = BIC_female,
        AIC_male = AIC_male,
        BIC_male = BIC_male
    )
}

forecast_M_fdm <- function(obs) {
    # Female (robust fdm)
    robust_fdm_female = fdm(obs, series = "female", method = "M")
    female_forecast = forecast_m_manual(obs, "female", h = 10)
    IC_female = Information_Criteria(residuals_mat = robust_fdm_female$residuals$y, K = 6)
    AIC_female = IC_female$AIC
    BIC_female = IC_female$BIC

    # Male (robust fdm)
    robust_fdm_male = fdm(obs, series = "male", method = "M")
    male_forecast = forecast_m_manual(obs, "male", h = 10)
    IC_male = Information_Criteria(residuals_mat = robust_fdm_male$residuals$y, K = 6)
    AIC_male = IC_male$AIC
    BIC_male = IC_male$BIC

    list(
        female_forecast = female_forecast,
        male_forecast = male_forecast,
        AIC_female = AIC_female,
        BIC_female = BIC_female,
        AIC_male = AIC_male,
        BIC_male = BIC_male
    )
}

forecast_pr <- function(obs) {
    pr_fdm = coherentfdm(obs)
    train_coherent_fdm = forecast(pr_fdm, h = 10)

    female_forecast = train_coherent_fdm$female$rate$female
    male_forecast = train_coherent_fdm$male$rate$male

    residuals_pr_female = train_coherent_fdm$female$model$female - train_coherent_fdm$female$fitted$y
    IC_female = Information_Criteria(residuals_mat = residuals_pr_female, K = 12)
    AIC_female = IC_female$AIC
    BIC_female = IC_female$BIC

    residuals_pr_male = train_coherent_fdm$male$model$male - train_coherent_fdm$male$fitted$y
    IC_male = Information_Criteria(residuals_mat = residuals_pr_male, K = 12)
    AIC_male = IC_male$AIC
    BIC_male = IC_male$BIC

    list(
        female_forecast = female_forecast,
        male_forecast = male_forecast,
        AIC_female = AIC_female,
        BIC_female = BIC_female,
        AIC_male = AIC_male,
        BIC_male = BIC_male
    )
}

forecast_cfr <- function(obs) {
    seed <- 982897
    testPoints <- 11 - (2022 - tail(obs$year, 1))

    femaleDir <- paste0('/workspaces/mortality/out/', seed, '/jp_female_Mrr/', testPoints, '/')
    maleDir <- paste0('/workspaces/mortality/out/', seed, '/jp_male_Mrr/', testPoints, '/')

    processData <- function(dir) {
        testFile <- paste0(dir, seed, ".Test.csv")
        trainFile <- paste0(dir, seed, ".Test.Predict.csv")
        
        testData <- read.csv(testFile)
        predictData <- read.csv(trainFile)
        
        testDataWithoutY <- testData[, !names(testData) %in% "y"]
        combinedArray <- cbind(testDataWithoutY, y = predictData$y)
        
        reshapedData <- dcast(combinedArray, age ~ year, value.var = "y")
        rownames(reshapedData) <- reshapedData$age
        reshapedData <- reshapedData[, -1]

        if (is.null(ncol(reshapedData))) {
            reshapedData <- as.data.frame(reshapedData)  # Convert to data frame
            colnames(reshapedData) <- unique(combinedArray$year)  # Assign the year as column name
        }

        numCols <- ncol(reshapedData)
        if (numCols < 10) {
            lastYear <- as.numeric(colnames(reshapedData)[numCols])
            newCols <- matrix(0, nrow = 101, ncol = 10 - numCols)  # Ensure all 101 rows
            colnames(newCols) <- seq(lastYear + 1, lastYear + (10 - numCols))
            reshapedData <- cbind(reshapedData, newCols)
        }

        exp(reshapedData)
    }

    reshapedFemale <- processData(femaleDir)
    reshapedMale <- processData(maleDir)
    
    # Convert to numeric matrix before returning
    reshapedFemale <- as.matrix(reshapedFemale)
    reshapedMale <- as.matrix(reshapedMale)
    
    list(
        female_forecast = reshapedFemale,
        male_forecast = reshapedMale,
        AIC_female = 0,
        BIC_female = 0,
        AIC_male = 0,
        BIC_male = 0
    )
}
