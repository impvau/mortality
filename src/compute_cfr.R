
compute_cfr <- function(filename) {

  # Read the CSV file
  data <- read.csv(filename, header = TRUE)
  
  # Rename columns sequentially as A2, B2, C2, ...
  col_names <- paste0(LETTERS[seq_along(data)], 2)
  colnames(data) <- col_names[seq_len(ncol(data))]

  # Filter data where column K2 (Prefecture) is 'Tokyo'
  data <- data[data$K2 == 'Tokyo', ]

  # Calculate the values for each formula
  data$mf1_values <- with(data, (0.00736879841817334*B2+0.0124522044116393*C2+0.00676675973591754*E2+0.00592996134835999*F2+0.00525753230436906*G2-0.00428168536740645)+(0.0023717068361975*B2+0.00671941013585718*C2+0.111518036964739*D2+0.00399164239676938*E2+0.0116745988683326*F2+0.000818582061018679*G2-0.000014858281573475)/((-0.000958279509395029*B2-0.000668925501185781*C2-0.00111520071499118*D2+0.00244723361339833*E2+0.00096035428625894*F2-0.00140381479188116*G2+0.00142072120347417)+(-0.000495178728024348*B2+0.000205841076495361*C2+0.0028126453900774*D2+0.000287972026813134*E2+0.000543079703295206*F2-0.000140959095521673*G2-0.00117440257849432)/((0.00299865207162427*B2+0.0043574163718923*C2+0.00319799256629375*D2+0.0038877134899826*E2+0.00468908553860588*F2+0.00511555470526143*G2-0.00532222960126568))))
  data$mf2_values <- with(data, (0.0549026961429838)+(0.149773037048441*B2+0.0819418708334398*C2+0.0681387928385716*D2+0.193358582801865*E2+0.148801183039656*F2+0.122052253714782*G2-0.0343450259169184)/((0.0124034142084161*E2-0.00445443402128939*F2-0.0109572685890062*G2)+(0.222015692772242*B2+0.114504318352971*D2+0.0616451497681987*E2)/((0.321383657255548*B2+0.150550559332269*D2+0.00464182772543656*F2+0.0633727624572414*G2))))
  data$mf3_values <- with(data, (0.268773321330741*B2+0.142606524033464*D2+0.257632889401994*E2+0.170345916187902*F2+0.162379056961163*G2)+(0.185442122052766)/((0.2281177964497)+(1.20346274949821)/((-0.139219608379127))))
  data$mm2_values <- with(data, (0.220639822686028*B2+0.126692332903669*C2+0.118935260482028*D2+0.1734952390321*E2+0.168411125732878*F2+0.147991650495354*G2+0.0272125180731219)+(0.046842543668936*B2-0.0122647763657363*G2)/((-0.0601888846500397*B2-0.077792075477603*D2+0.116587371482337*E2-0.131136245082779)+(0.279555041641073)/((0.420036359008485))))
  data$mm3_values <- with(data, (0.243121830119661*B2+0.150204650517927*C2+0.0695732414633917*D2+0.243679787604718*E2+0.162017365348492*F2+0.131349417315808*G2-0.0228047313277247))

  model_formulas <- c("mf1","mf2","mf3","mm2","mm3")

  for (model in model_formulas) {

    model_values_col <- paste0(model, "_values")

    # Calculate residuals for the model
    data[[paste0(model, "_residuals")]] <- with(data, A2 - data[[model_values_col]])

    # Get unique years
    unique_years <- sort(unique(data$I2))

    # Initialize an empty data frame for the reshaped data
    reshaped_data <- data.frame(Age = unique(data$H2))

    # Extract residuals for each year and merge with reshaped_data
    for (year in unique_years) {
      year_data <- data[data$I2 == year, ]
      
      # Create a data frame from the year's residuals
      year_residuals <- data.frame(Age = year_data$H2, Residual = year_data[[paste0(model, "_residuals")]])
      col_name <- paste0(year)
      names(year_residuals)[names(year_residuals) == "Residual"] <- col_name

      reshaped_data <- merge(reshaped_data, year_residuals, by = "Age", all.x = TRUE)
    }

    # Replace NA with 0 or appropriate value
    reshaped_data[is.na(reshaped_data)] <- 0

    # Append rows for ages 0, 1, 2, 98, 99, 100 with residuals set to 0
    additional_ages <- data.frame(Age = c(0, 1, 2, 98, 99, 100))
    for (year in unique_years) {
      additional_ages[paste(year)] <- 0
    }
    reshaped_data <- rbind(reshaped_data, additional_ages)

    # Sort by Age
    reshaped_data <- reshaped_data[order(reshaped_data$Age), ]

    # Remove the 'Age' (or first) column from reshaped_data before saving
    reshaped_data_to_save <- reshaped_data[-1]

    # Construct the output filename
    output_filename <- paste0("out/Tokyo_", sub(".csv", "", basename(filename)), "_", model, "_residuals.csv")

    # Write the reshaped data frame (without the first column) to a file
    write.csv(reshaped_data_to_save, file = output_filename, row.names = FALSE)
  }
}

compute_cfr("data/female_3to97.csv")
