#######################################
# Plots Japanese subnational mortality rates
# - currently commented as not yet integrated into the code
#######################################

# #output_path = paste("D:/OneDrive - The University of Newcastle/Data/Japanese Mortality Data/plots/")
# output_path <- "data/plots/"
# dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# # Raw data
# for(ij in 1:num_prefs)
# {
#   # female
#   data_female = log(get(state[ij])$rate$female)
#   data_female_impute = apply(ifelse(is.finite(data_female), data_female, NA), 2, na.interp)
#   data_demog_female = demogdata(data_female_impute, pop = get(state[ij])$pop$female, ages = get(state[ij])$age, 
#                               years = get(state[ij])$year, type = get(state[ij])$type, label = get(state[ij])$label, name = "female")
#   savepdf(paste(output_path, "State_female_", ij, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
#   plot(data_demog_female, transform = FALSE)
#   dev.off()
  
#   # male
#   data_male = log(get(state[ij])$rate$male)
#   data_male_impute = apply(ifelse(is.finite(data_male), data_male, NA), 2, na.interp)
#   data_demog_male = demogdata(data_male_impute, pop = get(state[ij])$pop$male, ages = get(state[ij])$age, 
#                                 years = get(state[ij])$year, type = get(state[ij])$type, label = get(state[ij])$label, name = "male")
#   savepdf(paste(output_path, "State_male_", ij, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
#   plot(data_demog_male, transform = FALSE)
#   dev.off()
  
  
#   # total
#   data_total = log(get(state[ij])$rate$total)
#   data_total_impute = apply(ifelse(is.finite(data_total), data_total, NA), 2, na.interp)
#   data_demog_total = demogdata(data_total_impute, pop = get(state[ij])$pop$total, ages = get(state[ij])$age, 
#                               years = get(state[ij])$year, type = get(state[ij])$type, label = get(state[ij])$label, name = "total")
#   savepdf(paste(output_path, "State_total_", ij, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
#   plot(data_demog_total, transform = FALSE)
#   dev.off()
# }

# # Smoothed data

# for(ij in 1:num_prefs)
# {
#   # female
#   savepdf(paste(output_path, "State_female_smooth", ij, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
#   plot(get(state_smooth[ij]), series = "female")
#   dev.off()
  
#   # male
#   savepdf(paste(output_path, "State_male_smooth", ij, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
#   plot(get(state_smooth[ij]), series = "total")
#   dev.off()
  
#   # total
#   savepdf(paste(output_path, "State_total_smooth", ij, sep = ""), width = 14, height = 10, toplines = 0.9, pointsize = 10)
#   plot(get(state_smooth[ij]), series = "male")
#   dev.off()
# }


# ##########

# savepdf("Japan_female_over_time", width = 14, height = 10, toplines = 0.9, pointsize = 10)
# plot(get(state_smooth[1]))
# dev.off()

# savepdf("Japan_female_over_time_local", width = 14, height = 10, toplines = 0.9, pointsize = 10)
# plot(get(state_smooth[1]), ages = 3:30, plotlegend = TRUE, legendpos = "bottomright")
# dev.off()

# savepdf("Japan_female_over_age", width = 14, height = 10, toplines = 0.9, pointsize = 10)
# plot(get(state_smooth[1]), plot.type = "time", xlab = "Year")
# dev.off()

# savepdf("Japan_female_over_age_local", width = 14, height = 10, toplines = 0.9, pointsize = 10)
# plot(get(state_smooth[1]), ages = 3:30, plot.type = "time", xlab = "Year")
# col_select = rainbow(min(1024, 1.25*28))
# legend("topright", lty = c(1,1,1), col = col_select[c(1, 16, 28)], c("3 years old", "18 years old", "30 years old"))
# dev.off()


# #############
# # FPC Output
# #############


# select_K <- function(tau, eigenvalue)
# {
#   k_max = length(eigenvalue)
#   k_all = rep(0, k_max-1)
#   for(k in 1:(k_max-1))
#   {
#     k_all[k] = (eigenvalue[k+1]/eigenvalue[k])*ifelse(eigenvalue[k]/eigenvalue[1] > tau, 1, 0) + ifelse(eigenvalue[k]/eigenvalue[1] < tau, 1, 0)
#   }
#   K_hat = which.min(k_all)
#   return(K_hat)
# }


# fpc_output <- function(ij, years)
# {
#   dat_female = log(extract.years(get(state_smooth[ij]), years)$rate$female)
#   dat_male = log(extract.years(get(state_smooth[ij]), years)$rate$male)
  
#   # compute long-run covariance
#   LRC_est_female = ftsa::long_run_covariance_estimation(dat = dat_female)
#   LRC_est_male = ftsa::long_run_covariance_estimation(dat = dat_male)
  
#   eigen_decomp_female = eigen(LRC_est_female, symmetric = TRUE)
#   eigen_decomp_male = eigen(LRC_est_male, symmetric = TRUE)
  
#   lambda_val_female = eigen_decomp_female$values[which(eigen_decomp_female$values > 0)]
#   ncomp_female = select_K(tau = 1/log(max(lambda_val_female[1], length(lambda_val_female))), eigenvalue = lambda_val_female)
#   basis_female = matrix(eigen_decomp_female$vectors[,1:6], ncol = 6)
#   eigen_female = eigen_decomp_female$values[1:6]
  
#   lambda_val_male = eigen_decomp_male$values[which(eigen_decomp_male$values > 0)]
#   ncomp_male = select_K(tau = 1/log(max(lambda_val_male[1], length(lambda_val_male))), eigenvalue = lambda_val_male)
#   basis_male = matrix(eigen_decomp_male$vectors[,1:6], ncol = 6)
#   eigen_male = eigen_decomp_male$values[1:6]
  
#   return(list(basis_female = basis_female, eigen_female = eigen_female, ncomp_female = ncomp_female,
#               basis_male = basis_male, eigen_male = eigen_male, ncomp_male = ncomp_male))
# }

# ####################
# # years = 1973:2013
# ###################


# Japan_basis_2013 = fpc_output(ij = 1, years = 1973:2013)
# Fukushima_basis_2013 = fpc_output(ij = 8, years = 1973:2013)
# Hokkaido_basis_2013 = fpc_output(ij = 2, years = 1973:2013)
# Okinawa_basis_2013 = fpc_output(ij = num_prefs, years = 1973:2013)

# female_basis_2013 = data.frame(cbind(0:100, Japan_basis_2013$basis_female, Fukushima_basis_2013$basis_female, Hokkaido_basis_2013$basis_female, Okinawa_basis_2013$basis_female))
# colnames(female_basis_2013) = c("Age", paste0("Japan_basis_", 1:6), paste0("Fukushima_basis_", 1:6), paste0("Hokkaido_basis_", 1:6), paste0("Okinawa_basis_", 1:6))

# male_basis_2013 = data.frame(cbind(0:100, Japan_basis_2013$basis_male, Fukushima_basis_2013$basis_male, Hokkaido_basis_2013$basis_male, Okinawa_basis_2013$basis_male))
# colnames(male_basis_2013) = c("Age", paste0("Japan_basis_", 1:6), paste0("Fukushima_basis_", 1:6), paste0("Hokkaido_basis_", 1:6), paste0("Okinawa_basis_", 1:6))

# write.csv(female_basis_2013, file = "female_basis_2013.csv")
# write.csv(male_basis_2013, file = "male_basis_2013.csv")

# savepdf("FPC_functions_female_2013", width = 20, height = 15)
# female_basis_2013 %>% select(contains(c("Age", "basis_1", "basis_2"))) %>% 
#   pivot_longer(c(paste0("Japan_basis_", 1:2), paste0("Fukushima_basis_", 1:2), paste0("Hokkaido_basis_", 1:2), paste0("Okinawa_basis_", 1:2))) %>%
#   mutate(region = case_when(name %in% c("Japan_basis_1", "Japan_basis_2") ~ "Japan",
#                             name %in% c("Fukushima_basis_1", "Fukushima_basis_2") ~ "Fukushima",
#                             name %in% c("Hokkaido_basis_1", "Hokkaido_basis_2") ~ "Hokkaido",
#                             name %in% c("Okinawa_basis_1", "Okinawa_basis_2") ~ "Okinawa"),
#          color = case_when(str_ends(name, "basis_1") ~ "1st FPC", TRUE ~ "2nd FPC")
#   ) %>% 
#   ggplot(aes(x = Age, y = value, color = color)) +
#   geom_line() + 
#   facet_wrap(~ factor(region, levels = c("Japan", "Fukushima", "Hokkaido", "Okinawa"))) +
#   xlab('Age') +
#   ylab('FPC Value') +
#   labs(title = 'First and Second Empirical Dynamic FPC Functions, 1973:2013') 
# dev.off()

# savepdf("FPC_functions_male_2013", width = 20, height = 15)
# male_basis_2013 %>% select(contains(c("Age", "basis_1", "basis_2"))) %>% 
#   pivot_longer(c(paste0("Japan_basis_", 1:2), paste0("Fukushima_basis_", 1:2), paste0("Hokkaido_basis_", 1:2), paste0("Okinawa_basis_", 1:2))) %>%
#   mutate(region = case_when(name %in% c("Japan_basis_1", "Japan_basis_2") ~ "Japan",
#                             name %in% c("Fukushima_basis_1", "Fukushima_basis_2") ~ "Fukushima",
#                             name %in% c("Hokkaido_basis_1", "Hokkaido_basis_2") ~ "Hokkaido",
#                             name %in% c("Okinawa_basis_1", "Okinawa_basis_2") ~ "Okinawa"),
#          color = case_when(str_ends(name, "basis_1") ~ "1st FPC", TRUE ~ "2nd FPC")
#   ) %>% 
#   ggplot(aes(x = Age, y = value, color = color)) +
#   geom_line() + 
#   facet_wrap(~ factor(region, levels = c("Japan", "Fukushima", "Hokkaido", "Okinawa"))) +
#   xlab('Age') +
#   ylab('FPC Value') +
#   labs(title = 'First and Second Empirical Dynamic FPC Functions, 1973:2013')
# dev.off()

# ####################
# # years = 1973:2003
# ###################

# Japan_basis_2003 = fpc_output(ij = 1, years = 1973:2003)
# Fukushima_basis_2003 = fpc_output(ij = 8, years = 1973:2003)
# Hokkaido_basis_2003 = fpc_output(ij = 2, years = 1973:2003)
# Okinawa_basis_2003 = fpc_output(ij = num_prefs, years = 1973:2003)

# female_basis_2003 = data.frame(cbind(0:100, Japan_basis_2003$basis_female, Fukushima_basis_2003$basis_female, Hokkaido_basis_2003$basis_female, Okinawa_basis_2003$basis_female))
# colnames(female_basis_2003) = c("Age", paste0("Japan_basis_", 1:6), paste0("Fukushima_basis_", 1:6), paste0("Hokkaido_basis_", 1:6), paste0("Okinawa_basis_", 1:6))

# male_basis_2003 = data.frame(cbind(0:100, Japan_basis_2003$basis_male, Fukushima_basis_2003$basis_male, Hokkaido_basis_2003$basis_male, Okinawa_basis_2003$basis_male))
# colnames(male_basis_2003) = c("Age", paste0("Japan_basis_", 1:6), paste0("Fukushima_basis_", 1:6), paste0("Hokkaido_basis_", 1:6), paste0("Okinawa_basis_", 1:6))

# write.csv(female_basis_2003, file = "female_basis_2003.csv")
# write.csv(male_basis_2003, file = "male_basis_2003.csv")

# savepdf("FPC_functions_female_2003", width = 20, height = 15)
# female_basis_2003 %>% select(contains(c("Age", "basis_1", "basis_2"))) %>% 
#   pivot_longer(c(paste0("Japan_basis_", 1:2), paste0("Fukushima_basis_", 1:2), paste0("Hokkaido_basis_", 1:2), paste0("Okinawa_basis_", 1:2))) %>%
#   mutate(region = case_when(name %in% c("Japan_basis_1", "Japan_basis_2") ~ "Japan",
#                             name %in% c("Fukushima_basis_1", "Fukushima_basis_2") ~ "Fukushima",
#                             name %in% c("Hokkaido_basis_1", "Hokkaido_basis_2") ~ "Hokkaido",
#                             name %in% c("Okinawa_basis_1", "Okinawa_basis_2") ~ "Okinawa"),
#          color = case_when(str_ends(name, "basis_1") ~ "1st FPC", TRUE ~ "2nd FPC")
#   ) %>% 
#   ggplot(aes(x = Age, y = value, color = color)) +
#   geom_line() + 
#   facet_wrap(~ factor(region, levels = c("Japan", "Fukushima", "Hokkaido", "Okinawa"))) +
#   xlab('Age') +
#   ylab('FPC Value') +
#   labs(title = 'First and Second Empirical Dynamic FPC Functions, 1973:2003') 
# dev.off()

# savepdf("FPC_functions_male_2003", width = 20, height = 15)
# male_basis_2003 %>% select(contains(c("Age", "basis_1", "basis_2"))) %>% 
#   pivot_longer(c(paste0("Japan_basis_", 1:2), paste0("Fukushima_basis_", 1:2), paste0("Hokkaido_basis_", 1:2), paste0("Okinawa_basis_", 1:2))) %>%
#   mutate(region = case_when(name %in% c("Japan_basis_1", "Japan_basis_2") ~ "Japan",
#                             name %in% c("Fukushima_basis_1", "Fukushima_basis_2") ~ "Fukushima",
#                             name %in% c("Hokkaido_basis_1", "Hokkaido_basis_2") ~ "Hokkaido",
#                             name %in% c("Okinawa_basis_1", "Okinawa_basis_2") ~ "Okinawa"),
#          color = case_when(str_ends(name, "basis_1") ~ "1st FPC", TRUE ~ "2nd FPC")
#   ) %>% 
#   ggplot(aes(x = Age, y = value, color = color)) +
#   geom_line() + 
#   facet_wrap(~ factor(region, levels = c("Japan", "Fukushima", "Hokkaido", "Okinawa"))) +
#   xlab('Age') +
#   ylab('FPC Value') +
#   labs(title = 'First and Second Empirical Dynamic FPC Functions, 1973:2003')
# dev.off()

# ####################
# # years = 1973:1993
# ###################

# Japan_basis_1993 = fpc_output(ij = 1, years = 1973:1993)
# Fukushima_basis_1993 = fpc_output(ij = 8, years = 1973:1993)
# Hokkaido_basis_1993 = fpc_output(ij = 2, years = 1973:1993)
# Okinawa_basis_1993 = fpc_output(ij = num_prefs, years = 1973:1993)

# female_basis_1993 = data.frame(cbind(0:100, Japan_basis_1993$basis_female, Fukushima_basis_1993$basis_female, Hokkaido_basis_1993$basis_female, Okinawa_basis_1993$basis_female))
# colnames(female_basis_1993) = c("Age", paste0("Japan_basis_", 1:6), paste0("Fukushima_basis_", 1:6), paste0("Hokkaido_basis_", 1:6), paste0("Okinawa_basis_", 1:6))

# male_basis_1993 = data.frame(cbind(0:100, Japan_basis_1993$basis_male, Fukushima_basis_1993$basis_male, Hokkaido_basis_1993$basis_male, Okinawa_basis_1993$basis_male))
# colnames(male_basis_1993) = c("Age", paste0("Japan_basis_", 1:6), paste0("Fukushima_basis_", 1:6), paste0("Hokkaido_basis_", 1:6), paste0("Okinawa_basis_", 1:6))

# write.csv(female_basis_1993, file = "female_basis_1993.csv")
# write.csv(male_basis_1993, file = "male_basis_1993.csv")

# savepdf("FPC_functions_female_1993", width = 20, height = 15)
# female_basis_1993 %>% select(contains(c("Age", "basis_1", "basis_2"))) %>% 
#   pivot_longer(c(paste0("Japan_basis_", 1:2), paste0("Fukushima_basis_", 1:2), paste0("Hokkaido_basis_", 1:2), paste0("Okinawa_basis_", 1:2))) %>%
#   mutate(region = case_when(name %in% c("Japan_basis_1", "Japan_basis_2") ~ "Japan",
#                             name %in% c("Fukushima_basis_1", "Fukushima_basis_2") ~ "Fukushima",
#                             name %in% c("Hokkaido_basis_1", "Hokkaido_basis_2") ~ "Hokkaido",
#                             name %in% c("Okinawa_basis_1", "Okinawa_basis_2") ~ "Okinawa"),
#          color = case_when(str_ends(name, "basis_1") ~ "1st FPC", TRUE ~ "2nd FPC")
#   ) %>% 
#   ggplot(aes(x = Age, y = value, color = color)) +
#   geom_line() + 
#   facet_wrap(~ factor(region, levels = c("Japan", "Fukushima", "Hokkaido", "Okinawa"))) +
#   xlab('Age') +
#   ylab('FPC Value') +
#   labs(title = 'First and Second Empirical Dynamic FPC Functions, 1973:1993') 
# dev.off()

# savepdf("FPC_functions_male_1993", width = 20, height = 15)
# male_basis_1993 %>% select(contains(c("Age", "basis_1", "basis_2"))) %>% 
#   pivot_longer(c(paste0("Japan_basis_", 1:2), paste0("Fukushima_basis_", 1:2), paste0("Hokkaido_basis_", 1:2), paste0("Okinawa_basis_", 1:2))) %>%
#   mutate(region = case_when(name %in% c("Japan_basis_1", "Japan_basis_2") ~ "Japan",
#                             name %in% c("Fukushima_basis_1", "Fukushima_basis_2") ~ "Fukushima",
#                             name %in% c("Hokkaido_basis_1", "Hokkaido_basis_2") ~ "Hokkaido",
#                             name %in% c("Okinawa_basis_1", "Okinawa_basis_2") ~ "Okinawa"),
#          color = case_when(str_ends(name, "basis_1") ~ "1st FPC", TRUE ~ "2nd FPC")
#   ) %>% 
#   ggplot(aes(x = Age, y = value, color = color)) +
#   geom_line() + 
#   facet_wrap(~ factor(region, levels = c("Japan", "Fukushima", "Hokkaido", "Okinawa"))) +
#   xlab('Age') +
#   ylab('FPC Value') +
#   labs(title = 'First and Second Empirical Dynamic FPC Functions, 1973:1993')
# dev.off()


# plot(Japan_basis_1993$basis_female[,1], type = "l", ylim = c(-0.16, -0.04), ylab = "1st FPC of Japan Female Series", xlab = "Age", lwd = 2)
# lines(Japan_basis_2003$basis_female[,1], col = 2, lwd = 2)
# lines(Japan_basis_2013$basis_female[,1], col = 3, lwd = 2)
# legend("bottomright", legend =  c(1993,2003,2013), col = 1:3, lty = rep(1,3), lwd = rep(2,3))


# #############
# # Japan all
# #############

# Japan_basis_all = fpc_output(ij = 1, years = 1973:2022)
# Japan_basis_female_all = data.frame(cbind(0:100, Japan_basis_all$basis_female))
# colnames(Japan_basis_female_all) = c("Age", paste0("Japan_basis_", 1:6))

# Japan_basis_female_all[,1:5] %>% pivot_longer(paste0("Japan_basis_", 1:4)) %>% 
#   mutate(FPC = case_when(str_ends(name, "basis_1") ~ "1st FPC",
#                          str_ends(name, "basis_2") ~ "2nd FPC",
#                          str_ends(name, "basis_3") ~ "3rd FPC",
#                          str_ends(name, "basis_4") ~ "4th FPC",
#                          str_ends(name, "basis_5") ~ "5th FPC",
#                          str_ends(name, "basis_6") ~ "6th FPC")) %>% 
#   ggplot(aes(x = Age, y = value)) +
#   geom_line() + 
#   facet_wrap(~ factor(FPC)) +
#   xlab('Age') +
#   ylab('FPC Value') +
#   labs(title = 'Empirical Dynamic FPC Functions for Japanese Female, 1973:2022')
