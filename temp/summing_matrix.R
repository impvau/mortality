####################################
# Preparation of the summing matrix
###################################


library(ftsa)
library(demography)

# Forecast exposure to risk at the bottom-level 

## top: exposure to risk for a given series at the bottom level
## bottom: exposure to risk for Japan total series
## year_horizon: forecast horizon

pop_fore_fun <- function(top, bottom, year_horizon)
{
  Japan_pop_fore = array(NA, dim = c(year_horizon,101,year_horizon))
  for(year in 41:50)
  {
    Japan_pop_fore[1,2:101,(year-40)] = top[2:101,year]/bottom[2:101,year]
    
    # need forecasts birth
    
    ratio = top[1,1:year]/bottom[1,1:year]
    
    # forecasts birth
    
    Japan_pop_fore[,1,(year-40)] =  forecast(auto.arima(ratio), h = year_horizon)$mean
    
    for(ij in 2:year_horizon)
    {
      # x in year t is x+1 in year t+1
      
      Japan_pop_fore[ij,2:101,(year-40)] = Japan_pop_fore[ij-1,1:100,(year-40)]   
    }
  }
  return(Japan_pop_fore)
}  

## Row 1

pop_ratio_P_F_to_T = c("pop_ratio_P1_F_to_T", "pop_ratio_P2_F_to_T", "pop_ratio_P3_F_to_T", 
                       "pop_ratio_P4_F_to_T", "pop_ratio_P5_F_to_T", "pop_ratio_P6_F_to_T",
                       "pop_ratio_P7_F_to_T", "pop_ratio_P8_F_to_T", "pop_ratio_P9_F_to_T",
                       "pop_ratio_P10_F_to_T", "pop_ratio_P11_F_to_T", "pop_ratio_P12_F_to_T",
                       "pop_ratio_P13_F_to_T", "pop_ratio_P14_F_to_T", "pop_ratio_P15_F_to_T",
                       "pop_ratio_P16_F_to_T", "pop_ratio_P17_F_to_T", "pop_ratio_P18_F_to_T",
                       "pop_ratio_P19_F_to_T", "pop_ratio_P20_F_to_T", "pop_ratio_P21_F_to_T",
                       "pop_ratio_P22_F_to_T", "pop_ratio_P23_F_to_T", "pop_ratio_P24_F_to_T",
                       "pop_ratio_P25_F_to_T", "pop_ratio_P26_F_to_T", "pop_ratio_P27_F_to_T",
                       "pop_ratio_P28_F_to_T", "pop_ratio_P29_F_to_T", "pop_ratio_P30_F_to_T",
                       "pop_ratio_P31_F_to_T", "pop_ratio_P32_F_to_T", "pop_ratio_P33_F_to_T",
                       "pop_ratio_P34_F_to_T", "pop_ratio_P35_F_to_T", "pop_ratio_P36_F_to_T",
                       "pop_ratio_P37_F_to_T", "pop_ratio_P38_F_to_T", "pop_ratio_P39_F_to_T",
                       "pop_ratio_P40_F_to_T", "pop_ratio_P41_F_to_T", "pop_ratio_P42_F_to_T",
                       "pop_ratio_P43_F_to_T", "pop_ratio_P44_F_to_T", "pop_ratio_P45_F_to_T",
                       "pop_ratio_P46_F_to_T", "pop_ratio_P47_F_to_T")

pop_ratio_P_M_to_T = c("pop_ratio_P1_M_to_T",  "pop_ratio_P2_M_to_T",  "pop_ratio_P3_M_to_T", 
                       "pop_ratio_P4_M_to_T",  "pop_ratio_P5_M_to_T",  "pop_ratio_P6_M_to_T",
                       "pop_ratio_P7_M_to_T",  "pop_ratio_P8_M_to_T",  "pop_ratio_P9_M_to_T",
                       "pop_ratio_P10_M_to_T", "pop_ratio_P11_M_to_T", "pop_ratio_P12_M_to_T",
                       "pop_ratio_P13_M_to_T", "pop_ratio_P14_M_to_T", "pop_ratio_P15_M_to_T",
                       "pop_ratio_P16_M_to_T", "pop_ratio_P17_M_to_T", "pop_ratio_P18_M_to_T",
                       "pop_ratio_P19_M_to_T", "pop_ratio_P20_M_to_T", "pop_ratio_P21_M_to_T",
                       "pop_ratio_P22_M_to_T", "pop_ratio_P23_M_to_T", "pop_ratio_P24_M_to_T",
                       "pop_ratio_P25_M_to_T", "pop_ratio_P26_M_to_T", "pop_ratio_P27_M_to_T",
                       "pop_ratio_P28_M_to_T", "pop_ratio_P29_M_to_T", "pop_ratio_P30_M_to_T",
                       "pop_ratio_P31_M_to_T", "pop_ratio_P32_M_to_T", "pop_ratio_P33_M_to_T",
                       "pop_ratio_P34_M_to_T", "pop_ratio_P35_M_to_T", "pop_ratio_P36_M_to_T",
                       "pop_ratio_P37_M_to_T", "pop_ratio_P38_M_to_T", "pop_ratio_P39_M_to_T",
                       "pop_ratio_P40_M_to_T", "pop_ratio_P41_M_to_T", "pop_ratio_P42_M_to_T",
                       "pop_ratio_P43_M_to_T", "pop_ratio_P44_M_to_T", "pop_ratio_P45_M_to_T",
                       "pop_ratio_P46_M_to_T", "pop_ratio_P47_M_to_T")

for(iw in 2:48)
{
  assign(pop_ratio_P_F_to_T[iw-1], pop_fore_fun(top = get(state[iw])$pop$female, bottom = Japan$pop$total, year_horizon = 10))
  assign(pop_ratio_P_M_to_T[iw-1], pop_fore_fun(top = get(state[iw])$pop$male,   bottom = Japan$pop$total, year_horizon = 10))
  
  print(iw)
}

## Row 2

pop_ratio_P_F_to_F = c("pop_ratio_P1_F_to_F", "pop_ratio_P2_F_to_F", "pop_ratio_P3_F_to_F", 
                       "pop_ratio_P4_F_to_F", "pop_ratio_P5_F_to_F", "pop_ratio_P6_F_to_F",
                       "pop_ratio_P7_F_to_F", "pop_ratio_P8_F_to_F", "pop_ratio_P9_F_to_F",
                       "pop_ratio_P10_F_to_F", "pop_ratio_P11_F_to_F", "pop_ratio_P12_F_to_F",
                       "pop_ratio_P13_F_to_F", "pop_ratio_P14_F_to_F", "pop_ratio_P15_F_to_F",
                       "pop_ratio_P16_F_to_F", "pop_ratio_P17_F_to_F", "pop_ratio_P18_F_to_F",
                       "pop_ratio_P19_F_to_F", "pop_ratio_P20_F_to_F", "pop_ratio_P21_F_to_F",
                       "pop_ratio_P22_F_to_F", "pop_ratio_P23_F_to_F", "pop_ratio_P24_F_to_F",
                       "pop_ratio_P25_F_to_F", "pop_ratio_P26_F_to_F", "pop_ratio_P27_F_to_F",
                       "pop_ratio_P28_F_to_F", "pop_ratio_P29_F_to_F", "pop_ratio_P30_F_to_F",
                       "pop_ratio_P31_F_to_F", "pop_ratio_P32_F_to_F", "pop_ratio_P33_F_to_F",
                       "pop_ratio_P34_F_to_F", "pop_ratio_P35_F_to_F", "pop_ratio_P36_F_to_F",
                       "pop_ratio_P37_F_to_F", "pop_ratio_P38_F_to_F", "pop_ratio_P39_F_to_F",
                       "pop_ratio_P40_F_to_F", "pop_ratio_P41_F_to_F", "pop_ratio_P42_F_to_F",
                       "pop_ratio_P43_F_to_F", "pop_ratio_P44_F_to_F", "pop_ratio_P45_F_to_F",
                       "pop_ratio_P46_F_to_F", "pop_ratio_P47_F_to_F")

## Row 3

pop_ratio_P_M_to_M = c("pop_ratio_P1_M_to_M",  "pop_ratio_P2_M_to_M",  "pop_ratio_P3_M_to_M", 
                       "pop_ratio_P4_M_to_M",  "pop_ratio_P5_M_to_M",  "pop_ratio_P6_M_to_M",
                       "pop_ratio_P7_M_to_M",  "pop_ratio_P8_M_to_M",  "pop_ratio_P9_M_to_M",
                       "pop_ratio_P10_M_to_M", "pop_ratio_P11_M_to_M", "pop_ratio_P12_M_to_M",
                       "pop_ratio_P13_M_to_M", "pop_ratio_P14_M_to_M", "pop_ratio_P15_M_to_M",
                       "pop_ratio_P16_M_to_M", "pop_ratio_P17_M_to_M", "pop_ratio_P18_M_to_M",
                       "pop_ratio_P19_M_to_M", "pop_ratio_P20_M_to_M", "pop_ratio_P21_M_to_M",
                       "pop_ratio_P22_M_to_M", "pop_ratio_P23_M_to_M", "pop_ratio_P24_M_to_M",
                       "pop_ratio_P25_M_to_M", "pop_ratio_P26_M_to_M", "pop_ratio_P27_M_to_M",
                       "pop_ratio_P28_M_to_M", "pop_ratio_P29_M_to_M", "pop_ratio_P30_M_to_M",
                       "pop_ratio_P31_M_to_M", "pop_ratio_P32_M_to_M", "pop_ratio_P33_M_to_M",
                       "pop_ratio_P34_M_to_M", "pop_ratio_P35_M_to_M", "pop_ratio_P36_M_to_M",
                       "pop_ratio_P37_M_to_M", "pop_ratio_P38_M_to_M", "pop_ratio_P39_M_to_M",
                       "pop_ratio_P40_M_to_M", "pop_ratio_P41_M_to_M", "pop_ratio_P42_M_to_M",
                       "pop_ratio_P43_M_to_M", "pop_ratio_P44_M_to_M", "pop_ratio_P45_M_to_M",
                       "pop_ratio_P46_M_to_M", "pop_ratio_P47_M_to_M")

for(iw in 2:48)
{
  assign(pop_ratio_P_F_to_F[iw-1], pop_fore_fun(top = get(state[iw])$pop$female, bottom = Japan$pop$female, year_horizon = 10))
  assign(pop_ratio_P_M_to_M[iw-1], pop_fore_fun(top = get(state[iw])$pop$male,   bottom = Japan$pop$male, year_horizon = 10))
  
  print(iw)
}

## Row 4

pop_ratio_P_F_to_R1_T = "pop_ratio_P1_F_to_R1_T"
pop_ratio_P_F_to_R2_T = c("pop_ratio_P2_F_to_R2_T", "pop_ratio_P3_F_to_R2_T", "pop_ratio_P4_F_to_R2_T",
                          "pop_ratio_P5_F_to_R2_T", "pop_ratio_P6_F_to_R2_T", "pop_ratio_P7_F_to_R2_T")
pop_ratio_P_F_to_R3_T = c("pop_ratio_P8_F_to_R3_T", "pop_ratio_P9_F_to_R3_T", "pop_ratio_P10_F_to_R3_T",
                          "pop_ratio_P11_F_to_R3_T", "pop_ratio_P12_F_to_R3_T", "pop_ratio_P13_F_to_R3_T",
                          "pop_ratio_P14_F_to_R3_T")
pop_ratio_P_F_to_R4_T = c("pop_ratio_P15_F_to_R4_T", "pop_ratio_P16_F_to_R4_T", "pop_ratio_P17_F_to_R4_T",
                          "pop_ratio_P18_F_to_R4_T", "pop_ratio_P19_F_to_R4_T", "pop_ratio_P20_F_to_R4_T", 
                          "pop_ratio_P21_F_to_R4_T", "pop_ratio_P22_F_to_R4_T", "pop_ratio_P23_F_to_R4_T")
pop_ratio_P_F_to_R5_T = c("pop_ratio_P24_F_to_R5_T", "pop_ratio_P25_F_to_R5_T", "pop_ratio_P26_F_to_R5_T", 
                          "pop_ratio_P27_F_to_R5_T", "pop_ratio_P28_F_to_R5_T", "pop_ratio_P29_F_to_R5_T", 
                          "pop_ratio_P30_F_to_R5_T")
pop_ratio_P_F_to_R6_T = c("pop_ratio_P31_F_to_R6_T", "pop_ratio_P32_F_to_R6_T", "pop_ratio_P33_F_to_R6_T",
                          "pop_ratio_P34_F_to_R6_T", "pop_ratio_P35_F_to_R6_T")
pop_ratio_P_F_to_R7_T = c("pop_ratio_P36_F_to_R7_T", "pop_ratio_P37_F_to_R7_T", "pop_ratio_P38_F_to_R7_T",
                          "pop_ratio_P39_F_to_R7_T")
pop_ratio_P_F_to_R8_T = c("pop_ratio_P40_F_to_R8_T", "pop_ratio_P41_F_to_R8_T", "pop_ratio_P42_F_to_R8_T",
                          "pop_ratio_P43_F_to_R8_T", "pop_ratio_P44_F_to_R8_T", "pop_ratio_P45_F_to_R8_T",
                          "pop_ratio_P46_F_to_R8_T", "pop_ratio_P47_F_to_R8_T")

assign(pop_ratio_P_F_to_R1_T, pop_fore_fun(top = get(state[2])$pop$female, bottom = mfts_R1$pop$total, year_horizon = 10))
for(iw in 2:7)
{
  assign(pop_ratio_P_F_to_R2_T[iw-1], pop_fore_fun(top = get(state[(iw+1)])$pop$female, bottom = mfts_R2$pop$total, year_horizon = 10))
}         
for(iw in 8:14)
{
  assign(pop_ratio_P_F_to_R3_T[iw-7], pop_fore_fun(top = get(state[(iw+1)])$pop$female, bottom = mfts_R3$pop$total, year_horizon = 10))
}
for(iw in 15:23)
{
  assign(pop_ratio_P_F_to_R4_T[iw-14], pop_fore_fun(top = get(state[(iw+1)])$pop$female, bottom = mfts_R4$pop$total, year_horizon = 10))
}
for(iw in 24:30)
{
  assign(pop_ratio_P_F_to_R5_T[iw-23], pop_fore_fun(top = get(state[(iw+1)])$pop$female, bottom = mfts_R5$pop$total, year_horizon = 10))
}
for(iw in 31:35)
{
  assign(pop_ratio_P_F_to_R6_T[iw-30], pop_fore_fun(top = get(state[(iw+1)])$pop$female, bottom = mfts_R6$pop$total, year_horizon = 10))
}
for(iw in 36:39)
{
  assign(pop_ratio_P_F_to_R7_T[iw-35], pop_fore_fun(top = get(state[(iw+1)])$pop$female, bottom = mfts_R7$pop$total, year_horizon = 10))
}
for(iw in 40:47)
{
  assign(pop_ratio_P_F_to_R8_T[iw-39], pop_fore_fun(top = get(state[(iw+1)])$pop$female, bottom = mfts_R8$pop$total, year_horizon = 10))
}

pop_ratio_P_M_to_R1_T = "pop_ratio_P1_M_to_R1_T"
pop_ratio_P_M_to_R2_T = c("pop_ratio_P2_M_to_R2_T", "pop_ratio_P3_M_to_R2_T", "pop_ratio_P4_M_to_R2_T",
                          "pop_ratio_P5_M_to_R2_T", "pop_ratio_P6_M_to_R2_T", "pop_ratio_P7_M_to_R2_T")
pop_ratio_P_M_to_R3_T = c("pop_ratio_P8_M_to_R3_T", "pop_ratio_P9_M_to_R3_T", "pop_ratio_P10_M_to_R3_T",
                          "pop_ratio_P11_M_to_R3_T", "pop_ratio_P12_M_to_R3_T", "pop_ratio_P13_M_to_R3_T",
                          "pop_ratio_P14_M_to_R3_T")
pop_ratio_P_M_to_R4_T = c("pop_ratio_P15_M_to_R4_T", "pop_ratio_P16_M_to_R4_T", "pop_ratio_P17_M_to_R4_T",
                          "pop_ratio_P18_M_to_R4_T", "pop_ratio_P19_M_to_R4_T", "pop_ratio_P20_M_to_R4_T", 
                          "pop_ratio_P21_M_to_R4_T", "pop_ratio_P22_M_to_R4_T", "pop_ratio_P23_M_to_R4_T")
pop_ratio_P_M_to_R5_T = c("pop_ratio_P24_M_to_R5_T", "pop_ratio_P25_M_to_R5_T", "pop_ratio_P26_M_to_R5_T", 
                          "pop_ratio_P27_M_to_R5_T", "pop_ratio_P28_M_to_R5_T", "pop_ratio_P29_M_to_R5_T", 
                          "pop_ratio_P30_M_to_R5_T")
pop_ratio_P_M_to_R6_T = c("pop_ratio_P31_M_to_R6_T", "pop_ratio_P32_M_to_R6_T", "pop_ratio_P33_M_to_R6_T",
                          "pop_ratio_P34_M_to_R6_T", "pop_ratio_P35_M_to_R6_T")
pop_ratio_P_M_to_R7_T = c("pop_ratio_P36_M_to_R7_T", "pop_ratio_P37_M_to_R7_T", "pop_ratio_P38_M_to_R7_T",
                          "pop_ratio_P39_M_to_R7_T")
pop_ratio_P_M_to_R8_T = c("pop_ratio_P40_M_to_R8_T", "pop_ratio_P41_M_to_R8_T", "pop_ratio_P42_M_to_R8_T",
                          "pop_ratio_P43_M_to_R8_T", "pop_ratio_P44_M_to_R8_T", "pop_ratio_P45_M_to_R8_T",
                          "pop_ratio_P46_M_to_R8_T", "pop_ratio_P47_M_to_R8_T")

assign(pop_ratio_P_M_to_R1_T, pop_fore_fun(top = get(state[2])$pop$male, bottom = mfts_R1$pop$total, year_horizon = 10))
for(iw in 2:7)
{
  assign(pop_ratio_P_M_to_R2_T[iw-1], pop_fore_fun(top = get(state[(iw+1)])$pop$male, bottom = mfts_R2$pop$total, year_horizon = 10))
}         
for(iw in 8:14)
{
  assign(pop_ratio_P_M_to_R3_T[iw-7], pop_fore_fun(top = get(state[(iw+1)])$pop$male, bottom = mfts_R3$pop$total, year_horizon = 10))
}
for(iw in 15:23)
{
  assign(pop_ratio_P_M_to_R4_T[iw-14], pop_fore_fun(top = get(state[(iw+1)])$pop$male, bottom = mfts_R4$pop$total, year_horizon = 10))
}
for(iw in 24:30)
{
  assign(pop_ratio_P_M_to_R5_T[iw-23], pop_fore_fun(top = get(state[(iw+1)])$pop$male, bottom = mfts_R5$pop$total, year_horizon = 10))
}
for(iw in 31:35)
{
  assign(pop_ratio_P_M_to_R6_T[iw-30], pop_fore_fun(top = get(state[(iw+1)])$pop$male, bottom = mfts_R6$pop$total, year_horizon = 10))
}
for(iw in 36:39)
{
  assign(pop_ratio_P_M_to_R7_T[iw-35], pop_fore_fun(top = get(state[(iw+1)])$pop$male, bottom = mfts_R7$pop$total, year_horizon = 10))
}
for(iw in 40:47)
{
  assign(pop_ratio_P_M_to_R8_T[iw-39], pop_fore_fun(top = get(state[(iw+1)])$pop$male, bottom = mfts_R8$pop$total, year_horizon = 10))
}


## Row 5

pop_ratio_P_F_to_R1_F = "pop_ratio_P1_F_to_R1_F"
pop_ratio_P_F_to_R2_F = c("pop_ratio_P2_F_to_R2_F", "pop_ratio_P3_F_to_R2_F", "pop_ratio_P4_F_to_R2_F",
                          "pop_ratio_P5_F_to_R2_F", "pop_ratio_P6_F_to_R2_F", "pop_ratio_P7_F_to_R2_F")
pop_ratio_P_F_to_R3_F = c("pop_ratio_P8_F_to_R3_F", "pop_ratio_P9_F_to_R3_F", "pop_ratio_P10_F_to_R3_F",
                          "pop_ratio_P11_F_to_R3_F", "pop_ratio_P12_F_to_R3_F", "pop_ratio_P13_F_to_R3_F",
                          "pop_ratio_P14_F_to_R3_F")
pop_ratio_P_F_to_R4_F = c("pop_ratio_P15_F_to_R4_F", "pop_ratio_P16_F_to_R4_F", "pop_ratio_P17_F_to_R4_F",
                          "pop_ratio_P18_F_to_R4_F", "pop_ratio_P19_F_to_R4_F", "pop_ratio_P20_F_to_R4_F", 
                          "pop_ratio_P21_F_to_R4_F", "pop_ratio_P22_F_to_R4_F", "pop_ratio_P23_F_to_R4_F")
pop_ratio_P_F_to_R5_F = c("pop_ratio_P24_F_to_R5_F", "pop_ratio_P25_F_to_R5_F", "pop_ratio_P26_F_to_R5_F", 
                          "pop_ratio_P27_F_to_R5_F", "pop_ratio_P28_F_to_R5_F", "pop_ratio_P29_F_to_R5_F", 
                          "pop_ratio_P30_F_to_R5_F")
pop_ratio_P_F_to_R6_F = c("pop_ratio_P31_F_to_R6_F", "pop_ratio_P32_F_to_R6_F", "pop_ratio_P33_F_to_R6_F",
                          "pop_ratio_P34_F_to_R6_F", "pop_ratio_P35_F_to_R6_F")
pop_ratio_P_F_to_R7_F = c("pop_ratio_P36_F_to_R7_F", "pop_ratio_P37_F_to_R7_F", "pop_ratio_P38_F_to_R7_F",
                          "pop_ratio_P39_F_to_R7_F")
pop_ratio_P_F_to_R8_F = c("pop_ratio_P40_F_to_R8_F", "pop_ratio_P41_F_to_R8_F", "pop_ratio_P42_F_to_R8_F",
                          "pop_ratio_P43_F_to_R8_F", "pop_ratio_P44_F_to_R8_F", "pop_ratio_P45_F_to_R8_F",
                          "pop_ratio_P46_F_to_R8_F", "pop_ratio_P47_F_to_R8_F")

assign(pop_ratio_P_F_to_R1_F, pop_fore_fun(top = get(state[2])$pop$female, bottom = mfts_R1$pop$female, year_horizon = 10))
for(iw in 2:7)
{
  assign(pop_ratio_P_F_to_R2_F[iw-1], pop_fore_fun(top = get(state[(iw+1)])$pop$female, bottom = mfts_R2$pop$female, year_horizon = 10))
}         
for(iw in 8:14)
{
  assign(pop_ratio_P_F_to_R3_F[iw-7], pop_fore_fun(top = get(state[(iw+1)])$pop$female, bottom = mfts_R3$pop$female, year_horizon = 10))
}
for(iw in 15:23)
{
  assign(pop_ratio_P_F_to_R4_F[iw-14], pop_fore_fun(top = get(state[(iw+1)])$pop$female, bottom = mfts_R4$pop$female, year_horizon = 10))
}
for(iw in 24:30)
{
  assign(pop_ratio_P_F_to_R5_F[iw-23], pop_fore_fun(top = get(state[(iw+1)])$pop$female, bottom = mfts_R5$pop$female, year_horizon = 10))
}
for(iw in 31:35)
{
  assign(pop_ratio_P_F_to_R6_F[iw-30], pop_fore_fun(top = get(state[(iw+1)])$pop$female, bottom = mfts_R6$pop$female, year_horizon = 10))
}
for(iw in 36:39)
{
  assign(pop_ratio_P_F_to_R7_F[iw-35], pop_fore_fun(top = get(state[(iw+1)])$pop$female, bottom = mfts_R7$pop$female, year_horizon = 10))
}
for(iw in 40:47)
{
  assign(pop_ratio_P_F_to_R8_F[iw-39], pop_fore_fun(top = get(state[(iw+1)])$pop$female, bottom = mfts_R8$pop$female, year_horizon = 10))
}

## Row 6

pop_ratio_P_M_to_R1_M = "pop_ratio_P1_M_to_R1_M"
pop_ratio_P_M_to_R2_M = c("pop_ratio_P2_M_to_R2_M", "pop_ratio_P3_M_to_R2_M", "pop_ratio_P4_M_to_R2_M",
                          "pop_ratio_P5_M_to_R2_M", "pop_ratio_P6_M_to_R2_M", "pop_ratio_P7_M_to_R2_M")
pop_ratio_P_M_to_R3_M = c("pop_ratio_P8_M_to_R3_M", "pop_ratio_P9_M_to_R3_M", "pop_ratio_P10_M_to_R3_M",
                          "pop_ratio_P11_M_to_R3_M", "pop_ratio_P12_M_to_R3_M", "pop_ratio_P13_M_to_R3_M",
                          "pop_ratio_P14_M_to_R3_M")
pop_ratio_P_M_to_R4_M = c("pop_ratio_P15_M_to_R4_M", "pop_ratio_P16_M_to_R4_M", "pop_ratio_P17_M_to_R4_M",
                          "pop_ratio_P18_M_to_R4_M", "pop_ratio_P19_M_to_R4_M", "pop_ratio_P20_M_to_R4_M", 
                          "pop_ratio_P21_M_to_R4_M", "pop_ratio_P22_M_to_R4_M", "pop_ratio_P23_M_to_R4_M")
pop_ratio_P_M_to_R5_M = c("pop_ratio_P24_M_to_R5_M", "pop_ratio_P25_M_to_R5_M", "pop_ratio_P26_M_to_R5_M", 
                          "pop_ratio_P27_M_to_R5_M", "pop_ratio_P28_M_to_R5_M", "pop_ratio_P29_M_to_R5_M", 
                          "pop_ratio_P30_M_to_R5_M")
pop_ratio_P_M_to_R6_M = c("pop_ratio_P31_M_to_R6_M", "pop_ratio_P32_M_to_R6_M", "pop_ratio_P33_M_to_R6_M",
                          "pop_ratio_P34_M_to_R6_M", "pop_ratio_P35_M_to_R6_M")
pop_ratio_P_M_to_R7_M = c("pop_ratio_P36_M_to_R7_M", "pop_ratio_P37_M_to_R7_M", "pop_ratio_P38_M_to_R7_M",
                          "pop_ratio_P39_M_to_R7_M")
pop_ratio_P_M_to_R8_M = c("pop_ratio_P40_M_to_R8_M", "pop_ratio_P41_M_to_R8_M", "pop_ratio_P42_M_to_R8_M",
                          "pop_ratio_P43_M_to_R8_M", "pop_ratio_P44_M_to_R8_M", "pop_ratio_P45_M_to_R8_M",
                          "pop_ratio_P46_M_to_R8_M", "pop_ratio_P47_M_to_R8_M")

assign(pop_ratio_P_M_to_R1_M, pop_fore_fun(top = get(state[2])$pop$male, bottom = mfts_R1$pop$male, year_horizon = 10))
for(iw in 2:7)
{
  assign(pop_ratio_P_M_to_R2_M[iw-1], pop_fore_fun(top = get(state[(iw+1)])$pop$male, bottom = mfts_R2$pop$male, year_horizon = 10))
}         
for(iw in 8:14)
{
  assign(pop_ratio_P_M_to_R3_M[iw-7], pop_fore_fun(top = get(state[(iw+1)])$pop$male, bottom = mfts_R3$pop$male, year_horizon = 10))
}
for(iw in 15:23)
{
  assign(pop_ratio_P_M_to_R4_M[iw-14], pop_fore_fun(top = get(state[(iw+1)])$pop$male, bottom = mfts_R4$pop$male, year_horizon = 10))
}
for(iw in 24:30)
{
  assign(pop_ratio_P_M_to_R5_M[iw-23], pop_fore_fun(top = get(state[(iw+1)])$pop$male, bottom = mfts_R5$pop$male, year_horizon = 10))
}
for(iw in 31:35)
{
  assign(pop_ratio_P_M_to_R6_M[iw-30], pop_fore_fun(top = get(state[(iw+1)])$pop$male, bottom = mfts_R6$pop$male, year_horizon = 10))
}
for(iw in 36:39)
{
  assign(pop_ratio_P_M_to_R7_M[iw-35], pop_fore_fun(top = get(state[(iw+1)])$pop$male, bottom = mfts_R7$pop$male, year_horizon = 10))
}
for(iw in 40:47)
{
  assign(pop_ratio_P_M_to_R8_M[iw-39], pop_fore_fun(top = get(state[(iw+1)])$pop$male, bottom = mfts_R8$pop$male, year_horizon = 10))
}

## Row 7

pop_ratio_P_F_to_P_T = c("pop_ratio_P1_F_to_P1_T", "pop_ratio_P2_F_to_P2_T", "pop_ratio_P3_F_to_P3_T",
                         "pop_ratio_P4_F_to_P4_T", "pop_ratio_P5_F_to_P5_T", "pop_ratio_P6_F_to_P6_T",
                         "pop_ratio_P7_F_to_P7_T", "pop_ratio_P8_F_to_P8_T", "pop_ratio_P9_F_to_P9_T",
                         "pop_ratio_P10_F_to_P10_T", "pop_ratio_P11_F_to_P11_T", "pop_ratio_P12_F_to_P12_T",
                         "pop_ratio_P13_F_to_P13_T", "pop_ratio_P14_F_to_P14_T",
                         "pop_ratio_P15_F_to_P15_T", "pop_ratio_P16_F_to_P16_T", "pop_ratio_P17_F_to_P17_T",
                         "pop_ratio_P18_F_to_P18_T", "pop_ratio_P19_F_to_P19_T", "pop_ratio_P20_F_to_P20_T",
                         "pop_ratio_P21_F_to_P21_T", "pop_ratio_P22_F_to_P22_T", "pop_ratio_P23_F_to_P23_T",
                         "pop_ratio_P24_F_to_P24_T", "pop_ratio_P25_F_to_P25_T", "pop_ratio_P26_F_to_P26_T",
                         "pop_ratio_P27_F_to_P27_T", "pop_ratio_P28_F_to_P28_T", "pop_ratio_P29_F_to_P29_T",
                         "pop_ratio_P30_F_to_P30_T", "pop_ratio_P31_F_to_P31_T", "pop_ratio_P32_F_to_P32_T",
                         "pop_ratio_P33_F_to_P33_T", "pop_ratio_P34_F_to_P34_T", "pop_ratio_P35_F_to_P35_T",
                         "pop_ratio_P36_F_to_P36_T", "pop_ratio_P37_F_to_P37_T", "pop_ratio_P38_F_to_P38_T",
                         "pop_ratio_P39_F_to_P39_T", "pop_ratio_P40_F_to_P40_T", "pop_ratio_P41_F_to_P41_T",
                         "pop_ratio_P42_F_to_P42_T", "pop_ratio_P43_F_to_P43_T", "pop_ratio_P44_F_to_P44_T",
                         "pop_ratio_P45_F_to_P45_T", "pop_ratio_P46_F_to_P46_T", "pop_ratio_P47_F_to_P47_T")

pop_ratio_P_M_to_P_T = c("pop_ratio_P1_M_to_P1_T", "pop_ratio_P2_M_to_P2_T", "pop_ratio_P3_M_to_P3_T",
                         "pop_ratio_P4_M_to_P4_T", "pop_ratio_P5_M_to_P5_T", "pop_ratio_P6_M_to_P6_T",
                         "pop_ratio_P7_M_to_P7_T", "pop_ratio_P8_M_to_P8_T", "pop_ratio_P9_M_to_P9_T",
                         "pop_ratio_P10_M_to_P10_T", "pop_ratio_P11_M_to_P11_T", "pop_ratio_P12_M_to_P12_T",
                         "pop_ratio_P13_M_to_P13_T", "pop_ratio_P14_M_to_P14_T",
                         "pop_ratio_P15_M_to_P15_T", "pop_ratio_P16_M_to_P16_T", "pop_ratio_P17_M_to_P17_T",
                         "pop_ratio_P18_M_to_P18_T", "pop_ratio_P19_M_to_P19_T", "pop_ratio_P20_M_to_P20_T",
                         "pop_ratio_P21_M_to_P21_T", "pop_ratio_P22_M_to_P22_T", "pop_ratio_P23_M_to_P23_T",
                         "pop_ratio_P24_M_to_P24_T", "pop_ratio_P25_M_to_P25_T", "pop_ratio_P26_M_to_P26_T",
                         "pop_ratio_P27_M_to_P27_T", "pop_ratio_P28_M_to_P28_T", "pop_ratio_P29_M_to_P29_T",
                         "pop_ratio_P30_M_to_P30_T", "pop_ratio_P31_M_to_P31_T", "pop_ratio_P32_M_to_P32_T",
                         "pop_ratio_P33_M_to_P33_T", "pop_ratio_P34_M_to_P34_T", "pop_ratio_P35_M_to_P35_T",
                         "pop_ratio_P36_M_to_P36_T", "pop_ratio_P37_M_to_P37_T", "pop_ratio_P38_M_to_P38_T",
                         "pop_ratio_P39_M_to_P39_T", "pop_ratio_P40_M_to_P40_T", "pop_ratio_P41_M_to_P41_T",
                         "pop_ratio_P42_M_to_P42_T", "pop_ratio_P43_M_to_P43_T", "pop_ratio_P44_M_to_P44_T",
                         "pop_ratio_P45_M_to_P45_T", "pop_ratio_P46_M_to_P46_T", "pop_ratio_P47_M_to_P47_T")

for(iw in 2:48)
{
  assign(pop_ratio_P_F_to_P_T[iw-1], pop_fore_fun(top = get(state[iw])$pop$female, bottom = get(state[iw])$pop$total, year_horizon = 10))
  assign(pop_ratio_P_M_to_P_T[iw-1], pop_fore_fun(top = get(state[iw])$pop$male,   bottom = get(state[iw])$pop$total, year_horizon = 10))          
}


##########################################################
# Construct summing matrix S by forecast exposure to risk
##########################################################

Smat_fun <- function(kj, age, no_prefecture = 47)
{
  level_0 = level_1_a = level_1_b = matrix(NA, 2*no_prefecture,(11-kj))
  for(iw in 1:no_prefecture)
  {
    # Level 0 
    
    level_0[(2*iw-1),] = get(pop_ratio_P_F_to_T[iw])[kj,age,1:(11-kj)]
    level_0[(2*iw),]   = get(pop_ratio_P_M_to_T[iw])[kj,age,1:(11-kj)]
    
    # Level 1 (disaggregate by sex)
    
    level_1_a[(2*iw-1),] = get(pop_ratio_P_F_to_F[iw])[kj,age,1:(11-kj)]
    level_1_a[(2*iw),]   = rep(0,(11-kj))
    
    level_1_b[(2*iw-1),] = rep(0,(11-kj))
    level_1_b[(2*iw),]   = get(pop_ratio_P_M_to_M[iw])[kj,age,1:(11-kj)]
  }
  level_2_a_1 = level_2_a_2 = level_2_a_3 = level_2_a_4 = level_2_a_5 = 
    level_2_a_6 = level_2_a_7 = level_2_a_8 = matrix(NA, 2*no_prefecture,(11-kj))
  
  ###################################  
  # Level 2 (disaggregate by region)
  ###################################
  
  # R1
  
  level_2_a_1[1,] = get(pop_ratio_P_F_to_R1_T[1])[kj,age,1:(11-kj)]  
  level_2_a_1[2,] = get(pop_ratio_P_M_to_R1_T[1])[kj,age,1:(11-kj)]
  level_2_a_1[3:(2*no_prefecture),] = matrix(rep(0,(11-kj)*92), ncol=(11-kj))
  
  # R2
  
  level_2_a_2[1:2, ] = matrix(rep(0,(11-kj)*2), ncol=(11-kj))
  for(iw in 1:6)
  {
    level_2_a_2[(2*iw+1),] = get(pop_ratio_P_F_to_R2_T[iw])[kj,age,1:(11-kj)]
    level_2_a_2[(2*iw+2),] = get(pop_ratio_P_M_to_R2_T[iw])[kj,age,1:(11-kj)]
  }  
  level_2_a_2[15:(2*no_prefecture),] = matrix(rep(0,(11-kj)*80), ncol=(11-kj))
  
  # R3
  
  level_2_a_3[1:14,] = matrix(rep(0,(11-kj)*14), ncol=(11-kj))  
  for(iw in 1:7)
  {
    level_2_a_3[(2*(iw+6)+1),] = get(pop_ratio_P_F_to_R3_T[iw])[kj,age,1:(11-kj)]
    level_2_a_3[(2*(iw+6)+2),] = get(pop_ratio_P_M_to_R3_T[iw])[kj,age,1:(11-kj)]
  }
  level_2_a_3[29:(2*no_prefecture),] = matrix(rep(0,(11-kj)*66), ncol=(11-kj))
  
  # R4
  
  level_2_a_4[1:28,] = matrix(rep(0,(11-kj)*28), ncol=(11-kj))
  for(iw in 1:9)
  {
    level_2_a_4[(2*(iw+13)+1),] = get(pop_ratio_P_F_to_R4_T[iw])[kj,age,1:(11-kj)]
    level_2_a_4[(2*(iw+13)+2),] = get(pop_ratio_P_M_to_R4_T[iw])[kj,age,1:(11-kj)]
  }
  level_2_a_4[47:(2*no_prefecture),] = matrix(rep(0,(11-kj)*48), ncol=(11-kj))
  
  # R5
  
  level_2_a_5[1:46,] = matrix(rep(0,(11-kj)*46), ncol=(11-kj))
  for(iw in 1:7)
  {
    level_2_a_5[(2*(iw+22)+1),] = get(pop_ratio_P_F_to_R5_T[iw])[kj,age,1:(11-kj)]
    level_2_a_5[(2*(iw+22)+2),] = get(pop_ratio_P_M_to_R5_T[iw])[kj,age,1:(11-kj)]
  }
  level_2_a_5[61:(2*no_prefecture),] = matrix(rep(0,(11-kj)*34), ncol=(11-kj))
  
  # R6
  
  level_2_a_6[1:60,] = matrix(rep(0,(11-kj)*60), ncol=(11-kj))
  for(iw in 1:5)
  {
    level_2_a_6[(2*(iw+29)+1),] = get(pop_ratio_P_F_to_R6_T[iw])[kj,age,1:(11-kj)]
    level_2_a_6[(2*(iw+29)+2),] = get(pop_ratio_P_M_to_R6_T[iw])[kj,age,1:(11-kj)]
  }
  level_2_a_6[71:(2*no_prefecture),] = matrix(rep(0,(11-kj)*24), ncol=(11-kj))
  
  # R7
  
  level_2_a_7[1:70,] = matrix(rep(0,(11-kj)*70), ncol=(11-kj))
  for(iw in 1:4)
  {
    level_2_a_7[(2*(iw+34)+1),] = get(pop_ratio_P_F_to_R7_T[iw])[kj,age,1:(11-kj)]
    level_2_a_7[(2*(iw+34)+2),] = get(pop_ratio_P_M_to_R7_T[iw])[kj,age,1:(11-kj)]
  }
  level_2_a_7[79:(2*no_prefecture),] = matrix(rep(0,(11-kj)*16), ncol=(11-kj))
  
  # R8
  
  level_2_a_8[1:78,] = matrix(rep(0,(11-kj)*78), ncol=(11-kj))
  for(iw in 1:8)
  {
    level_2_a_8[(2*(iw+38)+1),] = get(pop_ratio_P_F_to_R8_T[iw])[kj,age,1:(11-kj)]
    level_2_a_8[(2*(iw+38)+2),] = get(pop_ratio_P_M_to_R8_T[iw])[kj,age,1:(11-kj)] 
  }
  
  level_2_b_1 = level_2_b_2 = level_2_b_3 = level_2_b_4 = level_2_b_5 = level_2_b_6 = 
    level_2_b_7 = level_2_b_8 = matrix(NA, (2*no_prefecture),(11-kj))
  
  ##################################################
  # Level 2 (disaggregate by region + sex (female))
  ##################################################
  
  # R1
  
  level_2_b_1[1,] = get(pop_ratio_P_F_to_R1_F[1])[kj,age,1:(11-kj)]  
  level_2_b_1[2:(2*no_prefecture),] = matrix(rep(0,(11-kj)*93), ncol=(11-kj))
  
  # R2
  
  level_2_b_2[1:2, ] = matrix(rep(0,(11-kj)*2), ncol=(11-kj))
  for(iw in 1:6)
  {
    level_2_b_2[(2*iw+1),] = get(pop_ratio_P_F_to_R2_F[iw])[kj,age,1:(11-kj)]
    level_2_b_2[(2*iw+2),] = rep(0, (11-kj))
  }  
  level_2_b_2[15:(2*no_prefecture),] = matrix(rep(0,(11-kj)*80), ncol=(11-kj))
  
  # R3
  
  level_2_b_3[1:14,] = matrix(rep(0,(11-kj)*14), ncol=(11-kj))  
  for(iw in 1:7)
  {
    level_2_b_3[(2*(iw+6)+1),] = get(pop_ratio_P_F_to_R3_F[iw])[kj,age,1:(11-kj)]
    level_2_b_3[(2*(iw+6)+2),] = rep(0, (11-kj))
  }
  level_2_b_3[29:(2*no_prefecture),] = matrix(rep(0,(11-kj)*66), ncol=(11-kj))
  
  # R4
  
  level_2_b_4[1:28,] = matrix(rep(0,(11-kj)*28), ncol=(11-kj))
  for(iw in 1:9)
  {
    level_2_b_4[(2*(iw+13)+1),] = get(pop_ratio_P_F_to_R4_F[iw])[kj,age,1:(11-kj)]
    level_2_b_4[(2*(iw+13)+2),] = rep(0, (11-kj))
  }
  level_2_b_4[47:(2*no_prefecture),] = matrix(rep(0,(11-kj)*48), ncol=(11-kj))
  
  # R5
  
  level_2_b_5[1:46,] = matrix(rep(0,(11-kj)*46), ncol=(11-kj))
  for(iw in 1:7)
  {
    level_2_b_5[(2*(iw+22)+1),] = get(pop_ratio_P_F_to_R5_F[iw])[kj,age,1:(11-kj)]
    level_2_b_5[(2*(iw+22)+2),] = rep(0, (11-kj))
  }
  level_2_b_5[61:(2*no_prefecture),] = matrix(rep(0,(11-kj)*34), ncol=(11-kj))
  
  # R6
  
  level_2_b_6[1:60,] = matrix(rep(0,(11-kj)*60), ncol=(11-kj))
  for(iw in 1:5)
  {
    level_2_b_6[(2*(iw+29)+1),] = get(pop_ratio_P_F_to_R6_F[iw])[kj,age,1:(11-kj)]
    level_2_b_6[(2*(iw+29)+2),] = rep(0, (11-kj))
  }
  level_2_b_6[71:(2*no_prefecture),] = matrix(rep(0,(11-kj)*24), ncol=(11-kj))
  
  # R7
  
  level_2_b_7[1:70,] = matrix(rep(0,(11-kj)*70), ncol=(11-kj))
  for(iw in 1:4)
  {
    level_2_b_7[(2*(iw+34)+1),] = get(pop_ratio_P_F_to_R7_F[iw])[kj,age,1:(11-kj)]
    level_2_b_7[(2*(iw+34)+2),] = rep(0, (11-kj))
  }
  level_2_b_7[79:(2*no_prefecture),] = matrix(rep(0,(11-kj)*16), ncol=(11-kj))
  
  # R8
  
  level_2_b_8[1:78,] = matrix(rep(0,(11-kj)*78), ncol=(11-kj))
  for(iw in 1:8)
  {
    level_2_b_8[(2*(iw+38)+1),] = get(pop_ratio_P_F_to_R8_F[iw])[kj,age,1:(11-kj)]
    level_2_b_8[(2*(iw+38)+2),] = rep(0, (11-kj))
  }
  
  level_2_c_1 = level_2_c_2 = level_2_c_3 = level_2_c_4 = level_2_c_5 = level_2_c_6 = 
    level_2_c_7 = level_2_c_8 = matrix(NA, (2*no_prefecture),(11-kj))
  
  ################################################
  # Level 2 (disaggregate by region + sex (male))
  ################################################
  
  # R1
  
  level_2_c_1[1,] = rep(0,(11-kj))
  level_2_c_1[2,] = get(pop_ratio_P_M_to_R1_M[1])[kj,age,1:(11-kj)]
  level_2_c_1[3:(2*no_prefecture),] = matrix(rep(0,(11-kj)*92), ncol=(11-kj))
  
  # R2
  
  level_2_c_2[1:2, ] = matrix(rep(0,(11-kj)*2), ncol=(11-kj))
  for(iw in 1:6)
  {
    level_2_c_2[(2*iw+1),] = rep(0,(11-kj))
    level_2_c_2[(2*iw+2),] = get(pop_ratio_P_M_to_R2_M[iw])[kj,age,1:(11-kj)]
  }  
  level_2_c_2[15:(2*no_prefecture),] = matrix(rep(0,(11-kj)*80), ncol=(11-kj))
  
  # R3
  
  level_2_c_3[1:14,] = matrix(rep(0,(11-kj)*14), ncol=(11-kj))  
  for(iw in 1:7)
  {
    level_2_c_3[(2*(iw+6)+1),] = rep(0,(11-kj))
    level_2_c_3[(2*(iw+6)+2),] = get(pop_ratio_P_M_to_R3_M[iw])[kj,age,1:(11-kj)]
  }
  level_2_c_3[29:(2*no_prefecture),] = matrix(rep(0,(11-kj)*66), ncol=(11-kj))
  
  # R4
  
  level_2_c_4[1:28,] = matrix(rep(0,(11-kj)*28), ncol=(11-kj))
  for(iw in 1:9)
  {
    level_2_c_4[(2*(iw+13)+1),] = rep(0,(11-kj))
    level_2_c_4[(2*(iw+13)+2),] = get(pop_ratio_P_M_to_R4_M[iw])[kj,age,1:(11-kj)]
  }
  level_2_c_4[47:(2*no_prefecture),] = matrix(rep(0,(11-kj)*48), ncol=(11-kj))
  
  # R5
  
  level_2_c_5[1:46,] = matrix(rep(0,(11-kj)*46), ncol=(11-kj))
  for(iw in 1:7)
  {
    level_2_c_5[(2*(iw+22)+1),] = rep(0,(11-kj))
    level_2_c_5[(2*(iw+22)+2),] = get(pop_ratio_P_M_to_R5_M[iw])[kj,age,1:(11-kj)]
  }
  level_2_c_5[61:(2*no_prefecture),] = matrix(rep(0,(11-kj)*34), ncol=(11-kj))
  
  # R6
  
  level_2_c_6[1:60,] = matrix(rep(0,(11-kj)*60), ncol=(11-kj))
  for(iw in 1:5)
  {
    level_2_c_6[(2*(iw+29)+1),] = rep(0,(11-kj))
    level_2_c_6[(2*(iw+29)+2),] = get(pop_ratio_P_M_to_R6_M[iw])[kj,age,1:(11-kj)]
  }
  level_2_c_6[71:(2*no_prefecture),] = matrix(rep(0,(11-kj)*24), ncol=(11-kj))
  
  # R7
  
  level_2_c_7[1:70,] = matrix(rep(0,(11-kj)*70), ncol=(11-kj))
  for(iw in 1:4)
  {
    level_2_c_7[(2*(iw+34)+1),] = rep(0,(11-kj))
    level_2_c_7[(2*(iw+34)+2),] = get(pop_ratio_P_M_to_R7_M[iw])[kj,age,1:(11-kj)]
  }
  level_2_c_7[79:(2*no_prefecture),] = matrix(rep(0,(11-kj)*16), ncol=(11-kj))
  
  # R8
  
  level_2_c_8[1:78,] = matrix(rep(0,(11-kj)*78), ncol=(11-kj))
  for(iw in 1:8)
  {
    level_2_c_8[(2*(iw+38)+1),] = rep(0,(11-kj))
    level_2_c_8[(2*(iw+38)+2),] = get(pop_ratio_P_M_to_R8_M[iw])[kj,age,1:(11-kj)] 
  }
  
  #############################################################
  # Level 3 (disaggregate by prefecture + sex (female & male))
  #############################################################
  
  level_3 = array(0,dim = c((2*no_prefecture),(11-kj),47))
  for(iw in 1:47)
  {
    level_3[2*iw-1,,iw] = get(pop_ratio_P_F_to_P_T[iw])[kj,age,1:(11-kj)]
    level_3[2*iw,,iw]   = get(pop_ratio_P_M_to_P_T[iw])[kj,age,1:(11-kj)]
  }
  
  S_mat = array(NA, dim = c(168, (2*no_prefecture), (11-kj)))
  for(ik in 1:(11-kj))
  {
    S_mat[,,ik] = rbind(level_0[,ik],     level_1_a[,ik],   level_1_b[,ik],   level_2_a_1[,ik],
                        level_2_a_2[,ik], level_2_a_3[,ik], level_2_a_4[,ik], level_2_a_5[,ik],
                        level_2_a_6[,ik], level_2_a_7[,ik], level_2_a_8[,ik], level_2_b_1[,ik],
                        level_2_b_2[,ik], level_2_b_3[,ik], level_2_b_4[,ik], level_2_b_5[,ik],
                        level_2_b_6[,ik], level_2_b_7[,ik], level_2_b_8[,ik], level_2_c_1[,ik],
                        level_2_c_2[,ik], level_2_c_3[,ik], level_2_c_4[,ik], level_2_c_5[,ik],
                        level_2_c_6[,ik], level_2_c_7[,ik], level_2_c_8[,ik], t(level_3[,ik,]),
                        diag(94))
  }
  return(S_mat)
}



####################################################
# W_h summing matrix for MinT reconciliation method
####################################################

wh_fun <- function(kj, age)
{
  lowerD = hts:::lowerD
  shrink.estim = hts:::shrink.estim
  
  eh_mat = matrix(NA, nrow = 168, ncol = (39+kj))
  
  # level 0
  eh_mat[1,] = get(ind_state_train_residual_total[1])[[kj]][age,]
  
  # level 1 (disaggregate by sex)
  eh_mat[2,] = get(ind_state_train_residual_female[1])[[kj]][age,]
  eh_mat[3,] = get(ind_state_train_residual_male[1])[[kj]][age,]
  
  # level 2 & 3 (disaggregate by region & sex)
  for (ikw in 1:8)
  {
    eh_mat[(3+ikw),] = get(ind_region_train_residual_total[ikw])[[kj]][age,]
    eh_mat[(11+ikw),] = get(ind_region_train_residual_female[ikw])[[kj]][age,]
    eh_mat[(19+ikw),] = get(ind_region_train_residual_male[ikw])[[kj]][age,]
  }
  
  # level 4 (disaggregate by state)
  for (ikw in 1:47)
  {
    eh_mat[(27+ikw),] = get(ind_state_train_residual_total[(ikw+1)])[[kj]][age,]
  }
  
  # level 5 (disaggregate by state & sex)
  for (ikw in 1:47)
  {
    eh_mat[(74+(2*ikw-1)),] = get(ind_state_train_residual_female[(ikw+1)])[[kj]][age,]
    eh_mat[(74+(2*ikw)),] = get(ind_state_train_residual_male[(ikw+1)])[[kj]][age,]
  }
  
  target = lowerD(t(eh_mat))
  shrink = shrink.estim(t(eh_mat), target)
  wh_mat = shrink[[1]]
  
  return(wh_mat = wh_mat)
}


wh_fun_mfts <- function(kj, age)
{
  lowerD = hts:::lowerD
  shrink.estim = hts:::shrink.estim
  
  eh_mat = matrix(NA, nrow = 168, ncol = (39+kj))
  
  # level 0
  eh_mat[1,] = get(mfts_state_train_residual_total[1])[[kj]][age,]
  
  # level 1 (disaggregate by sex)
  eh_mat[2,] = get(mfts_state_train_residual_female[1])[[kj]][age,]
  eh_mat[3,] = get(mfts_state_train_residual_male[1])[[kj]][age,]
  
  # level 2 & 3 (disaggregate by region & sex)
  for (ikw in 1:8)
  {
    eh_mat[(3+ikw),] = get(mfts_region_train_residual_total[ikw])[[kj]][age,]
    eh_mat[(11+ikw),] = get(mfts_region_train_residual_female[ikw])[[kj]][age,]
    eh_mat[(19+ikw),] = get(mfts_region_train_residual_male[ikw])[[kj]][age,]
  }
  
  # level 4 (disaggregate by state)
  for (ikw in 1:47)
  {
    eh_mat[(27+ikw),] = get(mfts_state_train_residual_total[(ikw+1)])[[kj]][age,]
  }
  
  # level 5 (disaggregate by state & sex)
  for (ikw in 1:47)
  {
    eh_mat[(74+(2*ikw-1)),] = get(mfts_state_train_residual_female[(ikw+1)])[[kj]][age,]
    eh_mat[(74+(2*ikw)),] = get(mfts_state_train_residual_male[(ikw+1)])[[kj]][age,]
  }
  
  target = lowerD(t(eh_mat))
  shrink = shrink.estim(t(eh_mat), target)
  wh_mat = shrink[[1]]
  
  return(wh_mat = wh_mat)
}







