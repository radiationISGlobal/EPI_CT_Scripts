

SES_Cat <- function (dfr){
  ##########################################################################################################
  ## This proc recodifies sesorig variable from IARC
  ## Some countries send SES information: 
  ##    Belgium: in categories, quantile of revenue based on postal code 1-the lowest, 5-the highest
  ##    France: calculated as Townsend index (greater value implies an increased deprivation), quartiles = (2.5, 6.06, 9.09, 12.49)
  ##    Netherlands: Michael sends these cut points (27/11/2017): ≤1,600 Euro, 1,601– 1,800 Euro, 1,801–2,100 Euro, 2,101–2,400 Euro, >2,400 Euro
  ##    Spain: quintiles from Deprivation variable, from 1 (less vulnerable) to 5 (more vulnerable); 
  ##
  ## while others don't: Denmark, Germany, Norway, Sweden and UK
  ##
  ## sesorig2 will have 5 categories: 1 being the least favored, 5 beeing the most favored.
  ##
  ## //// INPUT
  ##    - dfr: dataframe with original variable: sesorig
  ##
  ## //// OUTPUT
  ##    - same dataframe where we have added sesorig2, SES variable categorised to be used in analysis
  ##
  ##########################################################################################################
 
  
  # sesorig2 is the variable to be used in analysis
  # 999 --> missing
  dfr$sesorig2 <- 999
  
  # .......................................................................................
  # Belgium, already categorised variable, just copy and recodify missing value, 9 to 99 
  dfr$sesorig2[dfr$country == 11] <- dfr$sesorig[dfr$country == 11]
  dfr$sesorig2[dfr$country == 11 & dfr$sesorig2 == 9] <- 999
  # .......................................................................................
  
  # .......................................................................................
  # Denmark, SES missing for whole country
  dfr$sesorig2[dfr$country == 12] <- 999
  # .......................................................................................
  
  # .......................................................................................
  # France
  # SES data is calculated as Townsend index (greater value implies an increased deprivation)
  # quintiles given by Marie Odile (28/11/2017): Q1 = 2.50, Q2 = 6.06, Q3 = 9.09, Q4 = 12.49
  FrenchCuts <- c(-999, 2.5, 6.06, 9.09, 12.49, 999) 
  dfr$sesorig2[dfr$country == 13] <- cut(dfr$sesorig[dfr$country == 13], breaks = FrenchCuts, include.lowest = TRUE)
  dfr$sesorig2[dfr$country == 13 & is.na(dfr$sesorig2)] <- 999
  # .......................................................................................
  
  # .......................................................................................
  # Germany, SES missing for whole country
  dfr$sesorig2[dfr$country == 14] <- 999
  # .......................................................................................
  
  # .......................................................................................
  # Netherlands
  # Michael sends these cut points (27/11/2017): ≤1,600 Euro, 1,601– 1,800 Euro, 1,801–2,100 Euro, 2,101–2,400 Euro, >2,400 Euro
  # 99999 = unknwon
  NethCuts <- c(0, 1600, 1800, 2100, 2400, 20000, 100000) 
  dfr$sesorig[dfr$sesorig == 99999] <- NA
  dfr$sesorig2[dfr$country == 15] <- cut(dfr$sesorig[dfr$country == 15], breaks = NethCuts, include.lowest = TRUE)
  dfr$sesorig2[dfr$country == 15 & is.na(dfr$sesorig2)] <- 999
  # .......................................................................................
  
  # .......................................................................................
  # Norway, SES missing for whole country
  dfr$sesorig2[dfr$country == 16] <- 999
  # .......................................................................................
  
  # .......................................................................................
  # Spain, already categorised variable, just copy and recodify missing value, 9 to 99 
  dfr$sesorig2[dfr$country == 17] <- dfr$sesorig[dfr$country == 17]
  dfr$sesorig2[dfr$country == 17 & dfr$sesorig2 == 9] <- 999
  # .......................................................................................
  
  # .......................................................................................
  # Sweden, SES missing for whole country
  dfr$sesorig2[dfr$country == 18] <- 999
  # .......................................................................................
  
  # .......................................................................................
  # UK, SES missing for whole country
  dfr$sesorig2[dfr$country == 19] <- 999
  # .......................................................................................
  
  
  # .......................................................................................
  # recodify sesorig2 from France and Spain, where lower values indicate higher SES.
  # We want in sesorig2, 1 = Less SES and 5 = High SES
  # France
  dfr$sesorig2[dfr$country == 13 & dfr$sesorig2 == 1] <- 11 # temp change to be finished later, 1 will be 5
  dfr$sesorig2[dfr$country == 13 & dfr$sesorig2 == 2] <- 12 # temp change to be finished later, 2 will be 4

  dfr$sesorig2[dfr$country == 13 & dfr$sesorig2 == 4] <- 2
  dfr$sesorig2[dfr$country == 13 & dfr$sesorig2 == 5] <- 1

  dfr$sesorig2[dfr$country == 13 & dfr$sesorig2 == 11] <- 5
  dfr$sesorig2[dfr$country == 13 & dfr$sesorig2 == 12] <- 4
  
  # Spain
  dfr$sesorig2[dfr$country == 17 & dfr$sesorig2 == 1] <- 11 # temp change to be finished later, 1 will be 5
  dfr$sesorig2[dfr$country == 17 & dfr$sesorig2 == 2] <- 12 # temp change to be finished later, 2 will be 4
  
  dfr$sesorig2[dfr$country == 17 & dfr$sesorig2 == 4] <- 2
  dfr$sesorig2[dfr$country == 17 & dfr$sesorig2 == 5] <- 1
  
  dfr$sesorig2[dfr$country == 17 & dfr$sesorig2 == 11] <- 5
  dfr$sesorig2[dfr$country == 17 & dfr$sesorig2 == 12] <- 4
  # .......................................................................................
  
  dfr$sesorig2 <- as.factor(dfr$sesorig2)

  dfr
}