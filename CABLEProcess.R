CABLEProcess = function(Site){
  
  # A function to extract the necessary data from the CABLE model outputs 
  # supplied by MDK for 2 sites - HS and SP. This is designed to work with the 
  # model from Liu et al, 2019. 
  # To work correctly, the CABLE netcdfs for the site must be saved in a
  # subfolders named as cable/out and cable/met. 
  # 
  # The below variables are extracted and return in a nice, usable fashion:
  # 
  #   Net Ecosystem Exchange - CABLE model output
  #   Incoming Shortwave Radiation - CABLE input
  #   Air Temperature - CABLE input
  #   Vapour Pressure Deficit - CABLE input
  #   Top-layer Soil Moisture Content - CABLE model output
  #   Precipitation - CABLE model output
  # 
  # 
  # ############################################################################
  # Function inputs and outputs
  # ############################################################################
  #
  # INPUTS:
  #  - Site: A character vector with the FluxNet siteID
  #  
  #  OUTPUTS:
  #  -
  
  # ############################################################################
  # Load CABLE model outputs
  # ############################################################################
  
  # Source the ncdf package
  library(ncdf4)
  # Load in the CABLE output data we need:
  # Look in folder "cable/out" for the data
  File = list.files("cable/out",pattern = Site)
  # Read the data into R 
  NCDF = nc_open(paste0("cable/out/",File))
  # Change timestamps into dates
  TIMESTAMP = ncvar_get(NCDF,"time")
  # Convert time into datetimes
  # Find the origin of the times
  time_from = substr(ncatt_get(NCDF, "time")$units, 15, 33)
  # times are in seconds since origin
  TIMESTAMP = as.POSIXct(TIMESTAMP, origin=time_from, tz = "GMT")
  
  # Extract the variables we require - NEE and the top 3 layers of soil moisture
  # We will average the soil moisture to get "top-layer". 
  Data = data.frame(TIMESTAMP)
  Data["NEE"] = ncvar_get(NCDF,"NEE")
  Data["SoilMoist_1"] = ncvar_get(NCDF,Var)[,1]
  Data["SoilMoist_2"] = ncvar_get(NCDF,Var)[,2]
  Data["SoilMoist_3"] = ncvar_get(NCDF,Var)[,3]
  
  # Average the soil moisture as stated
  Data["SoilMoist"] = rowMeans(Data[,3:5])
  
  Data = Data[,-(3:5)]

  # Here we assume that CABLE outputs do not need quality checking and that only
  # full years are modelled (maybe presumptuous)
  
  # ############################################################################
  # Load CABLE model inputs
  # ############################################################################
  # Here we load the inputs used to obtain the above CABLE outputs
  # We find the met file in the subfolder
  File = list.files("cable/met",pattern = Site)
  # Read the data into R 
  NCDF = nc_open(paste0("cable/met/",File))
  # Double check that the two files were the same length
  if(nrow(TIMESTAMP) != nrow(ncvar_get(NCDF,"time"))){
    message("Error! CABLE input and output files are different lengths!")
  }

  
  # List the variables we want to extract
  # We also boldly assume that the CABLE inputs were quality checked before they
  # were used and therefore don't check again
  Variables = c("SWdown",
                "Tair",
                "VPD",
                "Precip")
  # Extract the variables we require
  for (Var in Variables){
    Data[Var] = ncvar_get(NCDF,Var)
  }
  
  # ############################################################################
  # Retime data to daily
  # ############################################################################
  # Source required packages
  library(lubridate)
  library(magrittr)
  library(tidyverse)
  
  # Create dataframe of daily values
  Data_day <- Data %>%
    mutate(TIMESTAMP=as.Date(TIMESTAMP, 
                             format="%Y-%m-%d %H:%M:%S", 
                             tz = ncatt_get(NCDF, 0)$time_zone)) %>%
    group_by(TIMESTAMP) %>%               # group by the day column
    summarise(NEE=mean(NEE),
              SWdown=mean(SWdown),
              Tair=mean(Tair),
              VPD=mean(VPD),
              SoilMoist=mean(SoilMoist),
              Precip=sum(Precip))
  
  # ############################################################################
  # Create inputs required for modelling
  # ############################################################################
  
  # First, create the fixed parameters:
  
  # Number of short-term climatic predictors
  # Tave, SW, VPD, SWCcurrent and antSWC
  Nv = 5
  # Days into past considered for short-term predictors
  Nlag = 14
  # Time lag for precipitation (number of different time periods)
  NlagP = 8
  # Total number of climate covariates (see paper for info)
  Ns = 22
  # Number of blocks for precipitation
  NblocksP = NlagP
  # Time blocks i.e. how the Nlag days are grouped together
  block = c(1:7, rep(8:9, each = 2), rep(10, 3))
  # The size of the block that each day is included in
  BlockSize = c(rep(1, 7), rep(2, 4),rep(3, 3))
  # Number of blocks
  Nblocks = max(block)
  
  # Calculate other parameters:
  
  # Number of days that antecedent conditions can be calculated for
  Nmem = nrow(Data_day)-365
  # Indices of days that can be modelled
  Mem_records = 366:nrow(Data_day)
  
  # Create the climate predictor matrix
  # See Model_Liu inputs for correct order
  # SWC is repeated to account for current and antecedent.
  clim = matrix(c(Data_day$Tair,
                  Data_day$SWdown,
                  Data_day$VPD,
                  Data_day$SoilMoist,
                  Data_day$SoilMoist),
                ncol = Nv)
  
  # Mean centre the climatic variables
  clim = scale(clim,scale=FALSE)
  
  # Create the NEE vector
  NEE = Data_day$NEE
  
  ## NDVI
  
  # Note that we do not have NDVI for the CABLE outputs - we do however have LAI
  # I will need to explore how to deal with this
  load("VegIndex_NDVI.Rdata")
  NDVI_df = VegIndex[VegIndex$site==Site,]
  NDVI_df = NDVI_df[as.Date(NDVI_df$date) %in% as.Date(Data_day$TIMESTAMP),c("date","ndvi_sg")]
  NDVI_df$date = as.Date(NDVI_df$date)
  # Check for missing data
  for (i in Data_day$TIMESTAMP[!(Data_day$TIMESTAMP %in% NDVI_df$date)]){
    NewDate = as.Date(i,origin="1970-01-01")
    NewNDVI = mean(c(NDVI_df$ndvi_sg[NDVI_df$date==(i-1)],NDVI_df$ndvi_sg[NDVI_df$date==(i+1)]))
    df = data.frame("date" = NewDate, "ndvi_sg" = NewNDVI)
    NDVI_df = rbind(NDVI_df,df) 
  }
  NDVI_df = NDVI_df[order(NDVI_df$date),]
  
  NDVI = NDVI_df$ndvi_sg
  
  ## PPT
  
  # Source the ppt processing function
  source("PPTProcess.R")
  # Extract the ppt matrix
  ppt_multiscale = PPTProcess(Data_day)
  # Mean centre ppt
  ppt_multiscale = scale(ppt_multiscale,scale=FALSE)
  # Note this gives precip values of less than 0 which is intriguing.
  
  
  # ############################################################################
  # Create output list
  # ############################################################################
  
  output = list("Nv"=Nv,
                "Ns"=Ns,
                "Nlag"=Nlag,
                "Nmem"=Nmem,
                "NlagP"=NlagP,
                "Mem_records"=Mem_records,
                "clim"=clim,
                "ppt_multiscale"=ppt_multiscale,
                "NEE"=NEE,
                "NDVI"=NDVI,
                "NblocksP" = NblocksP,
                "block" = block,
                "BlockSize" = BlockSize,
                "Nblocks" = Nblocks)
  name = paste0(Site,"_Input")
  assign(name,output)
  save(list=c(name),file=paste0(name,"_NDVI.Rdata"))
  
  
  ##### STILL TO BE DONE - BETTER QC
}