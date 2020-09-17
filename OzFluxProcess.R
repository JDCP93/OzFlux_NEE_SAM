OzFluxProcess = function(Site){

  # A function to extract the necessary data from the daily OzFlux netcdf for a 
  # given site. This is designed to work with the model from Liu et al, 2019. 
  # To work correctly, the OzFlux netcdf for the site must be saved in a
  # subfolder named as Site_raw_data. 
  # 
  # The below variables are extracted, quality-checked and return in a nice, 
  # usable fashion:
  # 
  #   NEE_LL          Net Ecosystem Exchange
  #   SW_IN_F         Incoming Shortwave Radiation
  #   TA_F            Air Temperature
  #   VPD_F           Vapour Pressure Deficit
  #   SWC_F_MDS_1     Top-layer Soil Moisture Content
  #   P_F             Precipitation
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

  
  # ##################
  # Extract raw data
  # ##################
  # Source the ncdf package
  library(ncdf4)
  # Load in the OzFlux data we need:
  # Look in folder "Site_raw_data" for the data
  File = list.files("Site_data_v2",pattern = Site)
  # Read the data into R 
  NCDF = nc_open(paste0("Site_data_v2/",File))
  # Change timestamps into dates
  TIMESTAMP = ncvar_get(NCDF,"time")
  # Convert time into datetimes
  # Find the origin of the times
  time_from = substr(ncatt_get(NCDF, "time")$units, 12, 30)
  # times are in days since origin
  Days_to_Secs = 24*3600
  TIMESTAMP = as.POSIXct(TIMESTAMP*Days_to_Secs, origin=time_from, tz = ncatt_get(NCDF, 0)$time_zone)

  # List the variables we want to extract as well as their quality control
  # Note some sites are using L5 PI data as recommended by site PIs - as such
  # we must check and take different variables for this data
  if (substr(File,8,9)=="L5"){
  Variables = c("NEE",
                "Fsd",
                "Ta",
                "VPD",
                "Sws",
                "Precip")
  } else {
    Variables = c("Fc",
                  "Fsd",
                  "Ta",
                  "VPD",
                  "Sws",
                  "Precip")
  }
  # Extract the variables we require
  Data = data.frame(TIMESTAMP)
  for (Var in Variables){
    Data[Var] = ncvar_get(NCDF,Var)
    QC = paste0(Var,"_QCFlag")
    Data[QC] = ncvar_get(NCDF,QC)
  }
  
  # Give common names to NEE/Fc columns
  colnames(Data)[2] = "NEE"
  colnames(Data)[3] = "NEE_QCFlag"
  
  # ##################
  # Quality check data
  # ##################
  
  # Identify QC columns
  QCcols = grep("QC",colnames(Data))
  # Remove first row if any data is poor - repeat as necessary
  count = 0
  while(any(Data[1,QCcols]!=0)){
    Data = Data[-1,]
    count = count + 1
  }
  if (count > 0){
  message("Info! ",count," rows removed from start of record due to poor data.")
  }
  # Remove last row if any data is poor - repeat as necessary
  count = 0
  while(any(Data[nrow(Data),QCcols]!=0)){
    Data = Data[-nrow(Data),]
    count = count + 1
  }
  if (count > 0){
  message("Info! ",count," rows removed from end of record due to poor data.")
  }
  
  
  # Perform checks on the amount of poor data remaining:
  
  # Arbitarily decide that less than 95% measured/good data is 
  # worrying
  # Count columns that have one or more bad QC flag
  QC = sum(apply(Data[,QCcols],MARGIN=1,function(x) any(x != 0)))
  # Calculate percentage of data that is poor
  PercentQC = QC*100/nrow(Data)
  # If more than 5% of the data is poor, print a warning
  if (PercentQC > 5){
    message("Warning! ",
                 round(PercentQC,digits=3),
                 "% of data is poor or gap-filled!")
  }
  # Check for excessive consecutive streaks of poor data
  # Find the sequences of poor/good data
  Seq = rle(apply(Data[,QCcols],MARGIN=1,function(x) any(x != 0)))
  # Find the lengths of these sequences for the poor data
  Lengths = Seq$lengths[Seq$values==TRUE]
  # If a run of 5 or more days of poor data exists, print a warning
  if (length(Lengths)>0){
  if (max(Lengths)>=5){
    message("Warning! There is a run of ",
                 max(Lengths),
                 " consecutive half-hours with poor data!")
  }
  }
  
  # #####################
  # Retime data to daily
  # #####################
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
              Fsd=mean(Fsd),
              Ta=mean(Ta),
              VPD=mean(VPD),
              Sws=mean(Sws),
              Precip=sum(Precip))
  
  # ####################
  # Create inputs required for modelling
  # ####################
  
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
  clim = matrix(c(Data_day$Ta,
                  Data_day$Fsd,
                  Data_day$VPD,
                  Data_day$Sws,
                  Data_day$Sws),
                ncol = Nv)
  
  # Mean centre the climatic variables
  clim = scale(clim,scale=FALSE)
  
  # Create the NEE vector
  NEE = Data_day$NEE
  
  ## NDVI
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
  
  
  # ###############
  # Create output list
  # ###############
  
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
  save(list=c(name),file=paste0(name,".Rdata"))
  
  
##### STILL TO BE DONE - BETTER QC
}