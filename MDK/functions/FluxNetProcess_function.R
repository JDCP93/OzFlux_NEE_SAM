FluxNetProcess = function(Site){

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
  #  - "AU-Site_Input.Rdata": a .Rdata file containing a list of the same name. 
  #                           The list includes all necessary inputs for the 
  #                           NEEModel.R workflows as well as a dataframe of
  #                           daily data and a list of QC metrics

  # Let the user know which site the function is looking at
  message("*** Extracting data for ",Site," ***")
  
  # ##################
  # Extract raw data
  # ##################
  # Load in the Flux data we need:
  File = list.files("FluxNet/",pattern = paste0(Site,"_FLUXNET"))
  # Read the data into R 
  CSV = read.csv(paste0("FluxNet/",File[1]))
  
  # Extract the required variables
  Data = CSV[,c("TIMESTAMP","NEE_VUT_REF","TA_F","SW_IN_F","VPD_F","P_F")]
  
  # We assume all data is good-quality since Anna Ukkola gapfilled this
  
  # #####################
  # Retime data to daily
  # #####################
  Data$TIMESTAMP = as.POSIXct(strptime(Data$TIMESTAMP,format="%Y%m%d"))
  # ####################
  # Replace any bad data with NaNs
  # ####################
  Data[Data==-9999] = NaN
  
  ## NDVI
  # NDVI was extracted from Google Earth Engine on 17/09/20 at 12:00 
  # Product was MCD43A3.006 MODIS Albedo Daily 500m
  load("input/VegIndex.Rdata")
  NDVI_df = VegIndex[VegIndex$site==Site,]
  NDVI_df = NDVI_df[as.Date(NDVI_df$date) %in% as.Date(Data$TIMESTAMP),c("date","ndvi_sg")]
  NDVI_df$date = as.Date(NDVI_df$date)
  NDVI_df = NDVI_df[order(NDVI_df$date),]
  
  Data = full_join(Data,NDVI_df,by=c("TIMESTAMP"="date"))
  
  Data = Data[Data$TIMESTAMP>"1999-12-31",]
  # ####################
  # Create inputs required for modelling
  # ####################
  # Create the climate predictor matrix
  # SWC is repeated to account for current and antecedent.
  clim = matrix(c(Data$TA_F,
                  Data$SW_IN_F,
                  Data$VPD_F,
                  Data$P_F),
                ncol = 4)
  # Fill any climate NAs by interpolating over the gap
  clim = na.approx(clim)
  NDVI = na.approx(Data$ndvi_sg,rule=2)
  # Create the NEE vector
  NEE = Data$NEE
  ## PPT lags
  
  # Initialise the precipitation dataframe
  ppt = data.frame()
  # for each day in the main data series, calculate the antecedent rainfall
  for (i in 366:nrow(Data)){
    ppt[i,1] = sum(Data$P_F[(i-1):(i-13)],na.rm=TRUE)
    ppt[i,2] = sum(Data$P_F[(i-14):(i-20)],na.rm=TRUE)
    ppt[i,3] = sum(Data$P_F[(i-21):(i-29)],na.rm=TRUE)
    ppt[i,4] = sum(Data$P_F[(i-30):(i-59)],na.rm=TRUE)
    ppt[i,5] = sum(Data$P_F[(i-60):(i-119)],na.rm=TRUE)
    ppt[i,6] = sum(Data$P_F[(i-120):(i-179)],na.rm=TRUE)
    ppt[i,7] = sum(Data$P_F[(i-180):(i-269)],na.rm=TRUE)
    ppt[i,8] = sum(Data$P_F[(i-270):(i-365)],na.rm=TRUE)
    ppt[i,9] = sum(Data$P_F[(i-1):(i-90)],na.rm=TRUE)
    ppt[i,10] = sum(Data$P_F[(i-91):(i-180)],na.rm=TRUE)
    ppt[i,11] = sum(Data$P_F[(i-181):(i-365)],na.rm=TRUE)
    if (i>730){
      ppt[i,12] = sum(Data$P_F[(i-366):(i-730)],na.rm=TRUE)
    }
    if(i>1095){
      ppt[i,13] = sum(Data$P_F[(i-731):(i-1095)],na.rm=TRUE)
    }
    if(i>1460){
      ppt[i,14] = sum(Data$P_F[(i-1096):(i-1460)],na.rm=TRUE)
    }
  }
  
  # ###############
  # Create output list
  # ###############
  
  output = list("Time"=Data$TIMESTAMP,
                "clim"=clim,
                "ppt"=ppt,
                "NEE"=NEE,
                "NDVI"=NDVI)
  name = paste0(Site,"_Input")
  assign(name,output)
  save(list=c(name),file=paste0("input/",name,".Rdata"))
  
}
