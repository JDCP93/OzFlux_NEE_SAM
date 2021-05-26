rm(list=ls())

Sites = c("AU-ASM","AU-Cpr","AU-Cum","AU-DaS","AU-Dry","AU-Gin","AU-GWW",
          "AU-How","AU-Stp","AU-TTE","AU-Tum","AU-Whr","AU-Wom")

df = data.frame("Site" = Sites,
                "VPD_Q0" = NA,
                "VPD_Q10" = NA,
                "VPD_Q25" = NA,
                "VPD_Q50" = NA,
                "VPD_Q75" = NA,
                "VPD_Q90" = NA,
                "VPD_Q95" = NA,
                "VPD_Q100" = NA,
                "VPD_mean" = NA,
                "Precip" = NA,
                "Ta_Q0" = NA,
                "Ta_Q10" = NA,
                "Ta_Q25" = NA,
                "Ta_Q50" = NA,
                "Ta_Q75" = NA,
                "Ta_Q90" = NA,
                "Ta_Q95" = NA,
                "Ta_Q100" = NA,
                "Ta_mean" = NA)


for (Site in Sites){
# Let the user know which site the function is looking at
message("*** Extracting data for ",Site," ***")

# ##################
# Extract raw data
# ##################
# Source the ncdf package
library(ncdf4)
# Load in the OzFlux data we need:
# Look in folder "Site_raw_data" for the data
File = list.files("Site_data",pattern = Site)
# Read the data into R 
NCDF = nc_open(paste0("Site_data/",File))
# Change timestamps into dates
TIMESTAMP = ncvar_get(NCDF,"time")
# Convert time into datetimes
# Find the origin of the times
time_from = substr(ncatt_get(NCDF, "time")$units, 12, 30)
# times are in days since origin
Days_to_Secs = 24*3600
TIMESTAMP = as.POSIXct(TIMESTAMP*Days_to_Secs, origin=time_from, tz = ncatt_get(NCDF, 0)$time_zone)

# List the variables we want to extract as well as their quality control
Variables = c("VPD","Precip","Ta")

# Extract the variables we require
Data = data.frame(TIMESTAMP)
for (Var in Variables){
  Data[Var] = ncvar_get(NCDF,Var)
  QC = paste0(Var,"_QCFlag")
  Data[QC] = ncvar_get(NCDF,QC)
}


df[df$Site==Site,2:10] = c(as.numeric(quantile(Data$VPD,na.rm=TRUE,probs = c(0,0.1,0.25,0.5,0.75,0.9,0.95,1))),mean(Data$VPD,na.rm=TRUE))
df$Precip[df$Site==Site] = sum(Data$Precip,na.rm=TRUE)/(max(year(Data$TIMESTAMP))-min(year(Data$TIMESTAMP)))
df[df$Site==Site,12:20] = c(as.numeric(quantile(Data$Ta,na.rm=TRUE,probs = c(0,0.1,0.25,0.5,0.75,0.9,0.95,1))),mean(Data$Ta,na.rm=TRUE))
}

VPDPlot = ggplot(df,aes(x=Site)) +
      geom_errorbar(aes(ymin=VPD_Q10,ymax=VPD_Q90)) +
      geom_crossbar(aes(ymin=VPD_Q25,y=VPD_Q50,ymax=VPD_Q75)) +
      geom_point(aes(y=VPD_Q95),color="black",size = 1) +
      geom_point(aes(y=VPD_Q100),colour="black",size = 2) +
      geom_point(aes(y=VPD_mean),colour="red",shape = 8,size = 2) +
  theme_bw() +
  ylab("VPD")

VPDPlot

TaPlot = ggplot(df,aes(x=Site)) +
  geom_errorbar(aes(ymin=Ta_Q10,ymax=Ta_Q90)) +
  geom_crossbar(aes(ymin=Ta_Q25,y=Ta_Q50,ymax=Ta_Q75)) +
  geom_point(aes(y=Ta_Q95),color="black",size = 1) +
  geom_point(aes(y=Ta_Q100),colour="black",size = 2) +
  geom_point(aes(y=Ta_Q0),colour="black",size = 2) +
  geom_point(aes(y=Ta_mean),colour="red",shape = 8,size = 2) +
  theme_bw() +
  ylab("Ta")

TaPlot
