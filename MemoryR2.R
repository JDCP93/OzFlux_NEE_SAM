MemoryR2 <- function(Site,SiteCode){
  
# This is a function which analyses the MCMC outputs from the 3 different models 
# used to calculate current environmental effects, environmental memory and 
# biological memory. 
# The output files are loaded and the R2 is calculated for each model. The 
# cumulative weights for the SAM model are also extracted and plotted.
# 
# INPUTS:
#   Site - The site's full name with no spaces (as used for the input file)
#   SiteCode - The 2 letter code I used for the site's model outputs
# 
# OUTPUTS: The below are presented as a list
#   CUR.R2 - The R2 value of the model using only current climate conditions
#   SAM.R2 - The R2 value of the SAM model 
#   AR1.R2 - The R2 value when the SAM residuals are remodelled using an AR1 
#            process
#   5 Plots - Five plots called "SWRplot", "Tairplot", "VPDplot", "SWCplot" and  
#             "PPTplot" which are plots of the cumulative weights calculated in
#             the SAM model for each climatic driver
# 

# ******************************************************************************
# Load model outputs  
# ******************************************************************************

# load model input for Site
name = paste0(Site,"_Input")
load(paste0(Site,'_Input_NDVI.Rdata'))
assign("Input",eval(as.name(name)))
  
# Find the output files available
File = list.files("results",pattern = paste0("NEE_output_site_",SiteCode))
# Read the data into R 
load(paste0("results/",File))
# data inside is called "nee_daily"
assign('SAM',nee_daily)
rm(nee_daily)

# Find the output files available
File = list.files("results",pattern = paste0("NEE_current_output_site_",SiteCode))
# Read the data into R 
load(paste0("results/",File))
# data inside is called "nee_daily"
assign('CUR',nee_daily)
rm(nee_daily)

# Find the output files available
File = list.files("results",pattern = paste0("NEE_AR1_output_site_",SiteCode))
# Read the data into R 
load(paste0("results/",File))
# data inside is called "nee_daily"
assign('AR1',AR1.res)
rm(AR1.res)

# ******************************************************************************
# Extract and print the R2 values
# ******************************************************************************

# load necessary packages
library(coda)
library(lattice)
library(mcmcplots)
library(superdiag)

# Extract the NEE values
NEE_SAM_pred = summary(SAM)$statistics[substr(rownames(summary(SAM)$statistics),1,3)=="NEE",1]
NEE_CUR_pred = summary(CUR)$statistics[substr(rownames(summary(CUR)$statistics),1,3)=="NEE",1]
NEE_obs = Input$NEE[-(1:365)]

# Calculate R2
SAM.R2 = summary(lm(NEE_SAM_pred ~ NEE_obs))$r.squared
message(Site," has R2 = ",round(SAM.R2,3)," for SAM")

# Calculate AR(1) process
Nmem = Input$Nmem
NEE_AR1_pred = summary(window(AR1[,paste("NEE.res_rep[", 2:Nmem,"]", sep = '')]))$statistics[,1]

fit = lm((NEE_SAM_pred[2:Nmem] - NEE_AR1_pred) ~ NEE_obs[2:Nmem])
AR1.R2 = summary(fit)$r.squared

message(Site," has R2 = ",round(AR1.R2,3)," for AR(1)")
message(Site," has bio memory R2 improvement ",round(AR1.R2-SAM.R2,3))


# Calculate current climate impact only
fit = lm(NEE_CUR_pred ~ NEE_obs)
CUR.R2 = summary(fit)$r.squared

message(Site," has R2 = ",round(CUR.R2,3)," for current climate only")
message(Site," has enviro memory R2 improvement ",round(SAM.R2-CUR.R2,3))


# ******************************************************************************
# Plot the cumulative weights
# ******************************************************************************

# Find the summary of SAM run
SAM.summary = summary(SAM)

# Check cumulative weights
cumSWR_mean = SAM.summary$statistics[substr(rownames(SAM.summary$statistics),1,13)=="cum_weightA[2",1]
cumTair_mean = SAM.summary$statistics[substr(rownames(SAM.summary$statistics),1,13)=="cum_weightA[1",1]
cumVPD_mean = SAM.summary$statistics[substr(rownames(SAM.summary$statistics),1,13)=="cum_weightA[3",1]
cumSWC_mean = SAM.summary$statistics[substr(rownames(SAM.summary$statistics),1,13)=="cum_weightA[5",1]
cumPPT_mean = SAM.summary$statistics[substr(rownames(SAM.summary$statistics),1,12)=="cum_weightAP",1]

cumSWR_min = SAM.summary$quantiles[substr(rownames(SAM.summary$quantiles),1,13)=="cum_weightA[2",1]
cumTair_min = SAM.summary$quantiles[substr(rownames(SAM.summary$quantiles),1,13)=="cum_weightA[1",1]
cumVPD_min = SAM.summary$quantiles[substr(rownames(SAM.summary$quantiles),1,13)=="cum_weightA[3",1]
cumSWC_min = SAM.summary$quantiles[substr(rownames(SAM.summary$quantiles),1,13)=="cum_weightA[5",1]
cumPPT_min = SAM.summary$quantiles[substr(rownames(SAM.summary$quantiles),1,12)=="cum_weightAP",1]

cumSWR_max = SAM.summary$quantiles[substr(rownames(SAM.summary$quantiles),1,13)=="cum_weightA[2",5]
cumTair_max = SAM.summary$quantiles[substr(rownames(SAM.summary$quantiles),1,13)=="cum_weightA[1",5]
cumVPD_max = SAM.summary$quantiles[substr(rownames(SAM.summary$quantiles),1,13)=="cum_weightA[3",5]
cumSWC_max = SAM.summary$quantiles[substr(rownames(SAM.summary$quantiles),1,13)=="cum_weightA[5",5]
cumPPT_max = SAM.summary$quantiles[substr(rownames(SAM.summary$quantiles),1,12)=="cum_weightAP",5]

cumSWR = data.frame("mean" = cumSWR_mean,"min" = cumSWR_min, "max" = cumSWR_max)
cumTair = data.frame("mean" = cumTair_mean,"min" = cumTair_min, "max" = cumTair_max)
cumVPD = data.frame("mean" = cumVPD_mean,"min" = cumVPD_min, "max" = cumVPD_max)
cumSWC = data.frame("mean" = cumSWC_mean,"min" = cumSWC_min, "max" = cumSWC_max)
cumPPT = data.frame("mean" = cumPPT_mean,"min" = cumPPT_min, "max" = cumPPT_max)

library(ggplot2)
SWRplot = ggplot(cumSWR) +
  geom_path(aes(0:13,y=mean),size=1) +
  geom_linerange(aes(x=0:13,ymin=min,ymax=max),size=1) +
  geom_hline(yintercept=0.5,linetype = "dashed") +
  coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
  xlab("Days into Past") +
  ylab("Cumulative Weight") +
  theme_bw() +
  guides(color = "none") +
  ggtitle("SWR")

Tairplot = ggplot(cumTair) +
  geom_path(aes(0:13,mean),size=1) +
  geom_linerange(aes(x=0:13,ymin=min,ymax=max),size=1) +
  geom_hline(yintercept=0.5,linetype = "dashed") +
  coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
  xlab("Days into Past") +
  ylab("Cumulative Weight") +
  theme_bw() +
  guides(color = "none") +
  ggtitle("Tair")

VPDplot = ggplot(cumVPD) +
  geom_path(aes(0:13,mean),size=1) +
  geom_linerange(aes(x=0:13,ymin=min,ymax=max),size=1) +
  geom_hline(yintercept=0.5,linetype = "dashed") +
  coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
  xlab("Days into Past") +
  ylab("Cumulative Weight") +
  theme_bw() +
  guides(color = "none") +
  ggtitle("VPD")

SWCplot = ggplot(cumSWC) +
  geom_path(aes(0:13,mean),size=1) +
  geom_linerange(aes(x=0:13,ymin=min,ymax=max),size=1) +
  geom_hline(yintercept=0.5,linetype = "dashed") +
  coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
  xlab("Days into Past") +
  ylab("Cumulative Weight") +
  theme_bw() + 
  guides(color = "none") +
  ggtitle("SWC")

PPTplot = ggplot(cumPPT) +
  geom_path(aes(c(0,20,29,59,119,179,269,365),mean),size=1) +
  geom_linerange(aes(x=c(0,20,29,59,119,179,269,365),ymin=min,ymax=max),size=1) +
  geom_hline(yintercept=0.5,linetype = "dashed") +
  coord_cartesian(xlim = c(0, 365), ylim = c(0,1)) +
  xlab("Days into Past") +
  ylab("Cumulative Weight") +
  theme_bw() +
  guides(color = "none") +
  ggtitle("PPT")


# ******************************************************************************
# Calculate Site Metrics
# ******************************************************************************
# Source the ncdf package
library(ncdf4)
# Load in the OzFlux data we need:
# Look in folder "Site_raw_data" for the data
File = list.files("Site_raw_data",pattern = Site)
# Read the data into R 
NCDF = nc_open(paste0("Site_raw_data/",File))

# Change timestamps into dates
TIMESTAMP = ncvar_get(NCDF,"time")
# Convert time into datetimes
# Find the origin of the times
time_from = substr(ncatt_get(NCDF, "time")$units, 12, 30)
# times are in days since origin
Days_to_Secs = 24*3600
TIMESTAMP = as.POSIXct(TIMESTAMP*Days_to_Secs, origin=time_from, tz = ncatt_get(NCDF, 0)$time_zone)

# List the variables we want to extract as well as their quality control
Variables = c("NEE_LL",
              "Fsd",
              "Ta",
              "VPD",
              "Sws",
              "Precip")
# Extract the variables we require
Data = data.frame(TIMESTAMP)
for (Var in Variables){
  Data[Var] = ncvar_get(NCDF,Var)
  QC = paste0(Var,"_QCFlag")
  Data[QC] = ncvar_get(NCDF,QC)
}

# Check the quality of the data:
# Since all data is daily, _QC variables are percentage of measured/good 
# quality gapfill data, ranging from 0-1 

# Identify QC columns
QCcols = grep("QC",colnames(Data))
# Remove first row if any data is poor - repeat as necessary
count = 0
while(any(Data[1,QCcols]%%10!=0)){
  Data = Data[-1,]
  count = count + 1
}
# Remove last row if any data is poor - repeat as necessary
count = 0
while(any(Data[nrow(Data),QCcols]%%10!=0)){
  Data = Data[-nrow(Data),]
  count = count + 1
}

# Source required packages
library(lubridate)
library(magrittr)
library(tidyverse)
library(dplyr)
library(zoo)

# Create dataframe of daily and yearly values
Data_day <- Data %>%
  mutate(TIMESTAMP=as.Date(TIMESTAMP, 
                           format="%Y-%m-%d %H:%M:%S", 
                           tz = ncatt_get(NCDF, 0)$time_zone)) %>%
  group_by(TIMESTAMP) %>%               # group by the day column
  summarise(NEE_LL=mean(NEE_LL),
            Fsd=mean(Fsd),
            Ta=mean(Ta),
            VPD=mean(VPD),
            Sws=mean(Sws),
            Precip=sum(Precip))


Data_day$year = year(Data_day$TIMESTAMP)
Data_day$month = month(Data_day$TIMESTAMP)

Data_year <- Data_day %>%
  group_by(year) %>%               # group by the year column
  summarise(NEE_LL=mean(NEE_LL),
            Fsd=mean(Fsd),
            Ta=mean(Ta),
            VPD=mean(VPD),
            Sws=mean(Sws),
            Precip=sum(Precip))

Data_year = Data_year[Data_year$Precip>0,]

MAP = mean(Data_year$Precip)
MAT = mean(Data_year$Ta)
LAT = as.numeric(ncatt_get(NCDF, 0)$latitude)

# ******************************************************************************
# Output the important stuff
# ******************************************************************************

Output = list("CUR.R2" = CUR.R2,
              "SAM.R2" = SAM.R2,
              "AR1.R2" = AR1.R2,
              "SWRplot" = SWRplot,
              "Tairplot" = Tairplot,
              "VPDplot" = VPDplot,
              "SWCplot" = SWCplot,
              "PPTplot" = PPTplot,
              "MAP" = MAP,
              "MAT" = MAT,
              "LAT" = LAT)

}


