
# Make sure everything is clean
rm(list=ls())

# Load the required packages
library(tidyverse)
library(lubridate)

# List all sites
Sites = c("AU-ASM"
          ,"AU-Cpr"
          ,"AU-Cum"
          ,"AU-DaS"
          ,"AU-Dry"
          ,"AU-Gin"
          ,"AU-GWW"
          ,"AU-How"
          ,"AU-Stp"
          ,"AU-TTE"
          ,"AU-Tum"
          ,"AU-Whr"
          ,"AU-Wom"
)

# Initiliase metrics df
metrics = data.frame("Site" = Sites,
                     "MDP" = 0,
                     "MMP" = 0,
                     "MYP" = 0,
                     "MDT" = 0,
                     "MMT" = 0,
                     "MYT" = 0,
                     "CVDN" = 0,
                     "CVMN" = 0,
                     "CVYN" = 0,
                     "CVDR" = 0,
                     "CVMR" = 0,
                     "CVYR" = 0,
                     "CVDT" = 0,
                     "CVMT" = 0,
                     "CVYT" = 0,
                     "CVDP" = 0,
                     "CVMP" = 0,
                     "CVYP" = 0,
                     "CVDV" = 0,
                     "CVMV" = 0,
                     "CVYV" = 0,
                     "CVDS" = 0,
                     "CVMS" = 0,
                     "CVYS" = 0)

R2 = data.frame("Site" = Sites,
                "R2.CUR" = 0,
                "R2.SAM" = 0,
                "R2.AR1" = 0,
                "R2.KMP" = 0,
                "R2.KMC" = 0)

# For each site
for (Site in Sites){
  # Let the user know which site the function is looking at
  message("*** Analysing climate metrics for ",Site," ***")
  # Load the input and extract each climate variable
  load(paste0("inputs/RTPVS/",Site,"_Input_RTPVS.Rdata"))

  DailyData = eval(as.name(paste0(Site,"_Input")))$DailyData
  # Retime to monthly and yearly data
  MonthlyData = DailyData %>%
                group_by(year(TIMESTAMP),month(TIMESTAMP)) %>%               
                summarise(NEE = sum(NEE,na.rm=TRUE),
                          Fsd=mean(Fsd,na.rm=TRUE),
                          Ta=mean(Ta,na.rm=TRUE),
                          VPD=mean(VPD,na.rm=TRUE),
                          Sws=mean(Sws,na.rm=TRUE),
                          Precip=sum(Precip,na.rm=TRUE))
  
  YearlyData = DailyData %>%
    group_by(year(TIMESTAMP)) %>%               
    summarise(NEE = sum(NEE,na.rm=TRUE),
              Fsd=mean(Fsd,na.rm=TRUE),
              Ta=mean(Ta,na.rm=TRUE),
              VPD=mean(VPD,na.rm=TRUE),
              Sws=mean(Sws,na.rm=TRUE),
              Precip=sum(Precip,na.rm=TRUE))
  
  
  # Find the coefficient of variation for various values
  metrics$MDP[metrics$Site==Site] = mean(DailyData$Precip)
  metrics$MMP[metrics$Site==Site] = mean(MonthlyData$Precip)
  metrics$MYP[metrics$Site==Site] = mean(YearlyData$Precip)
  metrics$MDT[metrics$Site==Site] = mean(DailyData$Ta)
  metrics$MMT[metrics$Site==Site] = mean(MonthlyData$Ta)
  metrics$MYT[metrics$Site==Site] = mean(YearlyData$Ta)
  metrics$CVDP[metrics$Site==Site] = sd(DailyData$Precip,na.rm=TRUE)/mean(DailyData$Precip,na.rm=TRUE)
  metrics$CVMP[metrics$Site==Site] = sd(MonthlyData$Precip)/mean(MonthlyData$Precip)
  metrics$CVYP[metrics$Site==Site] = sd(YearlyData$Precip)/mean(YearlyData$Precip)
  metrics$CVDT[metrics$Site==Site] = sd(DailyData$Ta,na.rm=TRUE)/mean(DailyData$Ta,na.rm=TRUE)
  metrics$CVMT[metrics$Site==Site] = sd(MonthlyData$Ta)/mean(MonthlyData$Ta)
  metrics$CVYT[metrics$Site==Site] = sd(YearlyData$Ta)/mean(YearlyData$Ta)
  metrics$CVDN[metrics$Site==Site] = sd(DailyData$NEE,na.rm=TRUE)/abs(mean(DailyData$NEE,na.rm=TRUE))
  metrics$CVMN[metrics$Site==Site] = sd(MonthlyData$NEE)/abs(mean(MonthlyData$NEE))
  metrics$CVYN[metrics$Site==Site] = sd(YearlyData$NEE)/abs(mean(YearlyData$NEE))
  metrics$CVDV[metrics$Site==Site] = sd(DailyData$VPD,na.rm=TRUE)/mean(DailyData$VPD,na.rm=TRUE)
  metrics$CVMV[metrics$Site==Site] = sd(MonthlyData$VPD)/mean(MonthlyData$VPD)
  metrics$CVYV[metrics$Site==Site] = sd(YearlyData$VPD)/mean(YearlyData$VPD)
  metrics$CVDR[metrics$Site==Site] = sd(DailyData$Fsd,na.rm=TRUE)/mean(DailyData$Fsd,na.rm=TRUE)
  metrics$CVMR[metrics$Site==Site] = sd(MonthlyData$Fsd)/mean(MonthlyData$Fsd)
  metrics$CVYR[metrics$Site==Site] = sd(YearlyData$Fsd)/mean(YearlyData$Fsd)
  metrics$CVDS[metrics$Site==Site] = sd(DailyData$Sws,na.rm=TRUE)/mean(DailyData$Sws,na.rm=TRUE)
  metrics$CVMS[metrics$Site==Site] = sd(MonthlyData$Sws)/mean(MonthlyData$Sws)
  metrics$CVYS[metrics$Site==Site] = sd(YearlyData$Sws)/mean(YearlyData$Sws)
  
  # Clean up
  rm(list=c("YearlyData","MonthlyData","DailyData"))
  
  # Collect the R2 values from the analysis scripts
  message("Collating R2 values for ",Site)
  
  # Load the analysis results
  load(paste0("analysis/RTPVS/NEE_Analysis_RTPVS_",Site,".Rdata"))
  R2$R2.SAM[R2$Site==Site] = output$SAM.R2
  load(paste0("analysis/RTPVS/NEE_current_Analysis_RTPVS_",Site,".Rdata"))
  R2$R2.CUR[R2$Site==Site] = output$CUR.R2
  load(paste0("analysis/RTPVS/NEE_analysis_AR1_RTPVS_",Site,".Rdata"))
  R2$R2.AR1[R2$Site==Site] = output$AR1.R2
  load(paste0("alternate/RTPVS/results/NEE_output_kmean_RTPVS_",Site,".Rdata"))
  R2$R2.KMP[R2$Site==Site] = output$r.squared
#  load(paste0("alternate/RTPVS/results/NEE_output_kmean_current_RTPVS_",Site,".Rdata"))
#  R2$R2.KMC[R2$Site==Site] = output$r.squared
}


for (i in 2:ncol(metrics)){
  metricR2 = cor.test(metrics[,i], (R2$R2.SAM-R2$R2.CUR))$estimate
  metricPvalue = cor.test(metrics[,i], (R2$R2.SAM-R2$R2.CUR))$p.value
  message("Memory strength and ", colnames(metrics)[i], " are correlated with R2 value ", round(metricR2,3), " and p value ", round(metricPvalue,3))
}
