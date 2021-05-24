
rm(list=ls())

Sites = c("AU-ASM","AU-Cpr","AU-Cum","AU-DaS","AU-Dry","AU-Gin","AU-GWW",
          "AU-How","AU-Stp","AU-TTE","AU-Tum","AU-Whr","AU-Wom")

Transects = c("NATT","SAWS","SAWS","NATT","NATT","SAWS","SAWS",
              "NATT","NATT","NATT","SAWS","SAWS","SAWS")

#*******************************************************************************
# NEE Model Performance
#*******************************************************************************

source("functions/NEE_R2BarPlot_function_RTPV.R")
Plot = NEE_R2BarPlot_RTPV(Sites,Transects,"AnnualPPT", Clusters = 0)
Plot

#*******************************************************************************
# LE Model Performance
#*******************************************************************************

source("functions/LE_R2BarPlot_function_RTPV.R")
# LESites = c("AU-ASM","AU-Cum","AU-DaS","AU-Dry","AU-Gin","AU-GWW",
#             "AU-How","AU-Stp","AU-TTE","AU-Tum","AU-Wom")
# 
# LETransects = c("NATT","SAWS","NATT","NATT","SAWS","AU-SAWS",
#                 "NATT","NATT","NATT","SAWS","SAWS")
Plot = LE_R2BarPlot_RTPV(Sites,Transects,"AnnualPPT", Clusters = 0)
Plot

#*******************************************************************************
# Flux Model Performance
#*******************************************************************************

source("functions/Flux_R2BarPlot_function_RTPV.R")
Plot = Flux_R2BarPlot_RTPV(Sites,Transects,"AnnualPPT", Clusters = 0)
Plot

#*******************************************************************************
# Grouping Model Performance
#*******************************************************************************
NATT = c("AU-ASM","AU-DaS","AU-Dry","AU-How","AU-Stp","AU-TTE")
SAWS = c("AU-Cpr","AU-Cum","AU-Gin","AU-GWW","AU-Tum","AU-Whr","AU-Wom")

Plot = NEE_R2BarPlot_RTPV(NATT,"NATT","PPTSeasonality", Clusters = 0)
Plot

Plot = NEE_R2BarPlot_RTPV(SAWS,"SAWS","AnnualMeanTemp", Clusters = 0)
Plot


LENATT = c("AU-ASM","AU-DaS","AU-Dry","AU-How","AU-Stp","AU-TTE")
LESAWS = c("AU-Cpr","AU-Cum","AU-Gin","AU-GWW","AU-Tum","AU-Whr","AU-Wom")

Plot = LE_R2BarPlot_RTPV(LENATT,"NATT","AnnualPPT", Clusters = 0)
Plot

Plot = LE_R2BarPlot_RTPV(LESAWS,"SAWS","PPTDryQtr", Clusters = 0)
Plot

#*******************************************************************************
# Climate Sensitivity
#*******************************************************************************

source("functions/NEE_SDScaledSensitivity_RTPV.R")
Plot = NEE_SDScaledSensitivity_RTPV(Sites,Vars=c("Tair","Fsd","VPD","PPTshort","PPTlong"))
Plot

LESites = c("AU-ASM","AU-Cpr","AU-Cum","AU-DaS","AU-Dry","AU-Gin","AU-GWW",
            "AU-How","AU-Stp","AU-TTE","AU-Tum","AU-Whr","AU-Wom")

source("functions/LE_SDScaledSensitivity_RTPV.R")
Plot = LE_SDScaledSensitivity_RTPV(Sites,Vars=c("Tair","Fsd","VPD","PPTshort","PPTlong"))
Plot

#*******************************************************************************
# NEE stacked weights with different colours per transect
#*******************************************************************************

source("functions/NEE_StackedWeightPlot_Transects_RTPV.R")
TairSites = c("AU-Cpr","AU-Cum","AU-DaS","AU-Gin","AU-GWW",
          "AU-How","AU-Stp","AU-Tum","AU-Whr","AU-Wom")

TairTransects = c("SAWS","SAWS","NATT","SAWS","SAWS",
              "NATT","NATT","SAWS","SAWS","SAWS")

Plot = NEE_StackedWeightPlot_RTPV(TairSites,TairTransects,"Tair","AnnualPPT")
Plot

Plot = NEE_StackedWeightPlot_RTPV(Sites,Transects,"PPTlong","AnnualPPT")
Plot

#*******************************************************************************
# LE stacked weights with different colours per transect
#*******************************************************************************

source("functions/LE_StackedWeightPlot_Transects_RTPV.R")
TairSites = c("AU-ASM","AU-Cpr","AU-Cum","AU-DaS","AU-Gin","AU-GWW",
              "AU-How","AU-Stp","AU-TTE","AU-Tum","AU-Whr","AU-Wom")

TairTransects = c("NATT","SAWS","SAWS","NATT","SAWS","SAWS",
                  "NATT","NATT","NATT","SAWS","SAWS","SAWS")

Plot = LE_StackedWeightPlot_RTPV(TairSites,TairTransects,"Tair","AnnualPPT")
Plot

LESites = c("AU-ASM","AU-Cpr","AU-Cum","AU-DaS","AU-Dry","AU-Gin","AU-GWW",
            "AU-How","AU-Stp","AU-TTE","AU-Tum","AU-Whr","AU-Wom")

LETransects = c("NATT","SAWS","SAWS","NATT","NATT","SAWS","SAWS",
                "NATT","NATT","NATT","SAWS","SAWS","SAWS")

Plot = LE_StackedWeightPlot_RTPV(LESites,LETransects,"Fsd","AnnualPPT")
Plot

#*******************************************************************************
# Metric Improvements - SUPPLEMENTARY
#*******************************************************************************

source("functions/NEE_MetricsPlot_function_RTPV.R")
Plot = NEE_MetricsPlot_function_RTPV(Sites)
Plot

source("functions/LE_MetricsPlot_function_RTPV.R")
Plot = LE_MetricsPlot_function_RTPV(Sites)
Plot

#*******************************************************************************
# Model Performance Time Series - SUPPLEMENTARY
#*******************************************************************************

source("functions/NEE_DailyObsVsPred_MA_RTPV.R")
for (Site in Sites){
  Plots = NEE_DailyObsVsPred_MA_RTPV(Site)
  plot(Plots$StackedPlot)
}

#*******************************************************************************
# Individual Weight Plots - SUPPLEMENTARY
#*******************************************************************************

source("functions/NEE_WeightPlot_RTPV.R")
Plot = NEE_WeightPlot_RTPV(NATT)
Plot

Plot = NEE_WeightPlot_RTPV(SAWS)
Plot

source("functions/LE_WeightPlot_RTPV.R")
Plot = LE_WeightPlot_RTPV(LENATT)
Plot

LESAWS = c("AU-Cpr","AU-Cum","AU-Gin","AU-GWW","AU-Tum","AU-Whr","AU-Wom")
Plot = LE_WeightPlot_RTPV(LESAWS)
Plot

