
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
LESites = c("AU-Cum","AU-DaS","AU-Dry","AU-Gin","AU-GWW",
            "AU-How","AU-Stp","AU-TTE","AU-Tum","AU-Wom")

LETransects = c("SAWS","NATT","NATT","SAWS","AU-SAWS",
                "NATT","NATT","NATT","SAWS","SAWS")
Plot = LE_R2BarPlot_RTPV(LESites,LETransects,"AnnualPPT", Clusters = 0)
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


LENATT = c("AU-DaS","AU-Dry","AU-How","AU-Stp","AU-TTE")
LESAWS = c("AU-Cum","AU-Gin","AU-GWW","AU-Tum","AU-Wom")

Plot = LE_R2BarPlot_RTPV(LENATT,"NATT","AnnualPPT", Clusters = 0)
Plot

Plot = LE_R2BarPlot_RTPV(LESAWS,"SAWS","PPTDryQtr", Clusters = 0)
Plot

#*******************************************************************************
# NEE stacked weights with different colours per transect
#*******************************************************************************

source("functions/NEE_StackedWeightPlot_Transects_RTPV.R")
TairSites = c("AU-Cpr","AU-Cum","AU-DaS","AU-Gin","AU-GWW",
          "AU-How","AU-Stp","AU-Tum","AU-Whr","AU-Wom")

TairTransects = c("SAWS","SAWS","NATT","SAWS","SAWS",
              "NATT","NATT","SAWS","SAWS","SAWS")

Plot = StackedWeightPlot_RTPV(TairSites,TairTransects,"Tair","AnnualPPT")
Plot

Plot = StackedWeightPlot_RTPV(Sites,Transects,"PPTlong","AnnualPPT")
Plot

#*******************************************************************************
# Metric Improvements for NEE - SUPPLEMENTARY
#*******************************************************************************

source("functions/NEE_MetricsPlot_function_RTPV.R")
Plot = NEE_MetricsPlot_function_RTPV(Sites)
Plot

#*******************************************************************************
# Model Performance Time Series
#*******************************************************************************

source("functions/NEE_DailyObsVsPred_MA_RTPV.R")
for (Site in Sites){
  Plots = NEE_DailyObsVsPred_MA_RTPV(Site)
  plot(Plots$StackedPlot)
}

#*******************************************************************************
# Climate Sensitivity
#*******************************************************************************

source("functions/NEE_SensitivityPlot_RTPV.R")
Plot = NEE_SensitivityPlot_RTPV(Sites)
Plot

LESites = c("AU-Cum","AU-DaS","AU-Dry","AU-Gin","AU-GWW",
            "AU-How","AU-Stp","AU-TTE","AU-Tum","AU-Whr","AU-Wom")

LETransects = c("SAWS","NATT","NATT","SAWS","AU-SAWS",
                "NATT","NATT","NATT","SAWS","SAWS","SAWS")
source("functions/LE_SensitivityPlot_RTPV.R")
Plot = LE_SensitivityPlot_RTPV(LESites)
Plot
