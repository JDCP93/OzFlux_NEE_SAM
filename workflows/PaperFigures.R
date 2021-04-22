
rm(list=ls())

Sites = c("AU-ASM","AU-Cpr","AU-Cum","AU-DaS","AU-Dry","AU-Gin","AU-GWW",
          "AU-How","AU-Stp","AU-TTE","AU-Tum","AU-Whr","AU-Wom")

Transects = c("NATT","SAWS","SAWS","NATT","NATT","SAWS","SAWS",
              "NATT","NATT","NATT","SAWS","SAWS","SAWS")

#*******************************************************************************
# Model Performance
#*******************************************************************************

source("functions/NEE_R2BarPlot_function_RTPV.R")
Plot = NEE_R2BarPlot_RTPV(Sites,Transects,"AnnualPPT", Clusters = 0)
Plot

#*******************************************************************************
# Stacked weights with different colours per transect
#*******************************************************************************
source("functions/NEE_StackedWeightPlot_Transects_RTPV.R")
Plot = StackedWeightPlot_RTPV(Sites,Transects,"Tair","AnnualPPT")
Plot

Plot = StackedWeightPlot_RTPV(Sites,Transects,"PPTlong","AnnualPPT")
Plot

#*******************************************************************************
# Climate sensitivity
#*******************************************************************************
source("functions/NEE_SensitivityPlot_RTPV.R")
Plot = SensitivityPlot_RTPV(Sites)
Plot

#*******************************************************************************
# Metric Improvements for NEE - SUPPLEMENTARY
#*******************************************************************************
source("functions/NEE_MetricsPlot_function_RTPV.R")
Plot = NEE_MetricsPlot_function_RTPV(Sites)
Plot
