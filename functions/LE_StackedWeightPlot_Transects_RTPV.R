LE_StackedWeightPlot_RTPV = function(Sites,Transects,Var,Metric= "AnnualPPT"){
  
  # Sort must be one of the following metrics available in the world clim data:
  # "AnnualMeanTemp"   
  # "MeanDiurnalRange" 
  # "Isothermality"    
  # "TempSeasonality" 
  # "MaxTempHotMon"    
  # "MinTempColdMon"   
  # "TempAnnualRange"  
  # "MeanTempWetQtr"   
  # "MeanTempDryQtr"   
  # "MeanTempHotQtr"   
  # "MeanTempColdQtr"  
  # "AnnualPPT"       
  # "PPTWetMon"       
  # "PPTDryMon"        
  # "PPTSeasonality"   
  # "PPTWetQtr"        
  # "PPTDryQtr"        
  # "PPTHotQtr"        
  # "PPTColdQtr"      
  
  message("Stacking weights for ", Var," and ",Metric)
  # Source packages needed
  library(lubridate)
  library(magrittr)
  library(dplyr)
  library(coda)
  library(viridis)
  
  # Define the plot title and units
  source("functions/FindMetric.R")
  TitleUnits = FindMetric(Metric)
 
  Title = TitleUnits$Title
  Unit = TitleUnits$Units
  
  # Run the analysis of the model outputs if they don't exist

  for (Site in Sites){
    if (length(list.files("analysis/RTPV/",pattern = paste0("LE_analysis_RTPV_",Site))) != 0){
      message("Analysis file already exists for ",Site,". Moving to next site...")
    } else {
      message("Conducting model output analysis for ",Site,". Please wait...")
      source("functions/LE_analysis_function_RTPV.R")
      r2jags_analysis_RTPV(Site)
    }
  }

  
  # Collect the analysis outputs and name them with each site
  for (Site in Sites){
    File = list.files("analysis/RTPV/",pattern = paste0("LE_analysis_RTPV_",Site))
    message("Loading analysis output for ",Site," where file is ",File)
    load(paste0("analysis/RTPV/",File))
    assign(Site,output)
    rm(output)
  }
  
  message("Plotting weights for sites...")
  # Do some shit with it
  CumWeightParams = c(sprintf("cum_weightA[%d,%d]",rep(1:4,14),rep(1:14,each=4)),
                      sprintf("cum_weightAP[%d]",seq(1:8)))

  # Extract the cumulative weights
  CumWeights = data.frame("Site"=rep(Sites,each = 64),
                          "Transect"=rep(Transects,each=64),
                          "Variable" = rep(rownames(eval(as.name(Sites[1]))$CumWeights),length(Sites)),
                          "Lag" = rep(0,each = 64*length(Sites)),
                          "Low" = unlist(lapply(Sites,function(x) eval(as.name(x))$CumWeights$WeightsLow)),
                          "Med" = unlist(lapply(Sites,function(x) eval(as.name(x))$CumWeights$WeightsMedian)),
                          "High" = unlist(lapply(Sites,function(x) eval(as.name(x))$CumWeights$WeightsHigh)))
  
  
  # Assign lags
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="1]"] = 0
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="2]"] = 1
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="3]"] = 2
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="4]"] = 3
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="5]"] = 4
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="6]"] = 5
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="7]"] = 6
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="8]"] = 7
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="9]"] = 8
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="10]"] = 9
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="11]"] = 10
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="12]"] = 11
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="13]"] = 12
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="14]"] = 13
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="1]"] = 0
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="2]"] = 20
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="3]"] = 29
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="4]"] = 59
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="5]"] = 119
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="6]"] = 179
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="7]"] = 269
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="8]"] = 365
  
  # Rename variables
  CumWeights$Variable[substr(CumWeights$Variable,13,13)==1] = "Tair"
  CumWeights$Variable[substr(CumWeights$Variable,13,13)==2] = "Fsd"
  CumWeights$Variable[substr(CumWeights$Variable,13,13)==3] = "VPD"
  CumWeights$Variable[substr(CumWeights$Variable,13,13)==4] = "PPTshort"
  CumWeights$Variable[substr(CumWeights$Variable,11,12)=="AP"] = "PPTlong"
  
  # Limit the dataframe to the variables requested
  CumWeights = CumWeights[CumWeights$Variable == Var,]
  
  # Rename variables to nice names
  CumWeights$Variable[CumWeights$Variable == "Fsd"] = "Shortwave Radiation"
  CumWeights$Variable[CumWeights$Variable == "Tair"] = "Air Temperature"
  CumWeights$Variable[CumWeights$Variable == "PPTshort"] = "Short-term Precipitation"
  CumWeights$Variable[CumWeights$Variable == "PPTlong"] = "Long-term Precipitation"
  
  # Assign levels to Variable
  CumWeights$Variable = factor(CumWeights$Variable,levels = sort(unique(CumWeights$Variable)))
  
  # Assign vector length
  if (Var == "PPTlong"){
    veclen = 8
  } else {
    veclen = 14
  }
  
  # Summarise the weights
  WeightSummary <- CumWeights %>%
                    group_by(Site,Variable) %>%
                    summarise(Median=median(Med,na.rm=TRUE),
                              Range = nth(Med,veclen-1)-min(Med),
                              Intercept = min(Med),
                              IQR = IQR(Med),
                              Lag50 = min(Lag[Med>0.5])-min(Med[Med>0.5]))
                  
  # Order sites by chosen variable
  load("site_data/SiteMetrics_worldclim_0.5res.Rdata")
  metric = WorldClimMetrics[WorldClimMetrics$Sites %in% Sites,c("Sites",Metric)]
  # SiteOrder = paste0(metric[order(metric[,2]),1]," - ",metric[order(metric[,2]),2],Unit)
  # CumWeights$Site = paste0(CumWeights$Site," - ",rep(metric[,2],each = nrow(CumWeights)/length(Sites)),Unit)
  # CumWeights$Site = factor(CumWeights$Site,levels=SiteOrder)
  CumWeights$Site = factor(CumWeights$Site,levels=metric[order(metric[,2]),1])

  # Make sure the Var is nice!
  TitleVar = rep(0,length(Var))
  TitleVar[Var == "Fsd"] = "Shortwave Radiation"
  TitleVar[Var == "Tair"] = "Air Temperature"
  TitleVar[Var == "PPTshort"] = "Short-term Precipitation"
  TitleVar[Var == "PPTlong"] = "Long-term Precipitation"
  TitleVar[Var == "VPD"] = "VPD"
  
  
  # Plot the sensitivity covariates
  
  #colors =  turbo(length(Sites)+4)
  colors = viridis(length(Sites)+4)
  
  library(ggplot2)
  library(viridisLite)
  library(ggnewscale)
  library(viridis)
  StackedWeightsPlot = ggplot() +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_line(data = CumWeights[CumWeights$Transect=="NATT",],
              aes(x = Lag, 
                  y = Med, 
                  color = Site,
              ),
              size = 2) +
    geom_point(data = CumWeights[CumWeights$Transect=="NATT",],
               aes(x = Lag,
                   y = Med,
                   color = Site),
               size = 3) +
    scale_color_manual(name = "NATT",
                       values = colors[rev(1:length(unique(CumWeights$Site[CumWeights$Transect=="NATT"])))],
                       guide = guide_legend(order = 1)) +
    new_scale_color() + 
    geom_line(data = CumWeights[CumWeights$Transect=="SAWS",],
              aes(x = Lag, 
                  y = Med, 
                  color = Site,
              ),
              size = 2) +
    geom_point(data = CumWeights[CumWeights$Transect=="SAWS",],
               aes(x = Lag,
                   y = Med,
                   color = Site),
               size = 3) +
    scale_color_manual(name = "SAWS ",
                       values = colors[rev((length(unique(CumWeights$Site[CumWeights$Transect=="NATT"]))+5):(length(Sites)+4))],
                       guide = guide_legend(order = 2)) +
    coord_cartesian(ylim = c(0,1)) +
    ylab("Cumulative Weight") +
    xlab("Days into Past") +
    theme_bw() +
    theme(text = element_text(size=20),
          axis.text.x = element_text(angle=45, hjust=1))
  #  ggtitle(paste0(TitleVar," Cumulative Weights"), 
  #          subtitle = paste0("Sites ordered by ",Title))
}

