StackedWeightPlot_RTPV = function(Sites,Vars = c("Tair","Fsd","VPD","PPTshort","PPTlong","PPT"),Metric= "AnnualPPT"){
  
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
  
  
  # Source packages needed
  library(lubridate)
  library(magrittr)
  library(dplyr)
  library(coda)
  
  # Run the analysis of the model outputs if they don't exist
  source("r2jags_analysis_RTPV.R")
  for (Site in Sites){
    if (length(list.files("analysis/RTPV/",pattern = paste0("analysis_RTPV_",Site))) != 0){
      message("Analysis file already exists for ",Site,". Moving to next site...")
    } else {
      message("Conducting model output analysis for ",Site,". Please wait...")
      r2jags_analysis_RTPV(Site)
    }
  }
  message("Plotting weights for sites...")
  
  # Collect the analysis outputs and name them with each site
  for (Site in Sites){
    File = list.files("analysis/RTPV/",pattern = paste0("analysis_RTPV_",Site))
    load(paste0("analysis/RTPV/",File))
    assign(Site,output)
    rm(output)
  }
  

  # Do some shit with it
  CumWeightParams = c(sprintf("cum_weightA[%d,%d]",rep(1:4,14),rep(1:14,each=4)),
                      sprintf("cum_weightAP[%d]",seq(1:8)))

  # Extract the cumulative weights
  CumWeights = data.frame("Site"=rep(Sites,each = 64),
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
  CumWeights = CumWeights[CumWeights$Variable %in% Vars,]
  
  # Rename variables to nice names
  CumWeights$Variable[CumWeights$Variable == "Fsd"] = "Shortwave Radiation"
  CumWeights$Variable[CumWeights$Variable == "Tair"] = "Air Temperature"
  CumWeights$Variable[CumWeights$Variable == "PPTshort"] = "Short-term Precipitation"
  CumWeights$Variable[CumWeights$Variable == "PPTlong"] = "Long-term Precipitation"
  
  # Assign levels to Variable
  CumWeights$Variable = factor(CumWeights$Variable,levels = sort(unique(CumWeights$Variable)))
  
  # Summarise the weights
  WeightSummary <- CumWeights %>%
                    group_by(Site,Variable) %>%
                    summarise(Median=median(Med,na.rm=TRUE),
                              Range = nth(Med,13)-min(Med),
                              Intercept = min(Med),
                              IQR = IQR(Med))
                  
  # Order sites by chosen variable
  load("SiteMetrics_worldclim_0.5res.Rdata")
  metric = WorldClimMetrics[,c("Sites",Metric)]
  SiteOrder = paste0(metric[order(metric[,2]),1]," - ",metric[order(metric[,2]),2])
  CumWeights$Site = paste0(CumWeights$Site," - ",rep(metric[,2],each = nrow(CumWeights)/length(Sites)))
  CumWeights$Site = factor(CumWeights$Site,levels=SiteOrder)
  
  # Let's try and get a correlation between cumulative weights and metric
  MedCorr = cor.test(WeightSummary$Median,WorldClimMetrics[,Metric])$estimate
  RangeCorr = cor.test(WeightSummary$Range,WorldClimMetrics[,Metric])$estimate
  InterceptCorr = cor.test(WeightSummary$Intercept,WorldClimMetrics[,Metric])$estimate
  IQRCorr = cor.test(WeightSummary$IQR,WorldClimMetrics[,Metric])$estimate
  nIQRCorr = cor.test((WeightSummary$IQR/WeightSummary$Median),WorldClimMetrics[,Metric])$estimate
  
  # Plot the sensitivity covariates
  library(ggplot2)
  library(viridisLite)
  StackedWeightsPlot = ggplot(CumWeights) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_line(aes(x = Lag, 
                  y = Med, 
                  color = Site),
              size = 2) +
    geom_point(aes(x = Lag,
                  y = Med,
                  color = Site),
               size = 3) +
    facet_grid(.~Variable,
               scales = "free_x") +
    ylab("Cumulative Weight") +
    xlab("Days into Past") +
    scale_color_viridis_d(name="",
                          #option="magma",
                          begin=0,
                          end=1,
                          direction = 1) +
    theme_bw() +
    theme(text = element_text(size=20),
          axis.text.x = element_text(angle=45, hjust=1)) +
    guides(color = guide_legend(title = Metric)) +
    ggtitle(round(sum(c(abs(MedCorr),abs(RangeCorr),abs(InterceptCorr),abs(IQRCorr),abs(nIQRCorr)),na.rm=TRUE),2),
            subtitle = paste0("Md = ",round(MedCorr,2),
                              ", Rg = ", round(RangeCorr,2),
                             ", In = ",round(InterceptCorr,2),
                             ", QR = ",round(IQRCorr,2),
                              ", nQR = ", round(nIQRCorr,2)))
}

