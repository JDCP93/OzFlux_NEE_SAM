WeightPlot_RTPV = function(Sites,Vars = c("Tair","Fsd","VPD","PPTshort","PPTlong"),Metric = "AnnualPPT"){
  
  # Source packages needed
  library(lubridate)
  library(magrittr)
  library(dplyr)
  library(coda)
  
  # Run the analysis of the model outputs if they don't exist
  source("functions/NEE_analysis_function_RTPV.R")
  for (Site in Sites){
    if (length(list.files("analysis/RTPV/",pattern = paste0("NEE_analysis_RTPV_",Site))) != 0){
      message("Analysis file already exists for ",Site,". Moving to next site...")
    } else {
      message("Conducting model output analysis for ",Site,". Please wait...")
      r2jags_analysis_RTPV(Site)
    }
  }
  message("Plotting weights for sites...")
  
  # Collect the analysis outputs and name them with each site
  for (Site in Sites){
    File = list.files("analysis/RTPV/",pattern = paste0("NEE_analysis_RTPV_",Site))
    load(paste0("analysis/RTPV/",File))
    assign(Site,output)
    rm(output)
  }
  
  # Let's load in the prior information
  load("analysis/RTPV/NEE_prior_summary_RTPV.Rdata")
  # Do some shit with it
  CumWeightParams = c(sprintf("cum_weightA[%d,%d]",rep(1:4,14),rep(1:14,each=4)),
                      sprintf("cum_weightAP[%d]",seq(1:8)))
  # Extract 2.5%, median and 97.5%
  PriMed = Prior$statistics[rownames(Prior$statistics) %in% CumWeightParams,1]
  PriLow = Prior$quantiles[rownames(Prior$quantiles) %in% CumWeightParams,1]
  PriHigh = Prior$quantiles[rownames(Prior$quantiles) %in% CumWeightParams,5]
  
  # Extract the cumulative weights
  CumWeights = data.frame("Site"=rep(Sites,each = 64),
                    "Variable" = rep(rownames(eval(as.name(Sites[1]))$CumWeights),length(Sites)),
                    "Lag" = rep(0,each = 64*length(Sites)),
                    "Low" = unlist(lapply(Sites,function(x) eval(as.name(x))$CumWeights$WeightsLow)),
                    "Med" = unlist(lapply(Sites,function(x) eval(as.name(x))$CumWeights$WeightsMedian)),
                    "High" = unlist(lapply(Sites,function(x) eval(as.name(x))$CumWeights$WeightsHigh)),
                    "PriLow" = rep(PriLow,length(Sites)),
                    "PriMed" = rep(PriMed,length(Sites)),
                    "PriHigh" = rep(PriHigh,length(Sites)))
  
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
  levels = c("Shortwave Radiation",
             "Air Temperature",
             "VPD",
             "Short-term Precipitation",
             "Long-term Precipitation",
             "All Precipitation")
  CumWeights$Variable = factor(CumWeights$Variable,levels)
  
  # Order sites by chosen metric
  load("site_data/SiteMetrics_worldclim_0.5res.Rdata")
  metric = WorldClimMetrics[WorldClimMetrics$Sites %in% Sites,c("Sites",Metric)]
  colnames(metric) = c("Sites","Metric")
  SiteOrder = paste0(metric[order(metric[,2]),1]," - ",metric[order(metric[,2]),2])
  CumWeights$Site = paste0(CumWeights$Site,
                           " - ",
                           rep(metric[unique(match(CumWeights$Site,metric$Sites)),2],
                               each = nrow(CumWeights)/length(Sites)))
  CumWeights$Site = factor(CumWeights$Site,levels=SiteOrder)
  
  # Check whether the climate variable is significant - does the CI contain 0?
 # ?????? ESen$Significant = sign(ESen$Low*ESen$High)==1
  
  # Plot the sensitivity covariates
  library(ggplot2)
  library(viridisLite)
  WeightsPlot = ggplot(CumWeights) +
    geom_ribbon(aes(x = Lag, 
                    ymin = PriLow, 
                    ymax = PriHigh, 
                    color = Variable),
                alpha = 0.5,
                size = 0) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_line(aes(x = Lag, 
                  y = Med, 
                  color = Variable),
              size = 2) +
    geom_point(aes(x = Lag, 
                  y = Med, 
                  color = Variable),
              size = 2.5) +
    geom_linerange(aes(x = Lag, 
                        ymin = Low, 
                        y = Med, 
                        ymax = High,
                        color = Variable),
                    size = 2) +
    geom_errorbar(aes(x = Lag, 
                      ymin = Low, 
                      ymax = High,
                      color = Variable), 
                  width = 0.1) +
    facet_grid(Site~Variable,
               scales = "free_x") +
    ylab("Cumulative Weight") +
    xlab("Days into Past") +
    scale_color_viridis_d(name="",
                          begin=0.1,
                          end=0.9,
                          direction = -1) +
    theme_bw() +
    theme(text = element_text(size=20),
          axis.text.x = element_text(angle=45, hjust=1)) +
    guides(color = "none") +
    ggtitle(paste0("Sites ordered by ", Metric))
}

