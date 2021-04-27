LE_SensitivityPlot_RTPV = function(Sites,Vars = c("Tair","Fsd","VPD","PPTshort","PPTlong","PPT"),Metric = "AnnualPPT"){

  # Source packages needed
  library(lubridate)
  library(magrittr)
  library(dplyr)
  library(coda)
  
  source("functions/FindMetric.R")
  TitleUnits = FindMetric(Metric)
  
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
  message("Plotting weights for sites...")
  
  # Collect the analysis outputs and name them with each site
  for (Site in Sites){
    File = list.files("analysis/RTPV/",pattern = paste0("LE_analysis_RTPV_",Site))
    load(paste0("analysis/RTPV/",File))
    assign(Site,output)
    rm(output)
}

  # Extract the sensitvity covariates
  ESen = data.frame("Site"=rep(Sites,each = 6),
                    "Variable" = rep(rownames(eval(as.name(Sites[1]))$ESen),length(Sites)),
                    "Low" = unlist(lapply(Sites,function(x) eval(as.name(x))$ESen$ESenLow)),
                    "Med" = unlist(lapply(Sites,function(x) eval(as.name(x))$ESen$ESenMedian)),
                    "High" = unlist(lapply(Sites,function(x) eval(as.name(x))$ESen$ESenHigh)))
  # Check whether the climate variable is significant - does the CI contain 0?
  ESen$Significant = sign(ESen$Low*ESen$High)==1
  
  # Limit the dataframe to the variables requested
  ESen = ESen[ESen$Variable %in% Vars,]
  
  # Rename variables to nice names
  ESen$Variable[ESen$Variable == "Fsd"] = "Shortwave Radiation"
  ESen$Variable[ESen$Variable == "Tair"] = "Air Temperature"
  ESen$Variable[ESen$Variable == "PPTshort"] = "Short-term Precipitation"
  ESen$Variable[ESen$Variable == "PPTlong"] = "Long-term Precipitation"
  ESen$Variable[ESen$Variable == "PPT"] = "All Precipitation"
  
  # Assign levels to Variable
  levels = c("Shortwave Radiation",
             "Air Temperature",
             "VPD",
             "Short-term Precipitation",
             "Long-term Precipitation",
             "All Precipitation")
  ESen$Variable = factor(ESen$Variable,levels)
  
  # Order sites by chosen metric
  load("site_data/SiteMetrics_worldclim_0.5res.Rdata")
  metric = WorldClimMetrics[WorldClimMetrics$Sites %in% Sites,c("Sites",Metric)]
  colnames(metric) = c("Sites","Metric")
  SiteOrder = paste0(metric[order(metric[,2]),1]," - ",metric[order(metric[,2]),2],TitleUnits$Units)
  ESen$Site = paste0(ESen$Site,
                     " - ",
                     rep(metric[unique(match(ESen$Site,metric$Sites)),2],
                         each = nrow(ESen)/length(Sites)),TitleUnits$Units)
  ESen$Site = factor(ESen$Site,levels=SiteOrder)
  
  # Plot the sensitivity covariates
  library(ggplot2)
  library(viridisLite)
  ESenPlot = ggplot(ESen) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            geom_pointrange(aes(x = Site, 
                                ymin = Low, 
                                y = Med, 
                                ymax = High, 
                                color = Significant)) +
            geom_errorbar(aes(x = Site, 
                              ymin = Low, 
                              ymax = High, 
                              color = Significant), 
                          width = 0.5) +
            facet_wrap(Variable~.,
                        scales = "free_y",
                       ncol = (length(Vars)>=4)*2+(length(Vars)<4*1)) +
            scale_color_viridis_d(name="",
                                  labels=c("FALSE"="Non-Significant",
                                           "TRUE"="Significant"),
                                  guide="legend",
                                  begin=0.1,
                                  end=0.9,
                                  direction = -1) +
            ylab("Sensitivity") +
            theme_bw() +
            theme(legend.position = "bottom",
                  text = element_text(size=20),
                  axis.text.x = element_text(angle=45, hjust=1)) +
    ggtitle("LE Sensitivity to Climate Variables",
            subtitle = paste0("Sites ordered by ", TitleUnits$Title))
}