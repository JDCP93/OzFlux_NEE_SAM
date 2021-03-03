SensitivityPlot_RTPV = function(Sites,Vars = c("Tair","Fsd","VPD","PPTshort","PPTlong","PPT")){

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

  # Initialise MAP dataframe
  MAP = data.frame("Site"=Sites,
                   "MAP"=rep(NA,length(Sites)))
  
  # Collect the analysis outputs and name them with each site
  for (Site in Sites){
    File = list.files("analysis/RTPV/",pattern = paste0("analysis_RTPV_",Site))
    load(paste0("analysis/RTPV/",File))
    assign(Site,output)
    rm(output)
  
  
    # We also load the daily data to calculate MAP
    load(paste0("inputs/RTPV/",Site,"_Input_RTPV.Rdata"))
    Input = eval(as.name(paste0(Site,"_Input")))
    DailyData = Input$DailyData
    DailyData$year = year(DailyData$TIMESTAMP)
    
    YearlyData <- DailyData %>%
      group_by(year) %>%               # group by the year column
      summarise(Precip=sum(Precip,na.rm=TRUE))
    
    MAP$MAP[MAP$Site==Site] = mean(YearlyData$Precip)
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
  ESen$Variable = factor(ESen$Variable,levels = sort(unique(ESen$Variable)))
  
  # Order sites by MAP
  MAP = MAP[order(MAP$MAP,decreasing = TRUE),]
  ESen$Site = factor(ESen$Site,levels=MAP$Site)
  
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
                                  option="magma",
                                  begin=0.2,
                                  end=0.6) +
            ylab("Sensitivity") +
            theme_bw() +
            theme(legend.position = "top",
                  text = element_text(size=20),
                  axis.text.x = element_text(angle=45, hjust=1))
}