NEE_DailyObsVsPred_MA_RTPV = function(Site,k=15,plotAR1=F){
  # Source required libraries
  library(gridExtra)
  library(cowplot)
  library(ggplot2)
  library(zoo)
  
  # Load the analysed model outputs
  File = list.files("analysis/RTPV/",pattern = paste0("NEE_analysis_RTPV_",Site))
  message("Loading NEE analysis output for ",Site," where file is ",File)
  load(paste0("analysis/RTPV/",File))
  assign("SAM",output)
  rm(output)
  
  File = list.files("analysis/RTPV/",pattern = paste0("NEE_current_analysis_RTPV_",Site))
  message("Loading current NEE analysis output for ",Site," where file is ",File)
  load(paste0("analysis/RTPV/",File))
  assign("CUR",output)
  rm(output)
  
  File = list.files("analysis/RTPV/",pattern = paste0("NEE_AR1_analysis_RTPV_",Site))
  message("Loading AR1 NEE analysis output for ",Site," where file is ",File)
  load(paste0("analysis/RTPV/",File))
  assign("AR1",output)
  rm(output)
  # We also calculate a dataframe of moving averages to smooth out the plot
  # Set the window for moving average
  # Create the dataframe
  SAM_MA = data.frame("Date"=rollmedian(SAM$df$Date,k),
                     "Pred"=rollmedian(SAM$df$Pred,k),
                     "Min"=rollmedian(SAM$df$Min,k),
                     "Max"=rollmedian(SAM$df$Max,k),
                     "Obs"=rollmedian(SAM$df$Obs,k))
  
  CUR_MA = data.frame("Date"=rollmedian(CUR$df$Date,k),
                      "Pred"=rollmedian(CUR$df$Pred,k),
                      "Min"=rollmedian(CUR$df$Min,k),
                      "Max"=rollmedian(CUR$df$Max,k),
                      "Obs"=rollmedian(CUR$df$Obs,k))
  
  
  AR1_Pred = SAM$df$Pred[-1]-AR1$df$Pred
  AR1_Min = SAM$df$Min[-1]-AR1$df$Max
  AR1_Max = SAM$df$Max[-1]-AR1$df$Min
  AR1_Obs = SAM$df$Obs[-1]
  
  AR1df = data.frame("Date"=AR1$df$Date,
                     "Pred"=AR1_Pred,
                     "Min"=AR1_Min,
                     "Max"=AR1_Max,
                     "Obs"=AR1_Obs)
    
  AR1_MA = data.frame("Date"=rollmedian(AR1df$Date,k),
                       "Pred"=rollmedian(AR1df$Pred,k),
                       "Min"=rollmedian(AR1df$Min,k),
                       "Max"=rollmedian(AR1df$Max,k),
                       "Obs"=rollmedian(AR1df$Obs,k))

  
  # Create the plot, just to steal the legend
  SAM_ObsVsNEE_ma = ggplot(SAM_MA) +
    geom_ribbon(aes(x=Date,ymin=Min,ymax=Max, fill="Pred"),alpha=0.5) +
    geom_line(aes(x=Date,y=Obs,color="Obs")) +
    geom_line(aes(x=Date,y=Pred,color="Pred")) +
    scale_color_viridis_d(name="Daily Mean NEE",
                          labels=c("Obs"="Observations","Pred"="Predicted"),
                          guide="legend",
                          option="magma",
                          direction=-1,
                          begin=0.2,
                          end=0.8) +
    scale_fill_viridis_d(name="95% CI",
                         labels=c("Pred"="Predicted"),
                         guide="legend",
                         option="magma",
                         begin=0.2,
                         end=0.2) +
    theme_bw() +
    theme(legend.position = "bottom")
  # Grab the legend!
  legend = get_legend(SAM_ObsVsNEE_ma)
  
  # Recreate plot without legend
  SAM_ObsVsNEE_ma = ggplot(SAM_MA) +
    geom_ribbon(aes(x=Date,ymin=Min,ymax=Max, fill="Pred"),alpha=0.5) +
    geom_line(aes(x=Date,y=Obs,color="Obs")) +
    geom_line(aes(x=Date,y=Pred,color="Pred")) +
    scale_color_viridis_d(name="Mean Daily NEE",
                          labels=c("Obs"="Observations","Pred"="Predicted"),
                          guide="none",
                          option="magma",
                          direction=-1,
                          begin=0.2,
                          end=0.8) +
    scale_fill_viridis_d(name="95% CI",
                         labels=c("Pred"="Predicted"),
                         guide="none",
                         option="magma",
                         begin=0.2,
                         end=0.2) +
    ylab("NEE (umol/m^2/s)") +
    ggtitle("SAM Model (with lags)") +
    theme_bw()
  
  CUR_ObsVsNEE_ma = ggplot(CUR_MA) +
    geom_ribbon(aes(x=Date,ymin=Min,ymax=Max, fill="Pred"),alpha=0.5) +
    geom_line(aes(x=Date,y=Obs,color="Obs")) +
    geom_line(aes(x=Date,y=Pred,color="Pred")) +
    scale_color_viridis_d(labels=c("Obs"="Observations","Pred"="Predicted"),
                          guide="none",
                          option="magma",
                          direction=-1,
                          begin=0.2,
                          end=0.8) +
    scale_fill_viridis_d(labels=c("Pred"="Predicted"),
                         guide="none",
                         option="magma",
                         begin=0.2,
                         end=0.2) +
    theme_bw() +
    ggtitle("Current-only Model") +
    ylab("NEE (umol/m^2/s)")
  
  AR1_ObsVsNEE_ma = ggplot(AR1_MA) +
    geom_ribbon(aes(x=Date,ymin=Min,ymax=Max, fill="Pred"),alpha=0.5) +
    geom_line(aes(x=Date,y=Obs,color="Obs")) +
    geom_line(aes(x=Date,y=Pred,color="Pred")) +
    scale_color_viridis_d(labels=c("Obs"="Observations","Pred"="Predicted"),
                          guide="none",
                          option="magma",
                          direction=-1,
                          begin=0.2,
                          end=0.8) +
    scale_fill_viridis_d(labels=c("Pred"="Predicted"),
                         guide="none",
                         option="magma",
                         begin=0.2,
                         end=0.2) +
    theme_bw() +
    ggtitle("SAM Model + AR1-modelled residuals") +
    ylab("NEE (umol/m^2/s)")
  
  if (plotAR1==F){
    # Make layout matrix
    lay <- rbind(1,1,1,1,1,1,2,2,2,2,2,2,3)
    
    # Arrange the plots!
    ma_plot = grid.arrange(CUR_ObsVsNEE_ma,
                              SAM_ObsVsNEE_ma,
                              legend, 
                              ncol = 1, 
                              layout_matrix=lay,
                           top=paste0("Daily Mean NEE (Moving Average, k = ",k,") for ",Site))
  } else {
    # Make layout matrix
    lay <- rbind(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4)
    
    # Arrange the plots!
    ma_plot = grid.arrange(CUR_ObsVsNEE_ma,
                           SAM_ObsVsNEE_ma,
                           AR1_ObsVsNEE_ma,
                           legend, 
                           ncol = 1, 
                           layout_matrix=lay,
                           top=paste0("Daily Mean NEE (Moving Average, k = ",k,") for ",Site))
  }
  
}
