DailyObsVsPred = function(Site,k,Type,MA,MinDate,MaxDate){
  # Source required libraries
  library(gridExtra)
  library(cowplot)
  library(ggplot2)
  library(zoo)
  library(viridisLite)
  library(ggnewscale)
  library(viridis)
  
  message("*** Plotting observations vs predictions for ",Site," and model ",Type," ***")
  
  # Load the k-means+regression model outputs
  File = list.files("output/",pattern = paste0(Type,"_",MinDate,MaxDate,"_",k,"cluster_output_",Site))
  message("Loading k-means output for ",Site," where file is ",File)
  load(paste0("output/",File))
  assign("kmeans",output)
  rm(output)
  
  # Load the input
  load(paste0("input/",Site,"_Input.Rdata"))
  input = eval(as.name(paste0(Site,"_Input")))
  rm(list=paste0(Site,"_Input"))
  
  StartDate = as.Date(paste0(MinDate,"0101"),format="%Y%m%d")
  EndDate = as.Date(paste0(MaxDate+1,"0101"),format="%Y%m%d")
  index=!(input$Time>EndDate | input$Time<StartDate)
  if(Type=="shortPPT"){
    index[1:365]=FALSE
  }
  if(Type=="longPPT"){
    index[1:1460]=FALSE
  }
  # We also calculate a dataframe of moving averages to smooth out the plot
  NEE_MA = data.frame("Date"=rollmedian(input$Time[index],MA),
                     "Pred"=rollmedian(kmeans$series$NEE_pred,MA),
                     "Obs"=rollmedian(kmeans$series$NEE_obs,MA))
  
  titletype = c("Current Climate","PPT lags up to 1 year","PPT lags up to 4 years")[c(Type=="noPPT",Type=="shortPPT",Type=="longPPT")]
  titler2 = round(kmeans$r.squared,2)
  # Create the plot, just to steal the legend
  ma_plot = ggplot(NEE_MA) +
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
    theme(legend.position = "bottom") +
    ggtitle(bquote(.(titletype) ~ ":" ~ .(titler2) ~ r^{2})) +
    scale_x_datetime(limits = c(input$Time[1],tail(input$Time,n=1)))

  output = ma_plot
  
}
