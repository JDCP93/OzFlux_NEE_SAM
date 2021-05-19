DailyObsVsPred = function(Site,clusters,Type,MA,MaxDate){
  # Source required libraries
  library(gridExtra)
  library(cowplot)
  library(ggplot2)
  library(zoo)
  library(viridisLite)
  library(ggnewscale)
  library(viridis)
  
  # Load the k-means+regression model outputs
  File = list.files("output/",pattern = paste0(Type,"_",clusters,"cluster_output_",Site))
  message("Loading k-means output for ",Site," where file is ",File)
  load(paste0("output/",File))
  assign("kmeans",output)
  rm(output)
  
  # Load the input
  load(paste0("input/",Site,"_Input.Rdata"))
  input = eval(as.name(paste0(Site,"_Input")))
  rm(list=paste0(Site,"_Input"))
  
  End = as.Date(paste0(MaxDate+1,"0101"),format="%Y%m%d")
  l=length(input$Time[!(input$Time>End)])
  ll=length(input$Time)-length(kmeans$series$NEE_pred)
  # We also calculate a dataframe of moving averages to smooth out the plot
  NEE_MA = data.frame("Date"=rollmedian(input$Time[1461:l],MA),
                     "Pred"=rollmedian(kmeans$series$NEE_pred,MA),
                     "Obs"=rollmedian(input$NEE[1461:l],MA))
  
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
    theme(text = element_text(size = 20)) +
    theme(legend.position = "bottom") +
    ggtitle(paste0(Site," ",Type," ",clusters,"clusters - ",round(kmeans$r.squared,3)," r^2"))

  output = ma_plot
  
}
