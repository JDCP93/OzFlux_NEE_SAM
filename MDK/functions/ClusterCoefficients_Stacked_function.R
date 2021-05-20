ClusterCoefficients_Stacked = function(Site,k,MinDate,MaxDate){
  # Source required libraries
  library(gridExtra)
  library(cowplot)
  library(ggplot2)
  library(zoo)
  library(viridisLite)
  library(ggnewscale)
  library(viridis)
  library(scico)
  
  message("*** Plotting stacked cluster coefficients for ",Site," ***")
  
  # Load the k-means+regression model outputs
  File = list.files("output/",pattern = paste0("shortPPT_",MinDate,MaxDate,"_",k,"cluster_output_",Site))
  message("Loading k-means output for ",Site," where file is ",File)
  load(paste0("output/",File))
  assign("shortPPT",output)
  rm(output)
  
  # Load the k-means+regression model outputs
  File = list.files("output/",pattern = paste0("longPPT_",MinDate,MaxDate,"_",k,"cluster_output_",Site))
  message("Loading k-means output for ",Site," where file is ",File)
  load(paste0("output/",File))
  assign("longPPT",output)
  rm(output)
  
  # Load the input
  load(paste0("input/",Site,"_Input.Rdata"))
  input = eval(as.name(paste0(Site,"_Input")))
  rm(list=paste0(Site,"_Input"))
  
  StartDate = as.Date(paste0(MinDate,"0101"),format="%Y%m%d")
  EndDate = as.Date(paste0(MaxDate+1,"0101"),format="%Y%m%d")
  ShortIndex=!(input$Time>EndDate | input$Time<StartDate)
  LongIndex=!(input$Time>EndDate | input$Time<StartDate)

  ShortIndex[1:365]=FALSE
  LongIndex[1:1460]=FALSE

  ShortCluDF = data.frame("Date"=input$Time[ShortIndex],
                          "Lag" = "Short Lags Model",
                          "Cluster"=shortPPT$series$cluster)
  
  for (i in 1:k){
    coeff = shortPPT[[i]]$model$coefficients
    ShortCluDF[ShortCluDF$Cluster==i,names(coeff)] <- rep(unname(coeff),each=NROW(ShortCluDF[ShortCluDF$Cluster==i,]))
  }
  colnames(ShortCluDF) = str_replace(colnames(ShortCluDF),"climate_cluster","")
  ShortPlotDF <- pivot_longer(ShortCluDF, cols = "Ta":tail(colnames(ShortCluDF),n=1))
  
  LongCluDF = data.frame("Date"=input$Time[LongIndex],
                          "Lag" = "Long Lags Model",
                          "Cluster"=longPPT$series$cluster)
  
  for (i in 1:k){
    coeff = longPPT[[i]]$model$coefficients
    LongCluDF[LongCluDF$Cluster==i,names(coeff)] <- rep(unname(coeff),each=NROW(LongCluDF[LongCluDF$Cluster==i,]))
  }
  colnames(LongCluDF) = str_replace(colnames(LongCluDF),"climate_cluster","")
  LongPlotDF <- pivot_longer(LongCluDF, cols = "Ta":tail(colnames(LongCluDF),n=1))

  PlotDF = rbind(ShortPlotDF,LongPlotDF)
  
  PlotDF = PlotDF[!(PlotDF$name %in% c("Ta","VPD","Fsd","PPT","NDVI")),]
  PlotDF$name = str_replace(PlotDF$name,"PPT_","")
  PlotDF$name = str_replace(PlotDF$name,"_"," to ")
  PlotDF$name = paste0(PlotDF$name," days")
  
  PlotDF$name = factor(PlotDF$name,
                       levels = unique(PlotDF$name))
  
  PlotDF$value = abs(PlotDF$value)
  
  # Plot = ggplot(PlotDF) +
  #       geom_point(aes(x=Date,y=value,colour=name)) +
  #       scale_colour_viridis_d(name = "Lag", direction = -1) +
  #       scale_x_date(date_minor_breaks = "1 year") +
  #       theme_bw()
    
  Plot = ggplot(PlotDF) +
    geom_point(aes(x=Date,y=name,colour=value),size=6,shape = 15) +
    scale_colour_distiller(name = "Value", palette = "YlOrRd",limits = c(0,max(PlotDF$value)), direction = 1) +
    scale_x_datetime(date_breaks = "1 year",
                 date_labels = "%Y",
                 minor_breaks = NULL,
                 limits = c(input$Time[1],tail(input$Time,n=1))) +
    theme_bw() +
    ylab("Lag") +
    facet_grid(Lag~., scales = "free") +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(paste0(Site," Lag Weights"))
  
  Plot
}
