ClusterCoefficients = function(Site,k,Type){
  # Source required libraries
  library(gridExtra)
  library(cowplot)
  library(ggplot2)
  library(zoo)
  library(viridisLite)
  library(ggnewscale)
  library(viridis)
  library(scico)
  
  message("*** Plotting cluster coefficients for ",Site," and model ",Type," ***")
  
  # Load the k-means+regression model outputs
  File = list.files("alternate/RTPV/results/",pattern = paste0("NEE_output_",k,"cluster_kmean_",Type,"_NDVI_RTPV_",Site))
  message("Loading k-means output for ",Site," where file is ",File)
  load(paste0("alternate/RTPV/results/",File))
  assign("kmeans",output)
  rm(output)
  
  
  # Load the input
  load(paste0("inputs/RTPV/",Site,"_Input_RTPV.Rdata"))
  input = eval(as.name(paste0(Site,"_Input")))
  rm(list=paste0(Site,"_Input"))
  
  # We also calculate a dataframe of moving averages to smooth out the plot
  ClusterDF = data.frame("Date"=input$DailyData$TIMESTAMP[-(1:365)],
                         "Cluster"=kmeans$series$cluster)
  
  
  
  for (i in 1:k){
    coeff = kmeans[[i]]$model$coefficients
    ClusterDF[ClusterDF$Cluster==i,names(coeff)] <- rep(unname(coeff),each=NROW(ClusterDF[ClusterDF$Cluster==i,]))
  }
  
  colnames(ClusterDF) = str_replace(colnames(ClusterDF),"climate_cluster","")
  
  plotdf <- pivot_longer(ClusterDF, cols = "Ta":tail(colnames(ClusterDF),n=1))
  
  
  plotdf = plotdf[!(plotdf$name %in% c("Ta","VPD","Fsd","PPT","NDVI")),]
  
  plotdf$name = str_replace(plotdf$name,"PPT_","")
  plotdf$name = str_replace(plotdf$name,"_","-")
  
  plotdf$name = factor(plotdf$name,
                       levels = unique(plotdf$name))

  Plot = ggplot(plotdf) +
    geom_point(aes(x=Date,y=name,colour=abs(value)),size=6,shape = 15) +
    scale_colour_distiller(name = "Importance", palette = "YlOrRd",limits = c(0,max(abs(plotdf$value))),direction = 1) +
    scale_x_date(date_breaks = "1 year",
                     date_labels = "%Y",
                     minor_breaks = NULL,
                     limits = c(input$Time[1],tail(input$Time,n=1))) +
    theme_bw() +
    ylab("Lag (days)") +
    xlab("") +
    ggtitle(Site)
  
  Plot
}
