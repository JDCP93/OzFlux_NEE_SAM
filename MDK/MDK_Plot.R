rm(list=ls())
Site = "BE-Vie"
k = 8
Type = "longPPT"
MinDate = 1990
MaxDate = 2050
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
  ClusterDF = data.frame("Date"=input$Time[index],
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
  # Plot = ggplot(plotdf) +
  #       geom_point(aes(x=Date,y=value,colour=name)) +
  #       scale_colour_viridis_d(name = "Lag", direction = -1) +
  #       scale_x_date(date_minor_breaks = "1 year") +
  #       theme_bw()
  
  Plot = ggplot(plotdf) +
    geom_point(aes(x=Date,y=name,colour=abs(value)),size=6,shape = 15) +
    scale_colour_distiller(name = "Importance", palette = "YlOrRd",limits = c(0,max(abs(plotdf$value))),direction = 1) +
    scale_x_datetime(date_breaks = "1 year",
                     date_labels = "%Y",
                     minor_breaks = NULL) +
    theme_bw() +
    ylab("Lag (days)") +
    xlab("") +
    theme(panel.grid = element_blank(),
          text = element_text(size = 20))
  
  Plot
