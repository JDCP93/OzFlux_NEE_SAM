ClusterLocation = function(Site,k,Type,MinDate,MaxDate){
  # Source required libraries
  library(gridExtra)
  library(cowplot)
  library(ggplot2)
  library(zoo)
  library(viridisLite)
  library(ggnewscale)
  library(viridis)
  
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
  
  titletype = c("Current Climate","PPT lags up to 1 year","PPT lags up to 4 years")[c(Type=="noPPT",Type=="shortPPT",Type=="longPPT")]
  titler2 = round(kmeans$r.squared,2)
  # Create the plot, just to steal the legend
  cluster_plot = ggplot(ClusterDF) +
    geom_point(aes(x=Date,y=Cluster,color=Cluster)) +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.minor.y = element_blank()) +
    ggtitle(bquote(.(titletype) ~ ":" ~ .(titler2) ~ r^{2})) +
    scale_y_continuous(breaks = 1:k) +
    scale_x_datetime(date_breaks = "1 year",
                     date_labels = "%Y",
                     minor_breaks = NULL,
                     limits = c(input$Time[1],tail(input$Time,n=1)))

  output = cluster_plot
  
}
