rm(list=ls())

if(getwd()!="/Users/climate/Documents/OzFlux_NEE_SAM/MDK"){
  setwd("./MDK")
}

k = 8
MinDate = 1990
MaxDate = 2050

library(gridExtra)

source("functions/FluxNetProcess_function.R")
source("functions/kmean_noPPT_function.R")
source("functions/kmean_shortPPT_function.R")
source("functions/kmean_longPPT_function.R")
source("functions/DailyObsVsPred_function.R")
source("functions/Metrics_function.R")
source("functions/ClusterLocation_function.R")
source("functions/ClusterCoefficients_function.R")
source("functions/ClusterCoefficients_Stacked_function.R")

Sites = c("BE-Bra","BE-Vie","DE-Tha","CH-Dav","DK-Sor","FI-Hyy","IT-Col","IT-Cpz","IT-Ren","NL-Loo","RU-Fyo")
#Sites = c("BE-Bra","BE-Vie","DE-Tha")

for (Site in Sites){
  if (!file.exists(paste0("input/",Site,"_Input.Rdata"))){
    FluxNetProcess(Site)
  }
  
  foo1 = kmean_noPPT(Site,k,MinDate,MaxDate)

  foo2 = kmean_shortPPT(Site,k,MinDate,MaxDate)

  foo3 = kmean_longPPT(Site,k,MinDate,MaxDate)
  # plot3 = foo3$heatmap
  # plot2 = foo2$heatmap
  # plot1 = foo1$heatmap
  # 
  # grid.arrange(plot1,plot2,plot3, 
  #              top = paste(Site,"Clusters"),
  #              layout_matrix = rbind(c(1,1,1,1),
  #                                    c(2,2,3,3)))
  # Sys.sleep(3)
  # 
}

for (Site in Sites){
  
  # plot1 = ClusterLocation(Site,k,"noPPT",MinDate,MaxDate)
  # plot2 = ClusterLocation(Site,k,"shortPPT",MinDate,MaxDate)
  # plot3 = ClusterLocation(Site,k,"longPPT",MinDate,MaxDate)
  # grid.arrange(plot1,plot2,plot3, top = paste(Site,"Cluster Locations"))
  # Sys.sleep(3)
  
  # plot1 = ClusterCoefficients(Site,k,"shortPPT",MinDate,MaxDate)
  # plot2 = ClusterCoefficients(Site,k,"longPPT",MinDate,MaxDate)
  # grid.arrange(plot1,plot2, top = paste(Site,"Lag Weights"))
  # Sys.sleep(3)
  
  plot1 = ClusterCoefficients_Stacked(Site,k,MinDate,MaxDate)
  plot(plot1)
  Sys.sleep(3)
  
  # plot1 = DailyObsVsPred(Site,k,"noPPT",15,MinDate,MaxDate)
  # plot2 = DailyObsVsPred(Site,k,"shortPPT",15,MinDate,MaxDate)
  # plot3 = DailyObsVsPred(Site,k,"longPPT",15,MinDate,MaxDate)
  # grid.arrange(plot1,plot2,plot3, top = paste(Site,"Time Series"))
  # Sys.sleep(3)
  
}


MetricsPlot = Metrics_function(Sites,k,MinDate,MaxDate)
plot(MetricsPlot)
Sys.sleep(3)