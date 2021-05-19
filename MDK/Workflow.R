rm(list=ls())

k = 8
MaxDate = 2010
library(gridExtra)

source("functions/FluxNetProcess_function.R")
source("functions/kmean_noPPT_function.R")
source("functions/kmean_shortPPT_function.R")
source("functions/kmean_longPPT_function.R")
source("functions/DailyObsVsPred_function.R")

Sites = c("BE-Bra","BE-Vie","DE-Tha")

for (Site in Sites){
  if (!file.exists(paste0("input/",Site,"_Input.Rdata"))){
    FluxNetProcess(Site)
  }
    kmean_noPPT(Site,k,MaxDate)
    kmean_shortPPT(Site,k,MaxDate)
    kmean_longPPT(Site,k,MaxDate)

  plot1 = DailyObsVsPred(Site,k,"noPPT",15,MaxDate)
  plot2 = DailyObsVsPred(Site,k,"shortPPT",15,MaxDate)
  plot3 = DailyObsVsPred(Site,k,"longPPT",15,MaxDate)
  grid.arrange(plot1,plot2,plot3)
  Sys.sleep(3)
}

