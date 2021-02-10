metrics_RTPVS <- function(Site){
  
  # A function to take the output from a R2jags model run for an OzFlux site and
  # turn it into something useful and interesting and possibly, hopefully, 
  # insightful. Fingers crossed!
  # 
  # INPUTS:
  # - Site: A character vector of length 6 containing the offical OzFlux code
  #             for the site, including the 'AU-' part
  # 
  # OUTPUTS:
  # - We'll have to wait and see what I come up with!
  # 
  # HERE WE GO!
  # 
  
  # ##################
  # Let's ACTIVATE!
  # ##################
  
  # Let the user know which site the function is looking at
  message("*** Analysing R2jags output for ",Site," ***")
  # 
  # Load in the output data we are analysing
  # Look in folder "results" for the data
  File = list.files("output/RTPVS/",pattern = paste0("NEE_output_RTPVS_",Site))
  # Read the data into R - note that if multiple results are available for a 
  # site, we take the most recent
  load(paste0("output/RTPVS/",File[length(File)]))
  
  # Source the necessary packages
  library(coda)
  library(ggplot2)
  library(dplyr)
  library(rjags)
  library(R2jags)
  library(mcmcplots)
  library(lubridate)
  library(magrittr)
  library(zoo)
  
  if (class(output) == "list"){
    output.mcmc = output$output.mcmc
  }else{
    output.mcmc = as.mcmc.rjags(output)
  }
  rm(output)
  
  # ##################
  # Model Performance
  # ##################
  
  message("Running Model Performance for ",Site)
  # Load the observations
  name = paste0(Site,"_Input")
  load(paste0("inputs/RTPVS/",name,"_RTPVS.Rdata"))
  assign("obs",eval(as.name(name)))
  
  # Create dataframe of observed vs modelled with confidence intervals
#  NEE_pred = output$BUGSoutput$median$NEE_pred
#  NEE_pred_min = output$BUGSoutput$summary[substr(rownames(output$BUGSoutput$summary),1,3)=="NEE",3]
#  NEE_pred_max = output$BUGSoutput$summary[substr(rownames(output$BUGSoutput$summary),1,3)=="NEE",7]
  summary = summary(output.mcmc)
  rm(output.mcmc)
  NEE_pred = summary$statistics[substr(rownames(summary$statistics),1,5)=="NEE_p",1]
  NEE_obs = obs$NEE[-(1:365)]

  # Calculate metrics for the SAM model
  SAM.R2 = summary(lm(NEE_pred ~ NEE_obs))$r.squared
  Phi = summary$statistics["phi0",]
  SAM.MBE = sum(NEE_pred-NEE_obs,na.rm=TRUE)/length(NEE_pred)
  SAM.NME = sum(abs(NEE_pred-NEE_obs),na.rm=TRUE)/sum(abs(mean(NEE_obs,na.rm=TRUE)-NEE_obs),na.rm=TRUE)
  SAM.SDD = abs(1-sd(NEE_pred,na.rm=TRUE)/sd(NEE_obs,na.rm=TRUE))
  SAM.CCO = cor(NEE_pred,NEE_obs,use = "complete.obs", method = "pearson")
  
  # Create a nice output and save it
  output = list("SAM.R2" = SAM.R2,
                "SAM.MBE" = SAM.MBE,
                "SAM.NME" = SAM.NME,
                "SAM.SDD" = SAM.SDD,
                "SAM.CCO" = SAM.CCO,
                "Phi0" = Phi)
  
  save(output,file = paste0("analysis/RTPVS/NEE_metrics_RTPVS_",Site,"_",Sys.Date(),".Rdata"))
}

