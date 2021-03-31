NEE_AR1_metrics_RTPV <- function(Site){
  
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
  # Load in the output data we are analysing
  # Look in folder "results" for the data
  File = list.files("output/RTPV/",pattern = paste0("NEE_AR1_output_RTPV_",Site))
  # Read the data into R - note that if multiple results are available for a 
  # site, we take the most recent
  message("AR1 file is ",File[length(File)])
  load(paste0("output/RTPV/",File[length(File)]))
  AR1 = output
  rm(output)
  
  if (class(AR1) == "list"){
    AR1.mcmc = AR1$output.mcmc
  }else{
    AR1.mcmc = as.mcmc.rjags(AR1)
  }
  rm(AR1)
  
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
  
  # ##################
  # Model Performance
  # ##################
  
  message("Running Model Performance for ",Site)
  # Load the observations
  name = paste0(Site,"_Input")
  load(paste0("inputs/RTPV/",name,"_RTPV.Rdata"))
  assign("obs",eval(as.name(name)))
  
  message("Summarising AR1 for ", Site)
  AR1.summary = summary(AR1.mcmc)
  NEE.res_pred = AR1.summary$statistics[substr(rownames(AR1.summary$statistics),1,11)=="NEE.res_rep",1]
  NEE.res_pred_min = AR1.summary$quantiles[substr(rownames(AR1.summary$quantiles),1,11)=="NEE.res_rep",1]
  NEE.res_pred_max = AR1.summary$quantiles[substr(rownames(AR1.summary$quantiles),1,11)=="NEE.res_rep",5]
  NEE_obs = obs$NEE[-(1:366)]
  
  # Load in the output data 
  # Look in folder "results" for the data
  File = list.files("output/RTPV/",pattern = paste0("NEE_output_RTPV_",Site))
  # Read the data into R - note that if multiple results are available for a 
  # site, we take the most recent
  message("SAM file is ",File[length(File)])
  load(paste0("output/RTPV/",File[length(File)]))
  SAM = output
  rm(output)
  # Either take the object already saved as an mcmc object for the current 
  # workflows or, to maintain compatibility with older workflows, calculate it
  # from the rjags object
  if (class(SAM) == "list"){
    SAM.mcmc = SAM$output.mcmc
  }else{
    SAM.mcmc = as.mcmc.rjags(SAM)
  }
  rm(SAM)
  
  SAM.mcmc = as.mcmc.list(SAM.mcmc)
  # Extract predicted and observed NEE
  message("Summarising SAM for ", Site)
  SAM.summary = summary(SAM.mcmc)
  NEE_pred = SAM.summary$statistics[substr(rownames(SAM.summary$statistics),1,8)=="NEE_pred",1]
  NEE.res = NEE_obs - NEE_pred[-1]
  
  # Modelled NEE
  NEE_mod = NEE_pred[-1]-NEE.res_pred
  
  # Calculate metrics for the SAM model
  AR1.R2 = summary(lm(NEE_mod ~ NEE_obs))$r.squared
  AR1.MBE = sum(NEE_mod-NEE_obs,na.rm=TRUE)/length(NEE_pred)
  AR1.NME = sum(abs(NEE_mod-NEE_obs),na.rm=TRUE)/sum(abs(mean(NEE_obs,na.rm=TRUE)-NEE_obs),na.rm=TRUE)
  AR1.SDD = abs(1-sd(NEE_mod,na.rm=TRUE)/sd(NEE_obs,na.rm=TRUE))
  AR1.CCO = cor(NEE_mod,NEE_obs,use = "complete.obs", method = "pearson")
  
  # Create a nice output and save it
  output = list("AR1.R2" = AR1.R2,
                "AR1.MBE" = AR1.MBE,
                "AR1.NME" = AR1.NME,
                "AR1.SDD" = AR1.SDD,
                "AR1.CCO" = AR1.CCO)
  
  save(output,file = paste0("NEE_AR1_metrics_RTPV_",Site,"_",Sys.Date(),".Rdata"))
}

