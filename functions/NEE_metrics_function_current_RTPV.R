NEE_current_metrics_RTPV <- function(Site){
  
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
  # Look in folder "output" for the data
  File = list.files("output/RTPV/",pattern = paste0("NEE_current_output_RTPV_",Site))
  # Read the data into R - note that if multiple results are available for a 
  # site, we take the most recent
  message("File is ",File[length(File)])
  load(paste0("output/RTPV/",File[length(File)]))
  
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
  
  # Convert output to an mcmc object
  # Either take the object already saved as an mcmc object for the current 
  # workflows or, to maintain compatibility with older workflows, calculate it
  # from the rjags object
  if (class(output) == "list"){
    output.mcmc = output$output.mcmc
  }else{
    output.mcmc = as.mcmc.rjags(output)
  }
  # Make sure we can analyse this as an mcmc list
  output.mcmc = as.mcmc.list(output.mcmc)
  rm(output)
  
  # ##################
  # Model Performance
  # ##################
  
  message("Running Model Performance for ",Site)
  # Load the observations
  name = paste0(Site,"_Input")
  load(paste0("inputs/RTPV/",name,"_RTPV.Rdata"))
  assign("obs",eval(as.name(name)))
  
  # Create dataframe of observed vs modelled with confidence intervals
  summary = summary(output.mcmc)
  rm(output.mcmc)
  NEE_pred = summary$statistics[substr(rownames(summary$statistics),1,5)=="NEE_p",1]
  NEE_obs = obs$NEE[-(1:365)]
  
  # Calculate metrics for the SAM model
  CUR.R2 = summary(lm(NEE_pred ~ NEE_obs))$r.squared
  Phi = summary$statistics["phi0",]
  CUR.MBE = sum(NEE_pred-NEE_obs,na.rm=TRUE)/length(NEE_pred)
  CUR.NME = sum(abs(NEE_pred-NEE_obs),na.rm=TRUE)/sum(abs(mean(NEE_obs,na.rm=TRUE)-NEE_obs),na.rm=TRUE)
  CUR.SDD = abs(1-sd(NEE_pred,na.rm=TRUE)/sd(NEE_obs,na.rm=TRUE))
  CUR.CCO = cor(NEE_pred,NEE_obs,use = "complete.obs", method = "pearson")
  
  # Create a nice output and save it
  output = list("CUR.R2" = CUR.R2,
                "CUR.MBE" = CUR.MBE,
                "CUR.NME" = CUR.NME,
                "CUR.SDD" = CUR.SDD,
                "CUR.CCO" = CUR.CCO,
                "Phi0" = Phi)
  
  save(output,file = paste0("NEE_current_metrics_RTPV_",Site,"_",Sys.Date(),".Rdata"))
}

