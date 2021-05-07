LE_SAM_metrics_RTPV <- function(Site){
  
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
  File = list.files("output/RTPV/",pattern = paste0("LE_output_RTPV_",Site))
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
  name = paste0(Site,"_input")
  load(paste0("inputs/RTPV/LE/",name,"_LE_RTPV.Rdata"))
  assign("obs",eval(as.name(name)))
  
  # Create dataframe of observed vs modelled with confidence intervals
  summary = summary(output.mcmc)
  rm(output.mcmc)
  LE_pred = summary$statistics[substr(rownames(summary$statistics),1,4)=="LE_p",1]
  LE_obs = obs$LE[-(1:365)]
  
  # Calculate metrics for the SAM model
  SAM.R2 = summary(lm(LE_pred ~ LE_obs))$r.squared
  Phi = summary$statistics["phi0",]
  SAM.MBE = sum(LE_pred-LE_obs,na.rm=TRUE)/length(LE_pred)
  SAM.NME = sum(abs(LE_pred-LE_obs),na.rm=TRUE)/sum(abs(mean(LE_obs,na.rm=TRUE)-LE_obs),na.rm=TRUE)
  SAM.SDD = abs(1-sd(LE_pred,na.rm=TRUE)/sd(LE_obs,na.rm=TRUE))
  SAM.CCO = cor(LE_pred,LE_obs,use = "complete.obs", method = "pearson")
  
  # Create a nice output and save it
  output = list("SAM.R2" = SAM.R2,
                "SAM.MBE" = SAM.MBE,
                "SAM.NME" = SAM.NME,
                "SAM.SDD" = SAM.SDD,
                "SAM.CCO" = SAM.CCO,
                "Phi0" = Phi)
  
  save(output,file = paste0("LE_metrics_RTPV_",Site,"_",Sys.Date(),".Rdata"))
}

