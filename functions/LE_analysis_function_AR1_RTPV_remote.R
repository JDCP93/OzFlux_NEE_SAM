LE_analysis_AR1_RTPV <- function(Site){
  
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
  message("*** Analysing AR1 R2jags output for ",Site," ***")
  # 
  # Load in the output data we are analysing
  # Look in folder "results" for the data
  File = list.files("output/RTPV/",pattern = paste0("LE_AR1_output_RTPV_",Site))
  # Read the data into R - note that if multiple results are available for a 
  # site, we take the most recent
  message("AR1 file is ",File[length(File)])
  load(paste0("output/RTPV/",File[length(File)]))
  AR1 = output
  rm(output)
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
  # Convergence checks
  # ##################
  
  # List the "fundamental" parameters - e.g. those that are assigned priors and
  # are not a function of other parameters. Stochastic parameters? Maybe.
  stochastic.params = c("b0","b1","sig.res")

  # Convert output to an mcmc object
  # Either take the object already saved as an mcmc object for the current 
  # workflows or, to maintain compatibility with older workflows, calculate it
  # from the rjags object
  if (class(AR1) == "list"){
    AR1.mcmc = AR1$output.mcmc
  }else{
    AR1.mcmc = as.mcmc.rjags(AR1)
  }
  rm(AR1)

  message("Running Gelman diagnostics for ", Site)
  # We find the Gelman diagnostic (it has a proper name but I'm a hack)
  # I think it's the shrink factor or something lol
  Gelman = gelman.diag(AR1.mcmc,multivariate=FALSE)
  # Find how many, and which, parameters fall outside of the acceptable limits
  # which here is set to 1.1
  Gelman.Fail = Gelman$psrf[Gelman$psrf[,2]>1.1,]
  # This picks up NA values for those parameters that are fixed to a certain 
  # value - we should exclude these
  Gelman.Fail = Gelman.Fail[complete.cases(Gelman.Fail),]
  
  message("Running ESS diagnostics for ", Site)
  # We find the effective sample size for each parameter
  ESS.raw = effectiveSize(AR1.mcmc)
  # Where parameters are forced to 0, then the ESS is also 0. Therefore we exclude
  # these when considering the fit. In general, higher ESS is better, with 10,000+
  # being ideal
  ESS = ESS.raw[ESS.raw > 0]
  # Plot a histogram to visualise how the ESS distribution breaks down
  ESSPlot = ggplot(data.frame(ESS)) +
            geom_histogram(aes(ESS),binwidth=250)
  # See which parameters are way below 10,000 ESS
  ESS.Fail = ESS[ESS<10000] # & names(ESS) %in% stochastic.params]
  
  
  message("Running Geweke diagnostics for ", Site)
  # We calculate the Geweke diagnostic - this should fall within the confidence 
  # bounds of -2 and 2. 
  Geweke = geweke.diag(AR1.mcmc)
  # I think this is less important - or at least, it depends on the length of the
  # burn-in period
  # Count how many elements are outside the bounds
  GewekeCount = unlist(lapply(Geweke, function(i) sum((i$z>2 | i$z<(-2)), #& names(i$z) %in% stochastic.params,
                                                      na.rm=TRUE)))
  GewekeNames = (lapply(Geweke, function(i) names(i$z)[(i$z>2 | i$z<(-2))])) # & names(i$z) %in% stochastic.params]))
  Geweke.Fail = mean(GewekeCount)
  
  # ##################
  # Model Performance
  # ##################
  
  # Load the observations
  name = paste0(Site,"_input")
  load(paste0("inputs/RTPV/LE/",name,"_LE_RTPV.Rdata"))
  assign("obs",eval(as.name(name)))
  
  # Create dataframe of observed vs modelled with confidence intervals

  message("Summarising AR1 for ", Site)
  AR1.summary = summary(AR1.mcmc)
  LE.res_pred = AR1.summary$statistics[substr(rownames(AR1.summary$statistics),1,10)=="LE.res_rep",1]
  LE.res_pred_min = AR1.summary$quantiles[substr(rownames(AR1.summary$quantiles),1,10)=="LE.res_rep",1]
  LE.res_pred_max = AR1.summary$quantiles[substr(rownames(AR1.summary$quantiles),1,10)=="LE.res_rep",5]
  LE_obs = obs$LE[-(1:366)]
  
  # Load in the output data 
  # Look in folder "results" for the data
  File = list.files("output/RTPV/",pattern = paste0("LE_output_RTPV_",Site))
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
  # Extract predicted and observed LE
  message("Summarising SAM for ", Site)
  SAM.summary = summary(SAM.mcmc)
  LE_pred = SAM.summary$statistics[substr(rownames(SAM.summary$statistics),1,7)=="LE_pred",1]
  LE.res = LE_obs - LE_pred[-1]
  
  df = data.frame("Date"=obs$DailyData$TIMESTAMP[-(1:366)],
                  "Pred"=LE.res_pred,
                  "Min"=LE.res_pred_min,
                  "Max"=LE.res_pred_max,
                  "Obs"=LE.res)
  
  # Plot the daily data
  ObsVsLE_daily = ggplot(df) +
    geom_ribbon(aes(x=Date,ymin=Min,ymax=Max, fill="Pred"),alpha=0.5) +
    geom_line(aes(x=Date,y=Obs,color="Obs", fill="Obs")) +
    geom_line(aes(x=Date,y=Pred,color="Pred", fill="Obs")) +
    scale_color_viridis_d(name="Data",
                          labels=c("Obs"="Observations","Pred"="Predicted"),
                          guide="legend",
                          option="magma",
                          direction=-1,
                          begin=0.2,
                          end=0.8) +
    scale_fill_viridis_d(name="Data",
                         labels=c("Obs"="Observations","Pred"="Predicted"),
                         guide="legend",
                         option="magma",
                         direction=-1,
                         begin=0.2,
                         end=0.8) +
    theme_bw()
  
  # We also calculate a dataframe of moving averages to smooth out the plot
  # Set the window for moving average
  k = 15
  # Create the dataframe
  df_ma = data.frame("Date"=rollmedian(df$Date,k),
                    "Pred"=rollmedian(df$Pred,k),
                    "Min"=rollmedian(df$Min,k),
                    "Max"=rollmedian(df$Max,k),
                    "Obs"=rollmedian(df$Obs,k))
  
  # Plot the moving average data
  ObsVsLE_ma = ggplot(df_ma) +
    geom_ribbon(aes(x=Date,ymin=Min,ymax=Max, fill="Pred"),alpha=0.5) +
    geom_line(aes(x=Date,y=Obs,color="Obs", fill="Obs")) +
    geom_line(aes(x=Date,y=Pred,color="Pred", fill="Obs")) +
    scale_color_viridis_d(name="Data",
                          labels=c("Obs"="Observations","Pred"="Predicted"),
                          guide="legend",
                          option="magma",
                          direction=-1,
                          begin=0.2,
                          end=0.8) +
    scale_fill_viridis_d(name="Data",
                         labels=c("Obs"="Observations","Pred"="Predicted"),
                         guide="legend",
                         option="magma",
                         direction=-1,
                         begin=0.2,
                         end=0.8) +
    theme_bw()

  # Since the daily data is likely to be very noisy, aggregate into monthly
  # data with sums 
  # I HAVE NO IDEA IF SUMMING CONFIDENCE INTERVALS IS LEGIT
  df$year = year(df$Date)
  df$month = month(df$Date)
  
  df_monthly = df %>%
    group_by(year,month) %>%               
    summarise(Pred=sum(Pred),
              Min=sum(Min),
              Max=sum(Max),
              Obs=sum(Obs))
  
  df_monthly$Date = as.yearmon(paste(df_monthly$year, df_monthly$month), "%Y %m")
  # Plot monthly data
  ObsVsLE_monthly = ggplot(df_monthly) +
    geom_ribbon(aes(x=Date,ymin=Min,ymax=Max, fill="Pred"),alpha=0.5) +
    geom_line(aes(x=Date,y=Obs,color="Obs", fill="Obs")) +
    geom_line(aes(x=Date,y=Pred,color="Pred", fill="Obs")) +
    scale_color_viridis_d(name="Data",
                          labels=c("Obs"="Observations","Pred"="Predicted"),
                          guide="legend",
                          option="magma",
                          direction=-1,
                          begin=0.2,
                          end=0.8) +
    scale_fill_viridis_d(name="Data",
                         labels=c("Obs"="Observations","Pred"="Predicted"),
                         guide="legend",
                         option="magma",
                         direction=-1,
                         begin=0.2,
                         end=0.8) +
    theme_bw()
  
  # Modelled LE
  LE_mod = LE_pred[-1]-LE.res_pred
  
  # Calculate the r squared value for the SAM model
  AR1.R2 = summary(lm(LE_pred[-1]-LE.res_pred ~ LE_obs))$r.squared
  AR1.MBE = sum(LE_mod-LE_obs,na.rm=TRUE)/length(LE_pred)
  AR1.NME = sum(abs(LE_mod-LE_obs),na.rm=TRUE)/sum(abs(mean(LE_obs,na.rm=TRUE)-LE_obs),na.rm=TRUE)
  AR1.SDD = abs(1-sd(LE_mod,na.rm=TRUE)/sd(LE_obs,na.rm=TRUE))
  AR1.CCO = cor(LE_mod,LE_obs,use = "complete.obs", method = "pearson")
  
  # Create a nice output and save it
  output = list("Gelman.Fail" = Gelman.Fail,
                "ESS.Fail" = ESS.Fail,
                "Geweke.Fail" = Geweke.Fail,
                "AR1.R2" = AR1.R2,
                "AR1.MBE" = AR1.MBE,
                "AR1.NME" = AR1.NME,
                "AR1.SDD" = AR1.SDD,
                "AR1.CCO" = AR1.CCO,
                "df" = df)
  
  save(output,file = paste0("LE_AR1_analysis_RTPV_",Site,"_",Sys.Date(),".Rdata"))
}

