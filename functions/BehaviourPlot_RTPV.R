Behaviour_RTPV = function(Site){

  
  # Let the user know which site the function is looking at
  message("*** Comparing growing and dormant behaviour for ",Site," ***")
  # 
  # Load in the output data we are analysing
  # Look in folder "results" for the data
  File = list.files("output/RTPV/",pattern = paste0("NEE_output_RTPV_",Site))
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
  # tidy
  rm(output)
  # summarise
  summary = summary(output.mcmc)
  # tidy
  rm(output.mcmc)
  # Define params
  anCovariates = c(sprintf("an[%d]",seq(1:16)))
  # Extract 2.5%, median and 97.5%
  anMedian = summary$statistics[rownames(summary$statistics) %in% anCovariates,1]
  anLow = summary$quantiles[rownames(summary$quantiles) %in% anCovariates,1]
  anHigh = summary$quantiles[rownames(summary$quantiles) %in% anCovariates,5]
  
  
  agCovariates = c(sprintf("ag[%d]",seq(1:16)))
  # Extract 2.5%, median and 97.5%
  agMedian = summary$statistics[rownames(summary$statistics) %in% agCovariates,1]
  agLow = summary$quantiles[rownames(summary$quantiles) %in% agCovariates,1]
  agHigh = summary$quantiles[rownames(summary$quantiles) %in% agCovariates,5]
  
  Variables = factor(c("I","T","R","V","Ps","TxT","RxR","VxV","PsxPs","TxR","TxV","TxPs",
                "RxV","RxPs","VxPs","Pl"),levels = c("I","T","R","V","Ps","Pl","TxT","RxR","VxV","PsxPs","TxR","TxV","TxPs",
                                                     "RxV","RxPs","VxPs"))
  
  # Place in dataframe
  state.df = data.frame("Site" = rep(Site, by = 32),
                        "State" = rep(c("Growing","Dormant"), each = 16),
                        "Variable" = rep(Variables, by = 2),
                        "Low" = c(agLow,anLow),
                        "Med" = c(agMedian,anMedian),
                        "High" = c(agHigh,anHigh))
  
  state.df["Significant"] = "No"
  state.df[(sign(state.df$Low*state.df$High)==1),"Significant"] = "Yes"
  
  # Drop the intercept - it's not useful
  state.df = state.df[state.df$Variable!="I",]
  
  
  # I'M LOSIN' THE
  
  plot = ggplot(state.df) +
        geom_errorbar(aes(x = Variable,ymin = Low, ymax = High, colour = Significant)) +
    geom_point(aes(x = Variable, y = Med, colour = Significant)) +
    facet_grid(State~.) +
    ggtitle(Site)
  
  
  output = list("plot" = plot,
                "df" = state.df)

}