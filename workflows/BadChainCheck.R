remove_bad_chains <- function(Site,Model="SAM"){
  
  # A function to take the output from a R2jags model run for an OzFlux site and
  # remove any chains that we know are bad!
  # 
  # INPUTS:
  # - Site: A character vector of length 6 containing the offical OzFlux code
  #             for the site, including the 'AU-' part
  # - Model: Either "SAM","CUR", or "AR1" to determine which model output to 
  #           look at
  # 
  # OUTPUTS:
  # - The same output file from the model, but with output.mcmc only containing
  #   good, converged chains
  # 
  
  # ##################
  # Let's ACTIVATE!
  # ##################
  
  # Let the user know which site the function is looking at
  message("*** Checking bad chains for ",Site," ***")
  # 
  # Load in the output data we are analysing
  # Look in folder "results" for the data
  File = list.files("results",pattern = paste0("NEE_output_",Site))
  # Read the data into R - note that if multiple results are available for a 
  # site, we take the most recent
  message("File picked up is ",File)
  load(paste0("results/",File[length(File)]))
  
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
  
  # Define the important parameters
  stochastic.params = c("phi0",
                        "sig_y",
                        sprintf("deltaXAP[%d]",seq(1:8)),
                        sprintf("deltaXA[%d,%d]",rep(1:5,10),rep(1:10,each=5)),
                        sprintf("an[%d]",seq(1:22)),
                        sprintf("ag[%d]",seq(1:22)))
  
  #**********************************
  ## Identifying the parameters and chains that haven't converged
  #**********************************
  
  # For each chain
  for (i in 1:length(output.mcmc)){
    # Summarise the chain
    chain = summary(output.mcmc[[i]])
    name = paste0("Chain.",i)
    assign(name,chain)
    # Find the chain deviance
    dev = chain$statistics[substr(rownames(chain$statistics),1,3)=="dev",1]
    name = paste0("Dev.",i)
    assign(name,dev)
    # Trim the chain data to parameters of interest
    trim = chain$statistics[rownames(chain$statistics) %in% stochastic.params,1]
    name = paste0("Trim.",i)
    assign(name,trim)
  }
  
  # Put the chain data into one dataframe
  df = data.frame(rep(NA,length(Trim.1)))
  for (i in 1:length(output.mcmc)){
    df[i]= eval(as.name(paste0("Trim.",i)))
    colnames(df)[i] = paste0("Trim.",i)
    rownames(df) = names(Trim.1)
  }
  
  #************************
  # Not sure med+/-1.5IQR is picking up all non-convergence - essential if we have
  # 6 chains and 2 are wildly larger (no loss of generality) then the 75th 
  # percentile will always be heavily influenced by one of the larger values. This
  # increases the IQR to a size large enough to include all chains
  # Here we try an error of margin of 10% from the median
  # I am only unsure here where parameter values are small to begin with)
  #************************
  
  # Find any outlying chains wrt the parameters values
  conv.df = data.frame("chain","position","var")
  count = 0
  for (j in 1:nrow(df)){
    # Outliers calculated as Median +/- 1.5 IQR
    med = median(as.numeric(df[j,]))
    for (i in 1:length(output.mcmc))
      # parameter too different
      if (!between(df[j,i],min(med*0.9,med*1.1),max(med*0.9,med*1.1))){
        count = count + 1
        conv.df[count,] = c(i,j,names(Trim.1)[j])
      } else {
        # Do nothing
      }
  }
}
  