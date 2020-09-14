
# This is a script to analyse the outputs of workflow for convergence and 
# other fun and important shenanigans.
# Eventually this will be a function, because I am not a complete hack

# Tidy up
rm(list=ls())
# Load the required output
load("results/NEE_output_long3_site_HS_2020-09-07.rda")

# Load the required libraries and Kruschke's functions
library(coda)
library(ggplot2)
library(dplyr)
source('DBDA2E-utilities.R')

# We find the Gelman diagnostic (it has a proper name but I'm a hack)
Gelman = gelman.diag(nee_daily,multivariate=FALSE)
# This should be less than 1.1 for all parameters if the chains have converged
Oops1 = Gelman$psrf[Gelman$psrf[,2]>1.1,]
# "Bayesian credible intervals based on the t-distribution are too wide, 
# and have the potential to shrink by this factor if the MCMC run is continued."
# The above is from the function definition and is the case when the value is 
# substantially greater than 1

# We find the effective sample size for each parameter
ESS.raw = effectiveSize(nee_daily)
# Where parameters are forced to 0, then the ESS is also 0. Therefore we exclude
# these when considering the fit. In general, higher ESS is better, with 10,000+
# being ideal
ESS = ESS.raw[ESS.raw > 0]
ESSPlot = ggplot(data.frame(ESS)) +
  geom_histogram(aes(ESS),binwidth=250)
ESSPlot
# See how many parameters are way below 10,000 ESS
Oops2 = sum(ESS<7500)


# We calculate the Geweke diagnostic - this should fall within the confidence 
# bounds of -2 and 2. 
Geweke = geweke.diag(nee_daily)
# I think this is less important - or at least, it depends on the length of the
# burn-in period
# Count how many elements are outside the bounds
GewekeOOB = unlist(lapply(Geweke, function(i) sum(i$z>2 | i$z<(-2),na.rm=TRUE)))
Oops3 = mean(GewekeOOB)



#**********************************
## Identifying the parameters and chains that haven't converged
#**********************************

# For each chain
for (i in 1:length(nee_daily)){
  # Summarise the chain
  chain = summary(nee_daily[[i]])
  name = paste0("Chain.",i)
  assign(name,chain)
  # Find the chain deviance
  dev = chain$statistics[substr(rownames(chain$statistics),1,3)=="dev",1]
  name = paste0("Dev.",i)
  assign(name,dev)
  # Trim the chain data to parameters of interest
  trim = chain$statistics[!(substr(rownames(chain$statistics),1,3)=="NEE"),1]
  name = paste0("Trim.",i)
  assign(name,trim)
}

# Put the chain data into one dataframe
df = data.frame(rep(NA,length(Trim.1)))
for (i in 1:length(nee_daily)){
  df[i]= eval(as.name(paste0("Trim.",i)))
  colnames(df)[i] = paste0("Trim.",i)
}

# Find any outlying chains wrt the parameters values
conv.df = data.frame("chain","position","var")
count = 0
for (j in 1:nrow(df)){
  # Outliers calculated as Median +/- 1.5 IQR
  med = median(as.numeric(df[j,]))
  iqr = IQR(as.numeric(df[j,]))
  for (i in 1:length(nee_daily))
    # parameter too large
    if (df[j,i] > med+1.5*iqr){
      count = count + 1
      conv.df[count,] = c(i,j,names(Trim.1)[j])
      # parameter too small
    } else if (df[j,i] < med-1.5*iqr){
      count = count + 1
      conv.df[count,] = c(i,j,names(Trim.1)[j])
      # Parameter within tolerance 
    } else {
      # Do nothing
    }
}

# Limit to directly estimated (fundamental?) parameters
# Take first 3 characters of the fundamental parameters
fund.params = c("wei","sig","phi","dev","an[","ag[")
conv.params = conv.df[substr(conv.df$X.var.,1,3) %in% fund.params,]
# Let's count the number of parameters for which each chain hasn't converged
chain.count = conv.params %>% group_by(X.chain.) %>% summarise(count = n())
