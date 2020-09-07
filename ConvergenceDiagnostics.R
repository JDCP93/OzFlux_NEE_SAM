
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
