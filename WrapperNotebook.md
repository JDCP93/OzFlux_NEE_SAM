OzFlux NEE SAM Modelling
================
Jon Page
24/07/2020

This notebook is to document my modelling of OzFlux sites using a
replication of Liu et al’s model from their 2019 paper.

I have downloaded the model code from github
(<https://github.com/yliu11/NEE_Bayes_model>, accessed 15/06/20).

I need to coerce data into the required input formats and produce a
wrapper script.

Let’s activate\!

## Working on Howard Springs and Sturt Plains

I will first attempt to replicate the modelling at one site, Sturt
Plains.

I need to access the full fluxnet data for this, as the data I have from
previous modelling of this site does not include SWC. Daily FluxNet data
will be used.

``` r
if (file.exists(paste0(Site,"_Input_NDVI.Rdata"))){
  message("Gosh! Processed daily OzFlux data already exists for ",Site)
} else {
  source("OzFluxProcess_NDVI.R")
  OzFluxProcess(Site)
}
```

    ## Gosh! Processed daily OzFlux data already exists for SturtPlains

We have extracted the necessary data from the FluxNet and NDVI files. We
can now test-run the model:

``` r
library(R2jags)

# Load the input data
load(paste0(Site,"_Input.Rdata"))
# Assign to standard name
Data = eval(as.name(paste0(Site,"_Input")))

   # Define the parameters for the model operation
   # samples to be kept after burn in
   samples = 50000
   # iterations for burn in
   burn = samples * 0.1 
   # number of iterations where samplers adapt behaviour to maximise efficiency
   nadapt = 100  
   # The number of MCMC chains to run
   nchains = 4 
   # thinning rate
   # save every thin-th iteration to reduce correlation between 
   # consecutive values in the chain
   thin = 10 
   
    # Parameters to save
    parameters = c("NEE","muNEE")

   jags = jags(model.file='Model.R', data=Data, n.chains=nchains, parameters.to.save = parameters) 
```

This works and so we take it onto the storm servers for the greater
power.

## NIRV or NDVI?

The first model runs were performed using NIRV data. This has a much
smaller range than NDVI but correlates well. The SAM and AR(1) models
produce similar R2 values for HS and SP as those reported in Yao’s
paper. However, the current-only model has much higher R2 than Yao
found. I have not yet identified why this is, but have become intrigued
by the Veg Index used and its implications.

I have rerun the gap-filling from Sami, but this time for NDVI. Note
that both NIRV and NDVI have exactly the same gaps in the data.

We are also gap-filling 40% of the data\!\!\!\! This seems excessive but
may not be. The longest run of missing data is 73 days, with 1237 gaps.
The median is 3 days and the mean is 4.9 days. The mean run of data is
7.1 days and the median is 2.

``` r
load("VegIndex_NDVI.Rdata")
NDVI = VegIndex
load("VegIndex.Rdata")
NIRV = VegIndex

ndvi.na = rle(is.na(NDVI$ndvi))
max(ndvi.na$lengths[ndvi.na$values==TRUE])
```

    ## [1] 73

``` r
nirv.na = rle(is.na(NIRV$nirv))
max(nirv.na$lengths[nirv.na$values==TRUE])
```

    ## [1] 73

``` r
sum(is.na(NDVI$ndvi))/length(NDVI$ndvi)
```

    ## [1] 0.4082389

I will rerun the model with this data instead of the NIRV data and see
if there is any difference in the model results.

## Identified Issues

I was using the daily sums of NEE and SWR - these have been changed to
mean.

Soil moisture values are also interesting and quite different. Sturt
Plains in particular has some problematic values in the later years.

The time windows are also different between my work and Yao’s.

| Yao | Me |  
HS | 24/07/01 - 31/12/14 | 12/03/02 - 08/10/18 |  
SP | 14/08/08 - 31/12/14 | 28/08/08 - 13/09/18 |