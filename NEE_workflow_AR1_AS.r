# dailyNEE model at site level
# --- variable blocks for memory ---
# --- NON-HIERACHICAL VERSION ----
# yao.liu.uwyo@gmail.com, Jan 26, 2017

# Memory weights structured as:
# 1. daily memory weights of 1-7 days: tave, swc, vpd, sw
# 2. weekly memory weight of "the last week" (8-14 days before): tave, swc, vpd, sw
# 3. 8 wts total for precipitation memory weights at different scales: "current two week" = 0 because it is represented by swc. 
#    week 2-4 into the past (3 wts), month 2,3-6 into the past (3 wts of monthly and bi-monthly), 
#    season 3-4 (7-12 months into the past) (2 wts of seasonal for the rest of the year)

# Likelihood = [1] intercept/baseline + 
#              [2-6] ant_T + ant_VPD + ant_SW + cur_SWC + ant-1_SWC ------- main effect (7 days+ 1 week wts, except for ant-1_SWC 6 day wts) 
#              [7-11] ant_T^2 + ant_VPD^2 + ant_SW^2 + cur_SWC^2 + ant-1_SWC^2 -----------quadratic terms 
#              [12-21] pairwise{ant_T, ant_VPD, ant_SW, cur_SWC, ant-1_SWC}   ------------ interactions
#              [22] ant_P +  ------ precipitation effect (10 wts)

rm(list = ls())
message("Start the workflow at ",Sys.time())
############# CONFIGURE THE RUNS HERE #################
# load needed packages
nee_packs <- c('rjags', 'coda', 'stats', 'R2jags', 'dclone')
lapply(nee_packs, require, character.only = T)

# variables to monitor
# nee_monitor_vars <- c("aa","p_aa", "deltaXA", "weightA", "weightAP", 
#   "cum_weightA", "cum_weightAP", "sig_yn", "sig_yp", "NEE_pred", "NEE_resi")
nee_monitor_vars <- c("b0","b1","sig.res","NEE.res_rep")

### # source the functions and models
### source('./dNEE_site/default_utils_list.r') # utils file contains a list of functions to source... And utils.R is using the default model.
### # Model can be over written if needed:
source('AR1Model.R')

### # OPTIONAL: load inits from previous runs -----
### load_inits <- T
### #load(paste('/scratch/yl422/ah_init_files/ah_NEE_output_site_US-Wkg_2017-02-07_updnum_1.rda'))
### #init_directory <- '../output/20170508/ndvi/'
### init_directory <- '/scratch/yl422/ndvi_0508_inits/'
### outf_names <- list.files(path = init_directory, pattern = '.rda')
### load(paste(init_directory, outf_names[tail(grep('US-Wkg', outf_names), 1)], sep = ''))

#############  END oF RUN CONFIGURAT #################
# prepare site-level input data from raw data ------
load('./AliceSprings_Input_NDVI.Rdata') # change these to functions
attach(`AliceSprings_Input`)


# Load Howard Springs data
load('results/NEE_output_site_AS_2020-08-05.rda')
# data inside is called "nee_daily"
assign('AS',nee_daily)
rm(nee_daily)
summary=summary(AS)
NEE_pred = summary$statistics[substr(rownames(summary$statistics),1,3)=="NEE",1]
NEE_obs = `AliceSprings_Input`$NEE[-(1:365)]

NEE.res = NEE_pred - NEE_obs
Nmem = AliceSprings_Input$Nmem




message("Begin model run at ",Sys.time())

# parallelize using dclone ------
cl <- makeCluster(6, type = "SOCK")
parLoadModule(cl, "glm")

AR1.res <- jags.parfit(cl, 
                       list('NEE.res' = NEE.res, 'Nmem' = Nmem), 
                       nee_monitor_vars, 
                       AR1Model, n.chains = 6, 
                       n.adapt = 5000, 
                       n.update = 10000, 
                       n.iter = 75000, 
                       thin = 20)

message("Save model output at ",Sys.time())
save(AR1.res, file=paste('NEE_AR1_output_site_AS_', Sys.Date(),'.rda', sep = ''))
  


detach(`AliceSprings_Input`)
stopCluster(cl)

message("Finished the model run at ",Sys.time())
