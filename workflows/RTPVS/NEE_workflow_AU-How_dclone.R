# NEE SAM memory exploration
# Based on Liu et al, 2019
# Uses the same model

# Start the workflow
rm(list = ls())
message("Start the workflow at ",Sys.time())

# load needed packages
nee_packs <- c('rjags', 'coda', 'stats', 'R2jags', 'parallel','dclone')
lapply(nee_packs, require, character.only = T)

# variables to monitor
monitor_vars <- c("an", "ag", "phi0", "deltaXA", "weightA", "weightAP", "deltaXAP", 
                  "cum_weightA", "cum_weightAP", "sig_y", "NEE_pred","muNEE",
                  "ESen", 'deviance')

# Load the site inputs
load('./AU-How_Input.Rdata') 
data = `AU-How_Input`
inputdata = list("Nv"=data$Nv,
                 "Ns"=data$Ns,
                 "Nlag"=data$Nlag,
                 "Nmem"=data$Nmem,
                 "NlagP"=data$NlagP,
                 "Mem_records"=data$Mem_records,
                 "clim"=data$clim,
                 "ppt_multiscale"=data$ppt_multiscale,
                 "NEE"=data$NEE,
                 "NDVI"=data$NDVI,
                 "NblocksP"=data$NblocksP,
                 "block"=data$block,
                 "BlockSize"=data$BlockSize,
                 "Nblocks"=data$Nblocks)

# parallelize using dclone
cl <- makeCluster(6, type = "SOCK")
parLoadModule(cl, "glm")
parLoadModule(cl, 'lecuyer')
parLoadModule(cl, 'dic')
message("Begin model run at ",Sys.time())
# run model in parallel with 6 chains and cores
# 
source('NEEModel.R')
parJagsModel(cl, 
             name = 'OzFlux_NEE_Model', 
             file = NEEModel, 
             data = inputdata,
             n.chains = 6, 
             n.adapt = 100000, 
             quiet=FALSE)

message("Update the model at ",Sys.time())
parUpdate(cl, 
          "OzFlux_NEE_Model", 
          n.iter=100000)


samp_iter <- 1000000
message("Start the coda sampling at ",Sys.time())
NEE_Output <- parCodaSamples(cl, 
                             "OzFlux_NEE_Model",
                             variable.names = monitor_vars,
                             n.iter = samp_iter, 
                             thin = 200)

message("Save model output at ",Sys.time())
# Save the results
save(NEE_Output, file=paste('NEE_output_AU-How_', Sys.Date(),'_dclone.Rdata', sep = ''))

# Tidy up
rm(list=ls())
stopCluster(cl)

# Finish
message("Finished the model run at ",Sys.time())