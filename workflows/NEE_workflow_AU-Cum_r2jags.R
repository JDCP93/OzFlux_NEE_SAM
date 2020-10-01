# NEE SAM memory exploration
# Based on Liu et al, 2019
# Uses the same model

# Start the workflow
rm(list = ls())
message("Start the workflow at ",Sys.time())

# load needed packages
nee_packs <- c('rjags', 'coda', 'stats', 'R2jags', 'parallel','runjags')
lapply(nee_packs, require, character.only = T)

# variables to monitor
monitor_vars <- c("an", "ag", "phi0", "deltaXA", "weightA", "weightAP", "deltaXAP", 
                  "cum_weightA", "cum_weightAP", "sig_y", "NEE_pred","muNEE",
                  "ESen", 'deviance')

# Load the site inputs
load('./AU-Cum_Input.Rdata') 
data = `AU-Cum_Input`
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


# parallelize using runjags
message("Begin model run at ",Sys.time())
# run model in parallel with 6 chains and cores
output <- jags.parallel(model.file = 'NEEModel_v2.R',
                            parameters.to.save = monitor_vars,
                            data = inputdata,
                            n.chains = 6, 
                            n.burnin = 50000, 
                            n.iter = 500000,
                            jags.module = c('glm','dic'),
                            n.thin = 100)

message("Save model output at ",Sys.time())
# Save the results
save(output, file=paste('NEE_output_AU-Cum_', Sys.Date(),'.Rdata', sep = ''))

# Tidy up
rm(list=ls())

# Finish
message("Finished the model run at ",Sys.time())