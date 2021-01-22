# NEE SAM memory exploration
# Based on Liu et al, 2019
# Uses the same model

# Start the workflow
rm(list = ls())
message("Start the workflow at ",Sys.time())

# load needed packages
nee_packs <- c('rjags', 'coda', 'stats', 'R2jags', 'parallel','runjags','mcmcplots')
lapply(nee_packs, require, character.only = T)

# variables to monitor
monitor_vars <- c("an", "ag", "phi0", "deltaXA", "weightA", "weightAP", "deltaXAP", 
                  "cum_weightA", "cum_weightAP", "sig_y", "NEE_pred","muNEE",
                  "ESen")

# Load the site inputs
load('./AU-Wom_Input.Rdata') 
data = `AU-Wom_Input`
inputdata = list("Nv"=data$Nv,
                 "Ns"=data$Ns,
                 "Nlag"=data$Nlag,
                 "Nmem"=data$Nmem,
                 "NlagP"=data$NlagP,
                 "Mem_records"=data$Mem_records,
                 "clim"=data$clim,
                 "ppt_multiscale"=data$ppt_multiscale,
                 "NEE"=NULL,
                 "NDVI"=data$NDVI,
                 "NblocksP"=data$NblocksP,
                 "block"=data$block,
                 "BlockSize"=data$BlockSize,
                 "Nblocks"=data$Nblocks)


# parallelize using runjags
message("Begin model run at ",Sys.time())
# run model in parallel with 6 chains and cores

jags = jags.model('NEEModel_v2.R', data=inputdata, n.chains=4, n.adapt=5000) 
output = coda.samples(jags, n.iter=50000, n.burnin=5000, thin=100,
                   variable.names=monitor_vars)

message("Save model output at ",Sys.time())
# Transform output into mcmc object to save space
output.mcmc = output
output = list("output.mcmc"=output.mcmc)
# Save the results
save(output, file=paste('NEE_output_AU-Prior_', Sys.Date(),'.Rdata', sep = ''))

# Tidy up
rm(list=ls())

# Finish
message("Finished the model run at ",Sys.time())