# NEE SAM memory exploration
# Based on Liu et al, 2019
# Uses the same model

# Start the workflow
rm(list = ls())
message("Start the workflow at ",Sys.time())

# load needed packages
nee_packs <- c('rjags', 'coda', 'stats', 'R2jags', 'parallel','runjags')
lapply(nee_packs, require, character.only = T)

runjags.options("force.summary"=T)

# variables to monitor
monitor_vars <- c("an", "ag", "phi0", "deltaXA", "weightA", "weightAP", "deltaXAP", 
                      "cum_weightA", "cum_weightAP", "sig_y", "NEE_pred","muNEE",
                      "ESen", 'deviance')

# Load the site inputs
load('./AU-Cpr_Input.Rdata') 
data = `AU-Cpr_Input`
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

inputdata = dump.format(inputdata)

# parallelize using runjags
message("Begin model run at ",Sys.time())
# run model in parallel with 6 chains and cores
initial_results <- run.jags(model = 'NEEModel_v2.R',
                    monitor = monitor_vars,
                    data = inputdata,
                    n.chains = 6, 
                    burnin = 1000, 
                    sample = 50000,
                    adapt = 1000,
                    modules = c('glm','dic'),
                    thin = 10,
                    method = 'parallel',
                    n.sims = 6)

# Extend the run on a single core to calculate DIC
message("Calculating DIC for the model at ",Sys.time())
results <- extend.jags(initial_results,
                       add.monitor=c("pd","dic"),
                       sample = 10000,
                       adapt = 1000,
                       thin = 10,
                       method='rjags')

message("Save model output at ",Sys.time())
# Save the results
output = list("results"=results,"initial_results"=initial_results)
save(output, file=paste('NEE_output_AU-Cpr_', Sys.Date(),'.Rdata', sep = ''))
  
# Tidy up
rm(list=ls())

# Finish
message("Finished the model run at ",Sys.time())