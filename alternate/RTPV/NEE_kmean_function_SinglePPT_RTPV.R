
NEE_singlePPT_kmean_RTPV = function(Site,PPTLag = 2){
  
# Function to run k-means clustering for each of the individual PPT lags
# Inputs:
#       Site = OzFlux Site Code
#       PPTLag = integer between 2 and 8
#                   2 = Lag of 14 to 20 days
#                   3 = Lag of 21 to 29 days
#                   4 = Lag of 30 to 59 days
#                   5 = Lag of 60 to 119 days
#                   6 = Lag of 120 to 179 days
#                   7 = Lag of 180 to 269 days
#                   8 = Lag of 270 to 365 days
  
# Load the required packages
library(cluster)
library(tidyverse)
library(factoextra)
library(NbClust)
  
# Check that PPTLag is correct
PPTLagDef = c("lag of 14 to 20 days",
              "lag of 21 to 29 days",
              "lag of 30 to 59 days",
              "lag of 60 to 119 days",
              "lag of 120 to 179 days",
              "lag of 180 to 269 days",
              "lag of 270 to 365 days")

# Check that PPTLag is correct
PPTLagName = c("14-20",
               "21-29",
               "30-59",
               "60-119",
               "120-179",
               "180-269",
               "270-365")

if (PPTLag %in% c(2,3,4,5,6,7,8)){
  message("Performing k-means clustering with precip ",PPTLagDef[PPTLag-1]," for ",Site," at ",Sys.time())
} else {
  stop("PPTLag must be an integer between 2 and 8 inclusive! Try again!")
}

# Define a function to find the mode so we can identify the best number of clusters
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Load the input file and extract required data
load(paste0("inputs/RTPV/",Site,"_Input_RTPV.Rdata"))
input = eval(as.name(paste0(Site,"_Input")))
# Combine climate data and precip data
climate = cbind(input$clim,input$ppt_multiscale[,PPTLag])
colnames(climate) = c("Ta",
                      "Fsd",
                      "VPD",
                      "PPT",
                      "PPT_lag")
# Remove first year, which has no PPT data and scale
climate = scale(climate[-(1:365),])
NEE = input$NEE[-(1:365)]

# Calculate indices to find recommended number of clusters
cluster = NbClust(data = climate, min.nc = 2, max.nc = 25, method = "kmeans")
k = getmode(cluster$Best.nc)

# Find the cluster allocations for recommended number of clusters
kmean.output = kmeans(climate,k,iter.max = 25, nstart = 25)

# Initialise the comparison dataframe
compare = data.frame("NEE_obs" = NEE,"NEE_mod" = 0)

output = list()

for (i in 1:k){
  climate_cluster = climate[kmean.output$cluster==i,]
  NEE_cluster = NEE[kmean.output$cluster==i]
  lin.mod = lm(NEE_cluster ~ climate_cluster, na.action = na.exclude)
  r.squared = summary(lin.mod)$r.squared
  # Place the k-means fitted NEE into the data frame
  compare$NEE_mod[kmean.output$cluster==i] = fitted(lin.mod)
  # Assign and output the cluster info
  name = paste0("cluster_",i)
  assign(name,list("climate" = climate_cluster,
                   "NEE" = NEE_cluster,
                   "model" = lin.mod,
                   "r.squared" = r.squared,
                   "mod.r" = r.squared*nrow(climate_cluster)))
  
  output[[paste0("cluster_",i)]] = eval(as.name(name))
  # Tidy up
  rm(list = c("climate_cluster",
              "NEE_cluster",
              "name",
              "lin.mod",
              "r.squared"))
}

output[["r.squared"]] = summary(lm(compare$NEE_obs ~ compare$NEE_mod))$r.squared
output[["series"]] = compare

save(output,file = paste0("alternate/RTPV/results/NEE_output_kmean_PPT",PPTLagName[PPTLag-1],"_RTPV_",Site,".Rdata"))

}
