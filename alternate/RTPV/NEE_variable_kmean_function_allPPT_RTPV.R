
NEE_allPPT_variable_kmean_RTPV = function(Site){
  
# Load the required packages
library(cluster)
library(tidyverse)
library(factoextra)
library(NbClust)

# Define a function to find the mode so we can identify the best number of clusters
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

message("Finding optimal k for k-means clustering with all precip lags for ",Site," at ",Sys.time())
# Load the input file and extract required data
load(paste0("inputs/RTPV/",Site,"_Input_RTPV.Rdata"))
input = eval(as.name(paste0(Site,"_Input")))
# Combine climate data and precip data
climate = cbind(input$clim,input$ppt_multiscale)
colnames(climate) = c("Ta",
                      "Fsd",
                      "VPD",
                      "PPT",
                      "PPT0",
                      "PPT_14_20",
                      "PPT_21_29",
                      "PPT_30_59",
                      "PPT_60_119",
                      "PPT_120_179",
                      "PPT_180_269",
                      "PPT_270_365")
# Remove PPT intercept
climate = climate[,-c(5)]
# Remove first year, which has no PPT data and scale
climate = scale(climate[-(1:365),])
NEE = input$NEE[-(1:365)]

# Calculate indices to find recommended number of clusters
cluster = NbClust(climate, min.nc = 2, max.nc = 25, method = "kmeans")
k = getmode(cluster$Best.nc)

message("Performing ",k,"-cluster k-means clustering with NDVI and all lags for ",Site," at ",Sys.time())
# Find the cluster allocations for recommended number of clusters
kmean.output = kmeans(climate,k,iter.max = 25, nstart = 25)

# Initialise the comparison dataframe
compare = data.frame("NEE_obs" = NEE,"NEE_pred" = 0)

output = list()

for (i in 1:k){
  climate_cluster = climate[kmean.output$cluster==i,]
  NEE_cluster = NEE[kmean.output$cluster==i]
  lin.mod = lm(NEE_cluster ~ climate_cluster, na.action = na.exclude)
  r.squared = summary(lin.mod)$r.squared
  # Place the k-means fitted NEE into the data frame
  compare$NEE_pred[kmean.output$cluster==i] = fitted(lin.mod)
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

NEE_obs = compare$NEE_obs
NEE_pred = compare$NEE_pred

output[["r.squared"]] = summary(lm(compare$NEE_obs ~ compare$NEE_pred))$r.squared
output[["MBE"]] = sum(NEE_pred-NEE_obs,na.rm=TRUE)/length(NEE_pred)
output[["NME"]] = sum(abs(NEE_pred-NEE_obs),na.rm=TRUE)/sum(abs(mean(NEE_obs,na.rm=TRUE)-NEE_obs),na.rm=TRUE)
output[["SDD"]] = abs(1-sd(NEE_pred,na.rm=TRUE)/sd(NEE_obs,na.rm=TRUE))
output[["CCO"]] = cor(NEE_pred,NEE_obs,use = "complete.obs", method = "pearson")
output[["series"]] = compare
output[["totwithinss"]] = kmean.output$tot.withinss

ss <- silhouette(kmean.output$cluster, dist(climate))
ss = mean(ss[, 3])
output[["avg.sil"]] = ss

save(output,file = paste0("alternate/RTPV/results/NEE_output_kmean_allPPT_RTPV_",Site,".Rdata"))

}
