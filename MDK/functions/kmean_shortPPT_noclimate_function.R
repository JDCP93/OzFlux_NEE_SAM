
kmean_shortPPT_noclimate = function(Site,k,MinDate,MaxDate){
  
# Load the required packages
library(cluster)
library(tidyverse)
library(factoextra)
library(NbClust)

message("Performing ",k,"-cluster k-means clustering with all short-term precip lags for ",Site," at ",Sys.time())
# Load the input file and extract required data
load(paste0("input/",Site,"_Input.Rdata"))
input = eval(as.name(paste0(Site,"_Input")))
# Combine climate data and precip data
climate = cbind(input$NDVI,input$ppt[,1:7])
colnames(climate) = c("NDVI",
                      "PPT_1_14",
                      "PPT_15_30",
                      "PPT_31_60",
                      "PPT_61_120",
                      "PPT_121_180",
                      "PPT_181_270",
                      "PPT_271_365")

StartDate = as.Date(paste0(MinDate,"0101"),format="%Y%m%d")
EndDate = as.Date(paste0(MaxDate+1,"0101"),format="%Y%m%d")

index=!(input$Time>EndDate | input$Time<StartDate)
index[1:365] = FALSE
climate = climate[index,]
NEE = input$NEE[index]
# Scale
climate = scale(climate)


# Find the cluster allocations for recommended number of clusters
kmean.output = kmeans(climate,k,iter.max = 100, nstart = 50)

# Initialise the comparison dataframe
compare = data.frame("NEE_obs" = NEE,"NEE_pred" = 0, "cluster" = kmean.output$cluster)

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

NEE_obs = compare$NEE_obs[-(1:1095)]
NEE_pred = compare$NEE_pred[-(1:1095)]

if (any(kmean.output$size<50)){
  message("                     ##**## WARNING! ##**##\n",
          sum(kmean.output$size<50)," clusters have too few observations for a reliable regression!\n",
          "                     ##**## WARNING! ##**##")
  Sys.sleep(3)
}

output[["r.squared"]] = summary(lm(compare$NEE_obs ~ compare$NEE_pred))$r.squared
output[["MBE"]] = sum(NEE_pred-NEE_obs,na.rm=TRUE)/length(NEE_pred)
output[["NME"]] = sum(abs(NEE_pred-NEE_obs),na.rm=TRUE)/sum(abs(mean(NEE_obs,na.rm=TRUE)-NEE_obs),na.rm=TRUE)
output[["SDD"]] = abs(1-sd(NEE_pred,na.rm=TRUE)/sd(NEE_obs,na.rm=TRUE))
output[["CCO"]] = cor(NEE_pred,NEE_obs,use = "complete.obs", method = "pearson")
output[["series"]] = compare
output[["totwithinss"]] = kmean.output$tot.withinss
output[["bet.tot.ratio"]] = kmean.output$betweenss/kmean.output$totss

ss <- silhouette(kmean.output$cluster, dist(climate))
ss = mean(ss[, 3])
output[["avg.sil"]] = ss

# create dataset with the cluster number
cluster <- c(1:k)
center <- kmean.output$centers
center_df <- data.frame(cluster, center)
# Reshape the data
center_reshape <- gather(center_df, features, values, NDVI:PPT_271_365)
center_reshape$features = factor(center_reshape$features,levels = unique(center_reshape$features))
# Plot the heat map
heatmap = ggplot(data = center_reshape, aes(x = features, y = cluster, fill = values)) +
  scale_y_continuous(breaks = seq(1, k, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_distiller(palette="RdBu") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=-90))


output[["heatmap"]] = heatmap

save(output,file = paste0("output/kmeans_shortPPT_noclimate_",MinDate,MaxDate,"_",k,"cluster_output_",Site,".Rdata"))

output
}
