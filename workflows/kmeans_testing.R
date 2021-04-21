lagcheck = NDVI.df[!(NDVI.df$Model %in% c("current","alllags","allPPT")),]

maxmin = data.frame("Site" = unique(lagcheck$Site),
                    "Clusters" = rep(2:16,each = length(unique(lagcheck$Site))))

for (Site in unique(lagcheck$Site)){
  for (Cluster in 2:16){
    foo = lagcheck[lagcheck$Site==Site & lagcheck$Clusters == Cluster,]
    maxmin[maxmin$Site==Site & maxmin$Clusters == Cluster,"max"] = max(foo$R2)
    maxmin[maxmin$Site==Site & maxmin$Clusters == Cluster,"maxmodel"] = foo$Model[which(foo$R2 == max(foo$R2))]
    maxmin[maxmin$Site==Site & maxmin$Clusters == Cluster,"min"] = min(foo$R2)
    maxmin[maxmin$Site==Site & maxmin$Clusters == Cluster,"minmodel"] = foo$Model[which(foo$R2 == min(foo$R2))]
    maxmin[maxmin$Site==Site & maxmin$Clusters == Cluster,"cur"] = NDVI.df$R2[NDVI.df$Site == Site & NDVI.df$Clusters == Cluster & NDVI.df$Model == "current"]

}
}


Plot = ggplot(maxmin[maxmin$Site=="AU-ASM",]) +
        geom_point(aes(x=Clusters,y=max),color="red") +
        geom_point(aes(x=Clusters,y=min),color="blue") +
        geom_point(aes(x=Clusters,y=cur),color="green") +
        geom_text(aes(x=Clusters,y=max+0.05,label=maxmodel),color = "red") +
        geom_text(aes(x=Clusters,y=max-0.05,label=minmodel),color = "blue")

Plot


# Take mean and sd over clusters
sumR2 = NDVI.df[!(NDVI.df$Model %in% c("current","alllags","allPPT")),] %>%
  group_by(Site,Clusters) %>%
  summarise(sumR2 = sum(R2))


weights = data.frame("Site" = rep(Sites, each = 15*8),
                     "Clusters" = rep(2:16, each = 8, times = 13),
                     "Lag" = rep(c(0,
                                    "PPT14-20",
                                    "PPT21-29",
                                    "PPT30-59",
                                    "PPT60-119",
                                    "PPT120-179",
                                    "PPT180-269",
                                    "PPT270-365"), times = 15*13),
                     "Weight" = 0)


Models = c("PPT14-20",
           "PPT21-29",
           "PPT30-59",
           "PPT60-119",
           "PPT120-179",
           "PPT180-269",
           "PPT270-365")

for (Site in Sites){
  for (Cluster in 2:16){
    for (Model in Models){
      weights$Weight[weights$Site == Site & weights$Clusters==Cluster & weights$Lag == Model] = NDVI.df$R2[NDVI.df$Site == Site & NDVI.df$Clusters==Cluster & NDVI.df$Model == Model] - NDVI.df$R2[NDVI.df$Site == Site & NDVI.df$Clusters==Cluster & NDVI.df$Model == "Current"]
      weights$Weight[weights$Site == Site & weights$Clusters==Cluster & weights$Lag == Model] = weights$Weight[weights$Site == Site & weights$Clusters==Cluster & weights$Lag == Model]/sum(weights$Weight[weights$Site == Site & weights$Clusters==Cluster])
    }
  }
}

weights["cumweight"] = ave(weights$Weight, weights$Site, weights$Clusters, FUN=cumsum)
    

weights$Lag[weights$Lag == "PPT14-20"] = 20
weights$Lag[weights$Lag == "PPT21-29"] = 29
weights$Lag[weights$Lag == "PPT30-59"] = 59
weights$Lag[weights$Lag == "PPT60-119"] = 119
weights$Lag[weights$Lag == "PPT120-179"] = 179
weights$Lag[weights$Lag == "PPT180-269"] = 269
weights$Lag[weights$Lag == "PPT270-365"] = 365

weights$Lag= as.numeric(weights$Lag)


plot = ggplot(weights) +
        geom_line(aes(x=Lag,y=cumweight)) +
        geom_point(aes(x=Lag,y=cumweight)) +
  facet_grid(Site~Clusters,
             scales = "free_x") 

plot    
