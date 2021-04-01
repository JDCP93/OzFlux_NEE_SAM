NEE_MBEBarPlot_RTPV = function(Sites,Transects,Metric,Clusters = 0){


# Load the required packages
library(tidyverse)
library(lubridate)
library(viridis)
library(ggpubr)
library(gridExtra)

# Initiliase MBE data frame
MBE = data.frame("Site" = Sites,
                "Transect" = Transects,
                "MBE.CUR" = 0,
                "MBE.SAM" = 0,
                "MBE.AR1" = 0,
                "MBE.KMN" = 0,
                "MBE.KMC" = 0)

# For each site
for (Site in Sites){

  # Collect the MBE values from the analysis scripts
  message("Collating MBE values for ",Site)
  
  # Load the analysis results
  File = list.files("analysis/RTPV/metrics/",pattern = paste0("NEE_metrics_RTPV_",Site))
  load(paste0("analysis/RTPV/metrics/",File))
  MBE$MBE.SAM[MBE$Site==Site] = output$SAM.MBE
  
  File = list.files("analysis/RTPV/metrics/",pattern = paste0("NEE_current_metrics_RTPV_",Site))
  load(paste0("analysis/RTPV/metrics/",File))
  MBE$MBE.CUR[MBE$Site==Site] = output$CUR.MBE
  
  File = list.files("analysis/RTPV/metrics/",pattern = paste0("NEE_AR1_metrics_RTPV_",Site))
  load(paste0("analysis/RTPV/metrics/",File))
  MBE$MBE.AR1[MBE$Site==Site] = output$AR1.MBE
  if (Clusters > 0){
  load(paste0("alternate/RTPV/results/NEE_output_",Clusters,"cluster_kmean_current_NDVI_RTPV_",Site,".Rdata"))
  MBE$MBE.KMN[MBE$Site==Site] = output$MBE
  
  load(paste0("alternate/RTPV/results/NEE_output_",Clusters,"cluster_kmean_current_RTPV_",Site,".Rdata"))
  MBE$MBE.KMC[MBE$Site==Site] = output$MBE
  } else {
    MBE$MBE.KMN = 0
    MBE$MBE.KMC = 0
  }
}

# Source worldclim correlations and climate metrics
load("site_data/SiteMetrics_worldclim_0.5res.Rdata")

# Plot the data!
# Create the plot dataframe
Site = rep(Sites,5)
Site = factor(Site, levels = WorldClimMetrics[order(WorldClimMetrics[colnames(WorldClimMetrics)==Metric]),1])

Transect = rep(Transects,5)

# Order the models as we want
Model = rep(c("Current Environment (k-means with no NDVI)",
              "Current Environment (k-means with NDVI)",
              "Current Environment (SAM)",
              "Environmental Memory (SAM)",
              "Biological Memory (AR1)"),
            each=length(Sites))
Model = factor(Model,
               levels=c("Biological Memory (AR1)",
                        "Environmental Memory (SAM)",
                        "Current Environment (SAM)",
                        "Current Environment (k-means with NDVI)",
                        "Current Environment (k-means with no NDVI)"))

Value = c(MBE$MBE.KMC,
          MBE$MBE.KMN,
          MBE$MBE.CUR,
          MBE$MBE.SAM,
          MBE$MBE.AR1)

Fig = data.frame(Site,
                 Transect,
                 Model,
                 Value)


# If we aren't plotting k-means, remove this data
if (Clusters == 0){
  Fig = Fig[!(Fig$Model %in% c("Current Environment (k-means with no NDVI)",
                              "Current Environment (k-means with NDVI)")),]
}

# Plot for every site based on supplied metric
Plot = ggplot(Fig,aes(fill=Model,y=Value,x=Site,group = Model)) +
  geom_bar(position="dodge",stat="identity") +
  geom_bar(position="dodge",stat = "identity", color = "black",size = 1) +
  coord_flip(ylim=c(-0.15,0.15)) +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE)) +
  ylab(parse(text="MBE")) +
  theme_bw() +
  theme(legend.position = "bottom", 
        text = element_text(size = 20),
        legend.title = element_blank()) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  ggtitle(paste0("Model Performance, ordered by decreasing ",Metric))

Plot

}
