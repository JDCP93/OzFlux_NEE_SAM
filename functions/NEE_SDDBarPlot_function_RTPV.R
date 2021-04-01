NEE_SDDBarPlot_RTPV = function(Sites,Transects,Metric,Clusters = 0){


# Load the required packages
library(tidyverse)
library(lubridate)
library(viridis)
library(ggpubr)
library(gridExtra)

# Initiliase SDD data frame
SDD = data.frame("Site" = Sites,
                "Transect" = Transects,
                "SDD.CUR" = 0,
                "SDD.SAM" = 0,
                "SDD.AR1" = 0,
                "SDD.KMN" = 0,
                "SDD.KMC" = 0)

# For each site
for (Site in Sites){

  # Collect the SDD values from the analysis scripts
  message("Collating SDD values for ",Site)
  
  # Load the analysis results
  File = list.files("analysis/RTPV/metrics/",pattern = paste0("NEE_metrics_RTPV_",Site))
  load(paste0("analysis/RTPV/metrics/",File))
  SDD$SDD.SAM[SDD$Site==Site] = output$SAM.SDD
  
  File = list.files("analysis/RTPV/metrics/",pattern = paste0("NEE_current_metrics_RTPV_",Site))
  load(paste0("analysis/RTPV/metrics/",File))
  SDD$SDD.CUR[SDD$Site==Site] = output$CUR.SDD
  
  File = list.files("analysis/RTPV/metrics/",pattern = paste0("NEE_AR1_metrics_RTPV_",Site))
  load(paste0("analysis/RTPV/metrics/",File))
  SDD$SDD.AR1[SDD$Site==Site] = output$AR1.SDD
  if (Clusters > 0){
  load(paste0("alternate/RTPV/results/NEE_output_",Clusters,"cluster_kmean_current_NDVI_RTPV_",Site,".Rdata"))
  SDD$SDD.KMN[SDD$Site==Site] = output$SDD
  
  load(paste0("alternate/RTPV/results/NEE_output_",Clusters,"cluster_kmean_current_RTPV_",Site,".Rdata"))
  SDD$SDD.KMC[SDD$Site==Site] = output$SDD
  } else {
    SDD$SDD.KMN = 0
    SDD$SDD.KMC = 0
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

Value = c(SDD$SDD.KMC,
          SDD$SDD.KMN,
          SDD$SDD.CUR,
          SDD$SDD.SAM,
          SDD$SDD.AR1)

Fig = data.frame(Site,
                 Transect,
                 Model,
                 Value)

# Here we make sure that the bars are ordered by their size
Fig = Fig[order(Fig$Value, decreasing = TRUE),]
Fig$ValueFactor<-factor(Fig$Value, levels = unique(Fig$Value))

# We then replace value with the difference so that the bars are "cumulative"
for(site in Sites){
  Fig$Value[Fig$Site==site][1:4] = rev(diff(rev(Fig$Value[Fig$Site==site])))
}

# If we aren't plotting k-means, remove this data
if (Clusters == 0){
  Fig = Fig[!(Fig$Model %in% c("Current Environment (k-means with no NDVI)",
                              "Current Environment (k-means with NDVI)")),]
}

# Plot for every site based on supplied metric
Plot = ggplot(Fig,aes(fill=Model,y=Value,x=Site,group = ValueFactor)) +
  geom_bar(position="stack",stat="identity") +
  geom_bar(stat = "identity", color = "black",size = 1) +
  coord_flip(ylim=c(0,1)) +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE)) +
  ylab(parse(text="SDD")) +
  theme_bw() +
  theme(legend.position = "bottom", 
        text = element_text(size = 20),
        legend.title = element_blank()) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  ggtitle(paste0("Model Performance, ordered by decreasing ",Metric))

Plot

}
