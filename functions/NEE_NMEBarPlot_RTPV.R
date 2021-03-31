NEE_R2BarPlot_RTPV = function(Sites,Transects,Metric,Clusters = 0){


# Load the required packages
library(tidyverse)
library(lubridate)
library(viridis)
library(ggpubr)
library(gridExtra)

# Initiliase R2 data frame
R2 = data.frame("Site" = Sites,
                "Transect" = Transects,
                "R2.CUR" = 0,
                "R2.SAM" = 0,
                "R2.AR1" = 0,
                "R2.KMN" = 0,
                "R2.KMC" = 0)

# For each site
for (Site in Sites){

  # Collect the R2 values from the analysis scripts
  message("Collating R2 values for ",Site)
  
  # Load the analysis results
  File = list.files("analysis/RTPV/",pattern = paste0("NEE_analysis_RTPV_",Site))
  load(paste0("analysis/RTPV/",File))
  R2$R2.SAM[R2$Site==Site] = output$SAM.R2
  
  File = list.files("analysis/RTPV/",pattern = paste0("NEE_current_analysis_RTPV_",Site))
  load(paste0("analysis/RTPV/",File))
  R2$R2.CUR[R2$Site==Site] = output$CUR.R2
  
  File = list.files("analysis/RTPV/",pattern = paste0("NEE_AR1_analysis_RTPV_",Site))
  load(paste0("analysis/RTPV/",File))
  R2$R2.AR1[R2$Site==Site] = output$AR1.R2
  if (Clusters > 0){
  load(paste0("alternate/RTPV/results/NEE_output_",Clusters,"cluster_kmean_current_NDVI_RTPV_",Site,".Rdata"))
  R2$R2.KMN[R2$Site==Site] = output$r.squared
  
  load(paste0("alternate/RTPV/results/NEE_output_",Clusters,"cluster_kmean_current_RTPV_",Site,".Rdata"))
  R2$R2.KMC[R2$Site==Site] = output$r.squared
  } else {
    R2$R2.KMN = 0
    R2$R2.KMC = 0
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

Value = c(R2$R2.KMC,
          R2$R2.KMN,
          R2$R2.CUR,
          R2$R2.SAM,
          R2$R2.AR1)

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
  ylab(parse(text="R^2")) +
  theme_bw() +
  theme(legend.position = "bottom", 
        text = element_text(size = 20),
        legend.title = element_blank()) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  ggtitle(paste0("Model Performance, ordered by decreasing ",Metric))

Plot

}
