# Clean up
rm(list=ls())

# Load libraries
library(ggplot2)
library(viridis)
library(lubridate)
library(magrittr)
library(tidyverse)



# List all sites
Sites = c("AU-ASM",
          "AU-Cpr",
          "AU-Cum",
          "AU-DaS",
          "AU-Dry",
          "AU-Gin",
          "AU-GWW",
          "AU-How",
          "AU-Stp",
          "AU-TTE",
          "AU-Tum",
          "AU-Whr",
          "AU-Wom")

# Source worldclim correlations and climate metrics
load("site_data/SiteMetrics_worldclim_0.5res.Rdata")
Sites = factor(Sites, levels = WorldClimMetrics[order(WorldClimMetrics[colnames(WorldClimMetrics)=="AnnualPPT"]),1])

# List the transects
Transects = c("NATT",
              "SAWS",
              "SAWS",
              "NATT",
              "NATT",
              "SAWS",
              "SAWS",
              "NATT",
              "NATT",
              "NATT",
              "SAWS",
              "SAWS",
              "SAWS")

# Choose the cluster numbers
Clusters = 2:8

# List the k-mean models
Models = c("current","Fsdlags", "Tairlags","VPDlags","PPTlags", "allPPT","alllags","SAM","CABLE")

# Initiliase dataframes
df = data.frame("Site" = rep(Sites, each = length(Clusters)*length(Models)),
                "Transect" = rep(Transects, each = length(Clusters)*length(Models)),
                "Clusters" = rep(Clusters, times = length(Sites), each = length(Models)),
                "Model" = factor(rep(Models,
                                     times = length(Sites)*length(Clusters)),
                                 levels = Models),
                "NME" = NA,
                "SDD" = NA,
                "MBE" = NA,
                "R2" = NA,
                "CCO" = NA,
                "totwithinss" = NA,
                "avgsil" = NA)

# Load and extract the data
for (Site in Sites){
  for (Cluster in Clusters){
    for (Model in Models){
      if (file.exists(paste0("alternate/RTPV/results/NEE_output_",Cluster,"cluster_kmean_",Model,"_NDVI_RTPV_",Site,".Rdata"))){
        load(paste0("alternate/RTPV/results/NEE_output_",Cluster,"cluster_kmean_",Model,"_NDVI_RTPV_",Site,".Rdata"))
        df$R2[df$Clusters == Cluster & df$Model == Model & df$Site == Site] = output$r.squared
        df$CCO[df$Clusters == Cluster & df$Model == Model & df$Site == Site] = output$CCO
        df$MBE[df$Clusters == Cluster & df$Model == Model & df$Site == Site] = output$MBE
        df$NME[df$Clusters == Cluster & df$Model == Model & df$Site == Site] = output$NME
        df$SDD[df$Clusters == Cluster & df$Model == Model & df$Site == Site] = output$SDD
        df$totwithinss[df$Clusters == Cluster & df$Model == Model & df$Site == Site] = output$totwithinss
        df$avgsil[df$Clusters == Cluster & df$Model == Model & df$Site == Site] = output$avg.sil
      }
    }
  }
  file = list.files("analysis/RTPV/metrics",pattern = paste0("NEE_metrics_RTPV_",Site))
  load(paste0("analysis/RTPV/metrics/",file))
  df$R2[df$Site == Site & df$Model == "SAM"] = output$SAM.R2
  df$MBE[df$Site == Site & df$Model == "SAM"] = output$SAM.MBE
  df$NME[df$Site == Site & df$Model == "SAM"] = output$SAM.NME
  df$CCO[df$Site == Site & df$Model == "SAM"] = output$SAM.CCO
  df$SDD[df$Site == Site & df$Model == "SAM"] = output$SAM.SDD
  
  if (file.exists(paste0("CABLE/processed/",Site,"_metrics.Rdata"))){
    load(paste0("CABLE/processed/",Site,"_metrics.Rdata"))
    df$R2[df$Site == Site & df$Model == "CABLE"] = output$CABLE.NEE.R2
    df$MBE[df$Site == Site & df$Model == "CABLE"] = output$CABLE.NEE.MBE
    df$NME[df$Site == Site & df$Model == "CABLE"] = output$CABLE.NEE.NME
    df$CCO[df$Site == Site & df$Model == "CABLE"] = output$CABLE.NEE.CCO
    df$SDD[df$Site == Site & df$Model == "CABLE"] = output$CABLE.NEE.SDD
  }
}

# Plot boxplots for each site and model
Plot = ggplot() +
  geom_boxplot(data=df[df$Model!="CABLE",],aes(x=Model,y=R2,fill=Model)) +
  geom_point(data=df[df$Model%in%c("SAM","CABLE"),],aes(x="SAM",y=R2,color=Model,shape=Model),stroke=1) +
  facet_grid(.~Site) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size=20),
        legend.position = "bottom") +
  scale_y_continuous(breaks= seq(0,1,by=0.1),
                     expand = c(0,0)) +
  ylab(expression(R^2)) +
  xlab("") +
  scale_fill_viridis_d(name="",
                       breaks=c("current","Fsdlags", "Tairlags","VPDlags","PPTlags", "allPPT","alllags"),
                       labels = c("Current Climate",
                                  "+ Shortwave Radiation Lags",
                                  "+ Air Temperature Lags",
                                  "+ VPD lags",
                                  "+ Short-term PPT lags",
                                  "+ Long-term PPT lags",
                                  "+ all climate lags")) +

  scale_color_manual(name="",
                     values = c("red","blue"),
                     labels=c("SAM","CABLE")) +
  scale_shape_manual(name="",
                     values=c(8,4),
                     labels=c("SAM","CABLE")) +
  coord_cartesian(ylim = c(0,1)) +
  guides(fill = guide_legend(order = 1, ncol = 4, byrow = TRUE),colour = guide_legend(order = 2),shape=guide_legend(order=2))


Plot

