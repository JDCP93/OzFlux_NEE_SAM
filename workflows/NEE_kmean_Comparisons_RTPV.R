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
Clusters = 2:16

# List the k-mean models
Models = c("current",
           "PPT14-20",
           "PPT21-29",
           "PPT30-59",
           "PPT60-119",
           "PPT120-179",
           "PPT180-269",
           "PPT270-365",
           "allPPT",
           "alllags",
           "SAM")

# NDVI included or not?
NDVIs = factor(c("NoNDVI","NDVI"),
               levels = c("NoNDVI","NDVI"))

# Initiliase dataframes
df = data.frame("Site" = rep(Sites, each = length(Clusters)*length(Models)*length(NDVIs)),
                "Transect" = rep(Transects, each = length(Clusters)*length(Models)*length(NDVIs)),
                "Clusters" = rep(Clusters, times = length(Sites), each = length(Models)*length(NDVIs)),
                "Model" = factor(rep(Models,
                                     times = length(Sites)*length(Clusters), each = length(NDVIs)),
                                 levels = Models),
                "NDVI" = rep(NDVIs,
                             times = length(Sites)*length(Clusters)*length(Models)),
                "R2" = NA,
                "totwithinss" = NA,
                "avgsil" = NA)

SAM = data.frame("Site" = Sites,
                 "Transect" = Transects,
                 "R2" = NA)

# Load and extract the data
for (Site in Sites){
  for (Cluster in Clusters){
    for (Model in Models){
      # No NDVI
      if (file.exists(paste0("alternate/RTPV/results/NEE_output_",Cluster,"cluster_kmean_",Model,"_RTPV_",Site,".Rdata"))){
        load(paste0("alternate/RTPV/results/NEE_output_",Cluster,"cluster_kmean_",Model,"_RTPV_",Site,".Rdata"))
        df$R2[df$Clusters == Cluster & df$Model == Model & df$NDVI == "NoNDVI" & df$Site == Site] = output$r.squared
        df$totwithinss[df$Clusters == Cluster & df$Model == Model & df$NDVI == "NoNDVI" & df$Site == Site] = output$totwithinss
        df$avgsil[df$Clusters == Cluster & df$Model == Model & df$NDVI == "NoNDVI" & df$Site == Site] = output$avg.sil
      }
      # NDVI
      if (file.exists(paste0("alternate/RTPV/results/NEE_output_",Cluster,"cluster_kmean_",Model,"_NDVI_RTPV_",Site,".Rdata"))){
        load(paste0("alternate/RTPV/results/NEE_output_",Cluster,"cluster_kmean_",Model,"_NDVI_RTPV_",Site,".Rdata"))
        df$R2[df$Clusters == Cluster & df$Model == Model & df$NDVI == "NDVI" & df$Site == Site] = output$r.squared
        df$totwithinss[df$Clusters == Cluster & df$Model == Model & df$NDVI == "NDVI" & df$Site == Site] = output$totwithinss
        df$avgsil[df$Clusters == Cluster & df$Model == Model & df$NDVI == "NDVI" & df$Site == Site] = output$avg.sil
      }
    }
  }
  file = list.files("analysis/RTPV/",pattern = paste0("NEE_analysis_RTPV_",Site))
  load(paste0("analysis/RTPV/",file))
  SAM$R2[SAM$Site == Site] = output$SAM.R2
  df$R2[df$Site == Site & df$Model == "SAM"] = output$SAM.R2
}

# Box plot grouped by transect and NDVI
BoxPlot = ggplot(df[df$Model!="SAM",]) +
  geom_boxplot(aes(x = Transect, y = R2, fill = NDVI)) +
  scale_fill_viridis_d(guide = "none",
                      "Model",
                      begin = 0.2,
                      end = 0.8,
                      labels=c("No NDVI term","Includes NDVI")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        text = element_text(size=20),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(breaks= seq(0,1,by=0.1),
                     expand = c(0,0)) +
  ylab(expression(paste(R^2))) +
  xlab("") +
  #ggtitle(expression(paste("Model Performance (",""<=16," clusters)"))) +
  coord_cartesian(ylim = c(0,1)) 


BoxPlot

# Check whether including NDVI makes a difference
NATTttest = t.test(df$R2[df$Transect == "NATT" & df$NDVI == "NDVI" & df$Model!="SAM"],
                   df$R2[df$Transect == "NATT" & df$NDVI == "NoNDVI" & df$Model!="SAM"],
                   paired = TRUE)
SAWSttest = t.test(df$R2[df$Transect == "SAWS" & df$NDVI == "NDVI" & df$Model!="SAM"],
                   df$R2[df$Transect == "SAWS" & df$NDVI == "NoNDVI" & df$Model!="SAM"],
                   paired = TRUE)

if (NATTttest$p.value<0.05){
  message("The R2 values for the NATT are significantly different when NDVI is included")
} else {
  message("The R2 values for the NATT are NOT significantly different when NDVI is included")
}
if (SAWSttest$p.value<0.05){
  message("The R2 values for the SAWS are significantly different when NDVI is included")
} else {
  message("The R2 values for the SAWS are NOT significantly different when NDVI is included")
}


# Take mean and sd over sites and clusters
plot.df = df %>%
  group_by(Transect,NDVI,Model) %>%
  summarise(meanR2 = mean(R2),
            sdR2 = sd(R2))

plot.df$sdR2[plot.df$Model == "SAM"] = 0
# Plot the R2 per model, transect and NDVI
# Plot = ggplot(plot.df) +
#   geom_bar(aes(x = Model,y=meanR2, fill=NDVI),stat = "identity",position = "dodge") +
#   geom_errorbar(data = plot.df[plot.df$Model!="SAM",], aes(x = Model,ymin = meanR2-sdR2, ymax = meanR2+sdR2, group = NDVI),position = position_dodge(width = 1), width = 0.5) +
#   facet_grid(Transect~.) +
#   theme_bw() +
#   ylab("Mean R2") +
#   theme(panel.grid.major.x = element_blank(),
#         text = element_text(size=20)) +
#   scale_fill_viridis_d(begin=0.1,
#                        end = 0.9) +
#   coord_cartesian(ylim = c(0,1)) +
#   ggtitle("k-means Model Performance",
#           "Mean R2 with +/- 1 std. dev., averaged over sites and 2-16 clusters")
# Plot

#  Therefore it is clear that NDVI must be included in the k-means modelling
NDVI.df = df[df$NDVI=="NDVI",]

# Take mean and sd over clusters
plot.NDVIdf = NDVI.df %>%
  group_by(Model,Site) %>%
  summarise(meanR2 = mean(R2),
            sdR2 = sd(R2))
# Plot the R2 per site and model
Plot = ggplot(plot.NDVIdf) +
  geom_bar(aes(x = Model,y=meanR2, fill=Model),stat = "identity",position = "dodge") +
  geom_errorbar(data = plot.NDVIdf[plot.NDVIdf$Model!="SAM",],aes(x = Model,ymin = meanR2-sdR2, ymax = meanR2+sdR2, group = Model),position = position_dodge(width = 0.9), width = 0.5) +
  geom_text(aes(x = Model,y = 0.9, group = Model, label = round(meanR2,2)),position = position_dodge(width = 0.9)) +
  facet_grid(Site~.,scales="free") +
  theme_bw() +
  ylab("Mean R2") +
  theme(panel.grid.major.x = element_blank(),
        text = element_text(size=20)) +
  scale_fill_viridis_d(begin=0.1,
                       end = 0.9) +
  coord_cartesian(ylim = c(0,1)) +
  ggtitle("k-means with NDVI Model Performance",
          "Mean R2 with +/- 1 std. dev., averaged over 2-16 clusters")
Plot

# Limit to individual rainfall lags
Plot = ggplot(plot.NDVIdf[!plot.NDVIdf$Model%in%c("current","allPPT","alllags"),]) +
  geom_bar(aes(x = Model,y=meanR2, fill=Model),stat = "identity",position = "dodge") +
  geom_errorbar(data = plot.NDVIdf[!plot.NDVIdf$Model%in%c("current","allPPT","alllags","SAM"),],aes(x = Model,ymin = meanR2-sdR2, ymax = meanR2+sdR2, group = Model),position = position_dodge(width = 0.9), width = 0.5) +
  geom_text(aes(x = Model,y = 0.9, group = Model, label = round(meanR2,2)),position = position_dodge(width = 0.9)) +
  facet_grid(Site~.,scales="free") +
  theme_bw() +
  ylab("Mean R2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size=20)) +
  scale_y_continuous(breaks= seq(0,1,by=0.1),
                     expand = c(0,0)) +
  scale_fill_viridis_d(begin=0.1,
                       end = 0.9) +
  coord_cartesian(ylim = c(0,1)) +
  ggtitle("k-means with NDVI Model Performance",
          "Mean R2 with +/- 1 std. dev., averaged over 2-16 clusters")
Plot


# Plot boxplots for each site and model, limited to nCl clusters or less
nCl = 16
#SAMdf = NDVI.df[!NDVI.df$Model%in%c("allPPT","alllags") & NDVI.df$Clusters<=nCl,]
SAMdf = NDVI.df[NDVI.df$Clusters<=nCl,]
Plot = ggplot(SAMdf) +
      geom_boxplot(aes(x=Model,y=R2,fill=Model)) +
      geom_point(data=SAMdf[SAMdf$Model=="SAM",],aes(x=Model,y=R2,color=Model),shape=8,stroke=1) +
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
      scale_fill_viridis_d(name="k-means Models",
                           breaks=c("current",
                                    "PPT14-20",
                                    "PPT21-29",
                                    "PPT30-59",
                                    "PPT60-119",
                                    "PPT120-179",
                                    "PPT180-269",
                                    "PPT270-365",
                                    "allPPT",
                                    "alllags"),
                           labels = c("Current Climate",
                                      "+ 14-20 day PPT lag",
                                      "+ 21-29 day PPT lag",
                                      "+ 30-59 day PPT lag",
                                      "+ 60-119 day PPT lag",
                                      "+ 120-179 day PPT lag",
                                      "+ 180-269 day PPT lag",
                                      "+ 270-365 day PPT lag",
                                      "+ all PPT lags",
                                      "+ all climate lags")) +
      scale_color_manual(name="",
                         values = "red") +
      coord_cartesian(ylim = c(0,1)) +
      #ggtitle(paste0("k-means performance for each site and PPT lag, with NDVI and ",nCl," clusters or less")) +
      guides(fill = guide_legend(order = 1),colour = guide_legend(order = 2))


Plot


# Plot boxplots for each site and model, limited to nCl clusters or less
nCl = 4
alldf = NDVI.df[NDVI.df$Clusters<=nCl,]
Plot = ggplot(alldf) +
  geom_boxplot(aes(x=Model,y=R2,fill=Model)) +
  geom_point(data=alldf[alldf$Model=="SAM",],aes(x=Model,y=R2,color=Model),shape=8,stroke=1) +
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
  scale_fill_viridis_d(name="k-means Models",
                       breaks=c("current",
                                "PPT14-20",
                                "PPT21-29",
                                "PPT30-59",
                                "PPT60-119",
                                "PPT120-179",
                                "PPT180-269",
                                "PPT270-365",
                                "allPPT",
                                "alllags"),
                       labels = c("Current-only",
                                 "+ 14-20 day PPT lag",
                                 "+ 21-29 day PPT lag",
                                 "+ 30-59 day PPT lag",
                                 "+ 60-119 day PPT lag",
                                 "+ 120-179 day PPT lag",
                                 "+ 180-269 day PPT lag",
                                 "+ 270-365 day PPT lag",
                                 "+ all PPT lags",
                                 "+ all climate lags")) +
  scale_color_manual(name="",
                     values = "red") +
  coord_cartesian(ylim = c(0,1)) +
  guides(fill = guide_legend(order = 1),colour = guide_legend(order = 2)) +
  ggtitle(paste0("k-means performance for each site and lag effect, with NDVI and ",nCl," clusters or less"))

Plot
# # Plot boxplots for each site and model with nCl clusters
# nCl = 8
# Plot = ggplot(NDVI.df[!NDVI.df$Model%in%c("allPPT","alllags") & NDVI.df$Clusters==nCl,]) +
#   geom_segment(aes(x=Model,xend=Model,y=R2,yend=0,colour=Model),alpha=0.8) +
#   geom_point(aes(x=Model,y=R2,colour=Model,fill=Model),size = 2) +
#   facet_grid(.~Site) +
#   theme_bw() +
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         legend.position = "bottom",
#         text = element_text(size=20)) +
#   scale_y_continuous(breaks= seq(0,1,by=0.1),
#                      expand = c(0,0)) +
#   ylab("R2") +
#   xlab("") +
#   scale_colour_viridis_d(begin=0.1,
#                          end = 0.9) +
#   scale_fill_viridis_d(begin=0.1,
#                        end = 0.9) +
#   scale_shape_manual(values = c(25,8,15,3,16,4,18,17)) +
#   coord_cartesian(ylim = c(0,1)) +
#   ggtitle(paste0("R2 values for each site and rainfall lag, with NDVI and ",nCl," clusters")) +
#   guides(size="none") 
# Plot
