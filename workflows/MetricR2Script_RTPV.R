
# Make sure everything is clean
rm(list=ls())

# Load the required packages
library(tidyverse)
library(lubridate)
library(viridis)
library(ggpubr)
library(gridExtra)

# Decide which k-means models we want to show
Clusters = 2 

# List all sites
Sites = c("AU-ASM"
          ,"AU-Cpr"
          ,"AU-Cum"
          ,"AU-DaS"
          ,"AU-Dry"
          ,"AU-Gin"
          ,"AU-GWW"
          ,"AU-How"
          ,"AU-Stp"
          ,"AU-TTE"
          ,"AU-Tum"
          ,"AU-Whr"
          ,"AU-Wom"
)

# Assign transects
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
              "SAWS"
)

# Source worldclim correlations and climate metrics
load("site_data/SiteMetrics_worldclim_0.5res.Rdata")

# Initialise the dataframe of model R2 values
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

# Initialise the dataframe for the correlation values and p values
Correlations = data.frame("Metric" = colnames(WorldClimMetrics[-(1:4)]),
                          "AbsVal" = 0,
                          "AbsP" = 0,
                          "AbsImpVal" = 0,
                          "AbsImpP" = 0,
                          "RelImpVal" = 0,
                          "RelImpP" = 0,
                          "NATTAbsVal" = 0,
                          "NATTAbsP" = 0,
                          "NATTAbsImpVal" = 0,
                          "NATTAbsImpP" = 0,
                          "NATTRelImpVal" = 0,
                          "NATTRelImpP" = 0,
                          "SAWSAbsVal" = 0,
                          "SAWSAbsP" = 0,
                          "SAWSAbsImpVal" = 0,
                          "SAWSAbsImpP" = 0,
                          "SAWSRelImpVal" = 0,
                          "SAWSRelImpP" = 0)

# For each metric
for (i in 5:ncol(WorldClimMetrics)){
  
  # Calculate the correlations across every site
  # For absolute SAM R2 value
  metricR2 = cor.test(x = WorldClimMetrics[,i], 
                      y = R2$R2.SAM, 
                      method = "kendall")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[,i], 
                          y = R2$R2.SAM, 
                          method = "kendall")$p.value
  message("Absolute memory strength and ",
          colnames(WorldClimMetrics)[i],
          " are correlated with R2 value ",
          round(metricR2,3),
          " and p value ",
          round(metricPvalue,3))
  Correlations$AbsVal[i-4] = metricR2
  Correlations$AbsP[i-4] = metricPvalue
  # For absolute R2 improvement between SAM and current
  metricR2 = cor.test(x = WorldClimMetrics[,i], 
                      y = (R2$R2.SAM-R2$R2.CUR), 
                      method = "kendall")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[,i], 
                          y = (R2$R2.SAM-R2$R2.CUR), 
                          method = "kendall")$p.value
  message("Absolute memory improvement and ",
          colnames(WorldClimMetrics)[i],
          " are correlated with R2 value ",
          round(metricR2,3),
          " and p value ",
          round(metricPvalue,3))
  Correlations$AbsImpVal[i-4] = metricR2
  Correlations$AbsImpP[i-4] = metricPvalue
  # For relative R2 improvement between SAM and current
  metricR2 = cor.test(x = WorldClimMetrics[,i],
                      y = (R2$R2.SAM-R2$R2.CUR)/R2$R2.CUR,
                      method = "kendall")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[,i],
                          y = (R2$R2.SAM-R2$R2.CUR)/R2$R2.CUR,
                          method = "kendall")$p.value
  message("Relative memory improvement and ",
          colnames(WorldClimMetrics)[i],
          " are correlated with R2 value ",
          round(metricR2,3),
          " and p value ",
          round(metricPvalue,3),"\n")
  Correlations$RelImpVal[i-4] = metricR2
  Correlations$RelImpP[i-4] = metricPvalue
  
  # Calculate correlations for just NATT sites
  # For absolute SAM R2 value
  metricR2 = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="NATT",i],
                      y = (R2$R2.SAM[R2$Transect=="NATT"]),
                      method = "kendall")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="NATT",i],
                          y = (R2$R2.SAM[R2$Transect=="NATT"]),
                          method = "kendall")$p.value
  Correlations$NATTAbsVal[i-4] = metricR2
  Correlations$NATTAbsP[i-4] = metricPvalue
  # For absolute R2 improvement between SAM and current  
  metricR2 = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="NATT",i],
                      y = (R2$R2.SAM[R2$Transect=="NATT"]-R2$R2.CUR[R2$Transect=="NATT"]),
                      method = "kendall")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="NATT",i],
                          y = (R2$R2.SAM[R2$Transect=="NATT"]-R2$R2.CUR[R2$Transect=="NATT"]),
                          method = "kendall")$p.value
  Correlations$NATTAbsImpVal[i-4] = metricR2
  Correlations$NATTAbsImpP[i-4] = metricPvalue
  # For relative R2 improvement between SAM and current
  metricR2 = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="NATT",i],
                      y = (R2$R2.SAM[R2$Transect=="NATT"]-R2$R2.CUR[R2$Transect=="NATT"])/R2$R2.CUR[R2$Transect=="NATT"],
                      method = "kendall")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="NATT",i],
                          y = (R2$R2.SAM[R2$Transect=="NATT"]-R2$R2.CUR[R2$Transect=="NATT"])/R2$R2.CUR[R2$Transect=="NATT"],
                          method = "kendall")$p.value
  Correlations$NATTRelImpVal[i-4] = metricR2
  Correlations$NATTRelImpP[i-4] = metricPvalue
  
  
  # Calculate correlations for just SAWS sites
  # For absolute SAM R2 value
  metricR2 = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="SAWS",i],
                      y = (R2$R2.SAM[R2$Transect=="SAWS"]),
                      method = "kendall")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="SAWS",i],
                          y = (R2$R2.SAM[R2$Transect=="SAWS"]),
                          method = "kendall")$p.value
  Correlations$SAWSAbsVal[i-4] = metricR2
  Correlations$SAWSAbsP[i-4] = metricPvalue
  # For absolute R2 improvement between SAM and current  
  metricR2 = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="SAWS",i],
                      y = (R2$R2.SAM[R2$Transect=="SAWS"]-R2$R2.CUR[R2$Transect=="SAWS"]),
                      method = "kendall")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="SAWS",i],
                          y = (R2$R2.SAM[R2$Transect=="SAWS"]-R2$R2.CUR[R2$Transect=="SAWS"]),
                          method = "kendall")$p.value
  Correlations$SAWSAbsImpVal[i-4] = metricR2
  Correlations$SAWSAbsImpP[i-4] = metricPvalue
  # For relative R2 improvement between SAM and current
  metricR2 = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="SAWS",i],
                      y = (R2$R2.SAM[R2$Transect=="SAWS"]-R2$R2.CUR[R2$Transect=="SAWS"])/R2$R2.CUR[R2$Transect=="SAWS"],
                      method = "kendall")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="SAWS",i],
                          y = (R2$R2.SAM[R2$Transect=="SAWS"]-R2$R2.CUR[R2$Transect=="SAWS"])/R2$R2.CUR[R2$Transect=="SAWS"],
                          method = "kendall")$p.value
  Correlations$SAWSRelImpVal[i-4] = metricR2
  Correlations$SAWSRelImpP[i-4] = metricPvalue
}



# Plot the data!
AllPValue = data.frame("Metric" = Correlations$Metric,
                       "Absolute.R2" = Correlations$AbsP,
                       "Difference" = Correlations$AbsImpP,
                       "Relative.Improvement" = Correlations$RelImpP)
AllPValue = AllPValue %>% 
            pivot_longer(-Metric, names_to = "pvalues") %>% 
            group_by(pvalues) %>% 
            top_n(1, -value)
AllBestMetric = AllPValue$Metric[which.min(AllPValue$value)]
AllBestRelation = AllPValue$pvalues[which.min(AllPValue$value)]
# Create the plot dataframe
Site = rep(Sites,4)
Site = factor(Site, levels = WorldClimMetrics[order(WorldClimMetrics[[AllBestMetric]]),1])
Transect = rep(Transects,4)
Model = rep(c("Current Environment (k-means)",
              "Current Environment (SAM)",
              "Environmental Memory (SAM)",
              "Biological Memory (AR1)"),
            each=length(Sites))
Model = factor(Model,
               levels=c("Biological Memory (AR1)",
                        "Environmental Memory (SAM)",
                        "Current Environment (SAM)",
                        "Current Environment (k-means)"))

Value = c(R2$R2.KMC,R2$R2.CUR-R2$R2.KMC,R2$R2.SAM-R2$R2.CUR,R2$R2.AR1-R2$R2.SAM)

Fig = data.frame(Site,
                 Transect,
                 Model,
                 Value)


# Plot for every site based on best metric
Plot = ggplot(Fig,aes(fill=Model,y=Value,x=Site)) +
  geom_bar(position="stack",stat="identity") +
  geom_bar(stat = "identity", color = "black",size = 1) +
  coord_flip(ylim=c(0,1)) +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE)) +
  ylab(parse(text="R^2")) +
  theme_bw() +
  theme(legend.position = "bottom", 
        text = element_text(size = 20),
        legend.title = element_blank()) +
  ggtitle(paste0("Model Performance, ordered by ",AllBestMetric," Desc"),
          subtitle = AllBestRelation)

Plot

# Plot for just NATT based on mean daily temperature

NATTPValue = data.frame("Metric" = Correlations$Metric,
                       "Absolute.R2" = Correlations$NATTAbsP,
                       "Difference" = Correlations$NATTAbsImpP,
                       "Relative.Improvement" = Correlations$NATTRelImpP)
NATTPValue = NATTPValue %>% 
  pivot_longer(-Metric, names_to = "pvalues") %>% 
  group_by(pvalues) %>% 
  top_n(1, -value)
NATTBestMetric = NATTPValue$Metric[which.min(NATTPValue$value)]
NATTBestRelation = NATTPValue$pvalues[which.min(NATTPValue$value)]

Fig$Site = factor(Fig$Site, levels = WorldClimMetrics[order(WorldClimMetrics[[NATTBestMetric]]),1])

NATTPlot = ggplot(Fig[Fig$Transect=="NATT",],aes(fill=Model,y=Value,x=Site)) +
  geom_bar(position="stack",stat="identity") +
  geom_bar(stat = "identity", color = "black",size = 1) +
  coord_flip(ylim=c(0,1)) +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE)) +
  ylab(parse(text="R^2")) +
  theme_bw() +
  theme(legend.position = "bottom", 
        text = element_text(size = 20),
        legend.title = element_blank()) +
  ggtitle(paste0("NATT Model Performance, ordered by ",NATTBestMetric," Desc"),
          subtitle = NATTBestRelation)

NATTPlot


# Plot for just SAWS based on coefficient of variation of daily radiation
SAWSPValue = data.frame("Metric" = Correlations$Metric,
                        "Absolute.R2" = Correlations$SAWSAbsP,
                        "Difference" = Correlations$SAWSAbsImpP,
                        "Relative.Improvement" = Correlations$SAWSRelImpP)
SAWSPValue = SAWSPValue %>% 
  pivot_longer(-Metric, names_to = "pvalues") %>% 
  group_by(pvalues) %>% 
  top_n(1, -value)
SAWSBestMetric = SAWSPValue$Metric[which.min(SAWSPValue$value)] 
SAWSBestRelation = SAWSPValue$pvalues[which.min(SAWSPValue$value)]

Fig$Site = factor(Fig$Site, levels = WorldClimMetrics[order(WorldClimMetrics[[SAWSBestMetric]]),1])

SAWSPlot = ggplot(Fig[Fig$Transect=="SAWS",],aes(fill=Model,y=Value,x=Site)) +
  geom_bar(position="stack",stat="identity") +
  geom_bar(stat = "identity", color = "black",size = 1) +
  coord_flip(ylim=c(0,1)) +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE)) +
  ylab(parse(text="R^2")) +
  theme_bw() +
  theme(legend.position = "bottom", 
        text = element_text(size = 20),
        legend.title = element_blank()) +
  ggtitle(paste0("SAWS Model Performance, ordered by ",SAWSBestMetric," Desc"),
          subtitle = SAWSBestRelation)

SAWSPlot


# Scatter plot the metrics vs memory to show the correlations

Cor.df = cbind(WorldClimMetrics,R2[,-(1:2)])
Cor.df$RelDif = (Cor.df$R2.SAM-Cor.df$R2.CUR)/Cor.df$R2.CUR
Cor.df$AbsDif = (Cor.df$R2.SAM-Cor.df$R2.CUR)

# Calculate the linear trend line
MAPAllLine = lm(Cor.df$AbsDif ~ Cor.df$AnnualPPT)$coefficients
MAPNATTLine = lm(Cor.df$AbsDif[Cor.df$Transect=="NATT"] ~ Cor.df$AnnualPPT[Cor.df$Transect=="NATT"])$coefficients
MAPSAWSLine =lm(Cor.df$AbsDif[Cor.df$Transect=="SAWS"] ~ Cor.df$AnnualPPT[Cor.df$Transect=="SAWS"])$coefficients

# Find the range of NATT and SAWS x values
MAPNATT = Cor.df$AnnualPPT[Cor.df$Transect=="NATT"]
MAPSAWS = Cor.df$AnnualPPT[Cor.df$Transect=="SAWS"]

# Plot for all sites
AllCorPlot = ggplot(Cor.df,aes(color = Transect,x=AnnualPPT, y = AbsDif)) +
            geom_point(aes(),size = 3) +
            scale_color_viridis_d(guide = guide_legend(reverse = TRUE),
                                  option="magma",
                                  begin=0.2,
                                  end=0.8) +
            geom_line(aes(x = AnnualPPT, y = MAPAllLine[1] + MAPAllLine[2]*AnnualPPT),
                      color = magma(1,1,0.5,0.5),
                      size = 1) + 
            geom_line(aes(x = seq(min(MAPNATT),
                                  max(MAPNATT),
                                  length.out = nrow(Cor.df)), 
                          y = MAPNATTLine[1] + MAPNATTLine[2]*seq(min(MAPNATT),
                                                                  max(MAPNATT),
                                                                  length.out = nrow(Cor.df))),
                      color = magma(1,1,0.2,0.2),
                      linetype = "dashed",
                      size = 1) + 
            geom_line(aes(x = seq(min(MAPSAWS),
                                  max(MAPSAWS),
                                  length.out = nrow(Cor.df)), 
                          y = MAPSAWSLine[1] + MAPSAWSLine[2]*seq(min(MAPSAWS),
                                                                  max(MAPSAWS),
                                                                  length.out = nrow(Cor.df))),
                      color = magma(1,1,0.8,0.8),
                      linetype = "dashed",
                      size = 1) + 
            theme_bw() +
            ylab("Absolute Improvement from Ecological Memory") +
            xlab("Mean Annual Precipitation") +
            theme(text = element_text(size = 20)) +
            ggtitle(label = paste0("OzFlux Memory Improvement"),
                    subtitle=expression(paste("Spearman's ",rho," = -0.64, p-value = 0.022")))

AllCorPlot

# Plot for NATT sites

# Estimate trendlines
PPTSeaAllLine = lm(Cor.df$RelDif ~ WorldClim$Metrics$PPTSeasonality)$coefficients
PPTSeaNATTLine = lm(Cor.df$RelDif[Cor.df$Transect=="NATT"] ~ WorldClim$Metrics$PPTSeasonality[WorldClim$Metrics$Transect=="NATT"])$coefficients
PPTSeaSAWSLine =lm(Cor.df$RelDif[Cor.df$Transect=="SAWS"] ~ WorldClim$Metrics$PPTSeasonality[WorldClim$Metrics$Transect=="SAWS"])$coefficients

# Find the range of NATT and SAWS x values
PPTSeaNATT = WorldClim$Metrics$PPTSeasonality[Cor.df$Transect=="NATT"]
PPTSeaSAWS = WorldClim$Metrics$PPTSeasonality[Cor.df$Transect=="SAWS"]

NATTCorPlot = ggplot(Cor.df,aes(color = Transect,x=WorldClim$Metrics$PPTSeasonality, y = RelDif)) +
              geom_point(aes(),size = 3) +
              scale_color_viridis_d(guide = guide_legend(reverse = TRUE),
                                    option="magma",
                                    begin=0.2,
                                    end=0.8) +
              geom_line(aes(x = WorldClim$Metrics$PPTSeasonality, y = PPTSeaAllLine[1] + PPTSeaAllLine[2]*WorldClim$Metrics$PPTSeasonality),
                        color = magma(1,1,0.5,0.5),
                        linetype = "dashed",
                        size = 1) + 
              geom_line(aes(x = seq(min(PPTSeaNATT),
                                    max(PPTSeaNATT),
                                    length.out = nrow(Cor.df)), 
                            y = PPTSeaNATTLine[1] + PPTSeaNATTLine[2]*seq(min(PPTSeaNATT),
                                                                      max(PPTSeaNATT),
                                                                      length.out = nrow(Cor.df))),
                        color = magma(1,1,0.2,0.2),
                        size = 1) + 
              geom_line(aes(x = seq(min(PPTSeaSAWS),
                                    max(PPTSeaSAWS),
                                    length.out = nrow(Cor.df)), 
                            y = PPTSeaSAWSLine[1] + PPTSeaSAWSLine[2]*seq(min(PPTSeaSAWS),
                                                                      max(PPTSeaSAWS),
                                                                      length.out = nrow(Cor.df))),
                        color = magma(1,1,0.8,0.8),
                        linetype = "dashed",
                        size = 1) + 
              theme_bw() +
              ylab("Relative Improvement from Ecological Memory") +
              xlab("Precipitation Seasonality (Coefficient of Variation)") +
              theme(text = element_text(size = 20)) +
              ggtitle(label = paste0("NATT Memory Improvement"),
                      subtitle=expression(paste("Pearson's r = 0.87, p-value = 0.025")))

NATTCorPlot

# Plot for SAWS sites

# Estimate trendlines
MATAllLine = lm(Cor.df$RelDif ~ WorldClim$Metrics$AnnualMeanTemp)$coefficients
MATNATTLine = lm(Cor.df$RelDif[Cor.df$Transect=="NATT"] ~ WorldClim$Metrics$AnnualMeanTemp[WorldClim$Metrics$Transect=="NATT"])$coefficients
MATSAWSLine = lm(Cor.df$RelDif[Cor.df$Transect=="SAWS"] ~ WorldClim$Metrics$AnnualMeanTemp[WorldClim$Metrics$Transect=="SAWS"])$coefficients

# Find the range of NATT and SAWS x values
MATNATT = WorldClim$Metrics$AnnualMeanTemp[Cor.df$Transect=="NATT"]
MATSAWS = WorldClim$Metrics$AnnualMeanTemp[Cor.df$Transect=="SAWS"]


SAWSCorPlot = ggplot(Cor.df,aes(color = Transect,x=WorldClim$Metrics$AnnualMeanTemp, y = RelDif)) +
              geom_point(aes(),size = 3) +
              scale_color_viridis_d(guide = guide_legend(reverse = TRUE),
                                    option="magma",
                                    begin=0.2,
                                    end=0.8) +
              geom_line(aes(x = WorldClim$Metrics$AnnualMeanTemp, y = MATAllLine[1] + MATAllLine[2]*WorldClim$Metrics$AnnualMeanTemp),
                        color = magma(1,1,0.5,0.5),
                        linetype = "dashed",
                        size = 1) + 
              geom_line(aes(x = seq(min(MATNATT),
                                    max(MATNATT),
                                    length.out = nrow(Cor.df)), 
                            y = MATNATTLine[1] + MATNATTLine[2]*seq(min(MATNATT),
                                                                      max(MATNATT),
                                                                      length.out = nrow(Cor.df))),
                        color = magma(1,1,0.2,0.2),
                        linetype = "dashed",
                        size = 1) + 
              geom_line(aes(x = seq(min(MATSAWS),
                                    max(MATSAWS),
                                    length.out = nrow(Cor.df)), 
                            y = MATSAWSLine[1] + MATSAWSLine[2]*seq(min(MATSAWS),
                                                                      max(MATSAWS),
                                                                      length.out = nrow(Cor.df))),
                        color = magma(1,1,0.8,0.8),
                        size = 1) + 
              theme_bw() +
              theme(legend.position="bottom") +
              ylab("Relative Improvement from Ecological Memory") +
              xlab("Annual Mean Temperature") +
              theme(text = element_text(size = 20)) +
              ggtitle(label = paste0("SAWS Memory Improvement"),
                      subtitle=expression(paste("Pearson's r = 0.84, p-value = 0.017")))

SAWSCorPlot

# Write some text
text = paste("Relationships between the relative memory improvement",
             "i.e. (SAM r^2 - Current r^2)/Current r^2",
             "and climate metrics for the NATT, the SAWS, and all sites",
             "combined. Solid lines indicate significance at p = 0.05",
             "levels, dashed lines indicate non-significance.",
             "Subtitles provide correlation coefficients",
             "for the significant relationships.", sep = "\n")
text.p <- text_grob(text, face = "italic", size = 20, color = "black")

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend = get_legend(SAWSCorPlot)

AllCorPlot = AllCorPlot + theme(legend.position="none")
NATTCorPlot = NATTCorPlot + theme(legend.position="none")
SAWSCorPlot = SAWSCorPlot + theme(legend.position="none")

figure = grid.arrange(AllCorPlot, NATTCorPlot, SAWSCorPlot, text.p, legend,
                      layout_matrix = rbind(c(1,2), c(3,4),c(5,5)),
                       ncol = 2, 
                       nrow = 3,
                      widths = c(2.7, 2.7), 
                      heights = c(2.5, 2.5, 0.2))


figure
