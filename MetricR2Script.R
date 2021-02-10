
# Make sure everything is clean
rm(list=ls())

# Load the required packages
library(tidyverse)
library(lubridate)
library(viridis)
library(ggpubr)
library(gridExtra)

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

Transects = c("NATT",
              "SWAT",
              "SWAT",
              "NATT",
              "NATT",
              "SWAT",
              "SWAT",
              "NATT",
              "NATT",
              "NATT",
              "SWAT",
              "SWAT",
              "SWAT"
)

# Initiliase metrics df
metrics = data.frame("Site" = Sites,
                     "Transect" = Transects,
                     "MDP" = 0,
                     "MMP" = 0,
                     "MYP" = 0,
                     "MDT" = 0,
                     "MMT" = 0,
                     "MYT" = 0,
                     "CVDN" = 0,
                     "CVMN" = 0,
                     "CVYN" = 0,
                     "CVDR" = 0,
                     "CVMR" = 0,
                     "CVYR" = 0,
                     "CVDT" = 0,
                     "CVMT" = 0,
                     "CVYT" = 0,
                     "CVDP" = 0,
                     "CVMP" = 0,
                     "CVYP" = 0,
                     "CVDV" = 0,
                     "CVMV" = 0,
                     "CVYV" = 0,
                     "CVDS" = 0,
                     "CVMS" = 0,
                     "CVYS" = 0)

R2 = data.frame("Site" = Sites,
               "Transect" = Transects,
                "R2.CUR" = 0,
                "R2.SAM" = 0,
                "R2.AR1" = 0,
                "R2.KMP" = 0,
                "R2.KMC" = 0)

# For each site
for (Site in Sites){
  # Let the user know which site the function is looking at
  message("*** Analysing climate metrics for ",Site," ***")
  # Load the input and extract each climate variable
  load(paste0("inputs/RTPVS/",Site,"_Input_RTPVS.Rdata"))

  DailyData = eval(as.name(paste0(Site,"_Input")))$DailyData
  # Retime to monthly and yearly data
  MonthlyData = DailyData %>%
                group_by(year(TIMESTAMP),month(TIMESTAMP)) %>%               
                summarise(NEE = sum(NEE,na.rm=TRUE),
                          Fsd=mean(Fsd,na.rm=TRUE),
                          Ta=mean(Ta,na.rm=TRUE),
                          VPD=mean(VPD,na.rm=TRUE),
                          Sws=mean(Sws,na.rm=TRUE),
                          Precip=sum(Precip,na.rm=TRUE))
  
  YearlyData = DailyData %>%
    group_by(year(TIMESTAMP)) %>%               
    summarise(NEE = sum(NEE,na.rm=TRUE),
              Fsd=mean(Fsd,na.rm=TRUE),
              Ta=mean(Ta,na.rm=TRUE),
              VPD=mean(VPD,na.rm=TRUE),
              Sws=mean(Sws,na.rm=TRUE),
              Precip=sum(Precip,na.rm=TRUE))
  
  
  # Find the coefficient of variation for various values
  metrics$MDP[metrics$Site==Site] = mean(DailyData$Precip,na.rm=TRUE)
  metrics$MMP[metrics$Site==Site] = mean(MonthlyData$Precip)
  metrics$MYP[metrics$Site==Site] = mean(YearlyData$Precip)
  metrics$MDT[metrics$Site==Site] = mean(DailyData$Ta,na.rm=TRUE)
  metrics$MMT[metrics$Site==Site] = mean(MonthlyData$Ta)
  metrics$MYT[metrics$Site==Site] = mean(YearlyData$Ta)
  metrics$CVDP[metrics$Site==Site] = sd(DailyData$Precip,na.rm=TRUE)/mean(DailyData$Precip,na.rm=TRUE)
  metrics$CVMP[metrics$Site==Site] = sd(MonthlyData$Precip)/mean(MonthlyData$Precip)
  metrics$CVYP[metrics$Site==Site] = sd(YearlyData$Precip)/mean(YearlyData$Precip)
  metrics$CVDT[metrics$Site==Site] = sd(DailyData$Ta,na.rm=TRUE)/mean(DailyData$Ta,na.rm=TRUE)
  metrics$CVMT[metrics$Site==Site] = sd(MonthlyData$Ta)/mean(MonthlyData$Ta)
  metrics$CVYT[metrics$Site==Site] = sd(YearlyData$Ta)/mean(YearlyData$Ta)
  metrics$CVDN[metrics$Site==Site] = sd(DailyData$NEE,na.rm=TRUE)/abs(mean(DailyData$NEE,na.rm=TRUE))
  metrics$CVMN[metrics$Site==Site] = sd(MonthlyData$NEE)/abs(mean(MonthlyData$NEE))
  metrics$CVYN[metrics$Site==Site] = sd(YearlyData$NEE)/abs(mean(YearlyData$NEE))
  metrics$CVDV[metrics$Site==Site] = sd(DailyData$VPD,na.rm=TRUE)/mean(DailyData$VPD,na.rm=TRUE)
  metrics$CVMV[metrics$Site==Site] = sd(MonthlyData$VPD)/mean(MonthlyData$VPD)
  metrics$CVYV[metrics$Site==Site] = sd(YearlyData$VPD)/mean(YearlyData$VPD)
  metrics$CVDR[metrics$Site==Site] = sd(DailyData$Fsd,na.rm=TRUE)/mean(DailyData$Fsd,na.rm=TRUE)
  metrics$CVMR[metrics$Site==Site] = sd(MonthlyData$Fsd)/mean(MonthlyData$Fsd)
  metrics$CVYR[metrics$Site==Site] = sd(YearlyData$Fsd)/mean(YearlyData$Fsd)
  metrics$CVDS[metrics$Site==Site] = sd(DailyData$Sws,na.rm=TRUE)/mean(DailyData$Sws,na.rm=TRUE)
  metrics$CVMS[metrics$Site==Site] = sd(MonthlyData$Sws)/mean(MonthlyData$Sws)
  metrics$CVYS[metrics$Site==Site] = sd(YearlyData$Sws)/mean(YearlyData$Sws)
  
  # Clean up
  rm(list=c("YearlyData","MonthlyData","DailyData"))
  
  # Collect the R2 values from the analysis scripts
  message("Collating R2 values for ",Site)
  
  # Load the analysis results
  load(paste0("analysis/RTPVS/NEE_Analysis_RTPVS_",Site,".Rdata"))
  R2$R2.SAM[R2$Site==Site] = output$SAM.R2
  load(paste0("analysis/RTPVS/NEE_current_Analysis_RTPVS_",Site,".Rdata"))
  R2$R2.CUR[R2$Site==Site] = output$CUR.R2
  load(paste0("analysis/RTPVS/NEE_analysis_AR1_RTPVS_",Site,".Rdata"))
  R2$R2.AR1[R2$Site==Site] = output$AR1.R2
  load(paste0("alternate/RTPVS/results/NEE_output_kmean_RTPVS_",Site,".Rdata"))
  R2$R2.KMP[R2$Site==Site] = output$r.squared
  load(paste0("alternate/RTPVS/results/NEE_output_kmean_current_RTPVS_",Site,".Rdata"))
  R2$R2.KMC[R2$Site==Site] = output$r.squared
}

Correlations = data.frame("Metric" = colnames(metrics[-(1:2)]),
                          "AbsVal" = 0,
                          "AbsP" = 0,
                          "RelVal" = 0,
                          "RelP" = 0)

for (i in 3:ncol(metrics)){
  metricR2 = cor.test(x = metrics[,i], 
                      y = (R2$R2.SAM-R2$R2.CUR), 
                      method = "spearman")$estimate
  metricPvalue = cor.test(x = metrics[,i], 
                          y = (R2$R2.SAM-R2$R2.CUR), 
                          method = "spearman")$p.value
  message("Absolute memory strength and ",
          colnames(metrics)[i],
          " are correlated with R2 value ",
          round(metricR2,3),
          " and p value ",
          round(metricPvalue,3))
  Correlations$AbsVal[i-2] = metricR2
  Correlations$AbsP[i-2] = metricPvalue
  
  metricR2 = cor.test(x = metrics[,i],
                      y = (R2$R2.SAM-R2$R2.CUR)/R2$R2.CUR,
                      method = "spearman")$estimate
  metricPvalue = cor.test(x = metrics[,i],
                          y = (R2$R2.SAM-R2$R2.CUR)/R2$R2.CUR,
                          method = "spearman")$p.value
  message("Relative memory strength and ",
          colnames(metrics)[i],
          " are correlated with R2 value ",
          round(metricR2,3),
          " and p value ",
          round(metricPvalue,3),"\n")
  Correlations$RelVal[i-2] = metricR2
  Correlations$RelP[i-2] = metricPvalue
}



# Plot the data!


# Create the plot dataframe
Site = rep(Sites,4)
Site = factor(Site, levels = metrics[order(metrics$CVDT),1])
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


# Plot for every site based on coefficient of variation of daily temperature
Plot = ggplot(Fig,aes(fill=Model,y=Value,x=Site)) +
  geom_bar(position="stack",stat="identity") +
  geom_bar(stat = "identity", color = "black",size = 1) +
  #annotate("text", 
  #         x = Sites, 
  #         y=0.875, 
  #         label = paste0(round(metrics$CVDT,3))) +
  #annotate("text", 
  #         x = Sites, 
  #         y=1, 
  #         label = paste0(round((R2$R2.SAM-R2$R2.CUR)/R2$R2.CUR,3)*100,"%")) +
  coord_flip(ylim=c(0,1)) +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE)) +
  ylab(parse(text="R^2")) +
  theme_bw() +
  theme(legend.position = "bottom", 
        text = element_text(size = 20),
        legend.title = element_blank()) +
  ggtitle("Model Performance, ordered by Daily Temperature Variance Desc")

Plot

# Source worldclim correlations and climate metrics
load("analysis/RTPVS/worldclim_biovar_0.5res_RTPVS_correlations.Rdata")

# Plot for just NATT based on mean daily temperature
Fig$Site = factor(Fig$Site, levels = WorldClim$Metrics[order(WorldClim$Metrics$PPTSeasonality),1])

NATTPlot = ggplot(Fig[Fig$Transect=="NATT",],aes(fill=Model,y=Value,x=Site)) +
  geom_bar(position="stack",stat="identity") +
  geom_bar(stat = "identity", color = "black",size = 1) +
  # annotate("text", 
  #          x = unique(Fig$Site[Fig$Transect=="NATT"]), 
  #          y=0.875, 
  #          label = paste0(round(metrics$MDT[metrics$Transect=="NATT"],1))) +
  # annotate("text", 
  #          x = unique(Fig$Site[Fig$Transect=="NATT"]), 
  #          y=1, 
  #          label = paste0(round((R2$R2.SAM[R2$Transect=="NATT"]-R2$R2.CUR[R2$Transect=="NATT"])/R2$R2.CUR[R2$Transect=="NATT"],3)*100,"%")) +
  coord_flip(ylim=c(0,1)) +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE)) +
  ylab(parse(text="R^2")) +
  theme_bw() +
  theme(legend.position = "bottom", 
        text = element_text(size = 20),
        legend.title = element_blank()) +
  ggtitle("NATT Model Performance, ordered by PPT Seasonality Desc")

NATTPlot


# Plot for just SWAT based on coefficient of variation of daily radiation
Fig$Site = factor(Fig$Site, levels = WorldClim$Metrics[order(WorldClim$Metrics$AnnualMeanTemp),1])

SWATPlot = ggplot(Fig[Fig$Transect=="SWAT",],aes(fill=Model,y=Value,x=Site)) +
  geom_bar(position="stack",stat="identity") +
  geom_bar(stat = "identity", color = "black",size = 1) +
  # annotate("text",
  #          x = unique(Fig$Site[Fig$Transect=="SWAT"]), 
  #          y=0.875, 
  #          label = paste0(round(metrics$CVDR[metrics$Transect=="SWAT"],2))) +
  # annotate("text", 
  #          x = unique(Fig$Site[Fig$Transect=="SWAT"]), 
  #          y=1, 
  #          label = paste0(round((R2$R2.SAM[R2$Transect=="SWAT"]-R2$R2.CUR[R2$Transect=="SWAT"])/R2$R2.CUR[R2$Transect=="SWAT"],3)*100,"%")) +
  coord_flip(ylim=c(0,1)) +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE)) +
  ylab(parse(text="R^2")) +
  theme_bw() +
  theme(legend.position = "bottom", 
        text = element_text(size = 20),
        legend.title = element_blank()) +
  ggtitle("SWAT Model Performance, ordered by Annual Mean Temperature Desc")

SWATPlot


# Scatter plot the metrics vs memory to show the correlations

Cor.df = cbind(metrics,R2[,-(1:2)])
Cor.df$RelDif = (Cor.df$R2.SAM-Cor.df$R2.CUR)/Cor.df$R2.CUR

# Calculate the linear trend line
CVDTAllLine = lm(Cor.df$RelDif ~ Cor.df$CVDT)$coefficients
CVDTNATTLine = lm(Cor.df$RelDif[Cor.df$Transect=="NATT"] ~ Cor.df$CVDT[Cor.df$Transect=="NATT"])$coefficients
CVDTSWATLine =lm(Cor.df$RelDif[Cor.df$Transect=="SWAT"] ~ Cor.df$CVDT[Cor.df$Transect=="SWAT"])$coefficients

# Find the range of NATT and SWAT x values
CVDTNATT = Cor.df$CVDT[Cor.df$Transect=="NATT"]
CVDTSWAT = Cor.df$CVDT[Cor.df$Transect=="SWAT"]

# Plot for all sites
AllCorPlot = ggplot(Cor.df,aes(color = Transect,x=CVDT, y = RelDif)) +
            geom_point(aes(),size = 3) +
            scale_color_viridis_d(guide = guide_legend(reverse = TRUE),
                                  option="magma",
                                  begin=0.2,
                                  end=0.8) +
            geom_line(aes(x = CVDT, y = CVDTAllLine[1] + CVDTAllLine[2]*CVDT),
                      color = magma(1,1,0.5,0.5),
                      size = 1) + 
            geom_line(aes(x = seq(min(CVDTNATT),
                                  max(CVDTNATT),
                                  length.out = nrow(Cor.df)), 
                          y = CVDTNATTLine[1] + CVDTNATTLine[2]*seq(min(CVDTNATT),
                                                                  max(CVDTNATT),
                                                                  length.out = nrow(Cor.df))),
                      color = magma(1,1,0.2,0.2),
                      linetype = "dashed",
                      size = 1) + 
            geom_line(aes(x = seq(min(CVDTSWAT),
                                  max(CVDTSWAT),
                                  length.out = nrow(Cor.df)), 
                          y = CVDTSWATLine[1] + CVDTSWATLine[2]*seq(min(CVDTSWAT),
                                                                  max(CVDTSWAT),
                                                                  length.out = nrow(Cor.df))),
                      color = magma(1,1,0.8,0.8),
                      linetype = "dashed",
                      size = 1) + 
            theme_bw() +
            ylab("Relative Improvement from Ecological Memory") +
            xlab("Coefficient of Variation of Daily Temperature") +
            theme(text = element_text(size = 20)) +
            ggtitle(label = paste0("OzFlux Memory Improvement"),
                    subtitle=expression(paste("Spearman's ",rho," = -0.64, p-value = 0.022")))

AllCorPlot

# Plot for NATT sites

# Estimate trendlines
PPTSeaAllLine = lm(Cor.df$RelDif ~ WorldClim$Metrics$PPTSeasonality)$coefficients
PPTSeaNATTLine = lm(Cor.df$RelDif[Cor.df$Transect=="NATT"] ~ WorldClim$Metrics$PPTSeasonality[WorldClim$Metrics$Transect=="NATT"])$coefficients
PPTSeaSWATLine =lm(Cor.df$RelDif[Cor.df$Transect=="SWAT"] ~ WorldClim$Metrics$PPTSeasonality[WorldClim$Metrics$Transect=="SWAT"])$coefficients

# Find the range of NATT and SWAT x values
PPTSeaNATT = WorldClim$Metrics$PPTSeasonality[Cor.df$Transect=="NATT"]
PPTSeaSWAT = WorldClim$Metrics$PPTSeasonality[Cor.df$Transect=="SWAT"]

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
              geom_line(aes(x = seq(min(PPTSeaSWAT),
                                    max(PPTSeaSWAT),
                                    length.out = nrow(Cor.df)), 
                            y = PPTSeaSWATLine[1] + PPTSeaSWATLine[2]*seq(min(PPTSeaSWAT),
                                                                      max(PPTSeaSWAT),
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

# Plot for SWAT sites

# Estimate trendlines
MATAllLine = lm(Cor.df$RelDif ~ WorldClim$Metrics$AnnualMeanTemp)$coefficients
MATNATTLine = lm(Cor.df$RelDif[Cor.df$Transect=="NATT"] ~ WorldClim$Metrics$AnnualMeanTemp[WorldClim$Metrics$Transect=="NATT"])$coefficients
MATSWATLine = lm(Cor.df$RelDif[Cor.df$Transect=="SWAT"] ~ WorldClim$Metrics$AnnualMeanTemp[WorldClim$Metrics$Transect=="SWAT"])$coefficients

# Find the range of NATT and SWAT x values
MATNATT = WorldClim$Metrics$AnnualMeanTemp[Cor.df$Transect=="NATT"]
MATSWAT = WorldClim$Metrics$AnnualMeanTemp[Cor.df$Transect=="SWAT"]


SWATCorPlot = ggplot(Cor.df,aes(color = Transect,x=WorldClim$Metrics$AnnualMeanTemp, y = RelDif)) +
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
              geom_line(aes(x = seq(min(MATSWAT),
                                    max(MATSWAT),
                                    length.out = nrow(Cor.df)), 
                            y = MATSWATLine[1] + MATSWATLine[2]*seq(min(MATSWAT),
                                                                      max(MATSWAT),
                                                                      length.out = nrow(Cor.df))),
                        color = magma(1,1,0.8,0.8),
                        size = 1) + 
              theme_bw() +
              theme(legend.position="bottom") +
              ylab("Relative Improvement from Ecological Memory") +
              xlab("Annual Mean Temperature") +
              theme(text = element_text(size = 20)) +
              ggtitle(label = paste0("SWAT Memory Improvement"),
                      subtitle=expression(paste("Pearson's r = 0.84, p-value = 0.017")))

SWATCorPlot

# Write some text
text = paste("Relationships between the relative memory improvement",
             "i.e. (SAM r^2 - Current r^2)/Current r^2",
             "and climate metrics for the NATT, the SWAT, and all sites",
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

legend = get_legend(SWATCorPlot)

AllCorPlot = AllCorPlot + theme(legend.position="none")
NATTCorPlot = NATTCorPlot + theme(legend.position="none")
SWATCorPlot = SWATCorPlot + theme(legend.position="none")

figure = grid.arrange(AllCorPlot, NATTCorPlot, SWATCorPlot, text.p, legend,
                      layout_matrix = rbind(c(1,2), c(3,4),c(5,5)),
                       ncol = 2, 
                       nrow = 3,
                      widths = c(2.7, 2.7), 
                      heights = c(2.5, 2.5, 0.2))


figure
