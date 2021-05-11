
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
          #,"AU-Cpr"
          ,"AU-Cum"
          ,"AU-DaS"
          ,"AU-Dry"
          ,"AU-Gin"
          ,"AU-GWW"
          ,"AU-How"
          ,"AU-Stp"
          ,"AU-TTE"
          ,"AU-Tum"
          #,"AU-Whr"
          ,"AU-Wom"
)

# Assign transects
Transects = c("NATT",
              #"SAWS",
              "SAWS",
              "NATT",
              "NATT",
              "SAWS",
              "SAWS",
              "NATT",
              "NATT",
              "NATT",
              "SAWS",
              #"SAWS",
              "SAWS"
)

# Source worldclim correlations and climate metrics
load("site_data/SiteMetrics_worldclim_0.5res.Rdata")
WorldClimMetrics = WorldClimMetrics[WorldClimMetrics$Sites %in% Sites,]

# Initialise the dataframe of model R2 values
R2 = data.frame("Site" = Sites,
                "Transect" = Transects,
                "R2.CUR" = 0,
                "R2.SAM" = 0,
                "R2.AR1" = 0)

# For each site
for (Site in Sites){
  
  # Collect the R2 values from the analysis scripts
  message("Collating R2 values for ",Site)
  
  # Load the analysis results
  File = list.files("analysis/RTPV/",pattern = paste0("LE_analysis_RTPV_",Site))
  load(paste0("analysis/RTPV/",File))
  R2$R2.SAM[R2$Site==Site] = output$SAM.R2
  
  File = list.files("analysis/RTPV/",pattern = paste0("LE_current_analysis_RTPV_",Site))
  load(paste0("analysis/RTPV/",File))
  R2$R2.CUR[R2$Site==Site] = output$CUR.R2
  
  File = list.files("analysis/RTPV/",pattern = paste0("LE_AR1_analysis_RTPV_",Site))
  load(paste0("analysis/RTPV/",File))
  R2$R2.AR1[R2$Site==Site] = output$AR1.R2
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
                      method = "spearman")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[,i], 
                          y = R2$R2.SAM, 
                          method = "spearman")$p.value
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
                      method = "spearman")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[,i], 
                          y = (R2$R2.SAM-R2$R2.CUR), 
                          method = "spearman")$p.value
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
                      method = "spearman")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[,i],
                          y = (R2$R2.SAM-R2$R2.CUR)/R2$R2.CUR,
                          method = "spearman")$p.value
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
                      method = "spearman")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="NATT",i],
                          y = (R2$R2.SAM[R2$Transect=="NATT"]),
                          method = "spearman")$p.value
  Correlations$NATTAbsVal[i-4] = metricR2
  Correlations$NATTAbsP[i-4] = metricPvalue
  # For absolute R2 improvement between SAM and current  
  metricR2 = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="NATT",i],
                      y = (R2$R2.SAM[R2$Transect=="NATT"]-R2$R2.CUR[R2$Transect=="NATT"]),
                      method = "spearman")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="NATT",i],
                          y = (R2$R2.SAM[R2$Transect=="NATT"]-R2$R2.CUR[R2$Transect=="NATT"]),
                          method = "spearman")$p.value
  Correlations$NATTAbsImpVal[i-4] = metricR2
  Correlations$NATTAbsImpP[i-4] = metricPvalue
  # For relative R2 improvement between SAM and current
  metricR2 = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="NATT",i],
                      y = (R2$R2.SAM[R2$Transect=="NATT"]-R2$R2.CUR[R2$Transect=="NATT"])/R2$R2.CUR[R2$Transect=="NATT"],
                      method = "spearman")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="NATT",i],
                          y = (R2$R2.SAM[R2$Transect=="NATT"]-R2$R2.CUR[R2$Transect=="NATT"])/R2$R2.CUR[R2$Transect=="NATT"],
                          method = "spearman")$p.value
  Correlations$NATTRelImpVal[i-4] = metricR2
  Correlations$NATTRelImpP[i-4] = metricPvalue
  
  
  # Calculate correlations for just SAWS sites
  # For absolute SAM R2 value
  metricR2 = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="SAWS",i],
                      y = (R2$R2.SAM[R2$Transect=="SAWS"]),
                      method = "spearman")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="SAWS",i],
                          y = (R2$R2.SAM[R2$Transect=="SAWS"]),
                          method = "spearman")$p.value
  Correlations$SAWSAbsVal[i-4] = metricR2
  Correlations$SAWSAbsP[i-4] = metricPvalue
  # For absolute R2 improvement between SAM and current  
  metricR2 = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="SAWS",i],
                      y = (R2$R2.SAM[R2$Transect=="SAWS"]-R2$R2.CUR[R2$Transect=="SAWS"]),
                      method = "spearman")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="SAWS",i],
                          y = (R2$R2.SAM[R2$Transect=="SAWS"]-R2$R2.CUR[R2$Transect=="SAWS"]),
                          method = "spearman")$p.value
  Correlations$SAWSAbsImpVal[i-4] = metricR2
  Correlations$SAWSAbsImpP[i-4] = metricPvalue
  # For relative R2 improvement between SAM and current
  metricR2 = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="SAWS",i],
                      y = (R2$R2.SAM[R2$Transect=="SAWS"]-R2$R2.CUR[R2$Transect=="SAWS"])/R2$R2.CUR[R2$Transect=="SAWS"],
                      method = "spearman")$estimate
  metricPvalue = cor.test(x = WorldClimMetrics[WorldClimMetrics$Transect=="SAWS",i],
                          y = (R2$R2.SAM[R2$Transect=="SAWS"]-R2$R2.CUR[R2$Transect=="SAWS"])/R2$R2.CUR[R2$Transect=="SAWS"],
                          method = "spearman")$p.value
  Correlations$SAWSRelImpVal[i-4] = metricR2
  Correlations$SAWSRelImpP[i-4] = metricPvalue
}

# Scatter plot the metrics vs memory to show the correlations

Cor.df = WorldClimMetrics
Cor.df[["AbsVal"]] = R2$R2.SAM
Cor.df[["AbsImpVal"]] = (R2$R2.SAM-R2$R2.CUR)
Cor.df[["RelImpVal"]] = (R2$R2.SAM-R2$R2.CUR)/R2$R2.CUR


################################################################################################################
################################################################################################################
################################################################################################################

# Specify the metrics

################################################################################################################
################################################################################################################
################################################################################################################

# Calculate the linear trend line
AllLine = lm(Cor.df[["AbsImpVal"]] ~ Cor.df[["AnnualPPT"]])$coefficients
NATTLine = lm(Cor.df[["AbsImpVal"]][Cor.df$Transect=="NATT"] ~ Cor.df[["AnnualPPT"]][Cor.df$Transect=="NATT"])$coefficients
SAWSLine =lm(Cor.df[["AbsImpVal"]][Cor.df$Transect=="SAWS"] ~ Cor.df[["AnnualPPT"]][Cor.df$Transect=="SAWS"])$coefficients

# Find the range of NATT and SAWS x values
NATTmetric = Cor.df[["AnnualPPT"]][Cor.df$Transect=="NATT"]
SAWSmetric = Cor.df[["AnnualPPT"]][Cor.df$Transect=="SAWS"]

# Plot for all sites
AllCorPlot = ggplot(Cor.df,aes(color = Transect,
                               x=.data[["AnnualPPT"]], 
                               y = .data[["AbsImpVal"]])) +
  geom_point(aes(),size = 3) +
  scale_color_viridis_d(guide = guide_legend(reverse = TRUE),
                        option="magma",
                        begin=0.2,
                        end=0.8) +
  geom_line(aes(x = .data[["AnnualPPT"]], 
                y = AllLine[1] + AllLine[2]*.data[["AnnualPPT"]]),
            color = magma(1,1,0.5,0.5),
            size = 1) + 
  geom_line(aes(x = seq(min(NATTmetric),
                        max(NATTmetric),
                        length.out = nrow(Cor.df)), 
                y = NATTLine[1] + NATTLine[2]*seq(min(NATTmetric),
                                                  max(NATTmetric),
                                                  length.out = nrow(Cor.df))),
            color = magma(1,1,0.2,0.2),
            linetype = "dashed",
            size = 1) + 
  geom_line(aes(x = seq(min(SAWSmetric),
                        max(SAWSmetric),
                        length.out = nrow(Cor.df)), 
                y = SAWSLine[1] + SAWSLine[2]*seq(min(SAWSmetric),
                                                  max(SAWSmetric),
                                                  length.out = nrow(Cor.df))),
            color = magma(1,1,0.8,0.8),
            linetype = "dashed",
            size = 1) + 
  theme_bw() +
  ylab("Memory Improvement") +
  xlab("Mean Annual PPT") +
  theme(text = element_text(size = 20)) +
  ggtitle(label = paste0("All Sites"))
AllCorPlot

# Plot for NATT sites

# Calculate the linear trend line
AllLineNATT = lm(Cor.df[["AbsVal"]] ~ Cor.df[["AnnualPPT"]])$coefficients
NATTLineNATT = lm(Cor.df[["AbsVal"]][Cor.df$Transect=="NATT"] ~ Cor.df[["AnnualPPT"]][Cor.df$Transect=="NATT"])$coefficients
SAWSLineNATT =lm(Cor.df[["AbsVal"]][Cor.df$Transect=="SAWS"] ~ Cor.df[["AnnualPPT"]][Cor.df$Transect=="SAWS"])$coefficients

# Find the range of NATT and SAWS x values
NATTmetricNATT = Cor.df[["AnnualPPT"]][Cor.df$Transect=="NATT"]
SAWSmetricNATT = Cor.df[["AnnualPPT"]][Cor.df$Transect=="SAWS"]

# Plot for all sites
NATTCorPlot = ggplot(Cor.df,aes(color = Transect,
                                x=.data[["AnnualPPT"]], 
                                y = .data[["AbsVal"]])) +
  geom_point(aes(),size = 3) +
  scale_color_viridis_d(guide = guide_legend(reverse = TRUE),
                        option="magma",
                        begin=0.2,
                        end=0.8) +
  geom_line(aes(x = .data[["AnnualPPT"]], 
                y = AllLineNATT[1] + AllLineNATT[2]*.data[["AnnualPPT"]]),
            color = magma(1,1,0.5,0.5),
            linetype = "dashed",
            size = 1) + 
  geom_line(aes(x = seq(min(NATTmetricNATT),
                        max(NATTmetricNATT),
                        length.out = nrow(Cor.df)), 
                y = NATTLineNATT[1] + NATTLineNATT[2]*seq(min(NATTmetricNATT),
                                                          max(NATTmetricNATT),
                                                          length.out = nrow(Cor.df))),
            color = magma(1,1,0.2,0.2),
            size = 1) + 
  geom_line(aes(x = seq(min(SAWSmetricNATT),
                        max(SAWSmetricNATT),
                        length.out = nrow(Cor.df)), 
                y = SAWSLineNATT[1] + SAWSLineNATT[2]*seq(min(SAWSmetricNATT),
                                                          max(SAWSmetricNATT),
                                                          length.out = nrow(Cor.df))),
            color = magma(1,1,0.8,0.8),
            linetype = "dashed",
            size = 1) + 
  theme_bw() +
  ylab("Memory Performance") +
  xlab("Mean Annual PPT") +
  theme(text = element_text(size = 20)) +
  ggtitle(label = paste0("NATT"))

NATTCorPlot


# Plot for SAWS sites

# Calculate the linear trend line
AllLineSAWS = lm(Cor.df[["RelImpVal"]] ~ Cor.df[["PPTDryQtr"]])$coefficients
NATTLineSAWS = lm(Cor.df[["RelImpVal"]][Cor.df$Transect=="NATT"] ~ Cor.df[["PPTDryQtr"]][Cor.df$Transect=="NATT"])$coefficients
SAWSLineSAWS =lm(Cor.df[["RelImpVal"]][Cor.df$Transect=="SAWS"] ~ Cor.df[["PPTDryQtr"]][Cor.df$Transect=="SAWS"])$coefficients

# Find the range of NATT and SAWS x values
NATTmetricSAWS = Cor.df[["PPTDryQtr"]][Cor.df$Transect=="NATT"]
SAWSmetricSAWS = Cor.df[["PPTDryQtr"]][Cor.df$Transect=="SAWS"]

# Plot for all sites
SAWSCorPlot = ggplot(Cor.df,aes(color = Transect,
                                x=.data[["PPTDryQtr"]], 
                                y = .data[["RelImpVal"]])) +
  geom_point(aes(),size = 3) +
  geom_line(aes(x = .data[["PPTDryQtr"]], 
                y = AllLineSAWS[1] + AllLineSAWS[2]*.data[["PPTDryQtr"]]),
            color = magma(1,1,0.5,0.5),
            linetype = "dashed",
            size = 1) + 
  geom_line(aes(x = seq(min(NATTmetricSAWS),
                        max(NATTmetricSAWS),
                        length.out = nrow(Cor.df)), 
                y = NATTLineSAWS[1] + NATTLineSAWS[2]*seq(min(NATTmetricSAWS),
                                                          max(NATTmetricSAWS),
                                                          length.out = nrow(Cor.df))),
            color = magma(1,1,0.2,0.2),
            linetype = "dashed",
            size = 1) + 
  geom_line(aes(x = seq(min(SAWSmetricSAWS),
                        max(SAWSmetricSAWS),
                        length.out = nrow(Cor.df)), 
                y = SAWSLineSAWS[1] + SAWSLineSAWS[2]*seq(min(SAWSmetricSAWS),
                                                          max(SAWSmetricSAWS),
                                                          length.out = nrow(Cor.df))),
            color = magma(1,1,0.8,0.8),
            size = 1) + 
  theme_bw() +
  scale_color_viridis_d(guide = guide_legend(reverse = TRUE),
                        option="magma",
                        begin=0.2,
                        end=0.8) +
  ylab("Relative Mem. Improvement") +
  xlab("PPT in Driest Quarter") +
  theme(text = element_text(size = 20),
        legend.position = "bottom") +
  ggtitle(label = paste0("SAWS"))

SAWSCorPlot




# Write some text
text = paste("Relationships between different measures",
             "of memory impact on LE predictability",
             "and climate metrics for the NATT, the",
             "SAWS, and all sites combined. Solid ",
             "lines indicate significance at p < 0.05",
             "levels, dashed lines indicate",
             "non-significance.", sep = "\n")
text.p <- text_grob(text, face = "italic", size = 16, color = "black")

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



################################################################################################################
################################################################################################################
################################################################################################################

# Testing sans AU-Dry in NATT as clear outlier

################################################################################################################
################################################################################################################
################################################################################################################


foo = WorldClimMetrics[WorldClimMetrics$Transect=="NATT",]
foo = foo[foo$Sites!="AU-Dry",]

bar = R2[R2$Transect=="NATT",]
bar = bar[bar$Site!="AU-Dry",]

foobar = data.frame("Metric" = colnames(foo[-(1:4)]),
                    "NATTAbsVal" = 0,
                    "NATTAbsP" = 0,
                    "NATTAbsImpVal" = 0,
                    "NATTAbsImpP" = 0,
                    "NATTRelImpVal" = 0,
                    "NATTRelImpP" = 0)

for (i in 5:ncol(foo)){
  # For relative R2 improvement between SAM and current
  metricR2 = cor.test(x = foo[,i],
                      y = bar$R2.SAM,
                      method = "spearman")$estimate
  metricPvalue = cor.test(x = foo[,i],
                          y = bar$R2.SAM,
                          method = "spearman")$p.value
  foobar$NATTAbsVal[i-4] = metricR2
  foobar$NATTAbsP[i-4] = metricPvalue
  
  # For abs R2 improvement between SAM and current
  metricR2 = cor.test(x = foo[,i],
                      y = (bar$R2.SAM-bar$R2.CUR),
                      method = "spearman")$estimate
  metricPvalue = cor.test(x = foo[,i],
                          y = (bar$R2.SAM-bar$R2.CUR),
                          method = "spearman")$p.value
  foobar$NATTAbsImpVal[i-4] = metricR2
  foobar$NATTAbsImpP[i-4] = metricPvalue
  
  
  # For relative R2 improvement between SAM and current
  metricR2 = cor.test(x = foo[,i],
                      y = (bar$R2.SAM-bar$R2.CUR)/bar$R2.CUR,
                      method = "spearman")$estimate
  metricPvalue = cor.test(x = foo[,i],
                          y = (bar$R2.SAM-bar$R2.CUR)/bar$R2.CUR,
                          method = "spearman")$p.value
  foobar$NATTRelImpVal[i-4] = metricR2
  foobar$NATTRelImpP[i-4] = metricPvalue
}

