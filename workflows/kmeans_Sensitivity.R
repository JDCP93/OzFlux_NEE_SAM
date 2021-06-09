# Clean up
rm(list=ls())

# Load libraries
library(ggplot2)
library(viridis)
library(lubridate)
library(magrittr)
library(tidyverse)
library(cowplot)
library(coda)
library(gridExtra)
library(zoo)
library(viridisLite)
library(ggnewscale)
library(viridis)
library(scico)

# Define the 0-1 range function
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# List all sites
Sites_og = c("AU-ASM","AU-Cpr","AU-Cum","AU-DaS","AU-Dry","AU-Gin","AU-GWW",
             "AU-How","AU-Stp","AU-TTE","AU-Tum","AU-Whr","AU-Wom")

Metric = "AnnualPPT"

Vars=c("Tair","Fsd","VPD","PPTshort","PPTlong")

# Source worldclim correlations and climate metrics
load("site_data/SiteMetrics_worldclim_0.5res.Rdata")
Sites = factor(Sites_og, levels = WorldClimMetrics[order(WorldClimMetrics[colnames(WorldClimMetrics)=="AnnualPPT"]),1])

Transects = c("NATT","SAWS","SAWS","NATT","NATT","SAWS","SAWS",
              "NATT","NATT","NATT","SAWS","SAWS","SAWS")

##############################################################################
##############################################################################
##############################################################################
# k-means R^2 Sensitivity
##############################################################################
##############################################################################
##############################################################################

# Choose the cluster numbers
Clusters = 2:16

# List the k-mean models
Models = c("Fsdlags", "Tairlags","VPDlags","PPTlags", "allPPT")

# Initiliase dataframes
R2df = data.frame("Site" = rep(Sites, each = length(Clusters)*length(Models)),
                "Transect" = rep(Transects, each = length(Clusters)*length(Models)),
                "Clusters" = rep(Clusters, times = length(Sites), each = length(Models)),
                "Model" = factor(rep(Models,
                                     times = length(Sites)*length(Clusters)),
                                 levels = Models),
                "R2" = NA,
                "R2dif" = NA,
                "totwithinss" = NA,
                "avgsil" = NA)

# Load and extract the data
for (Site in Sites_og){
  for (Cluster in Clusters){
    for (Model in Models){
      load(paste0("alternate/RTPV/results/NEE_output_",Cluster,"cluster_kmean_current_NDVI_RTPV_",Site,".Rdata"))
      curR2 = output$r.squared
      if (file.exists(paste0("alternate/RTPV/results/NEE_output_",Cluster,"cluster_kmean_",Model,"_NDVI_RTPV_",Site,".Rdata"))){
        load(paste0("alternate/RTPV/results/NEE_output_",Cluster,"cluster_kmean_",Model,"_NDVI_RTPV_",Site,".Rdata"))
        R2df$R2[R2df$Clusters == Cluster & R2df$Model == Model & R2df$Site == Site] = output$r.squared
        R2df$R2dif[R2df$Clusters == Cluster & R2df$Model == Model & R2df$Site == Site] = output$r.squared-curR2
        R2df$totwithinss[R2df$Clusters == Cluster & R2df$Model == Model & R2df$Site == Site] = output$totwithinss
        R2df$avgsil[R2df$Clusters == Cluster & R2df$Model == Model & R2df$Site == Site] = output$avg.sil
      }
    }
  }
  file = list.files("analysis/RTPV/",pattern = paste0("NEE_analysis_RTPV_",Site))
  load(paste0("analysis/RTPV/",file))
  R2df$R2[R2df$Site == Site & R2df$Model == "SAM"] = output$SAM.R2
}

#Normalise to 0-1
R2df["R2difnorm"] = range01(R2df$R2dif)

##############################################################################
##############################################################################
##############################################################################
# SAM Sensitivity
##############################################################################
##############################################################################
##############################################################################

# Initialise dataframe for weights to apply to sensitivities
Weights = data.frame("Site" = Sites_og,
                     "Tair" = NA,
                     "Fsd" = NA,
                     "VPD" = NA,
                     "PPTshort" = NA,
                     "PPTlong" = NA,
                     "PPT" = NA)

# Collect the analysis outputs and name them with each site
for (Site in Sites_og){
  # Load analysis for ESen 
  File = list.files("analysis/RTPV/",pattern = paste0("NEE_analysis_RTPV_",Site))
  load(paste0("analysis/RTPV/",File))
  assign(Site,output)
  rm(output)
  
  # Load the coefficients
  File = list.files("analysis/RTPV/",pattern = paste0("NEE_summary_RTPV_",Site))
  load(paste0("analysis/RTPV/",File))
  
  # Load the input file and extract required data
  load(paste0("inputs/RTPV/",Site,"_Input_RTPV.Rdata"))
  input = eval(as.name(paste0(Site,"_Input")))
  
  # We now calculate the weighted climate timesteps on a site-by-site basis
  # Combine climate data and precip data
  climend = nrow(input$clim)
  climate = cbind(input$clim,
                  rbind(matrix(NA,1,4),input$clim[-climend,]),
                  rbind(matrix(NA,2,4),input$clim[-((climend-1):climend),]),
                  rbind(matrix(NA,3,4),input$clim[-((climend-2):climend),]),
                  rbind(matrix(NA,4,4),input$clim[-((climend-3):climend),]),
                  rbind(matrix(NA,5,4),input$clim[-((climend-4):climend),]),
                  rbind(matrix(NA,6,4),input$clim[-((climend-5):climend),]),
                  rbind(matrix(NA,7,4),input$clim[-((climend-6):climend),]),
                  rbind(matrix(NA,8,4),input$clim[-((climend-7):climend),]),
                  rbind(matrix(NA,9,4),input$clim[-((climend-8):climend),]),
                  rbind(matrix(NA,10,4),input$clim[-((climend-9):climend),]),
                  rbind(matrix(NA,11,4),input$clim[-((climend-10):climend),]),
                  rbind(matrix(NA,12,4),input$clim[-((climend-11):climend),]),
                  rbind(matrix(NA,13,4),input$clim[-((climend-12):climend),]),
                  input$ppt_multiscale)
  # Rename columns for ease
  colnames(climate) = c("Tair_1",
                        "Fsd_1",
                        "VPD_1",
                        "PPTshort_1",
                        "Tair_2",
                        "Fsd_2",
                        "VPD_2",
                        "PPTshort_2",
                        "Tair_3",
                        "Fsd_3",
                        "VPD_3",
                        "PPTshort_3",
                        "Tair_4",
                        "Fsd_4",
                        "VPD_4",
                        "PPTshort_4",
                        "Tair_5",
                        "Fsd_5",
                        "VPD_5",
                        "PPTshort_5",
                        "Tair_6",
                        "Fsd_6",
                        "VPD_6",
                        "PPTshort_6",
                        "Tair_7",
                        "Fsd_7",
                        "VPD_7",
                        "PPTshort_7",
                        "Tair_8",
                        "Fsd_8",
                        "VPD_8",
                        "PPTshort_8",
                        "Tair_9",
                        "Fsd_9",
                        "VPD_9",
                        "PPTshort_9",
                        "Tair_10",
                        "Fsd_10",
                        "VPD_10",
                        "PPTshort_10",
                        "Tair_11",
                        "Fsd_11",
                        "VPD_11",
                        "PPTshort_11",
                        "Tair_12",
                        "Fsd_12",
                        "VPD_12",
                        "PPTshort_12",
                        "Tair_13",
                        "Fsd_13",
                        "VPD_13",
                        "PPTshort_13",
                        "Tair_14",
                        "Fsd_14",
                        "VPD_14",
                        "PPTshort_14",
                        "PPTlong_1",
                        "PPTlong_2",
                        "PPTlong_3",
                        "PPTlong_4",
                        "PPTlong_5",
                        "PPTlong_6",
                        "PPTlong_7",
                        "PPTlong_8")
  
  # Remove first year, which has no PPT data
  climate = climate[-(1:365),]
  NEE = input$NEE[-(1:365)]
  
  # Extract climate weights
  weightnames = c(sprintf("weightA[%d,%d]",rep(1:4,14),rep(1:14,each=4)),
                  sprintf("weightAP[%d]",seq(1:8)))
  weights = output.summary$statistics[rownames(output.summary$statistics) %in% weightnames,1]
  # Rename weights
  names(weights)=str_replace_all(names(weights),pattern="weightA\\[1,","Tair_")
  names(weights)=str_replace_all(names(weights),pattern="weightA\\[2,","Fsd_")
  names(weights)=str_replace_all(names(weights),pattern="weightA\\[3,","VPD_")
  names(weights)=str_replace_all(names(weights),pattern="weightA\\[4,","PPTshort_")
  names(weights)=str_replace_all(names(weights),pattern="weightAP\\[","PPTlong_")
  names(weights)=str_replace_all(names(weights),pattern="\\]","")
  
  # Initialise matrix
  wts = data.frame("Timestep" = 1:nrow(climate),
                   "Tair" = NA,
                   "Fsd" = NA,
                   "VPD" = NA,
                   "PPTshort" = NA,
                   "PPTlong" = NA,
                   "PPT" = NA)
  
  # Calculated the weighted climate term at each timestep
  for (t in wts$Timestep){
    wts$Tair[t] = sum(weights[substr(names(weights),1,4)=="Tair"]*climate[t,substr(colnames(climate),1,4)=="Tair"])
    wts$Fsd[t] = sum(weights[substr(names(weights),1,3)=="Fsd"]*climate[t,substr(colnames(climate),1,3)=="Fsd"])
    wts$VPD[t] = sum(weights[substr(names(weights),1,3)=="VPD"]*climate[t,substr(colnames(climate),1,3)=="VPD"])
    wts$PPTshort[t] = sum(weights[substr(names(weights),1,4)=="PPTs"]*climate[t,substr(colnames(climate),1,4)=="PPTs"])
    wts$PPTlong[t] = sum(weights[substr(names(weights),1,4)=="PPTl"]*climate[t,substr(colnames(climate),1,4)=="PPTl"])
    wts$PPT = wts$PPTshort+wts$PPTlong
  }
  
  # Calculate the standard deviation of the weighted climate
  Weights[Weights$Site==Site,2:7] = sqrt(diag(var(wts)))[2:7]
  rm(output.summary)
}

# Extract the sensitivity covariates
ESen = data.frame("Site"=rep(Sites_og,each = 6),
                  "Variable" = rep(rownames(eval(as.name(Sites_og[1]))$ESen),length(Sites_og)),
                  "Low" = unlist(lapply(Sites_og,function(x) eval(as.name(x))$ESen$ESenLow)),
                  "Med" = unlist(lapply(Sites_og,function(x) eval(as.name(x))$ESen$ESenMedian)),
                  "High" = unlist(lapply(Sites_og,function(x) eval(as.name(x))$ESen$ESenHigh)))
# Check whether the climate variable is significant - does the CI contain 0?
ESen$Significant = sign(ESen$Low*ESen$High)==1


# Weighted ESen
ESen[ESen$Variable=="Tair",3:5] = ESen[ESen$Variable=="Tair",3:5]*Weights$Tair
ESen[ESen$Variable=="Fsd",3:5] = ESen[ESen$Variable=="Fsd",3:5]*Weights$Fsd
ESen[ESen$Variable=="VPD",3:5] = ESen[ESen$Variable=="VPD",3:5]*Weights$VPD
ESen[ESen$Variable=="PPTshort",3:5] = ESen[ESen$Variable=="PPTshort",3:5]*Weights$PPTshort
ESen[ESen$Variable=="PPTlong",3:5] = ESen[ESen$Variable=="PPTlong",3:5]*Weights$PPTlong
ESen[ESen$Variable=="PPT",3:5] = ESen[ESen$Variable=="PPT",3:5]*Weights$PPT

# Limit the dataframe to the variables requested
ESen = ESen[ESen$Variable %in% Vars,]

# Rename variables to nice names
ESen$Variable[ESen$Variable == "Fsd"] = "Shortwave Radiation"
ESen$Variable[ESen$Variable == "Tair"] = "Air Temperature"
ESen$Variable[ESen$Variable == "PPTshort"] = "Short-term Precipitation"
ESen$Variable[ESen$Variable == "PPTlong"] = "Long-term Precipitation"
ESen$Variable[ESen$Variable == "PPT"] = "All Precipitation"

# Assign levels to Variable
levels = c("Shortwave Radiation",
           "Air Temperature",
           "VPD",
           "Short-term Precipitation",
           "Long-term Precipitation",
           "All Precipitation")
ESen$Variable = factor(ESen$Variable,levels)

# Order sites by chosen metric
load("site_data/SiteMetrics_worldclim_0.5res.Rdata")
metric = WorldClimMetrics[WorldClimMetrics$Sites %in% Sites_og,c("Sites",Metric)]
colnames(metric) = c("Sites","Metric")
ESen$Site = factor(ESen$Site, levels = metric[order(metric[,2]),1])

# Pivot for plotting and turn into a 0-1 range. 
# Use absolute values since these indicate total sensitivity
SAMdf = pivot_longer(ESen,names_to = "Stat",cols = c("Low","Med","High"))
SAMdf$value = range01(abs(SAMdf$value))

##############################################################################
##############################################################################
##############################################################################
# k-means Coefficient Sensitivity
##############################################################################
##############################################################################
##############################################################################

# Each site has 65 coefficients in each of the clusters
ClusterDF = data.frame("Site" = rep(Sites_og,each=sum(Clusters)),
                       "ClusterTot" = rep(rep(Clusters,times=Clusters),length(Sites_og)),
                       "ClusterNo" = rep(sequence(1:max(Clusters))[-1],length(Sites_og)))

# Load in the data for each site and each clustering
for(Site in Sites_og){
  for (k in Clusters){
    # Load the k-means+regression model outputs
    File = list.files("alternate/RTPV/results/",pattern = paste0("NEE_output_",k,"cluster_kmean_alllags_scaledNEE_NDVI_RTPV_",Site))
    message("Loading k-means output for ",Site," where file is ",File)
    load(paste0("alternate/RTPV/results/",File))
    assign("kmeans",output)
    rm(output)
    
    # Load the input
    load(paste0("inputs/RTPV/",Site,"_Input_RTPV.Rdata"))
    input = eval(as.name(paste0(Site,"_Input")))
    rm(list=paste0(Site,"_Input"))
    
    for (i in 1:k){
      coeff = kmeans[[i]]$model$coefficients
      ClusterDF[ClusterDF$ClusterNo==i&ClusterDF$ClusterTot==k&ClusterDF$Site==Site,names(coeff)] <- rep(unname(coeff),each=NROW(ClusterDF[ClusterDF$ClusterNo==i&ClusterDF$ClusterTot==k&ClusterDF$Site==Site,]))
      ClusterDF[ClusterDF$ClusterNo==i&ClusterDF$ClusterTot==k&ClusterDF$Site==Site,"ClusterSize"] = length(kmeans[[i]]$NEE)
      ClusterDF[ClusterDF$ClusterNo==i&ClusterDF$ClusterTot==k&ClusterDF$Site==Site,"SeriesLength"] = nrow(kmeans$series)
    }
  }
}

# Rename the variables to make them nicer and remove the intercept term
colnames(ClusterDF) = str_replace(colnames(ClusterDF),"climate_cluster","")
ClusterDF = ClusterDF[,-4]

# Pivot the table for plotting
plotdf <- pivot_longer(ClusterDF,values_to = "Value", names_to = "Variable", cols = "Ta":tail(colnames(ClusterDF),n=1))

# We want total sensitivity so we rename all the lagged variables into just the
# variable affected
plotdf$Variable = gsub("Ta.*","Tair",plotdf$Variable)
plotdf$Variable = gsub("Fsd.*","Fsd",plotdf$Variable)
plotdf$Variable = gsub("VPD.*","VPD",plotdf$Variable)
plotdf$Variable = gsub("PPT_.....*","PPTlong",plotdf$Variable)
plotdf$Variable = gsub("PPT_.*","PPTshort",plotdf$Variable)
plotdf$Variable[plotdf$Variable=="PPT"] = "PPTshort"

# Remove NDVI and cluster info
plotdf = plotdf[!(plotdf$Variable %in% c("NDVI","ClusterSize","SeriesLength")),]
# Rename variables to nice names
plotdf$Variable[plotdf$Variable == "Fsd"] = "Shortwave Radiation"
plotdf$Variable[plotdf$Variable == "Tair"] = "Air Temperature"
plotdf$Variable[plotdf$Variable == "PPTshort"] = "Short-term Precipitation"
plotdf$Variable[plotdf$Variable == "PPTlong"] = "Long-term Precipitation"

# Assign levels to Variable
levels = c("Shortwave Radiation",
           "Air Temperature",
           "VPD",
           "Short-term Precipitation",
           "Long-term Precipitation")
plotdf$Variable = factor(plotdf$Variable,levels)

# Summarise the data by taking the mean of the absolute value
# This gives the mean sensitivity within each cluster, across all lags
plotdf = plotdf %>%
  group_by(Site,ClusterTot,ClusterNo,Variable) %>%
  summarise(Value = mean(abs(Value),na.rm=TRUE))

# Repeat each sensitivity measurement by the size of the cluster
# This provides a weighting - if we have high sensitivity but the cluster only
# includes 5 timesteps, then it has a smaller effect on the total sensitivity
plotdf = plotdf[rep(seq_len(nrow(plotdf)), rep(ClusterDF$ClusterSize,each=5)),]

# Summarise again - we take the mean over all clusters from all clusterings
KSendf = plotdf %>%
  group_by(Site,Variable) %>%
  summarise(Mean = mean(Value,na.rm=TRUE))

# Order sites by chosen metric
load("site_data/SiteMetrics_worldclim_0.5res.Rdata")
metric = WorldClimMetrics[WorldClimMetrics$Sites %in% Sites,c("Sites","AnnualPPT")]
colnames(metric) = c("Sites","Metric")
KSendf$Site = factor(KSendf$Site, levels = metric[order(metric[,2]),1])
KSendf$Mean = range01(KSendf$Mean)
##############################################################################
##############################################################################
##############################################################################
# Plotting
##############################################################################
##############################################################################
##############################################################################

kmeanR2Plot = ggplot(R2df) +
  geom_boxplot(aes(x=Model,y=R2difnorm,fill=Model)) +
  facet_grid(.~Site) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size=20),
        legend.position = "bottom") +
  scale_y_continuous(breaks= seq(0,1,by=0.5),
                     expand = c(0,0)) +
#  ylab(expression(R^2)) +
  ylab("") +
  xlab("") +
  scale_fill_viridis_d(name="",
                       breaks=c("Fsdlags",
                                "Tairlags",
                                "VPDlags",
                                "PPTlags",
                                "allPPT"),
                       labels = c("Shortwave Radiation",
                                  "Air Temperature",
                                  "VPD",
                                  "Short-term Precipitation",
                                  "Long-term Precipitation")) +
  coord_cartesian(ylim = c(0,1)) +
  ggtitle("k-means R^2")


SAMSenPlot = ggplot(SAMdf) +
  geom_boxplot(aes(x=Variable,y=value,fill=Variable)) +
  facet_grid(.~Site) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size=20),
        legend.position = "bottom") +
  ylab("") +
  scale_y_continuous(breaks= seq(0,1,by=0.5),
                     expand = c(0,0)) +
  xlab("") +
  scale_fill_viridis_d(name = "") +
  ggtitle("SAM")

kmeansCoeffPlot = ggplot(KSendf) +
  geom_point(aes(x=Variable,y=Mean,fill=Variable),size = 4,shape = 22,colour = "black")+
  facet_grid(.~Site) +
  theme_bw() +
  scale_fill_viridis_d() +
  scale_y_continuous(breaks= seq(0,1,by=0.5)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size=20),
        legend.position = "bottom") +
  xlab("") +
  ylab("") +
  ggtitle("k-means coefficients")


plot_grid(kmeanR2Plot,SAMSenPlot,kmeansCoeffPlot,ncol=1)

