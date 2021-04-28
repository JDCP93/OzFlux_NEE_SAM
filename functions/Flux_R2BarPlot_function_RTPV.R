Flux_R2BarPlot_RTPV = function(Sites,Transects,Metric="AnnualPPT",Clusters = 0){
  
  
  # Load the required packages
  library(tidyverse)
  library(lubridate)
  library(viridis)
  library(ggpubr)
  library(gridExtra)
  
  # Make a nice metric title
  Titles =c("Annual Mean Temp",
            "Mean Diurnal Range",
            "Isothermality",
            "Temp Seasonality",
            "Max Temp of Hottest Month",
            "Min Temp of Coldest Month",
            "Temp Annual Range",
            "Mean Temp in Wettest Qtr",
            "Mean Temp in Driest Qtr",
            "Mean Temp in Summer",
            "Mean Temp in Winter",
            "Mean Annual PPT",
            "PPT in Wettest Month",
            "PPT in Driest Month",
            "PPT Seasonality",
            "PPT in Wettest Qtr",
            "PPT in Driest Qtr",
            "PPT in Summer",
            "PPT in Winter")
  
  PotMetric = c("AnnualMeanTemp",
                "MeanDiurnalRange",
                "Isothermality",
                "TempSeasonality",
                "MaxTempHotMon",
                "MinTempColdMon",
                "TempAnnualRange",
                "MeanTempWetQtr",
                "MeanTempDryQtr",
                "MeanTempHotQtr",
                "MeanTempColdQtr",
                "AnnualPPT",
                "PPTWetMon",
                "PPTDryMon",
                "PPTSeasonality",
                "PPTWetQtr",
                "PPTDryQtr",
                "PPTHotQtr",
                "PPTColdQtr")
  
  Title = Titles[Metric == PotMetric]
  
  # Initiliase R2 data frame
  R2 = data.frame("Site" = rep(Sites,each = 2),
                  "Transect" = rep(Transects,each = 2),
                  "Flux" = rep(c("NEE","LE"),times = length(Sites)),
                  "R2.CUR" = NA,
                  "R2.SAM" = NA,
                  "R2.AR1" = NA)
  
  # For each site
  for (Site in Sites){
    
    # Collect the R2 values from the analysis scripts
    message("Collating R2 values for ",Site)
    
    # Load the LE analysis results
    File = list.files("analysis/RTPV/",pattern = paste0("LE_analysis_RTPV_",Site))
    if (length(File) != 0){
      load(paste0("analysis/RTPV/",File))
      R2$R2.SAM[R2$Site==Site & R2$Flux== "LE"] = output$SAM.R2
    }
    
    File = list.files("analysis/RTPV/",pattern = paste0("LE_current_analysis_RTPV_",Site))
    if (length(File) != 0){
    load(paste0("analysis/RTPV/",File))
    R2$R2.CUR[R2$Site==Site & R2$Flux== "LE"] = output$CUR.R2
    }
    
    File = list.files("analysis/RTPV/",pattern = paste0("LE_AR1_analysis_RTPV_",Site))
    if (length(File) != 0){
    load(paste0("analysis/RTPV/",File))
    R2$R2.AR1[R2$Site==Site & R2$Flux== "LE"] = output$AR1.R2
    }
    
    # Load the NEE analysis results
    File = list.files("analysis/RTPV/",pattern = paste0("NEE_analysis_RTPV_",Site))
    if (length(File) != 0){
      load(paste0("analysis/RTPV/",File))
      R2$R2.SAM[R2$Site==Site & R2$Flux== "NEE"] = output$SAM.R2
    }
    
    File = list.files("analysis/RTPV/",pattern = paste0("NEE_current_analysis_RTPV_",Site))
    if (length(File) != 0){
      load(paste0("analysis/RTPV/",File))
      R2$R2.CUR[R2$Site==Site & R2$Flux== "NEE"] = output$CUR.R2
    }
    
    File = list.files("analysis/RTPV/",pattern = paste0("NEE_AR1_analysis_RTPV_",Site))
    if (length(File) != 0){
      load(paste0("analysis/RTPV/",File))
      R2$R2.AR1[R2$Site==Site & R2$Flux== "NEE"] = output$AR1.R2
    }
    
  }
  
  # Source worldclim correlations and climate metrics
  load("site_data/SiteMetrics_worldclim_0.5res.Rdata")
  
  # Plot the data!
  # Create the plot dataframe
  Site = rep(Sites,each=2,times=3)
  Site = factor(Site, levels = WorldClimMetrics[order(WorldClimMetrics[colnames(WorldClimMetrics)==Metric]),1])
  
  Transect = rep(Transects,each=2,times=3)
  
  Flux = rep(c("NEE","LE"),times = length(Sites)*3)
  
  # Order the models as we want
  Model = rep(c("Current Environment (SAM)",
                "Environmental Memory (SAM)",
                "Biological Memory (AR1)"),
              each=length(Sites)*2)
  Model = factor(Model,
                 levels=c("Current Environment (SAM)",
                          "Environmental Memory (SAM)",
                          "Biological Memory (AR1)"))
  
  Value = c(R2$R2.CUR,
            R2$R2.SAM,
            R2$R2.AR1)
  
  Fig = data.frame(Site,
                   Transect,
                   Flux,
                   Model,
                   "Model2" = Model,
                   Value)
  # Here we make sure that the bars are ordered by their size
  Fig = Fig[order(Fig$Value, decreasing = TRUE),]
  Fig$ValueFactor<-factor(Fig$Value, levels = unique(Fig$Value))
  
  # We then replace value with the difference so that the bars are "cumulative"
  for(site in Sites){
    for(flux in c("NEE","LE")){
      Fig$Value[Fig$Site==site & Fig$Flux==flux][1:2] = rev(diff(rev(Fig$Value[Fig$Site==site & Fig$Flux==flux])))
    }
  }
  
  # Plot for every site based on supplied metric
  Plot = ggplot() +
    geom_bar(data = Fig[Fig$Flux=="NEE",],aes(fill=Model,y=Value,x=Flux,group = ValueFactor),stat = "identity",color = "black",size = 1) +
    scale_fill_viridis_d("NEE:",direction=-1,begin=0.2,end=1) +
    new_scale("fill") +
    geom_bar(data = Fig[Fig$Flux=="LE",],aes(fill=Model,y=Value,x=Flux,group = ValueFactor),stat = "identity",color = "black",size = 1) +
    scale_fill_viridis_d("   LE:",direction=-1,begin=0,end=0.8) +
    geom_point(data = Fig[Fig$Transect=="NATT",],aes(x=1.5,y = -0.025),shape=8,size = 2,show.legend=FALSE,stroke=1) +
    coord_flip(ylim=c(0,1)) +
    ylab(parse(text="R^2")) +
    theme_bw() +
    theme(legend.position = "bottom", 
          text = element_text(size = 20),
          panel.spacing = unit(0.1, "lines"),
          legend.box = "vertical",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    facet_grid(Site~.,switch="y")+
    ggtitle(paste0("Model Performance"),
            subtitle = paste0("Sites Ordered By ",Title)) 
  
  Plot
  
}
