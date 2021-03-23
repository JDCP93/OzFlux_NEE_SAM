
rm(list=ls())

library(ggplot2)
library(viridis)

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
          ,"AU-Wom")


df = data.frame("Site" = Sites,
                "Current" = NA,
                "Current_NDVI" = NA,
                "PPT14_20" = NA,
                "PPT14_20_NDVI" = NA,
                "PPT21_29" = NA,
                "PPT21_29_NDVI" = NA,
                "PPT30_59" = NA,
                "PPT30_59_NDVI" = NA,
                "PPT60_119" = NA,
                "PPT60_119_NDVI" = NA,
                "PPT120_179" = NA,
                "PPT120_179_NDVI" = NA,
                "PPT180_269" = NA,
                "PPT180_269_NDVI" = NA,
                "PPT270_365" = NA,
                "PPT270_365_NDVI" = NA,
                "AllPPT" = NA,
                "AllPPT_NDVI" = NA,
                "AllLags" = NA,
                "AllLags_NDVI" = NA)

plot.df = data.frame("Site" = rep(Sites, each = 22),
                    "Lag" = factor(rep(c("Current",
                                          "PPT14_20",
                                          "PPT21_29",
                                          "PPT30_59",
                                          "PPT60_119",
                                          "PPT120_179",
                                          "PPT180_269",
                                          "PPT270_365",
                                          "AllPPT",
                                          "AllLags",
                                          "SAM"), 
                                      by = 2*length(Sites)),
                                  levels = c("Current",
                                             "PPT14_20",
                                             "PPT21_29",
                                             "PPT30_59",
                                             "PPT60_119",
                                             "PPT120_179",
                                             "PPT180_269",
                                             "PPT270_365",
                                             "AllPPT",
                                             "AllLags",
                                             "SAM")),
                    "NDVI" = rep(c("Veg","NoVeg"),
                                 each = 11, 
                                 by = length(Sites)),
                    "R2" = NA)

for (Site in Sites){
  
  # Current K-means
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_current_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_current_RTPV_",Site,".Rdata"))
    df$Current[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "Current" & plot.df$NDVI == "NoVeg"] = output$r.squared
  }
  
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_current_NDVI_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_current_NDVI_RTPV_",Site,".Rdata"))
    df$Current_NDVI[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "Current" & plot.df$NDVI == "Veg"] = output$r.squared
  }
  
  # PPT 14-20 day lag
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_PPT14-20_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT14-20_RTPV_",Site,".Rdata"))
    df$PPT14_20[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "PPT14_20" & plot.df$NDVI == "NoVeg"] = output$r.squared
  }
  
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_PPT14-20_NDVI_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT14-20_NDVI_RTPV_",Site,".Rdata"))
    df$PPT14_20_NDVI[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "PPT14_20" & plot.df$NDVI == "Veg"] = output$r.squared
  }
  
  # PPT 21-29 day lag
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_PPT21-29_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT21-29_RTPV_",Site,".Rdata"))
    df$PPT21_29[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "PPT21_29" & plot.df$NDVI == "NoVeg"] = output$r.squared
  }
  
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_PPT21-29_NDVI_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT21-29_NDVI_RTPV_",Site,".Rdata"))
    df$PPT21_29_NDVI[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "PPT21_29" & plot.df$NDVI == "Veg"] = output$r.squared
  }
  
  # PPT 30-59 day lag
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_PPT30-59_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT30-59_RTPV_",Site,".Rdata"))
    df$PPT30_59[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "PPT30_59" & plot.df$NDVI == "NoVeg"] = output$r.squared
  }
  
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_PPT30-59_NDVI_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT30-59_NDVI_RTPV_",Site,".Rdata"))
    df$PPT30_59_NDVI[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "PPT30_59" & plot.df$NDVI == "Veg"] = output$r.squared
  }
  
  # PPT 60-119 day lag
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_PPT60-119_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT60-119_RTPV_",Site,".Rdata"))
    df$PPT60_119[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "PPT60_119" & plot.df$NDVI == "NoVeg"] = output$r.squared
  }
  
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_PPT60-119_NDVI_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT60-119_NDVI_RTPV_",Site,".Rdata"))
    df$PPT60_119_NDVI[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "PPT60_119" & plot.df$NDVI == "Veg"] = output$r.squared
  }
  
  # PPT 120-179 day lag
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_PPT120-179_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT120-179_RTPV_",Site,".Rdata"))
    df$PPT120_179[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "PPT120_179" & plot.df$NDVI == "NoVeg"] = output$r.squared
  }
  
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_PPT120-179_NDVI_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT120-179_NDVI_RTPV_",Site,".Rdata"))
    df$PPT120_179_NDVI[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "PPT120_179" & plot.df$NDVI == "Veg"] = output$r.squared
  }
  
  # PPT 180-269 day lag
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_PPT180-269_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT180-269_RTPV_",Site,".Rdata"))
    df$PPT180_269[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "PPT180_269" & plot.df$NDVI == "NoVeg"] = output$r.squared
  }
  
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_PPT180-269_NDVI_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT180-269_NDVI_RTPV_",Site,".Rdata"))
    df$PPT180_269_NDVI[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "PPT180_269" & plot.df$NDVI == "Veg"] = output$r.squared
  }
  
  # PPT 270-365 day lag
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_PPT270-365_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT270-365_RTPV_",Site,".Rdata"))
    df$PPT270_365[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "PPT270_365" & plot.df$NDVI == "NoVeg"] = output$r.squared
  }
  
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_PPT270-365_NDVI_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT270-365_NDVI_RTPV_",Site,".Rdata"))
    df$PPT270_365_NDVI[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "PPT270_365" & plot.df$NDVI == "Veg"] = output$r.squared
  }
  
  # All PPT lags but current climate
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_allPPT_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_allPPT_RTPV_",Site,".Rdata"))
    df$AllPPT[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "AllPPT" & plot.df$NDVI == "NoVeg"] = output$r.squared
  }
  
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_allPPT_NDVI_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_allPPT_NDVI_RTPV_",Site,".Rdata"))
    df$AllPPT_NDVI[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "AllPPT" & plot.df$NDVI == "Veg"] = output$r.squared
  }
  
  # All lags
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_alllags_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_alllags_RTPV_",Site,".Rdata"))
    df$AllLags[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "AllLags" & plot.df$NDVI == "NoVeg"] = output$r.squared
  }
  
  if (file.exists(paste0("alternate/RTPV/results/NEE_output_kmean_alllags_NDVI_RTPV_",Site,".Rdata"))){
    load(paste0("alternate/RTPV/results/NEE_output_kmean_alllags_NDVI_RTPV_",Site,".Rdata"))
    df$AllLags_NDVI[df$Site == Site] = output$r.squared
    plot.df$R2[plot.df$Site == Site & plot.df$Lag == "AllLags" & plot.df$NDVI == "Veg"] = output$r.squared
  }
  
  file = list.files("analysis/RTPV/",pattern = paste0("NEE_analysis_RTPV_",Site))
  load(paste0("analysis/RTPV/",file))
  plot.df$R2[plot.df$Site == Site & plot.df$Lag == "SAM" & plot.df$NDVI == "Veg"] = output$SAM.R2
  
}


Plot = ggplot(plot.df) +
        geom_bar(aes(x = Lag,y=R2,group = NDVI,fill=Lag, alpha = NDVI),stat = "identity",position = "dodge") +
        geom_text(aes(x = Lag, y = R2+0.1, label = signif(R2,2), group = NDVI),position = position_dodge(width = 1)) +
        facet_grid(Site~.) +
        theme_bw() +
        theme(panel.grid.major.x = element_blank()) +
        scale_alpha_manual(values=c(0.5, 1)) +
        coord_cartesian(ylim = c(0,1))

Plot        
