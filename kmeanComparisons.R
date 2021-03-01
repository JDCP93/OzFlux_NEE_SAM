
rm(list=ls())

# List all sites
Sites = c("AU-ASM"
          ,"AU-Cpr"
          ,"AU-Cum"
          ,"AU-DaS"
          ,"AU-Dry"
          ,"AU-Gin"
          ,"AU-GWW")


df = data.frame("Site" = Sites,
                "Current" = NA,
                "NDVI" = NA,
                "PPT14_20" = NA,
                "PPT21_29" = NA,
                "PPT30_59" = NA,
                "PPT60_119" = NA,
                "PPT120_179" = NA,
                "PPT180_269" = NA,
                "PPT270_365" = NA,
                "AllPPT" = NA,
                "AllLags" = NA)

for (Site in Sites){
  load(paste0("alternate/RTPV/results/NEE_output_kmean_current_RTPV_",Site,".Rdata"))
  df$Current[df$Site == Site] = output$r.squared
  
  load(paste0("alternate/RTPV/results/NEE_output_kmean_NDVI_RTPV_",Site,".Rdata"))
  df$NDVI[df$Site == Site] = output$r.squared
  
  load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT14-20_RTPV_",Site,".Rdata"))
  df$PPT14_20[df$Site == Site] = output$r.squared
  
  load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT21-29_RTPV_",Site,".Rdata"))
  df$PPT21_29[df$Site == Site] = output$r.squared
  
  load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT30-59_RTPV_",Site,".Rdata"))
  df$PPT30_59[df$Site == Site] = output$r.squared
  
  load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT60-119_RTPV_",Site,".Rdata"))
  df$PPT60_119[df$Site == Site] = output$r.squared
  
  load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT120-179_RTPV_",Site,".Rdata"))
  df$PPT120_179[df$Site == Site] = output$r.squared
  
  load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT180-269_RTPV_",Site,".Rdata"))
  df$PPT180_269[df$Site == Site] = output$r.squared
  
  load(paste0("alternate/RTPV/results/NEE_output_kmean_PPT270-365_RTPV_",Site,".Rdata"))
  df$PPT270_365[df$Site == Site] = output$r.squared
  
  load(paste0("alternate/RTPV/results/NEE_output_kmean_allPPT_RTPV_",Site,".Rdata"))
  df$AllPPT[df$Site == Site] = output$r.squared
  
  load(paste0("alternate/RTPV/results/NEE_output_kmean_alllags_RTPV_",Site,".Rdata"))
  df$AllLags[df$Site == Site] = output$r.squared
  
}
