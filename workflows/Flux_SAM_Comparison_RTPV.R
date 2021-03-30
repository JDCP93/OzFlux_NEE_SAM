
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
                "NEE_Current" = NA,
                "NEE_SAM" = NA,
                "LE_Current" = NA,
                "LE_SAM" = NA)

plot.df = data.frame("Site" = rep(Sites, each = 4),
                     "Flux" = factor(rep(c("NEE",
                                          "LE"), 
                                        by = 2*length(Sites)),
                                    levels = c("NEE",
                                               "LE")),
                     "Model" = rep(c("Current",
                                     "SAM"),
                                   each = 2,
                                   by = length(Sites)),
                     "R2" = NA)

for (Site in Sites){
  
  if (length(list.files("analysis/RTPV/",pattern = paste0("NEE_analysis_RTPV_",Site))) > 0){
    file = list.files("analysis/RTPV/",pattern = paste0("NEE_analysis_RTPV_",Site))
    load(paste0("analysis/RTPV/",file))
    plot.df$R2[plot.df$Site == Site & plot.df$Flux == "NEE" & plot.df$Model == "SAM"] = output$SAM.R2
    df$NEE_SAM[df$Site == Site] = output$SAM.R2
  }
  
  if (length(list.files("analysis/RTPV/",pattern = paste0("NEE_current_analysis_RTPV_",Site))) > 0){
    file = list.files("analysis/RTPV/",pattern = paste0("NEE_current_analysis_RTPV_",Site))
    load(paste0("analysis/RTPV/",file))
    plot.df$R2[plot.df$Site == Site & plot.df$Flux == "NEE" & plot.df$Model == "Current"] = output$CUR.R2
    df$NEE_Current[df$Site == Site] = output$CUR.R2
  }
 
  if (length(list.files("analysis/RTPV/",pattern = paste0("LE_analysis_RTPV_",Site))) > 0){
    file = list.files("analysis/RTPV/",pattern = paste0("LE_analysis_RTPV_",Site))
    load(paste0("analysis/RTPV/",file))
    plot.df$R2[plot.df$Site == Site & plot.df$Flux == "LE" & plot.df$Model == "SAM"] = output$SAM.R2
    df$LE_SAM[df$Site == Site] = output$SAM.R2
  }
  
  if (length(list.files("analysis/RTPV/",pattern = paste0("LE_current_analysis_RTPV_",Site))) > 0){
    file = list.files("analysis/RTPV/",pattern = paste0("LE_current_analysis_RTPV_",Site))
    load(paste0("analysis/RTPV/",file))
    plot.df$R2[plot.df$Site == Site & plot.df$Flux == "LE" & plot.df$Model == "Current"] = output$CUR.R2
    df$LE_Current[df$Site == Site] = output$CUR.R2
  }
  
}


plot.df = plot.df[plot.df$Site %in% df$Site[!is.na(df$LE_SAM)],]

Plot = ggplot(plot.df) +
  geom_bar(aes(x = Model,y=R2,group = Flux,fill=Flux),stat = "identity",position = "dodge") +
  geom_text(aes(x = Model, y = R2+0.1, label = signif(R2,2), group = Flux),position = position_dodge(width = 1)) +
  facet_grid(Site~.) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  scale_alpha_manual(values=c(0.5, 1)) +
  coord_cartesian(ylim = c(0,1)) +
  ggtitle("NEE and LE model performance")

Plot        
