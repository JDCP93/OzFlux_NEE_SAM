
# Make sure everything is clean
rm(list=ls())

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

# For each site
for (Site in Sites){
  # Let the user know which site the function is looking at
  message("*** Analysing climate correlations for ",Site," ***")
  # Load the input and extract each climate variable
  load(paste0("inputs/RTPVS/",Site,"_Input_RTPVS.Rdata"))
  Tair = eval(as.name(paste0(Site,"_Input")))$DailyData$Ta
  VPD = eval(as.name(paste0(Site,"_Input")))$DailyData$VPD
  SW = eval(as.name(paste0(Site,"_Input")))$DailyData$Fsd
  PPT = eval(as.name(paste0(Site,"_Input")))$DailyData$Precip

  # FInd the correlation between the climate variables
  TxV = cor.test(Tair,VPD)
  TxS = cor.test(Tair,SW)
  TxP = cor.test(Tair,PPT)
  VxS = cor.test(SW,VPD)
  VxP = cor.test(PPT,VPD)
  SxP = cor.test(PPT,SW)

  # Put these into a dataframe
  df = data.frame("Correlations" = rep(0,6),
                  "r.value" = rep(0,6),
                  "p.value" = rep(0,6))

  df$Correlations = c("TxV",
                      "TxS",
                      "TxP",
                      "VxS",
                      "VxP",
                      "SxP")

  df$r.value = c(TxV$estimate,
                 TxS$estimate,
                 TxP$estimate,
                 VxS$estimate,
                 VxP$estimate,
                 SxP$estimate)

  df$p.value = c(TxV$p.value,
                 TxS$p.value,
                 TxP$p.value,
                 VxS$p.value,
                 VxP$p.value,
                 SxP$p.value)

  # Output the data with a site-dependent name
  name = paste0(Site,"_cor")
  assign(name,df)
  # Clean up
  rm(list=c("Tair","VPD","SW","PPT","TxV","TxS","TxP","VxS","VxP","SxP","df"))
}

df = data.frame("Site" = rep(Sites,each=6),
                "Correlation" = rep(c("TxV",
                                      "TxS",
                                      "TxP",
                                      "VxS",
                                      "VxP",
                                      "SxP"), times = length(Sites)))

for(Site in Sites){
  df$r.value[df$Site == Site] = eval(as.name(paste0(Site,"_cor")))$r.value
  df$p.value[df$Site == Site] = eval(as.name(paste0(Site,"_cor")))$p.value
}

library(ggplot2)

plot = ggplot(df) +
        geom_point(aes(Site,r.value,color=(p.value<0.05 & abs(r.value)>0.75))) +
        geom_hline(yintercept = 0.75) +
        geom_hline(yintercept = -0.75) +
        ylim(-1,1) +
        facet_grid(Correlation~.,
                   scales="free")
