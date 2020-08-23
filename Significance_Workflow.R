rm(list=ls())
library(ggplot2)
source("SensitivityCheck_match.R")

HS = SensitivityCheck_match("HowardSprings","HS")

TT = SensitivityCheck_match("TiTreeEast","TT")

SP = SensitivityCheck_match("SturtPlains","SP")

DU = SensitivityCheck_match("DalyUncleared","DU")

DR = SensitivityCheck_match("DryRiver","DR")

# 1 Intercept
# 2 Tair
# 3 SWR
# 4 VPD
# 5 Current SWC
# 6 Antecedent SWC
# 7 Tair x Tair
# 8 SWR x SWR
# 9 VPD x VPD
# 10 cSWC x cSWC
# 11 aSWC x aSWC
# 12 Tair x SWR
# 13 Tair x VPD
# 14 Tair x cSWC
# 15 Tair x aSWC
# 16 SWR x VPD
# 17 SWR x cSWC
# 18 SWR x aSWC
# 19 VPD x cSWC
# 20 VPD x aSWC
# 21 cSWC x aSWC
# 22 PPT
Sites = list("HS"=HS,"TT"=TT,"SP"=SP,"DU"=DU,"DR"=DR)
Data = lapply(Sites,function(x) x[c(2,3,4,5,6,22),])

plotdata = data.frame("Site" = Data$HS$Site,
                      "Var" = rep(c("Tair","SWR","VPD","cSWC","aSWC","PPT"),2),
                      "Season" = rep(c("G","NG"),each = 6),
                      "Mean" = c(Data$HS$SAM_ag.mean,Data$HS$SAM_an.mean),
                      "Min" = c(Data$HS$SAM_ag.min,Data$HS$SAM_an.min),
                      "Max" = c(Data$HS$SAM_ag.max,Data$HS$SAM_an.max))

plotdata$Significant = (sign(plotdata$Min*plotdata$Max)==1)

Plot = ggplot(plotdata) +
  geom_pointrange(aes(x = Var,y = Mean, ymin = Min, ymax = Max, color = Season,shape = Significant),position = position_dodge(width = 0.5))

Plot

                  