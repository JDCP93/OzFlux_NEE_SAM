
# A junk script to test out various diagnostic tools and explore model output
# in a haphazard manner
rm(list=ls())
# load model inputs
load('SturtPlains_Input_NDVI.Rdata')
load('HowardSprings_Input_NDVI.Rdata')

# Load Howard Springs data
load('results/NEE_output_site_HS_2020-08-05.rda')
# data inside is called "nee_daily"
assign('HS',nee_daily)
rm(nee_daily)

# Load Sturt Plains data
load('results/NEE_output_site_SP_2020-08-04.rda')
# data inside is called "nee_daily"
assign('SP',nee_daily)
rm(nee_daily)


# set version
library(coda)

# # We want all values to be close to 1
#HS_Gelman = gelman.diag(HS,multivariate=FALSE)
#SP_Gelman = gelman.diag(SP,multivariate=FALSE)

# Check effective sample size
#mean(tail(effectiveSize(HS),300))
#mean(tail(effectiveSize(SP),300))


library(lattice)

library(mcmcplots)
library(superdiag)

source("DBDA2E-utilities.R")
# # Calculates Z-score and we want all the dots to fall within the -2:2 range
# HS_Geweke = geweke.diag(HS)
# SP_Geweke = geweke.diag(SP)
# 
#HS_Geweke = geweke.plot(HS)
#SP_Geweke = geweke.plot(SP)

# Use Kruschke's diag function

 Model = TT
 for (i in varnames(Model)[-grep(varnames(Model),pattern="NEE")]){
   
   diagMCMC(Model,i)
   print(i)
   question1 <- readline("Next plot? (Y/N)")
   
   if(regexpr(question1, 'n', ignore.case = TRUE) == 1){
     break
   } else {
     next  
   }
 }

# Summarise for other uses (means, quantiles, etc.)
HS.summary=summary(HS)
SP.summary=summary(SP)
# # Check which variables we tracked
# unique(substr(rownames(HS.summary$statistics),1,8))
# unique(substr(rownames(SP.summary$statistics),1,8))

# takes 5EVER OMGOSHHHHHH
# testing me
### xyplot(nee_daily)


# Check obs vs predicted
library(ggplot2)
library(gridExtra)
HS_NEE_pred = HS.summary$statistics[substr(rownames(HS.summary$statistics),1,3)=="NEE",1]
HS_NEE_obs = `HowardSprings_Input`$NEE[-(1:365)]

SP_NEE_pred = SP.summary$statistics[substr(rownames(SP.summary$statistics),1,3)=="NEE",1]
SP_NEE_obs = `SturtPlains_Input`$NEE[-(1:365)]

HS_ObsMod <- ggplot(data.frame(HS_NEE_obs,HS_NEE_pred)) +
             geom_point(aes(HS_NEE_obs,HS_NEE_pred)) +
             geom_abline(slope=1,intercept=0) 
             #  +
             # xlim(-3.5,3.5) +
             # ylim(-3.5,3.5)


SP_ObsMod <- ggplot(data.frame(SP_NEE_obs,SP_NEE_pred)) +
             geom_point(aes(SP_NEE_obs,SP_NEE_pred)) +
             geom_abline(slope=1,intercept=0) 
             #  +
             # xlim(-3.5,3.5) +
             # ylim(-3.5,3.5)


grid.arrange(HS_ObsMod,SP_ObsMod)

# Calculate R2

HS.SAM.R2 = summary(lm(HS_NEE_pred ~ HS_NEE_obs))$r.squared


SP.SAM.R2 = summary(lm(SP_NEE_pred ~ SP_NEE_obs))$r.squared

message("Howard Springs has R2 = ",round(HS.SAM.R2,3)," for SAM")
message("Sturt Plains has R2 = ",round(SP.SAM.R2,3), " for SAM")


# Check cumulative weights
HS_cumSWR_mean = HS.summary$statistics[substr(rownames(HS.summary$statistics),1,13)=="cum_weightA[2",1]
HS_cumTair_mean = HS.summary$statistics[substr(rownames(HS.summary$statistics),1,13)=="cum_weightA[1",1]
HS_cumVPD_mean = HS.summary$statistics[substr(rownames(HS.summary$statistics),1,13)=="cum_weightA[3",1]
HS_cumSWC_mean = HS.summary$statistics[substr(rownames(HS.summary$statistics),1,13)=="cum_weightA[5",1]
HS_cumPPT_mean = HS.summary$statistics[substr(rownames(HS.summary$statistics),1,12)=="cum_weightAP",1]

SP_cumSWR_mean = SP.summary$statistics[substr(rownames(SP.summary$statistics),1,13)=="cum_weightA[2",1]
SP_cumTair_mean = SP.summary$statistics[substr(rownames(SP.summary$statistics),1,13)=="cum_weightA[1",1]
SP_cumVPD_mean = SP.summary$statistics[substr(rownames(SP.summary$statistics),1,13)=="cum_weightA[3",1]
SP_cumSWC_mean = SP.summary$statistics[substr(rownames(SP.summary$statistics),1,13)=="cum_weightA[5",1]
SP_cumPPT_mean = SP.summary$statistics[substr(rownames(SP.summary$statistics),1,12)=="cum_weightAP",1]


HS_cumSWR_min = HS.summary$quantiles[substr(rownames(HS.summary$quantiles),1,13)=="cum_weightA[2",1]
HS_cumTair_min = HS.summary$quantiles[substr(rownames(HS.summary$quantiles),1,13)=="cum_weightA[1",1]
HS_cumVPD_min = HS.summary$quantiles[substr(rownames(HS.summary$quantiles),1,13)=="cum_weightA[3",1]
HS_cumSWC_min = HS.summary$quantiles[substr(rownames(HS.summary$quantiles),1,13)=="cum_weightA[5",1]
HS_cumPPT_min = HS.summary$quantiles[substr(rownames(HS.summary$quantiles),1,12)=="cum_weightAP",1]

SP_cumSWR_min = SP.summary$quantiles[substr(rownames(SP.summary$quantiles),1,13)=="cum_weightA[2",1]
SP_cumTair_min = SP.summary$quantiles[substr(rownames(SP.summary$quantiles),1,13)=="cum_weightA[1",1]
SP_cumVPD_min = SP.summary$quantiles[substr(rownames(SP.summary$quantiles),1,13)=="cum_weightA[3",1]
SP_cumSWC_min = SP.summary$quantiles[substr(rownames(SP.summary$quantiles),1,13)=="cum_weightA[5",1]
SP_cumPPT_min = SP.summary$quantiles[substr(rownames(SP.summary$quantiles),1,12)=="cum_weightAP",1]

HS_cumSWR_max = HS.summary$quantiles[substr(rownames(HS.summary$quantiles),1,13)=="cum_weightA[2",5]
HS_cumTair_max = HS.summary$quantiles[substr(rownames(HS.summary$quantiles),1,13)=="cum_weightA[1",5]
HS_cumVPD_max = HS.summary$quantiles[substr(rownames(HS.summary$quantiles),1,13)=="cum_weightA[3",5]
HS_cumSWC_max = HS.summary$quantiles[substr(rownames(HS.summary$quantiles),1,13)=="cum_weightA[5",5]
HS_cumPPT_max = HS.summary$quantiles[substr(rownames(HS.summary$quantiles),1,12)=="cum_weightAP",5]

SP_cumSWR_max = SP.summary$quantiles[substr(rownames(SP.summary$quantiles),1,13)=="cum_weightA[2",5]
SP_cumTair_max = SP.summary$quantiles[substr(rownames(SP.summary$quantiles),1,13)=="cum_weightA[1",5]
SP_cumVPD_max = SP.summary$quantiles[substr(rownames(SP.summary$quantiles),1,13)=="cum_weightA[3",5]
SP_cumSWC_max = SP.summary$quantiles[substr(rownames(SP.summary$quantiles),1,13)=="cum_weightA[5",5]
SP_cumPPT_max = SP.summary$quantiles[substr(rownames(SP.summary$quantiles),1,12)=="cum_weightAP",5]


cumSWR = data.frame("HS_mean" = HS_cumSWR_mean,"SP_mean" = SP_cumSWR_mean, "HS_min" = HS_cumSWR_min,"SP_min" = SP_cumSWR_min, "HS_max" = HS_cumSWR_max,"SP_max" = SP_cumSWR_max)
cumTair = data.frame("HS_mean" = HS_cumTair_mean,"SP_mean" = SP_cumTair_mean, "HS_min" = HS_cumTair_min,"SP_min" = SP_cumTair_min, "HS_max" = HS_cumTair_max,"SP_max" = SP_cumTair_max)
cumVPD = data.frame("HS_mean" = HS_cumVPD_mean,"SP_mean" = SP_cumVPD_mean, "HS_min" = HS_cumVPD_min,"SP_min" = SP_cumVPD_min, "HS_max" = HS_cumVPD_max,"SP_max" = SP_cumVPD_max)
cumSWC = data.frame("HS_mean" = HS_cumSWC_mean,"SP_mean" = SP_cumSWC_mean, "HS_min" = HS_cumSWC_min,"SP_min" = SP_cumSWC_min, "HS_max" = HS_cumSWC_max,"SP_max" = SP_cumSWC_max)
cumPPT = data.frame("HS_mean" = HS_cumPPT_mean,"SP_mean" = SP_cumPPT_mean, "HS_min" = HS_cumPPT_min,"SP_min" = SP_cumPPT_min, "HS_max" = HS_cumPPT_max,"SP_max" = SP_cumPPT_max)

SWRplot = ggplot(cumSWR) +
          geom_path(aes(0:13,y=HS_mean,color="HS"),size=1) +
          geom_path(aes(0:13,y=SP_mean,color="SP"),size=1) +
  geom_linerange(aes(x=-0.1:12.9,ymin=HS_min,ymax=HS_max,color="HS"),size=1) +
  geom_linerange(aes(x=0.1:13.1,ymin=SP_min,ymax=SP_max,color="SP"),size=1) +
          geom_hline(yintercept=0.5,linetype = "dashed") +
          coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
          xlab("Days into Past") +
          ylab("Cumulative Weight") +
          theme_bw() +
          guides(color = "none") +
          scale_color_manual(values=c("violet","firebrick")) +
          ggtitle("SWR")

Tairplot = ggplot(cumTair) +
            geom_path(aes(0:13,HS_mean,color="HS"),size=1) +
            geom_path(aes(0:13,SP_mean,color="SP"),size=1) +
  geom_linerange(aes(x=-0.1:12.9,ymin=HS_min,ymax=HS_max,color="HS"),size=1) +
  geom_linerange(aes(x=0.1:13.1,ymin=SP_min,ymax=SP_max,color="SP"),size=1) +
            geom_hline(yintercept=0.5,linetype = "dashed") +
            coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
            xlab("Days into Past") +
            ylab("Cumulative Weight") +
            theme_bw() +
            guides(color = "none") +
            scale_color_manual(values=c("violet","firebrick")) +
            ggtitle("Tair")

VPDplot = ggplot(cumVPD) +
          geom_path(aes(0:13,HS_mean,color="HS"),size=1) +
          geom_path(aes(0:13,SP_mean,color="SP"),size=1) +
  geom_linerange(aes(x=-0.1:12.9,ymin=HS_min,ymax=HS_max,color="HS"),size=1) +
  geom_linerange(aes(x=0.1:13.1,ymin=SP_min,ymax=SP_max,color="SP"),size=1) +
          geom_hline(yintercept=0.5,linetype = "dashed") +
          coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
          xlab("Days into Past") +
          ylab("Cumulative Weight") +
          theme_bw() +
          guides(color = "none") +
          scale_color_manual(values=c("violet","firebrick")) +
          ggtitle("VPD")

SWCplot = ggplot(cumSWC) +
          geom_path(aes(0:13,HS_mean,color="HS"),size=1) +
          geom_path(aes(0:13,SP_mean,color="SP"),size=1) +
  geom_linerange(aes(x=-0.1:12.9,ymin=HS_min,ymax=HS_max,color="HS"),size=1) +
  geom_linerange(aes(x=0.1:13.1,ymin=SP_min,ymax=SP_max,color="SP"),size=1) +
          geom_hline(yintercept=0.5,linetype = "dashed") +
          coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
          xlab("Days into Past") +
          ylab("Cumulative Weight") +
          theme_bw() + 
          guides(color = "none") +
          scale_color_manual(values=c("violet","firebrick")) +
          ggtitle("SWC")

PPTplot = ggplot(cumPPT) +
          geom_path(aes(c(0,20,29,59,119,179,269,365),HS_mean,color="HS"),size=1) +
          geom_path(aes(c(0,20,29,59,119,179,269,365),SP_mean,color="SP"),size=1) +
  geom_linerange(aes(x=c(-2,18.5,27.5,57,117,177,267,363),ymin=HS_min,ymax=HS_max,color="HS"),size=1) +
  geom_linerange(aes(x=c(2,21.5,30.5,61,121,181,271,367),ymin=SP_min,ymax=SP_max,color="SP"),size=1) +
          geom_hline(yintercept=0.5,linetype = "dashed") +
          coord_cartesian(xlim = c(0, 365), ylim = c(0,1)) +
          xlab("Days into Past") +
          ylab("Cumulative Weight") +
          theme_bw() +
          theme(legend.position = "bottom") +
          scale_color_manual(values=c("violet","firebrick"),name = "Site") +
          ggtitle("PPT")


grid.arrange(SWRplot,Tairplot,VPDplot,SWCplot,PPTplot,
             widths = c(1,1,1,1),
             heights = c(3,3,4),
             layout_matrix = rbind(c(1,1,2,2),
                                   c(3,3,4,4),
                                   c(NA,5,5,NA)))






# Calculate AR(1) process

load('results/NEE_AR1_output_site_HS_2020-08-06.rda')
HS.Nmem = HowardSprings_Input$Nmem
HS.res_pred = summary(window(AR1.res[,paste("NEE.res_rep[", 2:HS.Nmem,"]", sep = '')]))$statistics[,1]

HS.fit = lm((HS_NEE_pred[2:HS.Nmem] - HS.res_pred) ~ HS_NEE_obs[2:HS.Nmem])
HS.AR1.R2 = summary(HS.fit)$r.squared
rm(AR1.res)

load('results/NEE_AR1_output_site_SP_2020-08-05.rda')
SP.Nmem = SturtPlains_Input$Nmem
SP.res_pred = summary(window(AR1.res[,paste("NEE.res_rep[", 2:SP.Nmem,"]", sep = '')]))$statistics[,1]

SP.fit = lm((SP_NEE_pred[2:SP.Nmem] - SP.res_pred) ~ SP_NEE_obs[2:SP.Nmem])
SP.AR1.R2 = summary(SP.fit)$r.squared
rm(AR1.res)

message("Howard Springs has R2 = ",round(HS.AR1.R2,3)," for AR(1)")
message("Sturt Plains has R2 = ",round(SP.AR1.R2,3)," for AR(1)")


message("Howard Springs has bio memory R2 improvement ",round(HS.AR1.R2-HS.SAM.R2,3))
message("Sturt Plains has bio memory R2 improvement ",round(SP.AR1.R2-SP.SAM.R2,3))




# Calculate current climate impact only

load('results/NEE_current_output_site_SP_2020-08-04.rda')
# data inside is called "nee_daily"
assign('SP.cur',nee_daily)
rm(nee_daily)

load('results/NEE_current_output_site_HS_2020-08-05.rda')
# data inside is called "nee_daily"
assign('HS.cur',nee_daily)
rm(nee_daily)

SP_NEE_cur = summary(window(SP.cur[,paste("NEE_pred[", 1:SP.Nmem,"]", sep = '')]))$statistics[,1]
HS_NEE_cur = summary(window(HS.cur[,paste("NEE_pred[", 1:HS.Nmem,"]", sep = '')]))$statistics[,1]

SP.fit = lm(SP_NEE_cur ~ SP_NEE_obs)
SP.CUR.R2 = summary(SP.fit)$r.squared
HS.fit = lm(HS_NEE_cur ~ HS_NEE_obs)
HS.CUR.R2 = summary(HS.fit)$r.squared

message("Howard Springs has R2 = ",round(HS.CUR.R2,3)," for current climate only")
message("Sturt Plains has R2 = ",round(SP.CUR.R2,3)," for current climate only")

message("Howard Springs has enviro memory R2 improvement ",round(HS.SAM.R2-HS.CUR.R2,3))
message("Sturt Plains has enviro memory R2 improvement ",round(SP.SAM.R2-SP.CUR.R2,3))




# Calculate current without SWR climate impact

load('results/NEE_currentSWR_output_site_SP_2020-08-05.rda')
# # data inside is called "nee_daily"
assign('SP.curSWR',nee_daily)
rm(nee_daily)

load('results/NEE_currentSWR_output_site_HS_2020-08-07.rda')
# data inside is called "nee_daily"
assign('HS.curSWR',nee_daily)
rm(nee_daily)

SP_NEE_curSWR = summary(window(SP.curSWR[,paste("NEE_pred[", 1:SP.Nmem,"]", sep = '')]))$statistics[,1]
HS_NEE_curSWR = summary(window(HS.curSWR[,paste("NEE_pred[", 1:HS.Nmem,"]", sep = '')]))$statistics[,1]

SP.fit = lm(SP_NEE_curSWR ~ SP_NEE_obs)
SP.CURSWR.R2 = summary(SP.fit)$r.squared
HS.fit = lm(HS_NEE_curSWR ~ HS_NEE_obs)
HS.CURSWR.R2 = summary(HS.fit)$r.squared


# Calculate current without SWC climate impact

load('results/NEE_currentSWC_output_site_SP_2020-08-06.rda')
# # data inside is called "nee_daily"
assign('SP.curSWC',nee_daily)
rm(nee_daily)

load('results/NEE_currentSWC_output_site_HS_2020-08-07.rda')
# data inside is called "nee_daily"
assign('HS.curSWC',nee_daily)
rm(nee_daily)

SP_NEE_curSWC = summary(window(SP.curSWC[,paste("NEE_pred[", 1:SP.Nmem,"]", sep = '')]))$statistics[,1]
HS_NEE_curSWC = summary(window(HS.curSWC[,paste("NEE_pred[", 1:HS.Nmem,"]", sep = '')]))$statistics[,1]

SP.fit = lm(SP_NEE_curSWC ~ SP_NEE_obs)
SP.CURSWC.R2 = summary(SP.fit)$r.squared
HS.fit = lm(HS_NEE_curSWC ~ HS_NEE_obs)
HS.CURSWC.R2 = summary(HS.fit)$r.squared


# Calculate current without Tair climate impact

load('results/NEE_currentTair_output_site_SP_2020-08-08.rda')
# # data inside is called "nee_daily"
assign('SP.curTair',nee_daily)
rm(nee_daily)

load('results/NEE_currentTair_output_site_HS_2020-08-08.rda')
# data inside is called "nee_daily"
assign('HS.curTair',nee_daily)
rm(nee_daily)

SP_NEE_curTair = summary(window(SP.curTair[,paste("NEE_pred[", 1:SP.Nmem,"]", sep = '')]))$statistics[,1]
HS_NEE_curTair = summary(window(HS.curTair[,paste("NEE_pred[", 1:HS.Nmem,"]", sep = '')]))$statistics[,1]

SP.fit = lm(SP_NEE_curTair ~ SP_NEE_obs)
SP.CURTair.R2 = summary(SP.fit)$r.squared
HS.fit = lm(HS_NEE_curTair ~ HS_NEE_obs)
HS.CURTair.R2 = summary(HS.fit)$r.squared


# Calculate current without VPD climate impact

load('results/NEE_currentVPD_output_site_SP_2020-08-08.rda')
# # data inside is called "nee_daily"
assign('SP.curVPD',nee_daily)
rm(nee_daily)

load('results/NEE_currentVPD_output_site_HS_2020-08-08.rda')
# data inside is called "nee_daily"
assign('HS.curVPD',nee_daily)
rm(nee_daily)

SP_NEE_curVPD = summary(window(SP.curVPD[,paste("NEE_pred[", 1:SP.Nmem,"]", sep = '')]))$statistics[,1]
HS_NEE_curVPD = summary(window(HS.curVPD[,paste("NEE_pred[", 1:HS.Nmem,"]", sep = '')]))$statistics[,1]

SP.fit = lm(SP_NEE_curVPD ~ SP_NEE_obs)
SP.CURVPD.R2 = summary(SP.fit)$r.squared
HS.fit = lm(HS_NEE_curVPD ~ HS_NEE_obs)
HS.CURVPD.R2 = summary(HS.fit)$r.squared


HS.SWR.dif = HS.CUR.R2-HS.CURSWR.R2
SP.SWR.dif = SP.CUR.R2-SP.CURSWR.R2

HS.SWC.dif = HS.CUR.R2-HS.CURSWC.R2
SP.SWC.dif = SP.CUR.R2-SP.CURSWC.R2

HS.VPD.dif = HS.CUR.R2-HS.CURVPD.R2
SP.VPD.dif = SP.CUR.R2-SP.CURVPD.R2

HS.Tair.dif = HS.CUR.R2-HS.CURTair.R2
SP.Tair.dif = SP.CUR.R2-SP.CURTair.R2

HS.SWR.R2contrib = (HS.SWR.dif)*(HS.CUR.R2/(HS.SWR.dif+HS.SWC.dif+HS.VPD.dif+HS.Tair.dif))
SP.SWR.R2contrib = (SP.SWR.dif)*(SP.CUR.R2/(SP.SWR.dif+SP.SWC.dif+SP.VPD.dif+SP.Tair.dif))

HS.SWC.R2contrib = (HS.SWC.dif)*(HS.CUR.R2/(HS.SWR.dif+HS.SWC.dif+HS.VPD.dif+HS.Tair.dif))
SP.SWC.R2contrib = (SP.SWC.dif)*(SP.CUR.R2/(SP.SWR.dif+SP.SWC.dif+SP.VPD.dif+SP.Tair.dif))

HS.VPD.R2contrib = (HS.VPD.dif)*(HS.CUR.R2/(HS.SWR.dif+HS.SWC.dif+HS.VPD.dif+HS.Tair.dif))
SP.VPD.R2contrib = (SP.VPD.dif)*(SP.CUR.R2/(SP.SWR.dif+SP.SWC.dif+SP.VPD.dif+SP.Tair.dif))

HS.Tair.R2contrib = (HS.Tair.dif)*(HS.CUR.R2/(HS.SWR.dif+HS.SWC.dif+HS.VPD.dif+HS.Tair.dif))
SP.Tair.R2contrib = (SP.Tair.dif)*(SP.CUR.R2/(SP.SWR.dif+SP.SWC.dif+SP.VPD.dif+SP.Tair.dif))

message("Howard Springs has R2 = ",round(HS.SWR.R2contrib,3)," for current SWR only")
message("Sturt Plains has R2 = ",round(SP.SWR.R2contrib,3)," for current SWR only")




# Recreate figure 3

Site = c(rep("SP",4),rep("HS",4))
Model = rep(c("CurrentSWR","Current Environmental","Environmental Memory","Biological Memory"),2)
Model = factor(Model,levels=c("Biological Memory","Environmental Memory","Current Environmental","CurrentSWR"))
Value = c(SP.SWR.R2contrib, SP.CUR.R2-SP.SWR.R2contrib,SP.SAM.R2-SP.CUR.R2,SP.AR1.R2-SP.SAM.R2,HS.SWR.R2contrib,HS.CUR.R2-HS.SWR.R2contrib,HS.SAM.R2-HS.CUR.R2,HS.AR1.R2-HS.SAM.R2)

Fig3 = data.frame(Site,Model,Value)

ggplot(Fig3,aes(fill=Model,y=Value,x=Site)) +
  geom_bar(position="stack",stat="identity") +
  geom_bar(stat = "identity", color = "black",size = 1) +
  coord_flip(ylim=c(0,1)) +
  scale_fill_manual(values=c("darkgreen","royalblue4","skyblue","yellow"),
                    guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  theme(legend.position = "bottom")
  


Sites = list("HS"=HS,"SP"=SP,"TT"=TT)

MemoryPlot(Sites)



## Recreating Figure 3 using functions
## 
source("MemoryR2.R")
source("MemoryR2_CABLE.R")


HS = MemoryR2("HowardSprings","HS")

TT = MemoryR2("TiTreeEast","TT")

SP = MemoryR2("SturtPlains","SP")

DU = MemoryR2("DalyUncleared","DU")

# LF = MemoryR2("Litchfield","LF")

DR = MemoryR2("DryRiver","DR")

AS = MemoryR2("AliceSprings","AS")

SP_CABLE = MemoryR2_CABLE("SturtPlains","SP")

HS_CABLE = MemoryR2_CABLE("HowardSprings","HS")

Sites = list("HS"=HS,"TT"=TT,"SP"=SP,"DU"=DU,"AS"=AS,"DR"=DR)
source("MemoryPlot.R")
Plot = MemoryPlot(Sites,"LAT")

Plot

HS = MemoryR2("HowardSprings","HS")

SP = MemoryR2("SturtPlains","SP")

SP_CABLE = MemoryR2_CABLE("SturtPlains","SP")

HS_CABLE = MemoryR2_CABLE("HowardSprings","HS")

Sites = list("HS"=HS,"SP"=SP,"SP_CABLE"=SP_CABLE,"HS_CABLE"=HS_CABLE)
source("MemoryPlot.R")
Plot = MemoryPlot(Sites,"LAT")

Plot


## Check overlapping dates

load("SPDailyData.Rdata")
load("HSDailyData.Rdata")
load("DUDailyData.Rdata")
load("DRDailyData.Rdata")
load("LFDailyData.Rdata")
load("TTDailyData.Rdata")

library(ggplot2)
library(scales)

Site = c("HS","SP","DR","DU","TT","LF")
Min = c(min(HSDailyData$TIMESTAMP),min(SPDailyData$TIMESTAMP),min(DRDailyData$TIMESTAMP),min(DUDailyData$TIMESTAMP),min(TTDailyData$TIMESTAMP),min(LFDailyData$TIMESTAMP))
Max = c(max(HSDailyData$TIMESTAMP),max(SPDailyData$TIMESTAMP),max(DRDailyData$TIMESTAMP),max(DUDailyData$TIMESTAMP),max(TTDailyData$TIMESTAMP),max(LFDailyData$TIMESTAMP))

df = data.frame(Site,Min,Max)

ggplot(df) +
  geom_errorbar(aes(x=Site,ymin=Min,ymax=Max),size = 2) +
  theme_bw() +
  scale_y_date(labels = date_format("%Y"),  breaks = breaks_width("1 year")) +
  ylab("OzFlux Data Availability") +
  coord_flip()





# Plot for overlapping dates

source("MemoryR2_match.R")

HS = MemoryR2_match("HowardSprings","HS")

TT = MemoryR2_match("TiTreeEast","TT")

SP = MemoryR2_match("SturtPlains","SP")

DU = MemoryR2_match("DalyUncleared","DU")

DR = MemoryR2_match("DryRiver","DR")

Sites = list("HS"=HS,"TT"=TT,"SP"=SP,"DU"=DU,"DR"=DR)
source("MemoryPlot.R")
Plot = MemoryPlot(Sites,"LAT")

# Plot for split of Howard Springs

source("MemoryR2_match.R")
source("MemoryR2_split.R")

HS_2013 = MemoryR2_match("HowardSprings","HS")

HS_2003 = MemoryR2_split("HowardSprings","HS",1)

HS_2008 = MemoryR2_split("HowardSprings","HS",2)


Sites = list("2003-2007"=HS_2003,"2008-2012"=HS_2008,"2013-2017"=HS_2013)
source("MemoryPlot.R")
Plot = MemoryPlot(Sites,"LAT")




### Plotting CABLE vs OBS weights


SPSWR = data.frame(x = 0:13,
                   SP_mean = ggplot_build(SP$SWRplot)$data[[1]]$y,
                   SP_min = ggplot_build(SP$SWRplot)$data[[2]]$ymin,
                   SP_max = ggplot_build(SP$SWRplot)$data[[2]]$ymax,
                   SP_CABLE_mean = ggplot_build(SP_CABLE$SWRplot)$data[[1]]$y,
                   SP_CABLE_min = ggplot_build(SP_CABLE$SWRplot)$data[[2]]$ymin,
                   SP_CABLE_max = ggplot_build(SP_CABLE$SWRplot)$data[[2]]$ymax)



SPSWC = data.frame(x = 0:13,
                   SP_mean = ggplot_build(SP$SWCplot)$data[[1]]$y,
                   SP_min = ggplot_build(SP$SWCplot)$data[[2]]$ymin,
                   SP_max = ggplot_build(SP$SWCplot)$data[[2]]$ymax,
                   SP_CABLE_mean = ggplot_build(SP_CABLE$SWCplot)$data[[1]]$y,
                   SP_CABLE_min = ggplot_build(SP_CABLE$SWCplot)$data[[2]]$ymin,
                   SP_CABLE_max = ggplot_build(SP_CABLE$SWCplot)$data[[2]]$ymax)


SPTair = data.frame(x = 0:13,
                    SP_mean = ggplot_build(SP$Tairplot)$data[[1]]$y,
                    SP_min = ggplot_build(SP$Tairplot)$data[[2]]$ymin,
                    SP_max = ggplot_build(SP$Tairplot)$data[[2]]$ymax,
                    SP_CABLE_mean = ggplot_build(SP_CABLE$Tairplot)$data[[1]]$y,
                    SP_CABLE_min = ggplot_build(SP_CABLE$Tairplot)$data[[2]]$ymin,
                    SP_CABLE_max = ggplot_build(SP_CABLE$Tairplot)$data[[2]]$ymax)


SPVPD = data.frame(x = 0:13,
                   SP_mean = ggplot_build(SP$VPDplot)$data[[1]]$y,
                   SP_min = ggplot_build(SP$VPDplot)$data[[2]]$ymin,
                   SP_max = ggplot_build(SP$VPDplot)$data[[2]]$ymax,
                   SP_CABLE_mean = ggplot_build(SP_CABLE$VPDplot)$data[[1]]$y,
                   SP_CABLE_min = ggplot_build(SP_CABLE$VPDplot)$data[[2]]$ymin,
                   SP_CABLE_max = ggplot_build(SP_CABLE$VPDplot)$data[[2]]$ymax)



SPPPT = data.frame(x = c(0,20,29,59,119,179,269,365),
                   SP_mean = ggplot_build(SP$PPTplot)$data[[1]]$y,
                   SP_min = ggplot_build(SP$PPTplot)$data[[2]]$ymin,
                   SP_max = ggplot_build(SP$PPTplot)$data[[2]]$ymax,
                   SP_CABLE_mean = ggplot_build(SP_CABLE$PPTplot)$data[[1]]$y,
                   SP_CABLE_min = ggplot_build(SP_CABLE$PPTplot)$data[[2]]$ymin,
                   SP_CABLE_max = ggplot_build(SP_CABLE$PPTplot)$data[[2]]$ymax)


SPSWRPlot = ggplot(SPSWR) +
  geom_path(aes(x,y=SP_mean,color="Obs"),size=1) +
  geom_path(aes(x,y=SP_CABLE_mean,color="CABLE"),size=1) +
  geom_linerange(aes(x=-0.1:12.9,ymin=SP_min,ymax=SP_max,color="Obs"),size=1) +
  geom_linerange(aes(x=0.1:13.1,ymin=SP_CABLE_min,ymax=SP_CABLE_max,color="CABLE"),size=1) +
  geom_hline(yintercept=0.5,linetype = "dashed") +
  coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
  xlab("Days into Past") +
  ylab("Cumulative Weight") +
  theme_bw() +
  guides(color = "none") +
  scale_color_manual(values=c("blue","black")) +
  ggtitle("SWR")


SPSWCPlot = ggplot(SPSWC) +
  geom_path(aes(x,y=SP_mean,color="Obs"),size=1) +
  geom_path(aes(x,y=SP_CABLE_mean,color="CABLE"),size=1) +
  geom_linerange(aes(x=-0.1:12.9,ymin=SP_min,ymax=SP_max,color="Obs"),size=1) +
  geom_linerange(aes(x=0.1:13.1,ymin=SP_CABLE_min,ymax=SP_CABLE_max,color="CABLE"),size=1) +
  geom_hline(yintercept=0.5,linetype = "dashed") +
  coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
  xlab("Days into Past") +
  ylab("Cumulative Weight") +
  theme_bw() +
  guides(color = "none") +
  scale_color_manual(values=c("blue","black")) +
  ggtitle("SWC")

SPTairPlot = ggplot(SPTair) +
  geom_path(aes(x,y=SP_mean,color="Obs"),size=1) +
  geom_path(aes(x,y=SP_CABLE_mean,color="CABLE"),size=1) +
  geom_linerange(aes(x=-0.1:12.9,ymin=SP_min,ymax=SP_max,color="Obs"),size=1) +
  geom_linerange(aes(x=0.1:13.1,ymin=SP_CABLE_min,ymax=SP_CABLE_max,color="CABLE"),size=1) +
  geom_hline(yintercept=0.5,linetype = "dashed") +
  coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
  xlab("Days into Past") +
  ylab("Cumulative Weight") +
  theme_bw() +
  guides(color = "none") +
  scale_color_manual(values=c("blue","black")) +
  ggtitle("Tair")

SPVPDPlot = ggplot(SPVPD) +
  geom_path(aes(x,y=SP_mean,color="Obs"),size=1) +
  geom_path(aes(x,y=SP_CABLE_mean,color="CABLE"),size=1) +
  geom_linerange(aes(x=-0.1:12.9,ymin=SP_min,ymax=SP_max,color="Obs"),size=1) +
  geom_linerange(aes(x=0.1:13.1,ymin=SP_CABLE_min,ymax=SP_CABLE_max,color="CABLE"),size=1) +
  geom_hline(yintercept=0.5,linetype = "dashed") +
  coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
  xlab("Days into Past") +
  ylab("Cumulative Weight") +
  theme_bw() +
  guides(color = "none") +
  scale_color_manual(values=c("blue","black")) +
  ggtitle("VPD")


SPPPTPlot = ggplot(SPPPT) +
  geom_path(aes(c(0,20,29,59,119,179,269,365),SP_mean,color="Obs"),size=1) +
  geom_path(aes(c(0,20,29,59,119,179,269,365),SP_CABLE_mean,color="CABLE"),size=1) +
  geom_linerange(aes(x=c(-2,18.5,27.5,57,117,177,267,363),ymin=SP_min,ymax=SP_max,color="Obs"),size=1) +
  geom_linerange(aes(x=c(2,21.5,30.5,61,121,181,271,367),ymin=SP_CABLE_min,ymax=SP_CABLE_max,color="CABLE"),size=1) +
  geom_hline(yintercept=0.5,linetype = "dashed") +
  coord_cartesian(xlim = c(0, 365), ylim = c(0,1)) +
  xlab("Days into Past") +
  ylab("Cumulative Weight") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_manual(values=c("blue","black"),name = "Data") +
  ggtitle("PPT")


grid.arrange(SPSWRPlot,SPTairPlot,SPVPDPlot,SPSWCPlot,SPPPTPlot,
             widths = c(1,1,1,1),
             heights = c(3,3,4),
             layout_matrix = rbind(c(1,1,2,2),
                                   c(3,3,4,4),
                                   c(NA,5,5,NA)),
             top = "Sturt Plains")



### Plot Howard Springs weights for 5 year periods
rm(list = ls())


source("MemoryR2_match.R")
source("MemoryR2_split.R")

HS_2013 = MemoryR2_match("HowardSprings","HS")

HS_2003 = MemoryR2_split("HowardSprings","HS",1)

HS_2008 = MemoryR2_split("HowardSprings","HS",2)


Sites = list("2003-2007"=HS_2003,"2008-2012"=HS_2008,"2013-2017"=HS_2013)
source("MemoryPlot.R")

library(ggplot2)
library(gridExtra)
HSSWR = data.frame(x = 0:13,
                   HS_mean_2003 = ggplot_build(HS_2003$SWRplot)$data[[1]]$y,
                   HS_min_2003 = ggplot_build(HS_2003$SWRplot)$data[[2]]$ymin,
                   HS_max_2003 = ggplot_build(HS_2003$SWRplot)$data[[2]]$ymax,
                   HS_mean_2008 = ggplot_build(HS_2008$SWRplot)$data[[1]]$y,
                   HS_min_2008 = ggplot_build(HS_2008$SWRplot)$data[[2]]$ymin,
                   HS_max_2008 = ggplot_build(HS_2008$SWRplot)$data[[2]]$ymax,
                   HS_mean_2013 = ggplot_build(HS_2013$SWRplot)$data[[1]]$y,
                   HS_min_2013 = ggplot_build(HS_2013$SWRplot)$data[[2]]$ymin,
                   HS_max_2013 = ggplot_build(HS_2013$SWRplot)$data[[2]]$ymax)



HSSWC = data.frame(x = 0:13,
                   HS_mean_2003 = ggplot_build(HS_2003$SWCplot)$data[[1]]$y,
                   HS_min_2003 = ggplot_build(HS_2003$SWCplot)$data[[2]]$ymin,
                   HS_max_2003 = ggplot_build(HS_2003$SWCplot)$data[[2]]$ymax,
                   HS_mean_2008 = ggplot_build(HS_2008$SWCplot)$data[[1]]$y,
                   HS_min_2008 = ggplot_build(HS_2008$SWCplot)$data[[2]]$ymin,
                   HS_max_2008 = ggplot_build(HS_2008$SWCplot)$data[[2]]$ymax,
                   HS_mean_2013 = ggplot_build(HS_2013$SWCplot)$data[[1]]$y,
                   HS_min_2013 = ggplot_build(HS_2013$SWCplot)$data[[2]]$ymin,
                   HS_max_2013 = ggplot_build(HS_2013$SWCplot)$data[[2]]$ymax)


HSTair = data.frame(x = 0:13,
                    HS_mean_2003 = ggplot_build(HS_2003$Tairplot)$data[[1]]$y,
                    HS_min_2003 = ggplot_build(HS_2003$Tairplot)$data[[2]]$ymin,
                    HS_max_2003 = ggplot_build(HS_2003$Tairplot)$data[[2]]$ymax,
                    HS_mean_2008 = ggplot_build(HS_2008$Tairplot)$data[[1]]$y,
                    HS_min_2008 = ggplot_build(HS_2008$Tairplot)$data[[2]]$ymin,
                    HS_max_2008 = ggplot_build(HS_2008$Tairplot)$data[[2]]$ymax,
                    HS_mean_2013 = ggplot_build(HS_2013$Tairplot)$data[[1]]$y,
                    HS_min_2013 = ggplot_build(HS_2013$Tairplot)$data[[2]]$ymin,
                    HS_max_2013 = ggplot_build(HS_2013$Tairplot)$data[[2]]$ymax)


HSVPD = data.frame(x = 0:13,
                   HS_mean_2003 = ggplot_build(HS_2003$VPDplot)$data[[1]]$y,
                   HS_min_2003 = ggplot_build(HS_2003$VPDplot)$data[[2]]$ymin,
                   HS_max_2003 = ggplot_build(HS_2003$VPDplot)$data[[2]]$ymax,
                   HS_mean_2008 = ggplot_build(HS_2008$VPDplot)$data[[1]]$y,
                   HS_min_2008 = ggplot_build(HS_2008$VPDplot)$data[[2]]$ymin,
                   HS_max_2008 = ggplot_build(HS_2008$VPDplot)$data[[2]]$ymax,
                   HS_mean_2013 = ggplot_build(HS_2013$VPDplot)$data[[1]]$y,
                   HS_min_2013 = ggplot_build(HS_2013$VPDplot)$data[[2]]$ymin,
                   HS_max_2013 = ggplot_build(HS_2013$VPDplot)$data[[2]]$ymax)



HSPPT = data.frame(x = c(0,20,29,59,119,179,269,365),
                   HS_mean_2003 = ggplot_build(HS_2003$PPTplot)$data[[1]]$y,
                   HS_min_2003 = ggplot_build(HS_2003$PPTplot)$data[[2]]$ymin,
                   HS_max_2003 = ggplot_build(HS_2003$PPTplot)$data[[2]]$ymax,
                   HS_mean_2008 = ggplot_build(HS_2008$PPTplot)$data[[1]]$y,
                   HS_min_2008 = ggplot_build(HS_2008$PPTplot)$data[[2]]$ymin,
                   HS_max_2008 = ggplot_build(HS_2008$PPTplot)$data[[2]]$ymax,
                   HS_mean_2013 = ggplot_build(HS_2013$PPTplot)$data[[1]]$y,
                   HS_min_2013 = ggplot_build(HS_2013$PPTplot)$data[[2]]$ymin,
                   HS_max_2013 = ggplot_build(HS_2013$PPTplot)$data[[2]]$ymax)


HSSWRPlot = ggplot(HSSWR) +
  geom_path(aes(x,y=HS_mean_2003,color="2003-2007"),size=1) +
  geom_path(aes(x,y=HS_mean_2008,color="2008-2012"),size=1) +
  geom_path(aes(x,y=HS_mean_2013,color="2013-2017"),size=1) +
  geom_linerange(aes(x=-0.1:12.9,ymin=HS_min_2003,ymax=HS_max_2003,color="2003-2007"),size=1) +
  geom_linerange(aes(x=0:13,ymin=HS_min_2008,ymax=HS_max_2008,color="2008-2012"),size=1) +
  geom_linerange(aes(x=0.1:13.1,ymin=HS_min_2013,ymax=HS_max_2013,color="2013-2017"),size=1) +
  geom_hline(yintercept=0.5,linetype = "dashed") +
  coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
  xlab("Days into Past") +
  ylab("Cumulative Weight") +
  theme_bw() +
  guides(color = "none") +
  scale_color_manual(values=c("green","orange","purple")) +
  ggtitle("SWR")


HSSWCPlot = ggplot(HSSWC) +
  geom_path(aes(x,y=HS_mean_2003,color="2003-2007"),size=1) +
  geom_path(aes(x,y=HS_mean_2008,color="2008-2012"),size=1) +
  geom_path(aes(x,y=HS_mean_2013,color="2013-2017"),size=1) +
  geom_linerange(aes(x=-0.1:12.9,ymin=HS_min_2003,ymax=HS_max_2003,color="2003-2007"),size=1) +
  geom_linerange(aes(x=0:13,ymin=HS_min_2008,ymax=HS_max_2008,color="2008-2012"),size=1) +
  geom_linerange(aes(x=0.1:13.1,ymin=HS_min_2013,ymax=HS_max_2013,color="2013-2017"),size=1) +
  geom_hline(yintercept=0.5,linetype = "dashed") +
  coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
  xlab("Days into Past") +
  ylab("Cumulative Weight") +
  theme_bw() +
  guides(color = "none") +
  scale_color_manual(values=c("green","orange","purple")) +
  ggtitle("SWC")

HSTairPlot = ggplot(HSTair) +
  geom_path(aes(x,y=HS_mean_2003,color="2003-2007"),size=1) +
  geom_path(aes(x,y=HS_mean_2008,color="2008-2012"),size=1) +
  geom_path(aes(x,y=HS_mean_2013,color="2013-2017"),size=1) +
  geom_linerange(aes(x=-0.1:12.9,ymin=HS_min_2003,ymax=HS_max_2003,color="2003-2007"),size=1) +
  geom_linerange(aes(x=0:13,ymin=HS_min_2008,ymax=HS_max_2008,color="2008-2012"),size=1) +
  geom_linerange(aes(x=0.1:13.1,ymin=HS_min_2013,ymax=HS_max_2013,color="2013-2017"),size=1) +
  geom_hline(yintercept=0.5,linetype = "dashed") +
  coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
  xlab("Days into Past") +
  ylab("Cumulative Weight") +
  theme_bw() +
  guides(color = "none") +
  scale_color_manual(values=c("green","orange","purple")) +
  ggtitle("Tair")

HSVPDPlot = ggplot(HSVPD) +
  geom_path(aes(x,y=HS_mean_2003,color="2003-2007"),size=1) +
  geom_path(aes(x,y=HS_mean_2008,color="2008-2012"),size=1) +
  geom_path(aes(x,y=HS_mean_2013,color="2013-2017"),size=1) +
  geom_linerange(aes(x=-0.1:12.9,ymin=HS_min_2003,ymax=HS_max_2003,color="2003-2007"),size=1) +
  geom_linerange(aes(x=0:13,ymin=HS_min_2008,ymax=HS_max_2008,color="2008-2012"),size=1) +
  geom_linerange(aes(x=0.1:13.1,ymin=HS_min_2013,ymax=HS_max_2013,color="2013-2017"),size=1) +
  geom_hline(yintercept=0.5,linetype = "dashed") +
  coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
  xlab("Days into Past") +
  ylab("Cumulative Weight") +
  theme_bw() +
  guides(color = "none") +
  scale_color_manual(values=c("green","orange","purple")) +
  ggtitle("VPD")


HSPPTPlot = ggplot(HSPPT) +
  geom_path(aes(c(0,20,29,59,119,179,269,365),y=HS_mean_2003,color="2003-2007"),size=1) +
  geom_path(aes(c(0,20,29,59,119,179,269,365),y=HS_mean_2008,color="2008-2012"),size=1) +
  geom_path(aes(c(0,20,29,59,119,179,269,365),y=HS_mean_2013,color="2013-2017"),size=1) +
  geom_linerange(aes(x=c(-2,18,27,57,117,177,267,363),ymin=HS_min_2003,ymax=HS_max_2003,color="2003-2007"),size=1) +
  geom_linerange(aes(x=c(0,20,29,59,119,179,269,365),ymin=HS_min_2008,ymax=HS_max_2008,color="2008-2012"),size=1) +
  geom_linerange(aes(x=c(2,22,31,61,121,181,271,367),ymin=HS_min_2013,ymax=HS_max_2013,color="2013-2017"),size=1) +
  geom_hline(yintercept=0.5,linetype = "dashed") +
  coord_cartesian(xlim = c(0, 365), ylim = c(0,1)) +
  xlab("Days into Past") +
  ylab("Cumulative Weight") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_manual(values=c("green","orange","purple"),name = "Data") +
  ggtitle("PPT")


grid.arrange(HSSWRPlot,HSTairPlot,HSVPDPlot,HSSWCPlot,HSPPTPlot,
             widths = c(1,1,1,1),
             heights = c(3,3,4),
             layout_matrix = rbind(c(1,1,2,2),
                                   c(3,3,4,4),
                                   c(NA,5,5,NA)),
             top = "Howard Springs - 5 Year Periods")



###
### Plotting of NATT and SW with actual sites
###

source("MemoryR2.R")

HS = MemoryR2("HowardSprings","HS")

TT = MemoryR2("TiTreeEast","TT")

SP = MemoryR2("SturtPlains","SP")

DU = MemoryR2("DalyUncleared","DU")

DR = MemoryR2("DryRiver","DR")

AS = MemoryR2("AliceSprings","AS")

Sites = list("HS"=HS,"TT"=TT,"SP"=SP,"DU"=DU,"AS"=AS,"DR"=DR)
source("MemoryPlot.R")
Plot_NATT = MemoryPlot(Sites,"LAT")

Plot_NATT




TR = MemoryR2("Tumbarumba","TR")

WO = MemoryR2("Whroo","WO")

GW = MemoryR2("GreatWesternWoodlands","GW")

GG = MemoryR2("Gingin","GG")

CP = MemoryR2("Calperum","CP")

Sites = list("TR"=TR,"WO"=WO,"GW"=GW,"GG"=GG,"CP"=CP)
source("MemoryPlot.R")
Plot_SW = MemoryPlot(Sites,"MAP")

Plot_SW