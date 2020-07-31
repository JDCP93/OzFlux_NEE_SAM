
# A junk script to test out various diagnostic tools and explore model output
# in a haphazard manner
rm(list=ls())
# load model inputs
load('SturtPlains_Input_NIRV.Rdata')
load('HowardSprings_Input_NIRV.Rdata')

# Load Howard Springs data
load('results/NIRV&PoorESS/NEE_output_site_HS_2020-07-25.rda')
# data inside is called "nee_daily"
assign('HS',nee_daily)
rm(nee_daily)

# Load Sturt Plains data
load('results/NIRV&PoorESS/NEE_output_site_SP_2020-07-25.rda')
# data inside is called "nee_daily"
assign('SP',nee_daily)
rm(nee_daily)


# set version
library(coda)

# # We want all values to be close to 1
# HS_Gelman = gelman.diag(HS,multivariate=FALSE)
# SP_Gelman = gelman.diag(SP,multivariate=FALSE)


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
# diagMCMC(codaObject = HS, parName = "weightA[1,1]")
# diagMCMC(codaObject = SP, parName = "weightA[1,1]")

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
HS_cumSWR = HS.summary$statistics[substr(rownames(HS.summary$statistics),1,13)=="cum_weightA[2",1]
HS_cumTair = HS.summary$statistics[substr(rownames(HS.summary$statistics),1,13)=="cum_weightA[1",1]
HS_cumVPD = HS.summary$statistics[substr(rownames(HS.summary$statistics),1,13)=="cum_weightA[3",1]
HS_cumSWC = HS.summary$statistics[substr(rownames(HS.summary$statistics),1,13)=="cum_weightA[5",1]
HS_cumPPT = HS.summary$statistics[substr(rownames(HS.summary$statistics),1,12)=="cum_weightAP",1]

SP_cumSWR = SP.summary$statistics[substr(rownames(SP.summary$statistics),1,13)=="cum_weightA[2",1]
SP_cumTair = SP.summary$statistics[substr(rownames(SP.summary$statistics),1,13)=="cum_weightA[1",1]
SP_cumVPD = SP.summary$statistics[substr(rownames(SP.summary$statistics),1,13)=="cum_weightA[3",1]
SP_cumSWC = SP.summary$statistics[substr(rownames(SP.summary$statistics),1,13)=="cum_weightA[5",1]
SP_cumPPT = SP.summary$statistics[substr(rownames(SP.summary$statistics),1,12)=="cum_weightAP",1]


cumSWR = data.frame("HS" = HS_cumSWR,"SP" = SP_cumSWR)
cumTair = data.frame("HS" = HS_cumTair,"SP" = SP_cumTair)
cumVPD = data.frame("HS" = HS_cumVPD,"SP" = SP_cumVPD)
cumSWC = data.frame("HS" = HS_cumSWC,"SP" = SP_cumSWC)
cumPPT = data.frame("HS" = HS_cumPPT,"SP" = SP_cumPPT)

SWRplot = ggplot(cumSWR) +
          geom_path(aes(0:13,HS,color="HS"),size=1) +
          geom_path(aes(0:13,SP,color="SP"),size=1) +
          geom_hline(yintercept=0.5,linetype = "dashed") +
          coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
          xlab("Days into Past") +
          ylab("Cumulative Weight") +
          theme_bw() +
          guides(color = "none") +
          ggtitle("SWR")

Tairplot = ggplot(cumTair) +
            geom_path(aes(0:13,HS,color="HS"),size=1) +
            geom_path(aes(0:13,SP,color="SP"),size=1) +
            geom_hline(yintercept=0.5,linetype = "dashed") +
            coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
            xlab("Days into Past") +
            ylab("Cumulative Weight") +
            theme_bw() +
            guides(color = "none") +
            ggtitle("Tair")

VPDplot = ggplot(cumVPD) +
          geom_path(aes(0:13,HS,color="HS"),size=1) +
          geom_path(aes(0:13,SP,color="SP"),size=1) +
          geom_hline(yintercept=0.5,linetype = "dashed") +
          coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
          xlab("Days into Past") +
          ylab("Cumulative Weight") +
          theme_bw() +
          guides(color = "none") +
          ggtitle("VPD")

SWCplot = ggplot(cumSWC) +
          geom_path(aes(0:13,HS,color="HS"),size=1) +
          geom_path(aes(0:13,SP,color="SP"),size=1) +
          geom_hline(yintercept=0.5,linetype = "dashed") +
          coord_cartesian(xlim = c(0, 13), ylim = c(0,1)) +
          xlab("Days into Past") +
          ylab("Cumulative Weight") +
          theme_bw() + 
          guides(color = "none") +
          ggtitle("SWC")

PPTplot = ggplot(cumPPT) +
          geom_path(aes(c(0,20,29,59,119,179,269,365),HS,color="HS"),size=1) +
          geom_path(aes(c(0,20,29,59,119,179,269,365),SP,color="SP"),size=1) +
          geom_hline(yintercept=0.5,linetype = "dashed") +
          coord_cartesian(xlim = c(0, 365), ylim = c(0,1)) +
          xlab("Days into Past") +
          ylab("Cumulative Weight") +
          theme_bw() +
          theme(legend.position = "bottom") +
          scale_color_discrete(name = "Site") +
          ggtitle("PPT")


grid.arrange(SWRplot,Tairplot,VPDplot,SWCplot,PPTplot,
             widths = c(1,1,1,1),
             heights = c(3,3,4),
             layout_matrix = rbind(c(1,1,2,2),
                                   c(3,3,4,4),
                                   c(NA,5,5,NA)))






# Calculate AR(1) process

load('results/NIRV&PoorESS/NEE_AR1_output_site_HS_2020-07-27.rda')
HS.Nmem = HowardSprings_Input$Nmem
HS.res_pred = summary(window(AR1.res[,paste("NEE.res_rep[", 2:HS.Nmem,"]", sep = '')]))$statistics[,1]

HS.fit = lm((HS_NEE_pred[2:HS.Nmem] - HS.res_pred) ~ HS_NEE_obs[2:HS.Nmem])
HS.AR1.R2 = summary(HS.fit)$r.squared
rm(AR1.res)

load('results/NIRV&PoorESS/NEE_AR1_output_site_SP_2020-07-27.rda')
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

load('results/NIRV&PoorESS/NEE_current_output_site_SP_2020-07-28.rda')
# data inside is called "nee_daily"
assign('SP.cur',nee_daily)
rm(nee_daily)

load('results/NIRV&PoorESS/NEE_current_output_site_HS_2020-07-30.rda')
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

# load('results/NEE_currentSWR_output_site_SP_2020-07-28.rda')
# # data inside is called "nee_daily"
# assign('SP.curSWR',nee_daily)
# rm(nee_daily)

load('results/NIRV&PoorESS/NEE_currentSWR_output_site_HS_2020-07-31.rda')
# data inside is called "nee_daily"
assign('HS.curSWR',nee_daily)
rm(nee_daily)

# SP_NEE_curSWR = summary(window(SP.curSWR[,paste("NEE_pred[", 1:SP.Nmem,"]", sep = '')]))$statistics[,1]
HS_NEE_curSWR = summary(window(HS.curSWR[,paste("NEE_pred[", 1:HS.Nmem,"]", sep = '')]))$statistics[,1]

# SP.fit = lm(SP_NEE_curSWR ~ SP_NEE_obs)
# SP.CURSWR.R2 = summary(SP.fit)$r.squared
HS.fit = lm(HS_NEE_curSWR ~ HS_NEE_obs)
HS.CURSWR.R2 = summary(HS.fit)$r.squared

message("Howard Springs has R2 = ",round(HS.CURSWR.R2,3)," for current climate without SWR")
# message("Sturt Plains has R2 = ",round(SP.CURSWR.R2,3)," for current climate only")

message("Howard Springs has SWR memory R2 improvement ",round(HS.CUR.R2-HS.CURSWR.R2,3))
# message("Sturt Plains has SWR memory R2 improvement ",round(SP.CUR.R2-SP.CURSWR.R2,3))