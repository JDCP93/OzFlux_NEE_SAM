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



#*******************************************************************************
# Calculating mean air temperature
#*******************************************************************************

for (Site in Sites){
  load(paste0(Site,"_Input.Rdata"))
  output = mean(eval(as.name(paste0(Site,"_Input")))$DailyData$Ta,na.rm=TRUE)
  name = paste0(Site,"_meanTa")
  assign(name,output)
}


# Plot a histogram of air temperature
hist(`AU-Wom_Input`$DailyData$Ta)

#*******************************************************************************
# Plotting average climate data at each site
#*******************************************************************************

for (Site in Sites){
  load(paste0(Site,"_Input.Rdata"))
Input = eval(as.name(paste0(Site,"_Input")))
DailyData = Input$DailyData
DailyData$month = month(DailyData$TIMESTAMP)

AvgMonthData <- DailyData %>%
  group_by(month) %>%               # group by the year column
  summarise(NEE=mean(NEE,na.rm=TRUE),
            Fsd=mean(Fsd),
            Ta=mean(Ta),
            VPD=mean(VPD),
            Sws=mean(Sws),
            Precip=sum(Precip,na.rm=TRUE))

plot(AvgMonthData$month,AvgMonthData$NEE,type='l',main=paste(Site,"NEE"))
plot(AvgMonthData$month,AvgMonthData$Precip,type='l',main=paste(Site,"Precip"))
}

#*******************************************************************************
# Identifying the parameters and chains that haven't converged
#*******************************************************************************
library(coda)

stochastic.params = c("phi0",
                      "sig_y",
                      sprintf("deltaXAP[%d]",seq(1:8)),
                      sprintf("deltaXA[%d,%d]",rep(1:4,10),rep(1:10,each=4)),
                      sprintf("an[%d]",seq(1:16)),
                      sprintf("ag[%d]",seq(1:16)))
# For each chain
for (i in 1:length(output.mcmc)){
  # Summarise the chain
  chain = summary(output.mcmc[[i]])
  name = paste0("Chain.",i)
  assign(name,chain)
  # Find the chain deviance
  dev = chain$statistics[substr(rownames(chain$statistics),1,3)=="dev",1]
  name = paste0("Dev.",i)
  assign(name,dev)
  # Trim the chain data to parameters of interest
  trim = chain$statistics[rownames(chain$statistics) %in% stochastic.params,1]
  name = paste0("Trim.",i)
  assign(name,trim)
}

# Put the chain data into one dataframe
df = data.frame(rep(NA,length(Trim.1)))
for (i in 1:length(output.mcmc)){
  df[i]= eval(as.name(paste0("Trim.",i)))
  colnames(df)[i] = paste0("Trim.",i)
  rownames(df) = names(Trim.1)
}


## Use the below to remove the chains that haven't converged - change the value
# output.mcmc = output.mcmc[1:6]
  
#*******************************************************************************
# Plotting weights and sensitivities for RTPVS
#*******************************************************************************
  
  rm(list=ls())
  
  source("SensitivityPlot_RTPVS.R")
  source("WeightPlot_RTPVS.R")
  Sites = c("AU-ASM","AU-Cum","AU-Cpr","AU-DaS","AU-Dry","AU-Gin","AU-GWW","AU-How","AU-Stp","AU-TTE","AU-Tum","AU-Whr","AU-Wom")
  ESenPlot = SensitivityPlot_RTPVS(Sites,c("Precip"))
  WeightPlot = WeightPlot_RTPVS(Sites,c("Precip"))
  
  ESenPlot
  WeightPlot
  
#*******************************************************************************
# RTPV analysis
#*******************************************************************************  

  
Sites = c("AU-Gin","AU-Whr","AU-Wom")
source("r2jags_analysis_RTPV.R")
for (Site in Sites){
  r2jags_analysis_RTPV(Site)
}

#*******************************************************************************
# Conducting AR1 analysis
#*******************************************************************************

rm(list=ls())
source("r2jags_analysis_AR1.R")
Sites = c(#"AU-ASM"
          #,"AU-Cpr"
          #,"AU-Cum"
          "AU-DaS"
          ,"AU-Dry"
          ,"AU-Gin"
          ,"AU-GWW"
          ,"AU-Stp"
          ,"AU-TTE"
          ,"AU-Tum"
          ,"AU-Whr"
          ,"AU-Wom")
for (Site in Sites){
  r2jags_analysis_AR1(Site)
}


#*******************************************************************************
# Plotting RTPV SAM vs RTPVS SAM
#*******************************************************************************


rm(list=ls())
# List sites
Sites = c("AU-Cum"
          ,"AU-DaS"
          ,"AU-GWW"
          ,"AU-How"
          ,"AU-Stp"
)

Model = c("RTPVS","RTPV")

RTPVS.R2 = c(0.271,0.388,0.514,0.342,0.58)
RTPV.R2 = c(0.273,0.35,0.531,0.336,0.574)

df = data.frame("Site" = rep(Sites,2),
                "Model" = rep(Model,each=5),
                "Value" = c(RTPVS.R2,RTPV.R2))

plot = ggplot(df,aes(x = Site,y = Value, fill = Model)) +
  geom_bar(stat='identity', position='dodge',color="black") +
  coord_flip(ylim=c(0,1)) +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE),
                       begin=0.2,
                       end=0.8) +
  theme_bw()

plot

#*******************************************************************************
# Plotting k-means vs SAM models
#*******************************************************************************

rm(list=ls())
# List sites
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

Transects = c("NATT",
              "SAWS",
              "SAWS",
              "NATT",
              "NATT",
              "SAWS",
              "SAWS",
              "NATT",
              "NATT",
              "NATT",
              "SAWS",
              "SAWS",
              "SAWS"
)

Model = c("kmeanCur","kmeanPrecip","kmeanNDVI","SAMcur","SAMlag")

R2 = data.frame("Site" = Sites,
                "R2.KMP" = 0,
                "R2.KMC" = 0,
                "R2.KMN" = 0,
                "R2.Cur" = 0,
                "R2.SAM" = 0)

# For each site
for (Site in Sites){
  # Collect the R2 values from the analysis scripts
  message("Collating k-means R2 values for ",Site)
  # Load the analysis results
  load(paste0("alternate/RTPVS/results/NEE_output_kmean_RTPVS_",Site,".Rdata"))
  R2$R2.KMP[R2$Site==Site] = output$r.squared
  load(paste0("alternate/RTPVS/results/NEE_output_kmean_current_RTPVS_",Site,".Rdata"))
  R2$R2.KMC[R2$Site==Site] = output$r.squared
  load(paste0("alternate/RTPVS/results/NEE_output_kmean_currentNDVI_RTPVS_",Site,".Rdata"))
  R2$R2.KMN[R2$Site==Site] = output$r.squared
  load(paste0("analysis/RTPVS/NEE_current_Analysis_RTPVS_",Site,".Rdata"))
  R2$R2.Cur = output$CUR.R2
  load(paste0("analysis/RTPVS/NEE_Analysis_RTPVS_",Site,".Rdata"))
  R2$R2.SAM = output$SAM.R2
}

# Significance tests between transects

t.test(R2$R2.KMC[R2$Transect=="NATT"],R2$R2.KMC[R2$Transect=="SAWS"],alternative = "less")
t.test(R2$R2.CUR[R2$Transect=="NATT"],R2$R2.CUR[R2$Transect=="SAWS"])
t.test(R2$R2.SAM[R2$Transect=="NATT"],R2$R2.SAM[R2$Transect=="SAWS"])
t.test((R2$R2.SAM[R2$Transect=="NATT"]-R2$R2.CUR[R2$Transect=="NATT"])/R2$R2.CUR[R2$Transect=="NATT"],(R2$R2.SAM[R2$Transect=="SAWS"]-R2$R2.CUR[R2$Transect=="SAWS"])/R2$R2.CUR[R2$Transect=="SAWS"])

# Plotting
df = data.frame("Site" = rep(Sites,5),
                "Model" = rep(Model,each=13),
                "Value" = c(R2$R2.KMC,R2$R2.KMP,R2$R2.KMN,R2$R2.Cur,R2$R2.SAM))

plot = ggplot(df,aes(x = Site,y = Value, fill = Model)) +
  geom_bar(stat='identity', position='dodge',color="black") +
  coord_flip(ylim=c(0,1)) +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE),
                       begin=0.2,
                       end=0.8) +
  theme_bw()

plot