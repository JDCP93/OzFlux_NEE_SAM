Sites = c("AU-ASM",
          "AU-Cpr",
          "AU-Cum",
          "AU-DaS",
          "AU-Dry",
          "AU-Gin",
          "AU-GWW",
          "AU-How",
          "AU-Stp",
          "AU-TTE",
          "AU-Tum",
          "AU-Whr",
          "AU-Wom")


for (Site in Sites){
  load(paste0(Site,"_Input.Rdata"))
  output = mean(eval(as.name(paste0(Site,"_Input")))$DailyData$Ta,na.rm=TRUE)
  name = paste0(Site,"_meanTa")
  assign(name,output)
}



hist(`AU-Wom_Input`$DailyData$Ta)


source("r2jags_analysis.R")
r2jags_analysis("AU-Gin")


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




#**********************************
## Identifying the parameters and chains that haven't converged
#**********************************

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

Sites = c("AU-DaS","AU-Stp","AU-TTE")

for(Site in Sites){
  
  File = list.files("results",pattern = Site)
  # Read the data into R - note that if multiple results are available for a 
  # site, we take the most recent
  load(paste0("results/",File[length(File)]))
  
  SensitivityCovariates = c(sprintf("ESen[%d]",seq(1:7)))
  
  Median = summary$statistics[rownames(summary$statistics) %in% SensitivityCovariates,1]
  Low = summary$quantiles[rownames(summary$quantiles) %in% SensitivityCovariates,1]
  High = summary$quantiles[rownames(summary$quantiles) %in% SensitivityCovariates,5]
}
  
  
  rm(list=ls())
  
  source("SensitivityPlot.R")
  source("WeightPlot.R")
  Sites = c("AU-Cpr","AU-DaS","AU-Dry","AU-Stp","AU-TTE","AU-Wom")
  ESenPlot = SensitivityPlot(Sites,c("Fsd","Precip"))
  WeightPlot = WeightPlot(Sites,c("Fsd","Precip"))
  