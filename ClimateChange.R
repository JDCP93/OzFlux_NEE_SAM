rm(list=ls())

# Load the daily climate data
load("SPDailyData.Rdata")
load("HSDailyData.Rdata")
load("DUDailyData.Rdata")
load("DRDailyData.Rdata")
load("LFDailyData.Rdata")
load("TTDailyData.Rdata")

# Create monthly and yearly data frames as well
library(lubridate)
library(magrittr)
library(dplyr)
library(zoo)
library(ggplot2)
library(gridExtra)
SPDailyData$year = year(SPDailyData$TIMESTAMP)
SPDailyData$month = month(SPDailyData$TIMESTAMP)

SPYearlyData <- SPDailyData %>%
  group_by(year) %>%               # group by the year column
  summarise(NEE_LL=mean(NEE_LL),
            Fsd=mean(Fsd),
            Ta=mean(Ta),
            VPD=mean(VPD),
            Sws=mean(Sws),
            Precip=sum(Precip))

SPYearlyData$TIMESTAMP = SPYearlyData$year

SPMonthlyData <- SPDailyData %>%
  group_by(year,month) %>%               # group by the year column
  summarise(NEE_LL=mean(NEE_LL),
            Fsd=mean(Fsd),
            Ta=mean(Ta),
            VPD=mean(VPD),
            Sws=mean(Sws),
            Precip=sum(Precip))

SPMonthlyData$TIMESTAMP = as.yearmon(paste(SPMonthlyData$year, SPMonthlyData$month), "%Y %m")


HSDailyData$year = year(HSDailyData$TIMESTAMP)
HSDailyData$month = month(HSDailyData$TIMESTAMP)

HSYearlyData <- HSDailyData %>%
  group_by(year) %>%               # group by the year column
  summarise(NEE_LL=mean(NEE_LL),
            Fsd=mean(Fsd),
            Ta.mean=mean(Ta),
            Ta.min=min(Ta),
            Ta.max=max(Ta),
            VPD=mean(VPD),
            Sws=mean(Sws),
            Precip=sum(Precip))

HSYearlyData$TIMESTAMP = HSYearlyData$year

HSMonthlyData <- HSDailyData %>%
  group_by(year,month) %>%               # group by the year column
  summarise(NEE_LL=mean(NEE_LL),
            Fsd=mean(Fsd),
            Ta.mean=mean(Ta),
            Ta.min=min(Ta),
            Ta.max=max(Ta),
            VPD=mean(VPD),
            Sws=mean(Sws),
            Precip=sum(Precip))

HSMonthlyData$TIMESTAMP = as.yearmon(paste(HSMonthlyData$year, HSMonthlyData$month), "%Y %m")

# Remove 2018 as it is only 1 day
HSYearlyData = HSYearlyData[HSYearlyData$year<2018,]
SPYearlyData = SPYearlyData[SPYearlyData$year<2018,]
HSMonthlyData = HSMonthlyData[HSMonthlyData$year<2018,]
SPMonthlyData = SPMonthlyData[SPMonthlyData$year<2018,]

# Tidy the daily data
SPDailyData = subset(SPDailyData, select = -c(year,month))
HSDailyData = subset(HSDailyData, select = -c(year,month))

# Find the variable names
vars = colnames(SPDailyData)
vars = vars[c(-1,-7)]


# Find quantiles of the data for each variable and each site
climate.data = data.frame(matrix(0,30,8))
colnames(climate.data) = c("site","series","var","min","low","med","high","max")
k = 0

for (i in vars){
  k = k+1
  data = quantile(pull(SPDailyData[SPDailyData$TIMESTAMP < "2015-01-01",],i))
  climate.data[k,] = c("site" = "SP",
                          "series" = "Pre-2015",
                          "var" = i,
                          "min" = data[[1]],
                          "low" = data[[2]],
                          "med" = data[[3]],
                          "high" = data[[4]],
                          "max" = data[[5]])
  k = k+1
  data = quantile(pull(SPDailyData[SPDailyData$TIMESTAMP >= "2015-01-01",],i))
  climate.data[k,] = c("site" = "SP",
                          "series" = "Post-2015",
                          "var" = i,
                          "min" = data[[1]],
                          "low" = data[[2]],
                          "med" = data[[3]],
                          "high" = data[[4]],
                          "max" = data[[5]])
  k = k+1
  data = quantile(pull(SPDailyData,i))
  climate.data[k,] = c("site" = "SP",
                          "series" = "All",
                          "var" = i,
                          "min" = data[[1]],
                          "low" = data[[2]],
                          "med" = data[[3]],
                          "high" = data[[4]],
                          "max" = data[[5]])
  
  
  k = k+1
  data = quantile(pull(HSDailyData[HSDailyData$TIMESTAMP < "2015-01-01",],i))
  climate.data[k,] = c("site" = "HS",
                       "series" = "Pre-2015",
                       "var" = i,
                       "min" = data[[1]],
                       "low" = data[[2]],
                       "med" = data[[3]],
                       "high" = data[[4]],
                       "max" = data[[5]])
  k = k+1
  data = quantile(pull(HSDailyData[HSDailyData$TIMESTAMP >= "2015-01-01",],i))
  climate.data[k,] = c("site" = "HS",
                       "series" = "Post-2015",
                       "var" = i,
                       "min" = data[[1]],
                       "low" = data[[2]],
                       "med" = data[[3]],
                       "high" = data[[4]],
                       "max" = data[[5]])
  k = k+1
  data = quantile(pull(HSDailyData,i))
  climate.data[k,] = c("site" = "HS",
                       "series" = "All",
                       "var" = i,
                       "min" = data[[1]],
                       "low" = data[[2]],
                       "med" = data[[3]],
                       "high" = data[[4]],
                       "max" = data[[5]])
}

# Make the data numeric
climate.data[,4:8] = sapply(climate.data[,4:8],as.numeric)

# Extract and plot the data for SP
SP.climate.data = climate.data[climate.data$site=="SP",]
SP.climate.data$series = factor(SP.climate.data$series, levels = c("Pre-2015","Post-2015","All"))
SP.climate.data$var = factor(SP.climate.data$var, levels = c("NEE_LL","Fsd","Ta","VPD","Sws"))

SP.climate.plot = ggplot(data = SP.climate.data) +
  geom_crossbar(aes(x = series,ymin = low, y = med, ymax = high)) +
  geom_errorbar(aes(x = series,ymin = min, ymax = max)) +
      facet_grid(var~., scales = "free") +
  xlab("") +
  ylab("Value")

SP.climate.plot

# Extract and plot the data for HS
HS.climate.data = climate.data[climate.data$site=="HS",]
HS.climate.data$series = factor(HS.climate.data$series, levels = c("Pre-2015","Post-2015","All"))
HS.climate.data$var = factor(HS.climate.data$var, levels = c("NEE_LL","Fsd","Ta","VPD","Sws"))

HS.climate.plot = ggplot(data = HS.climate.data) +
  geom_crossbar(aes(x = series,ymin = low, y = med, ymax = high)) +
  geom_errorbar(aes(x = series,ymin = min, ymax = max)) +
  facet_grid(var~., scales = "free") +
  xlab("") +
  ylab("Value")

HS.climate.plot

# Plot density plots

SP.VPD = ggplot() +
  geom_density(data = SPDailyData[SPDailyData$TIMESTAMP<"2015-01-01",],aes(x = VPD, y = ..density..,color = "Pre-2015"),size = 1) +
  geom_density(data = SPDailyData[SPDailyData$TIMESTAMP>="2015-01-01",], aes(x = VPD, y = ..density.., color = "Post-2015"), size = 0.5) +
  geom_density(data = SPDailyData, aes(x = VPD, y = ..density.., color = "All"), size = 1) +
  geom_vline(data = SPDailyData[SPDailyData$TIMESTAMP<"2015-01-01",],aes(xintercept = mean(VPD),color="Pre-2015"),linetype = "dashed") +
  geom_vline(data = SPDailyData[SPDailyData$TIMESTAMP>="2015-01-01",],aes(xintercept = mean(VPD),color="Post-2015"),linetype = "dashed") +
  geom_vline(data = SPDailyData,aes(xintercept = mean(VPD),color="All"),linetype = "dashed") +
  scale_color_discrete(name = "") +
  theme_bw() +
  theme(axis.text.y = element_blank()) +
  guides(color = "none")
  
SP.NEE = ggplot() +
  geom_density(data = SPDailyData[SPDailyData$TIMESTAMP<"2015-01-01",],aes(x = NEE_LL, y = ..density..,color = "Pre-2015"),size = 1) +
  geom_density(data = SPDailyData[SPDailyData$TIMESTAMP>="2015-01-01",], aes(x = NEE_LL, y = ..density.., color = "Post-2015"), size = 0.5) +
  geom_density(data = SPDailyData, aes(x = NEE_LL, y = ..density.., color = "All"), size = 1) +
  geom_vline(data = SPDailyData[SPDailyData$TIMESTAMP<"2015-01-01",],aes(xintercept = mean(NEE_LL),color="Pre-2015"),linetype = "dashed") +
  geom_vline(data = SPDailyData[SPDailyData$TIMESTAMP>="2015-01-01",],aes(xintercept = mean(NEE_LL),color="Post-2015"),linetype = "dashed") +
  geom_vline(data = SPDailyData,aes(xintercept = mean(NEE_LL),color="All"),linetype = "dashed") +
  scale_color_discrete(name = "") +
  theme_bw() +
  theme(axis.text.y = element_blank())+
  guides(color = "none")

SP.SWR = ggplot() +
  geom_density(data = SPDailyData[SPDailyData$TIMESTAMP<"2015-01-01",],aes(x = Fsd, y = ..density..,color = "Pre-2015"),size = 1) +
  geom_density(data = SPDailyData[SPDailyData$TIMESTAMP>="2015-01-01",], aes(x = Fsd, y = ..density.., color = "Post-2015"), size = 0.5) +
  geom_density(data = SPDailyData, aes(x = Fsd, y = ..density.., color = "All"), size = 1) +
  geom_vline(data = SPDailyData[SPDailyData$TIMESTAMP<"2015-01-01",],aes(xintercept = mean(Fsd),color="Pre-2015"),linetype = "dashed") +
  geom_vline(data = SPDailyData[SPDailyData$TIMESTAMP>="2015-01-01",],aes(xintercept = mean(Fsd),color="Post-2015"),linetype = "dashed") +
  geom_vline(data = SPDailyData,aes(xintercept = mean(Fsd),color="All"),linetype = "dashed") +
  scale_color_discrete(name = "") +
  theme_bw() +
  theme(axis.text.y = element_blank())+
  guides(color = "none")

SP.SWC = ggplot() +
  geom_density(data = SPDailyData[SPDailyData$TIMESTAMP<"2015-01-01",],aes(x = Sws, y = ..density..,color = "Pre-2015"),size = 1) +
  geom_density(data = SPDailyData[SPDailyData$TIMESTAMP>="2015-01-01",], aes(x = Sws, y = ..density.., color = "Post-2015"), size = 0.5) +
  geom_density(data = SPDailyData, aes(x = Sws, y = ..density.., color = "All"), size = 1) +
  geom_vline(data = SPDailyData[SPDailyData$TIMESTAMP<"2015-01-01",],aes(xintercept = mean(Sws),color="Pre-2015"),linetype = "dashed") +
  geom_vline(data = SPDailyData[SPDailyData$TIMESTAMP>="2015-01-01",],aes(xintercept = mean(Sws),color="Post-2015"),linetype = "dashed") +
  geom_vline(data = SPDailyData,aes(xintercept = mean(Sws),color="All"),linetype = "dashed") +
  scale_color_discrete(name = "") +
  theme_bw() +
  theme(axis.text.y = element_blank())+
  guides(color = "none")

SP.Tair = ggplot() +
  geom_density(data = SPDailyData[SPDailyData$TIMESTAMP<"2015-01-01",],aes(x = Ta, y = ..density..,color = "Pre-2015"),size = 1) +
  geom_density(data = SPDailyData[SPDailyData$TIMESTAMP>="2015-01-01",], aes(x = Ta, y = ..density.., color = "Post-2015"), size = 0.5) +
  geom_density(data = SPDailyData, aes(x = Ta, y = ..density.., color = "All"), size = 1) +
  geom_vline(data = SPDailyData[SPDailyData$TIMESTAMP<"2015-01-01",],aes(xintercept = mean(Ta),color="Pre-2015"),linetype = "dashed") +
  geom_vline(data = SPDailyData[SPDailyData$TIMESTAMP>="2015-01-01",],aes(xintercept = mean(Ta),color="Post-2015"),linetype = "dashed") +
  geom_vline(data = SPDailyData,aes(xintercept = mean(Ta),color="All"),linetype = "dashed") +
  scale_color_discrete(name = "") +
  theme_bw() +
  theme(axis.text.y = element_blank())+
  guides(color = "none")

SP.PPT = ggplot() +
  geom_density(data = SPYearlyData[SPYearlyData$year<2015,],aes(x = Precip, y = ..density..,color = "Pre-2015"),size = 1) +
  geom_density(data = SPYearlyData[SPYearlyData$year>=2015,], aes(x = Precip, y = ..density.., color = "Post-2015"), size = 0.5) +
  geom_density(data = SPYearlyData, aes(x = Precip, y = ..density.., color = "All"), size = 1) +
  geom_vline(data = SPYearlyData[SPYearlyData$year<2015,],aes(xintercept = mean(Precip),color="Pre-2015"),linetype = "dashed") +
  geom_vline(data = SPYearlyData[SPYearlyData$year>=2015,],aes(xintercept = mean(Precip),color="Post-2015"),linetype = "dashed") +
  geom_vline(data = SPYearlyData,aes(xintercept = mean(Precip),color="All"),linetype = "dashed") +
  scale_color_discrete(name = "") +
  theme_bw() +
  theme(axis.text.y = element_blank())+
  theme(legend.position = "bottom")

grid.arrange(SP.NEE, SP.SWR, SP.SWC, SP.VPD, SP.Tair, SP.PPT, top = "SP",             
             widths = c(1,1,1,1),
             heights = c(3,3,3,4),
             layout_matrix = rbind(c(NA,1,1,NA),
                                   c(2,2,3,3),
                                   c(4,4,5,5),
                                   c(NA,6,6,NA)))


HS.VPD = ggplot() +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP<"2015-01-01",],aes(x = VPD, y = ..density..,color = "Pre-2015"),size = 1) +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP>="2015-01-01",], aes(x = VPD, y = ..density.., color = "Post-2015"), size = 0.5) +
  geom_density(data = HSDailyData, aes(x = VPD, y = ..density.., color = "All"), size = 1) +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP<"2015-01-01",],aes(xintercept = mean(VPD),color="Pre-2015"),linetype = "dashed") +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP>="2015-01-01",],aes(xintercept = mean(VPD),color="Post-2015"),linetype = "dashed") +
  geom_vline(data = HSDailyData,aes(xintercept = mean(VPD),color="All"),linetype = "dashed") +
  scale_color_discrete(name = "") +
  theme_bw() +
  theme(axis.text.y = element_blank()) +
  guides(color = "none")

HS.NEE = ggplot() +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP<"2015-01-01",],aes(x = NEE_LL, y = ..density..,color = "Pre-2015"),size = 1) +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP>="2015-01-01",], aes(x = NEE_LL, y = ..density.., color = "Post-2015"), size = 0.5) +
  geom_density(data = HSDailyData, aes(x = NEE_LL, y = ..density.., color = "All"), size = 1) +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP<"2015-01-01",],aes(xintercept = mean(NEE_LL),color="Pre-2015"),linetype = "dashed") +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP>="2015-01-01",],aes(xintercept = mean(NEE_LL),color="Post-2015"),linetype = "dashed") +
  geom_vline(data = HSDailyData,aes(xintercept = mean(NEE_LL),color="All"),linetype = "dashed") +
  scale_color_discrete(name = "") +
  theme_bw() +
  theme(axis.text.y = element_blank())+
  guides(color = "none")

HS.SWR = ggplot() +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP<"2015-01-01",],aes(x = Fsd, y = ..density..,color = "Pre-2015"),size = 1) +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP>="2015-01-01",], aes(x = Fsd, y = ..density.., color = "Post-2015"), size = 0.5) +
  geom_density(data = HSDailyData, aes(x = Fsd, y = ..density.., color = "All"), size = 1) +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP<"2015-01-01",],aes(xintercept = mean(Fsd),color="Pre-2015"),linetype = "dashed") +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP>="2015-01-01",],aes(xintercept = mean(Fsd),color="Post-2015"),linetype = "dashed") +
  geom_vline(data = HSDailyData,aes(xintercept = mean(Fsd),color="All"),linetype = "dashed") +
  scale_color_discrete(name = "") +
  theme_bw() +
  theme(axis.text.y = element_blank())+
  guides(color = "none")

HS.SWC = ggplot() +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP<"2015-01-01",],aes(x = Sws, y = ..density..,color = "Pre-2015"),size = 1) +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP>="2015-01-01",], aes(x = Sws, y = ..density.., color = "Post-2015"), size = 0.5) +
  geom_density(data = HSDailyData, aes(x = Sws, y = ..density.., color = "All"), size = 1) +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP<"2015-01-01",],aes(xintercept = mean(Sws),color="Pre-2015"),linetype = "dashed") +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP>="2015-01-01",],aes(xintercept = mean(Sws),color="Post-2015"),linetype = "dashed") +
  geom_vline(data = HSDailyData,aes(xintercept = mean(Sws),color="All"),linetype = "dashed") +
  scale_color_discrete(name = "") +
  theme_bw() +
  theme(axis.text.y = element_blank())+
  guides(color = "none")

HS.Tair = ggplot() +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP<"2015-01-01",],aes(x = Ta, y = ..density..,color = "Pre-2015"),size = 1) +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP>="2015-01-01",], aes(x = Ta, y = ..density.., color = "Post-2015"), size = 0.5) +
  geom_density(data = HSDailyData, aes(x = Ta, y = ..density.., color = "All"), size = 1) +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP<"2015-01-01",],aes(xintercept = mean(Ta),color="Pre-2015"),linetype = "dashed") +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP>="2015-01-01",],aes(xintercept = mean(Ta),color="Post-2015"),linetype = "dashed") +
  geom_vline(data = HSDailyData,aes(xintercept = mean(Ta),color="All"),linetype = "dashed") +
  scale_color_discrete(name = "") +
  theme_bw() +
  theme(axis.text.y = element_blank())+
  guides(color = "none")

HS.PPT = ggplot() +
  geom_density(data = HSYearlyData[HSYearlyData$year<2015,],aes(x = Precip, y = ..density..,color = "Pre-2015"),size = 1) +
  geom_density(data = HSYearlyData[HSYearlyData$year>=2015,], aes(x = Precip, y = ..density.., color = "Post-2015"), size = 0.5) +
  geom_density(data = HSYearlyData, aes(x = Precip, y = ..density.., color = "All"), size = 1) +
  geom_vline(data = HSYearlyData[HSYearlyData$year<2015,],aes(xintercept = mean(Precip),color="Pre-2015"),linetype = "dashed") +
  geom_vline(data = HSYearlyData[HSYearlyData$year>=2015,],aes(xintercept = mean(Precip),color="Post-2015"),linetype = "dashed") +
  geom_vline(data = HSYearlyData,aes(xintercept = mean(Precip),color="All"),linetype = "dashed") +
  scale_color_discrete(name = "") +
  theme_bw() +
  theme(axis.text.y = element_blank())+
  theme(legend.position = "bottom")

grid.arrange(HS.NEE, HS.SWR, HS.SWC, HS.VPD, HS.Tair, HS.PPT, top = "HS",             
             widths = c(1,1,1,1),
             heights = c(3,3,3,4),
             layout_matrix = rbind(c(NA,1,1,NA),
                                   c(2,2,3,3),
                                   c(4,4,5,5),
                                   c(NA,6,6,NA)))


### PLOT DENSITIES FOR 5 YEAR PERIODS

HS.VPD = ggplot() +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP<"2008-01-01",],aes(x = VPD, y = ..density..,color = "2003-2007"),size = 1) +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP>="2008-01-01" & HSDailyData$TIMESTAMP < "2013-01-01",],aes(x = VPD, y = ..density..,color = "2008-2012"),size = 1) +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP>="2013-01-01",], aes(x = VPD, y = ..density.., color = "2013-2017"), size = 1) +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP<"2008-01-01",],aes(xintercept = mean(VPD),color="2003-2007"),linetype = "dashed") +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP>="2008-01-01" & HSDailyData$TIMESTAMP < "2013-01-01",],aes(xintercept = mean(VPD),color="2008-2012"),linetype = "dashed") +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP>="2013-01-01",],aes(xintercept = mean(VPD),color="2013-2017"),linetype = "dashed") +
  scale_color_manual(values=c("green","orange","purple")) +
  theme_bw() +
  theme(axis.text.y = element_blank()) +
  xlab("VPD") +
  guides(color = "none")

HS.NEE = ggplot() +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP<"2008-01-01",],aes(x = NEE_LL, y = ..density..,color = "2003-2007"),size = 1) +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP>="2008-01-01" & HSDailyData$TIMESTAMP < "2013-01-01",],aes(x = NEE_LL, y = ..density..,color = "2008-2012"),size = 1) +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP>="2013-01-01",], aes(x = NEE_LL, y = ..density.., color = "2013-2017"), size = 1) +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP<"2008-01-01",],aes(xintercept = mean(NEE_LL),color="2003-2007"),linetype = "dashed") +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP>="2008-01-01" & HSDailyData$TIMESTAMP < "2013-01-01",],aes(xintercept = mean(NEE_LL),color="2008-2012"),linetype = "dashed") +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP>="2013-01-01",],aes(xintercept = mean(NEE_LL),color="2013-2017"),linetype = "dashed") +
  scale_color_manual(values=c("green","orange","purple")) +
  theme_bw() +
  theme(axis.text.y = element_blank()) +
  xlab("NEE") +
  guides(color = "none")

HS.SWR = ggplot() +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP<"2008-01-01",],aes(x = Fsd, y = ..density..,color = "2003-2007"),size = 1) +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP>="2008-01-01" & HSDailyData$TIMESTAMP < "2013-01-01",],aes(x = Fsd, y = ..density..,color = "2008-2012"),size = 1) +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP>="2013-01-01",], aes(x = Fsd, y = ..density.., color = "2013-2017"), size = 1) +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP<"2008-01-01",],aes(xintercept = mean(Fsd),color="2003-2007"),linetype = "dashed") +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP>="2008-01-01" & HSDailyData$TIMESTAMP < "2013-01-01",],aes(xintercept = mean(Fsd),color="2008-2012"),linetype = "dashed") +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP>="2013-01-01",],aes(xintercept = mean(Fsd),color="2013-2017"),linetype = "dashed") +
  scale_color_manual(values=c("green","orange","purple")) +
  theme_bw() +
  theme(axis.text.y = element_blank())+
  xlab("SWR") +
  guides(color = "none")

HS.SWC = ggplot() +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP<"2008-01-01",],aes(x = Sws, y = ..density..,color = "2003-2007"),size = 1) +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP>="2008-01-01" & HSDailyData$TIMESTAMP < "2013-01-01",],aes(x = Sws, y = ..density..,color = "2008-2012"),size = 1) +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP>="2013-01-01",], aes(x = Sws, y = ..density.., color = "2013-2017"), size = 1) +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP<"2008-01-01",],aes(xintercept = mean(Sws),color="2003-2007"),linetype = "dashed") +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP>="2008-01-01" & HSDailyData$TIMESTAMP < "2013-01-01",],aes(xintercept = mean(Sws),color="2008-2012"),linetype = "dashed") +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP>="2013-01-01",],aes(xintercept = mean(Sws),color="2013-2017"),linetype = "dashed") +
  scale_color_manual(values=c("green","orange","purple")) +
  theme_bw() +
  theme(axis.text.y = element_blank())+
  xlab("SWC") +
  guides(color = "none")

HS.Tair = ggplot() +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP<"2008-01-01",],aes(x = Ta, y = ..density..,color = "2003-2007"),size = 1) +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP>="2008-01-01" & HSDailyData$TIMESTAMP < "2013-01-01",],aes(x = Ta, y = ..density..,color = "2008-2012"),size = 1) +
  geom_density(data = HSDailyData[HSDailyData$TIMESTAMP>="2013-01-01",], aes(x = Ta, y = ..density.., color = "2013-2017"), size = 1) +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP<"2008-01-01",],aes(xintercept = mean(Ta),color="2003-2007"),linetype = "dashed") +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP>="2008-01-01" & HSDailyData$TIMESTAMP < "2013-01-01",],aes(xintercept = mean(Ta),color="2008-2012"),linetype = "dashed") +
  geom_vline(data = HSDailyData[HSDailyData$TIMESTAMP>="2013-01-01",],aes(xintercept = mean(Ta),color="2013-2017"),linetype = "dashed") +
  scale_color_manual(values=c("green","orange","purple")) +
  theme_bw() +
  theme(axis.text.y = element_blank())+
  xlab("Tair") +
  guides(color = "none")

HS.PPT = ggplot() +
  geom_density(data = HSYearlyData[HSYearlyData$year<2008,],aes(x = Precip, y = ..density..,color = "2003-2007"),size = 1) +
  geom_density(data = HSYearlyData[HSYearlyData$year>=2008 & HSYearlyData$year<2013,], aes(x = Precip, y = ..density.., color = "2008-2012"), size = 1) +
  geom_density(data = HSYearlyData[HSYearlyData$year>=2013,], aes(x = Precip, y = ..density.., color = "2013-2017"), size = 1) +
  geom_vline(data = HSYearlyData[HSYearlyData$year<2008,],aes(xintercept = mean(Precip),color="2003-2007"),linetype = "dashed") +
  geom_vline(data = HSYearlyData[HSYearlyData$year>=2008 & HSYearlyData$year<2013,],aes(xintercept = mean(Precip),color="2008-2012"),linetype = "dashed") +
  geom_vline(data = HSYearlyData[HSYearlyData$year>=2013,],aes(xintercept = mean(Precip),color="2013-2017"),linetype = "dashed") +
  scale_color_manual(values=c("green","orange","purple")) +
  theme_bw() +
  theme(axis.text.y = element_blank())+
  xlab("PPT") +
  theme(legend.position = "bottom")

grid.arrange(HS.NEE, HS.SWR, HS.Tair, HS.VPD, HS.SWC, HS.PPT, top = "HS",             
             widths = c(1,1,1,1),
             heights = c(3,3,3,4),
             layout_matrix = rbind(c(NA,1,1,NA),
                                   c(2,2,3,3),
                                   c(4,4,5,5),
                                   c(NA,6,6,NA)))




HS.PPT = ggplot() +
  geom_density(data = HSMonthlyData[HSMonthlyData$year<2008,],aes(x = Precip, y = ..density..,color = "2003-2007"),size = 1) +
  geom_density(data = HSMonthlyData[HSMonthlyData$year>=2008 & HSMonthlyData$year<2013,], aes(x = Precip, y = ..density.., color = "2008-2012"), size = 1) +
  geom_density(data = HSMonthlyData[HSMonthlyData$year>=2013,], aes(x = Precip, y = ..density.., color = "2013-2017"), size = 1) +
  geom_vline(data = HSMonthlyData[HSMonthlyData$year<2008,],aes(xintercept = mean(Precip),color="2003-2007"),linetype = "dashed") +
  geom_vline(data = HSMonthlyData[HSMonthlyData$year>=2008 & HSMonthlyData$year<2013,],aes(xintercept = mean(Precip),color="2008-2012"),linetype = "dashed") +
  geom_vline(data = HSMonthlyData[HSMonthlyData$year>=2013,],aes(xintercept = mean(Precip),color="2013-2017"),linetype = "dashed") +
  scale_color_manual(values=c("green","orange","purple")) +
  theme_bw() +
  xlab("PPT") +
  theme(legend.position = "bottom")

HS.PPT

# Calculate metrics for the 5 year periods

PPTmean = mean(HSYearlyData$Precip)
PPTmean.2003 = mean(HSYearlyData$Precip[HSYearlyData$year<2008])
PPTmean.2008 = mean(HSYearlyData$Precip[HSYearlyData$year>=2008 & HSYearlyData$year<2013])
PPTmean.2013 = mean(HSYearlyData$Precip[HSYearlyData$year>=2013])

PPTsd = sd(HSYearlyData$Precip)
PPTsd.2003 = sd(HSYearlyData$Precip[HSYearlyData$year<2008])
PPTsd.2008 = sd(HSYearlyData$Precip[HSYearlyData$year>=2008 & HSYearlyData$year<2013])
PPTsd.2013 = sd(HSYearlyData$Precip[HSYearlyData$year>=2013])

PPTcvp = PPTsd/PPTmean
PPTcvp.2003 = PPTsd.2003/PPTmean.2003
PPTcvp.2008 = PPTsd.2008/PPTmean.2008
PPTcvp.2013 = PPTsd.2013/PPTmean.2013

VPDmean = mean(HSYearlyData$VPD)
VPDmean.2003 = mean(HSYearlyData$VPD[HSYearlyData$year<2008])
VPDmean.2008 = mean(HSYearlyData$VPD[HSYearlyData$year>=2008 & HSYearlyData$year<2013])
VPDmean.2013 = mean(HSYearlyData$VPD[HSYearlyData$year>=2013])

VPDsd = sd(HSYearlyData$VPD)
VPDsd.2003 = sd(HSYearlyData$VPD[HSYearlyData$year<2008])
VPDsd.2008 = sd(HSYearlyData$VPD[HSYearlyData$year>=2008 & HSYearlyData$year<2013])
VPDsd.2013 = sd(HSYearlyData$VPD[HSYearlyData$year>=2013])

VPDcvp = VPDsd/VPDmean
VPDcvp.2003 = VPDsd.2003/VPDmean.2003
VPDcvp.2008 = VPDsd.2008/VPDmean.2008
VPDcvp.2013 = VPDsd.2013/VPDmean.2013


Fsdmean.2003 = mean(HSYearlyData$Fsd[HSYearlyData$year<2008])
Fsdmean.2008 = mean(HSYearlyData$Fsd[HSYearlyData$year>=2008 & HSYearlyData$year<2013])
Fsdmean.2013 = mean(HSYearlyData$Fsd[HSYearlyData$year>=2013])

Fsdsd.2003 = sd(HSYearlyData$Fsd[HSYearlyData$year<2008])
Fsdsd.2008 = sd(HSYearlyData$Fsd[HSYearlyData$year>=2008 & HSYearlyData$year<2013])
Fsdsd.2013 = sd(HSYearlyData$Fsd[HSYearlyData$year>=2013])

Fsdcvp.2003 = Fsdsd.2003/Fsdmean.2003
Fsdcvp.2008 = Fsdsd.2008/Fsdmean.2008
Fsdcvp.2013 = Fsdsd.2013/Fsdmean.2013




TaRange.mean = mean(HSYearlyData$Ta.max-HSYearlyData$Ta.min)
TaRange.mean.2003 = mean(HSYearlyData$Ta.max[HSYearlyData$year<2008]-HSYearlyData$Ta.min[HSYearlyData$year<2008])
TaRange.mean.2008 = mean(HSYearlyData$Ta.max[HSYearlyData$year>=2008 & HSYearlyData$year<2013]-HSYearlyData$Ta.min[HSYearlyData$year>=2008 & HSYearlyData$year<2013])
TaRange.mean.2013 = mean(HSYearlyData$Ta.max[HSYearlyData$year>=2013]-HSYearlyData$Ta.min[HSYearlyData$year>=2013])

TaRange.sd = sd(HSYearlyData$Ta.max-HSYearlyData$Ta.min)
TaRange.sd.2003 = sd(HSYearlyData$Ta.max[HSYearlyData$year<2008]-HSYearlyData$Ta.min[HSYearlyData$year<2008])
TaRange.sd.2008 = sd(HSYearlyData$Ta.max[HSYearlyData$year>=2008 & HSYearlyData$year<2013]-HSYearlyData$Ta.min[HSYearlyData$year>=2008 & HSYearlyData$year<2013])
TaRange.sd.2013 = sd(HSYearlyData$Ta.max[HSYearlyData$year>=2013]-HSYearlyData$Ta.min[HSYearlyData$year>=2013])

TaRange.cvp = TaRange.sd/TaRange.mean
TaRange.cvp.2003 = TaRange.sd.2003/TaRange.mean.2003
TaRange.cvp.2008 = TaRange.sd.2008/TaRange.mean.2008
TaRange.cvp.2013 = TaRange.sd.2013/TaRange.mean.2013
