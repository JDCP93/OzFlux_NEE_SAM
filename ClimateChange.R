rm(list=ls())

load("SPDailyData.Rdata")
load("HSDailyData.Rdata")


vars = colnames(SPDailyData)
vars = vars[c(-1,-7)]

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

climate.data[,4:8] = sapply(climate.data[,4:8],as.numeric)

SP.climate.data = climate.data[climate.data$site=="SP",]

SP.climate.plot = ggplot(data = SP.climate.data) +
  geom_crossbar(aes(x = series,ymin = low, y = med, ymax = high)) +
  geom_errorbar(aes(x = series,ymin = min, ymax = max)) +
      facet_grid(var~., scales = "free")

SP.climate.plot




HS.climate.data = climate.data[climate.data$site=="HS",]

HS.climate.plot = ggplot(data = HS.climate.data) +
  geom_crossbar(aes(x = series,ymin = low, y = med, ymax = high)) +
  geom_errorbar(aes(x = series,ymin = min, ymax = max)) +
  facet_grid(var~., scales = "free")

HS.climate.plot
