rm(list=ls())

library(raster)


r <- getData("worldclim",var="bio",res=10)

r <- r[[1:19]]
names(r) <- c("AnnualMeanTemp",
              "MeanDiurnalRange",
              "Isothermality",
              "TempSeasonality",
              "MaxTempHotMon",
              "MinTempColdMon",
              "TempAnnualRange",
              "MeanTempWetQtr",
              "MeanTempDryQtr",
              "MeanTempHotQtr",
              "MeanTempColdQtr",
              "AnnualPPT",
              "PPTWetMon",
              "PPTDryMon",
              "PPTSeasonality",
              "PPTWetQtr",
              "PPTDryQtr",
              "PPTHotQtr",
              "PPTColdQtr")


lats <- c(-22.2828, -34.0027, -33.6152, -14.1592, -15.2588, -31.3764, -30.1913, -12.4943, -17.1507, -22.2870, -35.6566, -36.6732, -37.4222)
lons <- c(133.2493, 140.5877, 150.7236, 131.3881, 132.3706, 115.7139, 120.6541, 131.1523, 133.3502, 133.6400, 148.1517, 145.0294, 144.0944) 

coords <- data.frame(x=lons,y=lats)

points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)

df <- cbind.data.frame(coordinates(points),values)

df

df[,3:4] = df[,3:4]/10
df[,5:6] = df[,5:6]/100
df[,7:13] = df[,7:13]/10

e = extent(110,155,-45, -10)

plot(r[[16]], ext = e)
plot(points,add=T)

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

RelMem = c(0.095952,
           0.092979,
           0.178261,
           0.158209,
           0.247934,
           0.197452,
           0.209412,
           0.167235,
           0.164659,
           0.078582,
           0.153846,
           0.066914,
           0.092308)

AbsMem = c(0.064,
           0.049,
           0.041,
           0.053,
           0.09,
           0.062,
           0.089,
           0.049,
           0.082,
           0.051,
           0.036,
           0.036,
           0.048)

 df = cbind(Sites,AbsMem,RelMem,df)
 
 
Corr = data.frame("Metric" = names(r),
                  "RelPVal" = 0,
                  "RelCorr" = 0,
                  "AbsPVal" = 0,
                  "AbsCorr" = 0)
          
for (i in 6:24){
  Corr$RelPVal[i-5] = cor.test(df[,i],RelMem,method = "pearson")$p.value
  Corr$RelCorr[i-5] = cor.test(df[,i],RelMem,method = "pearson")$estimate
  Corr$AbsPVal[i-5] = cor.test(df[,i],AbsMem,method = "pearson")$p.value
  Corr$AbsCorr[i-5] = cor.test(df[,i],AbsMem,method = "pearson")$estimate
}
