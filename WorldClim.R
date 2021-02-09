# Clean up
rm(list=ls())

## Download the package if necessary
#devtools::install_github("kapitzas/WorldClimTiles")

# Source library
library(raster)
library(WorldClimTiles)

# #Code to download and merge worldclim data if necessary
# r1 <- getData("worldclim",var="bio",res=0.5,lat = 0,lon=100)
# r2 <- getData("worldclim",var="bio",res=0.5,lat = 0,lon=120)
# r3 <- getData("worldclim",var="bio",res=0.5,lat = 0,lon=150)
# r4 <- getData("worldclim",var="bio",res=0.5,lat = -30,lon=100)
# r5 <- getData("worldclim",var="bio",res=0.5,lat = -30,lon=120)
# r6 <- getData("worldclim",var="bio",res=0.5,lat = -30,lon=150)
# 
# r = raster::merge(r1,r2,r3,r4,r5,r6)
# 
# save(r,file="worldclim_biovar_0.5res.Rdata")


# Otherwise we load the file that already exists
 
load("worldclim_biovar_0.5res.Rdata")

# # Use the WorldClimTiles package to merge the tiles
# aus <- getData("GADM", country = "AUS", level = 0)
# tilenames <- tile_name(aus)
# tiles <- tile_get(tilenames, "bio")
# r <- tile_merge(tiles)

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

values <- raster::extract(r,points)

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

Transect = c("NATT",
              "SWAT",
              "SWAT",
              "NATT",
              "NATT",
              "SWAT",
              "SWAT",
              "NATT",
              "NATT",
              "NATT",
              "SWAT",
              "SWAT",
              "SWAT")


RelMem = c(0.09646399,
           0.09307989,
           0.17663594,
           0.15703944,
           0.24810158,
           0.19673790,
           0.20952859,
           0.16505797,
           0.16314338,
           0.08339588,
           0.15174571,
           0.06636320,
           0.09213355)

AbsMem = c(0.06434558,
           0.04902036,
           0.04060867,
           0.05268169,
           0.09004726,
           0.06178927,
           0.08912096,
           0.04843445,
           0.08132378,
           0.05408572,
           0.03551165,
           0.03570634,
           0.04788454)

df = cbind(Sites,Transect,AbsMem,RelMem,df)
 
 
Corr = data.frame("Metric" = names(r),
                  "RelPVal" = 0,
                  "RelCorr" = 0,
                  "AbsPVal" = 0,
                  "AbsCorr" = 0,
                  "NATTRelPVal" = 0,
                  "NATTRelCorr" = 0,
                  "NATTAbsPVal" = 0,
                  "NATTAbsCorr" = 0,
                  "SWATRelPVal" = 0,
                  "SWATRelCorr" = 0,
                  "SWATAbsPVal" = 0,
                  "SWATAbsCorr" = 0)
          
for (i in 6:24){
  Corr$RelPVal[i-5] = cor.test(df[,i],RelMem,method = "pearson")$p.value
  Corr$RelCorr[i-5] = cor.test(df[,i],RelMem,method = "pearson")$estimate
  Corr$AbsPVal[i-5] = cor.test(df[,i],AbsMem,method = "pearson")$p.value
  Corr$AbsCorr[i-5] = cor.test(df[,i],AbsMem,method = "pearson")$estimate
  
  Corr$NATTRelPVal[i-5] = cor.test(df[df$Transect=="NATT",i],df$RelMem[df$Transect=="NATT"],method = "pearson")$p.value
  Corr$NATTRelCorr[i-5] = cor.test(df[df$Transect=="NATT",i],df$RelMem[df$Transect=="NATT"],method = "pearson")$estimate
  Corr$NATTAbsPVal[i-5] = cor.test(df[df$Transect=="NATT",i],df$AbsMem[df$Transect=="NATT"],method = "pearson")$p.value
  Corr$NATTAbsCorr[i-5] = cor.test(df[df$Transect=="NATT",i],df$AbsMem[df$Transect=="NATT"],method = "pearson")$estimate
  
  Corr$SWATRelPVal[i-5] = cor.test(df[df$Transect=="SWAT",i],df$RelMem[df$Transect=="SWAT"],method = "pearson")$p.value
  Corr$SWATRelCorr[i-5] = cor.test(df[df$Transect=="SWAT",i],df$RelMem[df$Transect=="SWAT"],method = "pearson")$estimate
  Corr$SWATAbsPVal[i-5] = cor.test(df[df$Transect=="SWAT",i],df$AbsMem[df$Transect=="SWAT"],method = "pearson")$p.value
  Corr$SWATAbsCorr[i-5] = cor.test(df[df$Transect=="SWAT",i],df$AbsMem[df$Transect=="SWAT"],method = "pearson")$estimate
}

WorldClimCorr = Corr

save(WorldClimCorr,file = "worldclim_biovar_0.5res_RTPVS_correlations.Rdata")
