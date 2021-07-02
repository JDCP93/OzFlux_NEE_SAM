rm(list=ls())

library(raster)
library(tidyverse)

AI = raster("PET/ai_et0.tif")

lats <- c(-22.2828, -34.0027, -33.6152, -14.1592, -15.2588, -31.3764, -30.1913, -12.4943, -17.1507, -22.2870, -35.6566, -36.6732, -37.4222)
lons <- c(133.2493, 140.5877, 150.7236, 131.3881, 132.3706, 115.7139, 120.6541, 131.1523, 133.3502, 133.6400, 148.1517, 145.0294, 144.0944) 

coords <- data.frame(x=lons,y=lats)

points <-  data.frame(x=lons,y=lats) %>%
            SpatialPoints(proj4string = crs(AI))

values <- raster::extract(AI,points)

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

OzFlux = data.frame(Sites,"AI" = values/10000)


OzFlux$Level = findInterval(OzFlux$AI, c(0, 0.03, 0.2, 0.5, 0.65), rightmost.closed = TRUE)

OzFlux$Level <- ifelse(OzFlux$Level==1,"Hyper Arid",
                       ifelse(OzFlux$Level==2,"Arid",
                              ifelse(OzFlux$Level==3,"Semi-Arid",
                                     ifelse(OzFlux$Level==4,"Dry sub-humid",
                                            ifelse(OzFlux$Level==5,"Humid",NA)))))

# Source worldclim correlations and climate metrics
load("site_data/SiteMetrics_worldclim_0.5res.Rdata")

WorldClimMetrics$AridityIndex = OzFlux$AI

save(WorldClimMetrics,file = "SiteMetrics_worldclim_0.5res_AI.Rdata")
