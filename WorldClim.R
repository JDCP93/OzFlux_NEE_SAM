library(raster)
library(sp)

r <- getData("worldclim",var="bio",res=10)

r <- r[[1:19]]
names(r) <- c("AMT","MeanDiurnalRange","Isothermality",
              Temperature Seasonality,
              Max Temperature of Warmest Month,
              Min Temperature of Coldest Month,
              Temperature Annual Range (BIO5-BIO6)
              Mean Temperature of Wettest Quarter,
              Mean Temperature of Driest Quarter,
              Mean Temperature of Warmest Quarter,
              Mean Temperature of Coldest Quarter,
              Annual Precipitation

    BIO13 = Precipitation of Wettest Month

    BIO14 = Precipitation of Driest Month

    BIO15 = Precipitation Seasonality (Coefficient of Variation)

    BIO16 = Precipitation of Wettest Quarter

    BIO17 = Precipitation of Driest Quarter

    BIO18 = Precipitation of Warmest Quarter

    BIO19 = Precipitation of Coldest Quarter


lats <- c(-22.2828, -34.0027, -33.6152, -14.1592, -15.2588, -31.3764, -30.1913, -12.4943, -17.1507, -22.2870, -35.6566, -36.6732, -37.4222)
lons <- c(133.2493, 140.5877, 150.7236, 131.3881, 132.3706, 115.7139, 120.6541, 131.1523, 133.3502, 133.6400, 148.1517, 145.0294, 144.0944) 

coords <- data.frame(x=lons,y=lats)

points <- SpatialPoints(coords, proj4string = r@crs)

values <- extract(r,points)

df <- cbind.data.frame(coordinates(points),values)

df

e = extent(110,180,-90, -0)

plot(r[[1]], , ext = e)
plot(points,add=T)

