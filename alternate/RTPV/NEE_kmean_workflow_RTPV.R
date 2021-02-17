
# Tidy up
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
# Source the function
source("alternate/RTPV/NEE_kmean_function_allPPT_RTPV.R")
source("alternate/RTPV/NEE_kmean_function_current_RTPV.R")
source("alternate/RTPV/NEE_kmean_function_NDVI_RTPV.R")
# Calculate the values
for (Site in Sites){
  NEE_current_kmean_RTPV(Site)
  NEE_allPPT_kmean_RTPV(Site)
  NEE_NDVI_kmean_RTPV(Site)
}
