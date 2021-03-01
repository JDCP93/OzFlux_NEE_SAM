
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
 #         ,"AU-How"
 #         ,"AU-Stp"
 #         ,"AU-TTE"
 #         ,"AU-Tum"
 #         ,"AU-Whr"
 #         ,"AU-Wom"
          )
# Source the function
source("alternate/RTPV/NEE_kmean_function_current_RTPV.R")
source("alternate/RTPV/NEE_kmean_function_current_NDVI_RTPV.R")
source("alternate/RTPV/NEE_kmean_function_singlePPT_RTPV.R")
source("alternate/RTPV/NEE_kmean_function_singlePPT_NDVI_RTPV.R")
source("alternate/RTPV/NEE_kmean_function_allPPT_RTPV.R")
source("alternate/RTPV/NEE_kmean_function_allPPT_NDVI_RTPV.R")
source("alternate/RTPV/NEE_kmean_function_alllags_RTPV.R")
source("alternate/RTPV/NEE_kmean_function_alllags_NDVI_RTPV.R")
# Calculate the values
for (Site in Sites){
#  NEE_current_kmean_RTPV(Site)
#  NEE_current_NDVI_kmean_RTPV(Site)
  for (k in 2:8){
#    NEE_singlePPT_kmean_RTPV(Site,k)
    NEE_singlePPT_NDVI_kmean_RTPV(Site,k)
  }
#  NEE_allPPT_kmean_RTPV(Site)
  NEE_allPPT_NDVI_kmean_RTPV(Site)
#  NEE_alllags_kmean_RTPV(Site)
  NEE_alllags_NDVI_kmean_RTPV(Site)
}
