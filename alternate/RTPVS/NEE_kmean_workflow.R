
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
source("alternate/RTPVS/NEE_kmean_function_allPrecip.R")
source("alternate/RTPVS/NEE_kmean_function_current.R")
source("alternate/RTPVS/NEE_kmean_function_currentNDVI.R")
# Calculate the values
for (Site in Sites){
 # NEE_current_kmean(Site)
 # NEE_kmean(Site)
  NEE_currentNDVI_kmean(Site)
}
