
# Tidy up
rm(list=ls())
# List sites
Sites = "AU-TTE"
# Source the function
source("alternate/NEE_kmean_function.R")
# Calculate the values
for (Site in Sites){
  NEE_kmean(Site)
}
