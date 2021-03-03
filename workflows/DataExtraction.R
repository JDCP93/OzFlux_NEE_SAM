#*******************************************************************************

# Workflow to extract workable data from the raw OzFlux datafiles
# Author: Jon Page

#*******************************************************************************

# L6 default data downloaded for all sites on 17/09/20 at 10:45
# L5 site_pi data downloaded for ASM and TTE on 17/09/20 at 10:45 at suggestion
# of site PIs Jamie Cleverly and Peter Isaac

#*******************************************************************************

# Tidy up
rm(list=ls())

# List all sites using OzFlux codes
# Split into NATT and SoWo
Sites_NATT = c("AU-ASM","AU-DaS","AU-Dry","AU-How","AU-Lit","AU-Stp","AU-TTE")
Sites_SWAT = c("AU-Cpr","AU-Cum","AU-Gin","AU-GWW","AU-Tum","AU-Whr","AU-Wom")
Sites = c("AU-ASM","AU-Cpr","AU-Cum","AU-DaS","AU-Dry","AU-Gin","AU-GWW","AU-How","AU-Lit","AU-Stp","AU-TTE","AU-Tum","AU-Whr","AU-Wom")

# Call extraction functions
source("OzFluxProcess.R")
# Run extraction process for each site
for (Site in Sites){
  OzFluxProcess(Site)
}

source("OzFluxProcess_noSWC.R")

for (Site in Sites){
  OzFluxProcess_noSWC(Site)
}
