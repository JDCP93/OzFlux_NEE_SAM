SensitivityCheck <- function(Site,SiteCode){

    
  
  
# load model input for Site
name = paste0(Site,"_Input")
load(paste0(Site,'_Input_2013-01-01_2017-12-31.Rdata'))
assign("Input",eval(as.name(name)))

# Find the output files available
File = list.files("results",pattern = paste0("NEE_output_match_site_",SiteCode))
# Read the data into R 
load(paste0("results/",File))
# data inside is called "nee_daily"
assign('SAM',nee_daily)
rm(nee_daily)

# Find the output files available
File = list.files("results",pattern = paste0("NEE_current_output_match_site_",SiteCode))
# Read the data into R 
load(paste0("results/",File))
# data inside is called "nee_daily"
assign('CUR',nee_daily)
rm(nee_daily)

# Find the output files available
File = list.files("results",pattern = paste0("NEE_AR1_output_match_site_",SiteCode))
# Read the data into R 
load(paste0("results/",File))
# data inside is called "nee_daily"
assign('AR1',AR1.res)
rm(AR1.res)

# ******************************************************************************
# Extract the covariate values for growing season and non-growing season
# sensitivity
# ******************************************************************************

# load necessary packages
library(coda)
library(lattice)
library(mcmcplots)
library(superdiag)

SAM.summary = summary(SAM)
CUR.summary = summary(CUR)

# Extract the ag and an values
SAM_an.mean = SAM.summary$statistics[substr(rownames(SAM.summary$statistics),1,2)=="an",1]
CUR_an.mean = CUR.summary$statistics[substr(rownames(CUR.summary$statistics),1,2)=="an",1]
SAM_ag.mean = SAM.summary$statistics[substr(rownames(SAM.summary$statistics),1,2)=="ag",1]
CUR_ag.mean = CUR.summary$statistics[substr(rownames(CUR.summary$statistics),1,2)=="ag",1]

SAM_an.min = SAM.summary$quantiles[substr(rownames(SAM.summary$quantiles),1,2)=="an",1]
CUR_an.min = CUR.summary$quantiles[substr(rownames(CUR.summary$quantiles),1,2)=="an",1]
SAM_ag.min = SAM.summary$quantiles[substr(rownames(SAM.summary$quantiles),1,2)=="ag",1]
CUR_ag.min = CUR.summary$quantiles[substr(rownames(CUR.summary$quantiles),1,2)=="ag",1]

SAM_an.max = SAM.summary$quantiles[substr(rownames(SAM.summary$quantiles),1,2)=="an",5]
CUR_an.max = CUR.summary$quantiles[substr(rownames(CUR.summary$quantiles),1,2)=="an",5]
SAM_ag.max = SAM.summary$quantiles[substr(rownames(SAM.summary$quantiles),1,2)=="ag",5]
CUR_ag.max = CUR.summary$quantiles[substr(rownames(CUR.summary$quantiles),1,2)=="ag",5]


Site.rep = rep(SiteCode,22)
Data = data.frame("Site" = Site.rep,
                  SAM_an.min,
                  SAM_an.mean,
                  SAM_an.max,
                  SAM_ag.min,
                  SAM_ag.mean,
                  SAM_ag.max)
}