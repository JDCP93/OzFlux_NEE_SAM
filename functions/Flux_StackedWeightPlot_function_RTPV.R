Flux_StackedWeightPlot_RTPV = function(Sites,Vars){

  message("Stacking weights for LE and NEE for ", Vars)
  # Source packages needed
  library(lubridate)
  library(magrittr)
  library(dplyr)
  library(coda)

  # Collect the analysis outputs and name them with each site
  for (Site in Sites){
    File = list.files("analysis/RTPV/",pattern = paste0("NEE_analysis_RTPV_",Site))
    message("Loading NEE analysis output for ",Site," where file is ",File)
    load(paste0("analysis/RTPV/",File))
    assign(paste0("NEE_",Site),output)
    rm(output)

    File = list.files("analysis/RTPV/",pattern = paste0("LE_analysis_RTPV_",Site))
    message("Loading LE analysis output for ",Site," where file is ",File)
    load(paste0("analysis/RTPV/",File))
    assign(paste0("LE_",Site),output)
    rm(output)
    
  }

  message("Plotting weights for sites...")
  # Do some shit with it
  CumWeightParams = c(sprintf("cum_weightA[%d,%d]",rep(1:4,14),rep(1:14,each=4)),
                      sprintf("cum_weightAP[%d]",seq(1:8)))

  # Extract the cumulative weights
  CumWeights = data.frame("Site"=rep(Sites,each = 64,times = 2),
                          "Flux" = rep(c("NEE","LE"),each = 64*length(Sites)),
                          "Variable" = rep(rownames(eval(as.name(paste0("NEE_",Sites)[1]))$CumWeights),length(Sites)*2),
                          "Lag" = rep(0,each = 64*length(Sites)*2),
                          "Low" = c(unlist(lapply(paste0("NEE_",Sites),function(x) eval(as.name(x))$CumWeights$WeightsLow)),
                                    unlist(lapply(paste0("LE_",Sites),function(x) eval(as.name(x))$CumWeights$WeightsLow))),
                          "Med" = c(unlist(lapply(paste0("NEE_",Sites),function(x) eval(as.name(x))$CumWeights$WeightsMedian)),
                                    unlist(lapply(paste0("LE_",Sites),function(x) eval(as.name(x))$CumWeights$WeightsMedian))),
                          "High" = c(unlist(lapply(paste0("NEE_",Sites),function(x) eval(as.name(x))$CumWeights$WeightsHigh)),
                                     unlist(lapply(paste0("LE_",Sites),function(x) eval(as.name(x))$CumWeights$WeightsHigh))))


  # Assign lags
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="1]"] = 0
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="2]"] = 1
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="3]"] = 2
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="4]"] = 3
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="5]"] = 4
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="6]"] = 5
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="7]"] = 6
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="8]"] = 7
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="9]"] = 8
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="10]"] = 9
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="11]"] = 10
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="12]"] = 11
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="13]"] = 12
  CumWeights$Lag[substr(CumWeights$Variable,15,18)=="14]"] = 13
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="1]"] = 0
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="2]"] = 20
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="3]"] = 29
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="4]"] = 59
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="5]"] = 119
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="6]"] = 179
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="7]"] = 269
  CumWeights$Lag[substr(CumWeights$Variable,14,15)=="8]"] = 365

  # Rename variables
  CumWeights$Variable[substr(CumWeights$Variable,13,13)==1] = "Tair"
  CumWeights$Variable[substr(CumWeights$Variable,13,13)==2] = "Fsd"
  CumWeights$Variable[substr(CumWeights$Variable,13,13)==3] = "VPD"
  CumWeights$Variable[substr(CumWeights$Variable,13,13)==4] = "PPTshort"
  CumWeights$Variable[substr(CumWeights$Variable,11,12)=="AP"] = "PPTlong"

  # Limit the dataframe to the variables requested
  CumWeights = CumWeights[CumWeights$Variable == Vars,]

  # Rename variables to nice names
  CumWeights$Variable[CumWeights$Variable == "Fsd"] = "Shortwave Radiation"
  CumWeights$Variable[CumWeights$Variable == "Tair"] = "Air Temperature"
  CumWeights$Variable[CumWeights$Variable == "PPTshort"] = "Short-term Precipitation"
  CumWeights$Variable[CumWeights$Variable == "PPTlong"] = "Long-term Precipitation"

  # Assign levels to Variable
  CumWeights$Variable = factor(CumWeights$Variable,levels = sort(unique(CumWeights$Variable)))

  # Plot the sensitivity covariates
  library(ggplot2)
  library(viridisLite)
  StackedWeightsPlot = ggplot(CumWeights) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_errorbar(aes(x = Lag,
                      ymin = Low,
                      ymax = High,
                      color = Flux),
                  width = 0.5,
                  alpha = 0.5,
                  size = 1) +
    geom_line(aes(x = Lag,
                  y = Med,
                  color = Flux),
              size = 2,
              alpha = 0.5) +
    facet_grid(Site~Variable,
               scales = "free_x") +
    coord_cartesian(ylim = c(0,1)) +
    ylab("Cumulative Weight") +
    xlab("Days into Past") +
    theme_bw() +
    theme(text = element_text(size=20),
          axis.text.x = element_text(angle=45, hjust=1)) +
    guides(color = guide_legend(title = "Flux"))
}
