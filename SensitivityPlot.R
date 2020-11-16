SensitivityPlot = function(Sites,Vars = c("Tair","Fsd","VPD","curSWC","antSWC","Precip","SWC")){

# Run the analysis of the model outputs if they don't exist
source("r2jags_analysis.R")
for (Site in Sites){
  if (file.exists(paste0("NEE_Analysis_",Site,".Rdata"))){
    message("Analysis file already exists for ",Site,". Moving to next site...")
  } else {
    message("Conducting model output analysis for ",Site,". Please wait...")
    r2jags_analysis(Site)
  }
}
message("Plotting sensitivities for sites...")

# Initialise MAP dataframe
MAP = data.frame("Site"=Sites,
                 "MAP"=rep(NA,length(Sites)))

for (Site in Sites){
  # Collect the analysis outputs and name them with each site
  load(paste0("NEE_Analysis_",Site,".Rdata"))
  assign(Site,output)
  rm(output)
  
  
  # We also load the daily data to calculate MAP
  load(paste0(Site,"_Input.Rdata"))
  Input = eval(as.name(paste0(Site,"_Input")))
  DailyData = Input$DailyData
  DailyData$year = year(DailyData$TIMESTAMP)
  
  YearlyData <- DailyData %>%
    group_by(year) %>%               # group by the year column
    summarise(Precip=sum(Precip,na.rm=TRUE))
  
  MAP$MAP[MAP$Site==Site] = mean(YearlyData$Precip)
}

# Extract the sensitvity covariates
ESen = data.frame("Site"=rep(Sites,each = 7),
          "Variable" = rep(rownames(eval(as.name(Sites[1]))$ESen),length(Sites)),
          "Low" = unlist(lapply(Sites,function(x) eval(as.name(x))$ESen$ESenLow)),
          "Med" = unlist(lapply(Sites,function(x) eval(as.name(x))$ESen$ESenMedian)),
          "High" = unlist(lapply(Sites,function(x) eval(as.name(x))$ESen$ESenHigh)))
# Check whether the climate variable is significant - does the CI contain 0?
ESen$Significant = sign(ESen$Low*ESen$High)==1

# Limit the dataframe to the variables requested
ESen = ESen[ESen$Variable %in% Vars,]

# Rename variables to nice names
ESen$Variable[ESen$Variable == "Fsd"] = "Shortwave Radiation"
ESen$Variable[ESen$Variable == "Tair"] = "Air Temperature"
ESen$Variable[ESen$Variable == "antSWC"] = "Antecedent SWC"
ESen$Variable[ESen$Variable == "curSWC"] = "Current SWC"
ESen$Variable[ESen$Variable == "SWC"] = "Antecedent + Current SWC"

# Assign levels to Variable
ESen$Variable = factor(ESen$Variable,levels = sort(unique(ESen$Variable)))

# Order sites by MAP
MAP = MAP[order(MAP$MAP,decreasing = TRUE),]
ESen$Site = factor(ESen$Site,levels=MAP$Site)

# Plot the sensitivity covariates
library(ggplot2)
library(viridisLite)
ESenPlot = ggplot(ESen) +
  geom_hline(yintercept = 0, linetype = "dashed") +
              geom_pointrange(aes(x = Site, 
                                  ymin = Low, 
                                  y = Med, 
                                  ymax = High, 
                                  color = Significant)) +
  geom_errorbar(aes(x = Site, 
                    ymin = Low, 
                    ymax = High, 
                    color = Significant), 
                width = 0.5) +
  facet_wrap(Variable~.,
              scales = "free_y",
             ncol = 2) +
  scale_color_viridis_d(name="",
                        labels=c("FALSE"="Non-Significant","TRUE"="Significant"),
                        guide="legend",
                        option="magma",
                        begin=0.2,
                        end=0.6) +
  ylab("Sensitivity") +
  theme_bw() +
  theme(legend.position = "top",
        text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust=1))
}