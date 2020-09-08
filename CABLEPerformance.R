CABLEPerformance = function(Site){

# A function for fnding CABLE performance at a site
# INPUTS:
# - Site: The 2 letter code used for the site
# OUTPUTS:
# - Output: A list of performance metrics and a plot of obs vs CABLE NEE

# load the observations and the model outputs
# Assign to generic names
load(paste0(Site,"DailyData.Rdata"))
assign("Obs",eval(as.name(paste0(Site,"DailyData"))))
load(paste0(Site,"DailyData_CABLE.Rdata"))
assign("CABLE",eval(as.name(paste0(Site,"DailyData_CABLE"))))

# Combine the modelled and observed data over the same time period
data = merge(Obs,CABLE,by.x="TIMESTAMP",by.y="TIMESTAMP")

# Calculate performance metrics
MAE = mean(abs(data$NEE_LL-data$NEE))
RMSE = sqrt(mean((data$NEE_LL-data$NEE)^2))
R2 = summary(lm(data$NEE ~ data$NEE_LL))$r.squared
Adj.R2 = summary(lm(data$NEE ~ data$NEE_LL))$adj.r.squared


# Calculate max and min
minLim = min(data$NEE,data$NEE_LL)
maxLim = max(data$NEE,data$NEE_LL)
# Plot
ObsModPlot <- ggplot(data.frame(data$NEE_LL,data$NEE)) +
  geom_point(aes(data$NEE_LL,data$NEE)) +
  geom_abline(slope=1,intercept=0) +
  xlab("Observed NEE") +
  ylab("CABLE NEE ") +
  theme_bw() +
  coord_fixed(xlim=c(minLim,maxLim),
              ylim=c(minLim,maxLim)) +
  annotate("text", x = maxLim-0.25*(maxLim-minLim), y = minLim+0.05*(maxLim-minLim),
           label = paste0('atop(R^2 ==', signif(R2,3), ', RMSE ==', signif(RMSE,3),')'),parse=TRUE,size = 5) +
 annotate("text", x = maxLim-0.05*(maxLim-minLim), y = minLim+0.05*(maxLim-minLim),
       label = paste0('atop(AdjR^2 ==', signif(Adj.R2,3), ', MAE ==', signif(MAE,3),')'),parse=TRUE, size = 5) +
theme(text = element_text(size = 20))

#Combine into output
Output = list("ObsModPlot"=ObsModPlot,
              "MAE"=MAE,
              "RMSE"=RMSE,
              "R2"=R2,
              "Adj.R2"=Adj.R2)

}
