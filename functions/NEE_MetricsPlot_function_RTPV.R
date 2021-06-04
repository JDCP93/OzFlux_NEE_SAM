NEE_MetricsPlot_function_RTPV = function(Sites){

# Initialise dataframe
df = data.frame("Site" = rep(Sites,each = 15),
                "Model" = rep(c("Current Environment (SAM)",
                                "Environmental Memory (SAM)",
                                "Biological Memory (AR1)"),
                              each = 5,
                              times = length(Sites)),
                "Metric" = rep(c("R^2",
                                 "Mean Bias Error",
                                 "Normalised Mean Error",
                                 "Std. Dev. Difference",
                                 "Correlation Coeff."),
                               times = 3*length(Sites)),
                "Value" = NA)


for (Site in Sites){

  File = list.files("analysis/RTPV/metrics/",
                    pattern = paste0("NEE_current_metrics_RTPV_",Site))
  load(paste0("analysis/RTPV/metrics/",File))
  df$Value[df$Site == Site & df$Model == "Current Environment (SAM)"] = unlist(output[c(1:5)])

  File = list.files("analysis/RTPV/metrics/",
                    pattern = paste0("NEE_metrics_RTPV_",Site))
  load(paste0("analysis/RTPV/metrics/",File))
  df$Value[df$Site == Site & df$Model == "Environmental Memory (SAM)"] = unlist(output[c(1:5)])

  File = list.files("analysis/RTPV/metrics/",
                    pattern = paste0("NEE_AR1_metrics_RTPV_",Site))
  load(paste0("analysis/RTPV/metrics/",File))
  df$Value[df$Site == Site & df$Model == "Biological Memory (AR1)"] = unlist(output[c(1:5)])
}

df$Model = factor(df$Model,
                  levels = c("Current Environment (SAM)",
                              "Environmental Memory (SAM)",
                              "Biological Memory (AR1)"))
df$Metric = factor(df$Metric, 
                   levels = c("Normalised Mean Error",
                              "Std. Dev. Difference",
                              "Mean Bias Error",
                              "R^2",
                              "Correlation Coeff."),
                   labels = c(expression("Normalised~Mean~Error"),
                              expression("Std.~Dev.~Difference"),
                              expression("Mean~Bias~Error"),
                              expression("R^2"),
                              expression("Correlation~Coeff.")))

Plot = ggplot(df) +
  geom_hline(yintercept=0,alpha = 0.5,size = 0.5) +
  geom_bar(aes(x = Site,
               y=Value,
               group = Model,
               fill=Model),
           stat = "identity",
           position = "dodge",
           color="black") +
  #geom_text(aes(x = Site,
  #              y = Value/2, 
  #              group = Model, 
  #              label = round(Value,2)),
  #          position = position_dodge(width = 0.9)) +
  facet_grid(Metric~.,
             scales="free",
             labeller = label_parsed) +
  theme_bw() +
  xlab("") +
  scale_fill_viridis_d(direction = -1) +
  scale_y_continuous(expand=c(0.1,0)) +
  theme(panel.grid.major.x = element_blank(),
        text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

}
