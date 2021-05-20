Metrics_function = function(Sites,k,MinDate,MaxDate){
  
  df = data.frame(Site = rep(Sites,
                             each=3*5),
                  Model = rep(c("Current Climate",
                                "PPT lag up to 1 year",
                                "PPT lag up to 4 years"),
                              by=3,
                              each=5),
                  Metric = rep(c("R^2",
                                 "Mean Bias Error",
                                 "Normalised Mean Error",
                                 "Std. Dev. Difference",
                                 "Correlation Coeff."),
                               times = 3*length(Sites)))
  
  
  for (Site in Sites){
  # Load the k-means+regression model outputs
  File = list.files("output/",pattern = paste0("noPPT_",MinDate,MaxDate,"_",k,"cluster_output_",Site))
  message("Loading k-means output for ",Site," where file is ",File)
  load(paste0("output/",File))
  df$Value[df$Site == Site & df$Model == "Current Climate"] = unlist(output[c((k+1):(k+5))])
  
  
  File = list.files("output/",pattern = paste0("shortPPT_",MinDate,MaxDate,"_",k,"cluster_output_",Site))
  message("Loading k-means output for ",Site," where file is ",File)
  load(paste0("output/",File))
  df$Value[df$Site == Site & df$Model == "PPT lag up to 1 year"] = unlist(output[c((k+1):(k+5))])

  File = list.files("output/",pattern = paste0("longPPT_",MinDate,MaxDate,"_",k,"cluster_output_",Site))
  message("Loading k-means output for ",Site," where file is ",File)
  load(paste0("output/",File))
  df$Value[df$Site == Site & df$Model == "PPT lag up to 4 years"] = unlist(output[c((k+1):(k+5))])
  rm(output)
  }
  
  df$Value = signif(df$Value,3)

  df$Model = factor(df$Model,
                    levels = c("Current Climate",
                               "PPT lag up to 1 year",
                               "PPT lag up to 4 years"))
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
    geom_bar(aes(x = Site,
                 y=Value,
                 group = Model,
                 fill=Model),
             stat = "identity",
             position = "dodge") +
    geom_text(aes(x = Site,
                  y = Value/2, 
                  group = Model, 
                  label = round(Value,3)),
              position = position_dodge(width = 0.9)) +
    facet_grid(Metric~.,
               scales="free",
               labeller = label_parsed) +
    theme_bw() +
    scale_fill_viridis_d(direction = -1) +
    theme(panel.grid.major.x = element_blank(),
          text = element_text(size=20),
          axis.text.x = element_text(angle=45, hjust=1),
          legend.position = "bottom",
          legend.title = element_blank()) 
  Plot
  
}