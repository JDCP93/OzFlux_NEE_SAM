NEE_MetricsPlot_function_RTPV = function(Sites){


df = data.frame("Site" = rep(Sites,each = 15),
                "Model" = rep(c("CUR","SAM","AR1"),each = 5, times = length(Sites)),
                "Metric" = rep(c("R2","MBE","NME","SDD","CCO"), times = 3*length(Sites)),
                "Value" = NA)


for (Site in Sites){

  File = list.files("analysis/RTPV/metrics/",pattern = paste0("NEE_current_metrics_RTPV_",Site))
  load(paste0("analysis/RTPV/metrics/",File))
  df$Value[df$Site == Site & df$Model == "CUR"] = unlist(output[c(1:5)])

  File = list.files("analysis/RTPV/metrics/",pattern = paste0("NEE_metrics_RTPV_",Site))
  load(paste0("analysis/RTPV/metrics/",File))
  df$Value[df$Site == Site & df$Model == "SAM"] = unlist(output[c(1:5)])

  File = list.files("analysis/RTPV/metrics/",pattern = paste0("NEE_AR1_metrics_RTPV_",Site))
  load(paste0("analysis/RTPV/metrics/",File))
  df$Value[df$Site == Site & df$Model == "AR1"] = unlist(output[c(1:5)])
}

df$Model = factor(df$Model,levels = c("CUR","SAM","AR1"))
df$Metric = factor(df$Metric, levels = c("R2","CCO","MBE","NME","SDD"))

Plot = ggplot(df) +
  geom_bar(aes(x = Site,y=Value,group = Model,fill=Model),stat = "identity",position = "dodge") +
  geom_text(aes(x = Site, y = Value+0.1, group = Model, label = round(Value,2)),position = position_dodge(width = 1)) +
  facet_grid(Metric~.,scales="free") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

}
