Flux_TimeSeries_RTPV = function(Site){

  message("Loading NEE input for ",Site)
  load(paste0("inputs/RTPV/",Site,"_Input_RTPV.Rdata"))
  assign(paste0("NEE_input"),eval(as.name(paste0(Site,"_Input"))))
  rm(list=paste0(Site,"_Input"))
  
  message("Loading LE input for ",Site)
  load(paste0("inputs/RTPV/LE/",Site,"_input_LE_RTPV.Rdata"))
  assign(paste0("LE_input"),eval(as.name(paste0(Site,"_input"))))
  rm(list=paste0(Site,"_input"))
  
  df = data.frame("Time" = rep(NEE_input$DailyData$TIMESTAMP,times=4),
                  "Variable" = rep(c("NEE","LE","PPT","NDVI"),
                                   each=length(NEE_input$DailyData$TIMESTAMP)),
                  "Value" = c(NEE_input$DailyData$NEE,
                              LE_input$DailyData$Fe,
                              NEE_input$DailyData$Precip,
                              NEE_input$NDVI))
  
  df$Variable = factor(df$Variable,levels=c("NEE","LE","PPT","NDVI"))
  
  Plot = ggplot(df) +
        geom_path(aes(x=Time,y=Value,color=Variable)) +
        scale_color_manual(values = c("green","red","royalblue","darkgreen"),
                           guide = "none") +
    facet_grid(Variable~.,scales = "free_y") +
    theme_bw() +
    ggtitle(paste0("Time Series of Flux, Rain and NDVI at ",Site))

  Plot
}
