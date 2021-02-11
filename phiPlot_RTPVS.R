PhiPlot = function(Sites,Group,PPT=TRUE){
  
  # Load packages
  library(ggplot2)
  
  # Initialise dataframe
  df = data.frame("Site" = Sites,
                  "Phi0" = 0,
                  "NDVImin" = 0,
                  "NDVImean" = 0,
                  "NDVImax" = 0,
                  "NDVIsd" = 0,
                  "phimin" = 0,
                  "phimax" = 0)

  for (Site in Sites){
    # Collect the analysis outputs and name them with each site
    File = list.files("analysis/RTPVS/",pattern = paste0("NEE_metrics_RTPVS_",Site))
    # Read the data into R - note that if multiple results are available for a 
    # site, we take the most recent
    load(paste0("analysis/RTPVS/",File[length(File)]))

    # We also load the daily data to calculate MAP
    load(paste0("inputs/RTPVS/",Site,"_Input_RTPVS.Rdata"))
    Input = eval(as.name(paste0(Site,"_Input")))
    
    # Extract Phi0
    df$Phi0[df$Site==Site] = output$Phi0[1]
    
    # Extract NDVI
    df$NDVImin[df$Site==Site] = min(Input$NDVI)
    df$NDVImean[df$Site==Site] = mean(Input$NDVI)
    df$NDVImax[df$Site==Site] = max(Input$NDVI)
    df$NDVIsd[df$Site==Site] = sd(Input$NDVI)
    
    df$phimax[df$Site==Site] = max((1-df$Phi0[df$Site==Site]+df$Phi0[df$Site==Site]*Input$NDVI)*Input$NDVI)
    df$phimin[df$Site==Site] = min((1-df$Phi0[df$Site==Site]+df$Phi0[df$Site==Site]*Input$NDVI)*Input$NDVI)
  }
  
  # Define generic x
  x = seq(0,1,0.01)
  # Calculate the value of phi for all sites
  phi = (1-df$Phi0[df$Site==Site]+df$Phi0[df$Site==Site]*x)*x
  phi = sapply(df$Phi0, function(t) (1-t+t*x)*x)
  
  # Create plot dataframe
  plot.df = data.frame("Site" = rep(Sites,each=101),
                       "Category" = rep(Group,each=101),
                       "phi" = as.vector(phi),
                       "NDVI" = rep(x,length(Sites)))
  
  # Load climate metrics
  load("analysis/RTPVS/worldclim_biovar_0.5res_RTPVS_correlations.Rdata")
  metrics = WorldClim$Metrics
  
  if(PPT){
    plot.df$Site = factor(plot.df$Site,levels = metrics$Sites[order(metrics$AnnualPPT)])
    title = "Sites, increasing rainfall"
  }else{
    plot.df$Site = factor(plot.df$Site,levels = df$Site[order(df$NDVImean)])
    title = "Sites, increasing mean NDVI"
  }
  # Plot!
  
  plot = ggplot(plot.df,aes(x=NDVI,y=phi)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted",color = "black") +
                  geom_line(aes(x = NDVI,y = phi, group = interaction(Site,Category), color = Site, linetype = Category),size = 1.5) +
  # scale_color_viridis_d(name = "Sites, decreasing dryness",
  #                       guide="legend",
  #                       begin = 0.9,
  #                       end = 0.1) +
    scale_color_manual(name=title,
                       # values = c(viridis(7,begin = 0.25, end = 0.75, direction = -1)[1:2],
                       #            magma(6,begin = 0.25, end = 0.75)[1:3],
                       #            viridis(7,begin = 0.25, end = 0.75, direction = -1)[3:4],
                       #            magma(6,begin = 0.25, end = 0.75)[4],
                       #            viridis(7,begin = 0.25, end = 0.75, direction = -1)[5:6],
                       #            magma(6,begin = 0.25, end = 0.75)[5:6],
                       #            viridis(7,begin = 0.25, end = 0.75, direction = -1)[7])) +
                       values = c(brewer.pal(9,"Greens")[3:4],
                                 brewer.pal(9,"Purples")[4:6],
                                 brewer.pal(9,"Greens")[5:6],
                                 brewer.pal(9,"Purples")[7],
                                 brewer.pal(9,"Greens")[7:8],
                                 brewer.pal(9,"Purples")[8:9],
                                 brewer.pal(9,"Greens")[9])) +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    scale_x_continuous(limits = c(0,1), expand = c(0, 0),breaks=c(0,1)) +
    scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
    ggtitle("NDVI relation to Growing/Dormant Partitioning") +
    xlab("NDVI") +
    ylab("Fraction attributed to Growing behaviour")
  
  
  plot.df2 = data.frame("Site" = rep(Sites,each=20),
                        "NDVI" = 0,
                        "phi" = 0)
  
  for (Site in Sites){
    phi0 = df$Phi0[df$Site==Site]
    plot.df2$NDVI[plot.df2$Site==Site] = seq(df$NDVImin[df$Site==Site],df$NDVImax[df$Site==Site],length.out = 20)
    NDVI = plot.df2$NDVI[plot.df2$Site==Site]
    plot.df2$phi[plot.df2$Site==Site] = (1-phi0+phi0*NDVI)*NDVI
  }
  
  
  
  plot = ggplot(plot.df2,aes(x=NDVI,y=phi)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted",color = "black") +
    geom_line(aes(x = NDVI,y = phi,color=Site),size = 1.5) +
    # scale_color_viridis_d(name = "Sites, decreasing dryness",
    #                       guide="legend",
    #                       begin = 0.9,
    #                 
    theme_bw() +
    theme(text = element_text(size = 20)) +
    scale_x_continuous(limits = c(0,1), expand = c(0, 0),breaks=c(0,1)) +
    scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
    ggtitle("NDVI relation to Growing/Dormant Partitioning") +
    xlab("NDVI") +
    ylab("Fraction attributed to Growing behaviour")
  
  plot
  
}
  
Sites = c("AU-ASM",
          "AU-Cpr",
          "AU-Cum",
          "AU-DaS",
          "AU-Dry",
          "AU-Gin",
          "AU-GWW",
          "AU-How",
          "AU-Stp",
          "AU-TTE",
          "AU-Tum",
          "AU-Whr",
          "AU-Wom")

Category = c("NATT",
          "SAWS",
          "SAWS",
          "NATT",
          "NATT",
          "SAWS",
          "SAWS",
          "NATT",
          "NATT",
          "NATT",
          "SAWS",
          "SAWS",
          "SAWS")

plot = PhiPlot(Sites,Category,PPT=TRUE)
