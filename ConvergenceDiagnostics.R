load("results/NEE_output_long_site_HS_2020-08-31.rda")

library(coda)
library(ggplot2)

Gelman = gelman.diag(nee_daily,multivariate=FALSE)

Oops = Gelman$psrf[Gelman$psrf[,2]>1.1,]

ESS.raw = effectiveSize(nee_daily)
ESS = ESS.raw[ESS.raw > 0]
ESSPlot = ggplot(data.frame(ESS)) +
  geom_histogram(aes(ESS),binwidth=250)



Geweke = geweke.diag(nee_daily)

GewekePlot = geweke.plot(nee_daily)
