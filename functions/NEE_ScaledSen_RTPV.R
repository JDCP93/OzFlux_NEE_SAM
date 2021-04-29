NEE_ScaledSen = function(Site){
  
  library(coda)
  library(gtools)
  library(stringr)
  
  # Load the input file and extract required data
  load(paste0("inputs/RTPV/",Site,"_Input_RTPV.Rdata"))
  input = eval(as.name(paste0(Site,"_Input")))
  # Combine climate data and precip data
  climend = nrow(input$clim)
  climate = cbind(input$clim,
                  rbind(matrix(NA,1,4),input$clim[-climend,]),
                  rbind(matrix(NA,2,4),input$clim[-((climend-1):climend),]),
                  rbind(matrix(NA,3,4),input$clim[-((climend-2):climend),]),
                  rbind(matrix(NA,4,4),input$clim[-((climend-3):climend),]),
                  rbind(matrix(NA,5,4),input$clim[-((climend-4):climend),]),
                  rbind(matrix(NA,6,4),input$clim[-((climend-5):climend),]),
                  rbind(matrix(NA,7,4),input$clim[-((climend-6):climend),]),
                  rbind(matrix(NA,8,4),input$clim[-((climend-7):climend),]),
                  rbind(matrix(NA,9,4),input$clim[-((climend-8):climend),]),
                  rbind(matrix(NA,10,4),input$clim[-((climend-9):climend),]),
                  rbind(matrix(NA,11,4),input$clim[-((climend-10):climend),]),
                  rbind(matrix(NA,12,4),input$clim[-((climend-11):climend),]),
                  rbind(matrix(NA,13,4),input$clim[-((climend-12):climend),]),
                  input$ppt_multiscale)
  
  
  colnames(climate) = c("Tair_1",
                        "Fsd_1",
                        "VPD_1",
                        "PPTshort_1",
                        "Tair_2",
                        "Fsd_2",
                        "VPD_2",
                        "PPTshort_2",
                        "Tair_3",
                        "Fsd_3",
                        "VPD_3",
                        "PPTshort_3",
                        "Tair_4",
                        "Fsd_4",
                        "VPD_4",
                        "PPTshort_4",
                        "Tair_5",
                        "Fsd_5",
                        "VPD_5",
                        "PPTshort_5",
                        "Tair_6",
                        "Fsd_6",
                        "VPD_6",
                        "PPTshort_6",
                        "Tair_7",
                        "Fsd_7",
                        "VPD_7",
                        "PPTshort_7",
                        "Tair_8",
                        "Fsd_8",
                        "VPD_8",
                        "PPTshort_8",
                        "Tair_9",
                        "Fsd_9",
                        "VPD_9",
                        "PPTshort_9",
                        "Tair_10",
                        "Fsd_10",
                        "VPD_10",
                        "PPTshort_10",
                        "Tair_11",
                        "Fsd_11",
                        "VPD_11",
                        "PPTshort_11",
                        "Tair_12",
                        "Fsd_12",
                        "VPD_12",
                        "PPTshort_12",
                        "Tair_13",
                        "Fsd_13",
                        "VPD_13",
                        "PPTshort_13",
                        "Tair_14",
                        "Fsd_14",
                        "VPD_14",
                        "PPTshort_14",
                        "PPTlong_1",
                        "PPTlong_2",
                        "PPTlong_3",
                        "PPTlong_4",
                        "PPTlong_5",
                        "PPTlong_6",
                        "PPTlong_7",
                        "PPTlong_8")
  
  # Remove first year, which has no PPT data
  climate = climate[-(1:365),]
  NEE = input$NEE[-(1:365)]
  

  # Load the coefficients
  File = list.files("analysis/RTPV/",pattern = paste0("NEE_summary_RTPV_",Site))
  load(paste0("analysis/RTPV/",File))
  
  # Extract coefficients and weights
  coeff = output.summary$statistics[substr(rownames(output.summary$statistics),1,1)=="a",1]
  coeff_min = output.summary$quantiles[substr(rownames(output.summary$quantiles),1,1)=="a",1]
  coeff_max = output.summary$quantiles[substr(rownames(output.summary$quantiles),1,1)=="a",5]
  
  weightnames = c(sprintf("weightA[%d,%d]",rep(1:4,14),rep(1:14,each=4)),
                      sprintf("weightAP[%d]",seq(1:8)))
  
  weights = output.summary$statistics[rownames(output.summary$statistics) %in% weightnames,1]
  weights_min = output.summary$quantiles[rownames(output.summary$quantiles) %in% weightnames,1]
  weights_max = output.summary$quantiles[rownames(output.summary$quantiles) %in% weightnames,5]
  # Rename weights
  names(weights)=str_replace_all(names(weights),pattern="weightA\\[1,","Tair_")
  names(weights)=str_replace_all(names(weights),pattern="weightA\\[2,","Fsd_")
  names(weights)=str_replace_all(names(weights),pattern="weightA\\[3,","VPD_")
  names(weights)=str_replace_all(names(weights),pattern="weightA\\[4,","PPTshort_")
  names(weights)=str_replace_all(names(weights),pattern="weightAP\\[","PPTlong_")
  names(weights)=str_replace_all(names(weights),pattern="\\]","")
  
  # Initialise matrix
  wts = data.frame("Timestep" = 1:nrow(climate),
                   "Tair" = NA,
                   "Fsd" = NA,
                   "VPD" = NA,
                   "PPTshort" = NA,
                   "PPTlong" = NA)
  
  # Calculated the weighted climate term at each timestep
  for (t in wts$Timestep){
    wts$Tair[t] = sum(weights[substr(names(weights),1,4)=="Tair"]*climate[t,substr(colnames(climate),1,4)=="Tair"])
    wts$Fsd[t] = sum(weights[substr(names(weights),1,3)=="Fsd"]*climate[t,substr(colnames(climate),1,3)=="Fsd"])
    wts$VPD[t] = sum(weights[substr(names(weights),1,3)=="VPD"]*climate[t,substr(colnames(climate),1,3)=="VPD"])
    wts$PPTshort[t] = sum(weights[substr(names(weights),1,4)=="PPTs"]*climate[t,substr(colnames(climate),1,4)=="PPTs"])
    wts$PPTlong[t] = sum(weights[substr(names(weights),1,4)=="PPTl"]*climate[t,substr(colnames(climate),1,4)=="PPTl"])
  }
  
  # Calculate the mean weighted climate
  meanwts = colMeans(wts[2:6])
  
  # Calculate the weighted coefficients
  wcoeff = c(coeff[2:5]*meanwts[1:4],
             coeff[6:9]*meanwts[1:4]*meanwts[1:4],
             coeff[10:15]*meanwts[combinations(n=4,r=2)[,1]]*meanwts[combinations(n=4,r=2)[,2]],
             coeff[16]*meanwts[5],
             coeff[18:21]*meanwts[1:4],
             coeff[22:25]*meanwts[1:4]*meanwts[1:4],
             coeff[26:31]*meanwts[combinations(n=4,r=2)[,1]]*meanwts[combinations(n=4,r=2)[,2]],
             coeff[32]*meanwts[5])
  
  
  wSen = data.frame("Tair" = wcoeff[c(1,5,9,10,11,16,20,24,25,26)],
                    "Fsd" = wcoeff[c(2,6,9,12,13,17,21,24,27,28)],
                    "VPD" = wcoeff[c(3,7,10,12,14,18,22,25,27,29)],
                    "PPTshort" = wcoeff[c(4,8,11,13,14,19,23,26,28,29)],
                    "PPTlong" = wcoeff[c(15,30,NA,NA,NA,NA,NA,NA,NA,NA)])
  rownames(wSen) = 1:10
  
  SumSen = colSums(wSen,na.rm=TRUE)
  
  
  
  # Calculate the weighted coefficients for lower 95% CI
  wcoeff_min = c(coeff_min[2:5]*meanwts[1:4],
             coeff_min[6:9]*meanwts[1:4]*meanwts[1:4],
             coeff_min[10:15]*meanwts[combinations(n=4,r=2)[,1]]*meanwts[combinations(n=4,r=2)[,2]],
             coeff_min[16]*meanwts[5],
             coeff_min[18:21]*meanwts[1:4],
             coeff_min[22:25]*meanwts[1:4]*meanwts[1:4],
             coeff_min[26:31]*meanwts[combinations(n=4,r=2)[,1]]*meanwts[combinations(n=4,r=2)[,2]],
             coeff_min[32]*meanwts[5])
  
  
  wSen_min = data.frame("Tair" = wcoeff_min[c(1,5,9,10,11,16,20,24,25,26)],
                    "Fsd" = wcoeff_min[c(2,6,9,12,13,17,21,24,27,28)],
                    "VPD" = wcoeff_min[c(3,7,10,12,14,18,22,25,27,29)],
                    "PPTshort" = wcoeff_min[c(4,8,11,13,14,19,23,26,28,29)],
                    "PPTlong" = wcoeff_min[c(15,30,NA,NA,NA,NA,NA,NA,NA,NA)])
  rownames(wSen_min) = 1:10
  
  SumSen_min = colSums(wSen_min,na.rm=TRUE)
  
  
  
  # Calculate the weighted coefficients for upper 95% CI
  wcoeff_max = c(coeff_max[2:5]*meanwts[1:4],
                 coeff_max[6:9]*meanwts[1:4]*meanwts[1:4],
                 coeff_max[10:15]*meanwts[combinations(n=4,r=2)[,1]]*meanwts[combinations(n=4,r=2)[,2]],
                 coeff_max[16]*meanwts[5],
                 coeff_max[18:21]*meanwts[1:4],
                 coeff_max[22:25]*meanwts[1:4]*meanwts[1:4],
                 coeff_max[26:31]*meanwts[combinations(n=4,r=2)[,1]]*meanwts[combinations(n=4,r=2)[,2]],
                 coeff_max[32]*meanwts[5])
  
  
  wSen_max = data.frame("Tair" = wcoeff_max[c(1,5,9,10,11,16,20,24,25,26)],
                        "Fsd" = wcoeff_max[c(2,6,9,12,13,17,21,24,27,28)],
                        "VPD" = wcoeff_max[c(3,7,10,12,14,18,22,25,27,29)],
                        "PPTshort" = wcoeff_max[c(4,8,11,13,14,19,23,26,28,29)],
                        "PPTlong" = wcoeff_max[c(15,30,NA,NA,NA,NA,NA,NA,NA,NA)])
  rownames(wSen_max) = 1:10
  
  SumSen_max = colSums(wSen_max,na.rm=TRUE)
  
  
  Sen = data.frame("min" = SumSen_min,
                      "med" = SumSen,
                      "max" = SumSen_max)
}