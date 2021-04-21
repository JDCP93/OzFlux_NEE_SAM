FindMetric = function(Metric){
  # Make a nice metric title
  Titles =c("Annual Mean Temp",
            "Mean Diurnal Range",
            "Isothermality",
            "Temp Seasonality",
            "Max Temp of Hottest Month",
            "Min Temp of Coldest Month",
            "Temp Annual Range",
            "Mean Temp in Wettest Qtr",
            "Mean Temp in Driest Qtr",
            "Mean Temp in Summer",
            "Mean Temp in Winter",
            "Mean Annual PPT",
            "PPT in Wettest Month",
            "PPT in Driest Month",
            "PPT Seasonality",
            "PPT in Wettest Qtr",
            "PPT in Driest Qtr",
            "PPT in Summer",
            "PPT in Winter")
  
  PotMetric = c("AnnualMeanTemp",
                "MeanDiurnalRange",
                "Isothermality",
                "TempSeasonality",
                "MaxTempHotMon",
                "MinTempColdMon",
                "TempAnnualRange",
                "MeanTempWetQtr",
                "MeanTempDryQtr",
                "MeanTempHotQtr",
                "MeanTempColdQtr",
                "AnnualPPT",
                "PPTWetMon",
                "PPTDryMon",
                "PPTSeasonality",
                "PPTWetQtr",
                "PPTDryQtr",
                "PPTHotQtr",
                "PPTColdQtr")
  
  Units = c("°C",
            "°C",
            "%",
            "°C",
            "°C",
            "°C",
            "°C",
            "°C",
            "°C",
            "°C",
            "°C",
            "mm",
            "mm",
            "mm",
            "%",
            "mm",
            "mm",
            "mm",
            "mm")
  
  Title = Titles[Metric == PotMetric]
  Units = Units[Metric == PotMetric]
  
  output = list("Title" = Title,
                "Units" = Units)
}