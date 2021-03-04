# OzFlux_NEE_SAM

Repo to track modelling of Aussie ecosystems using SAM

## Models

All models, inputs, and outputs have a code in the filename to identify which
climate variables are involved out of the 5 below:

 - R: Shortwave Radiation
 - T: Air Temperature
 - P: Precipitation
 - V: Vapour Pressure Deficit
 - S: Soil Moisture Content

Therefore, a file called "Model_RTPV" is a model which uses the 4 climate drivers
specified by RTPV - it doesn't include soil moisture.

RTPVS is the analysis that Liu et al (2019) performed for other sites globally.

|an/ag|RTPV|RTPVS|
|:-:|:-:|:-:|
|1|Intercept|Intercept|
|2|Tair|Tair|
|3|Fsd|Fsd|
|4|VPD|VPD|
|5|PPTshort|SWCcur|
|6|Tair x Tair|SWCant|
|7|Fsd x Fsd|Tair x Tair|
|8|VPD x VPD|Fsd x Fsd|
|9|PPTshort x PPTshort|VPD x VPD|
|10|Tair x Fsd|SWCcur x SWCcur|
|11|Tair x VPD|SWCant x SWCant|
|12|Tair x PPTshort|Tair x Fsd|
|13|Fsd x VPD|Tair x VPD|
|14|Fsd x PPTshort|Tair x SWCcur|
|15|VPD x PPTshort|Tair x SWCant|
|16|PPTlong|Fsd x VPD|
|17|---|Fsd x SWCcur|
|18|---|Fsd x SWCant|
|19|---|VPD x SWCcur|
|20|---|VPD x SWCant|
|21|---|SWCcur x SWCant|
|22|---|PPTlong|