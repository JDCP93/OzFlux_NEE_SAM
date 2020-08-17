source("MemoryR2.R")
source("MemoryR2_CABLE.R")

HS = MemoryR2("HowardSprings","HS")

SP = MemoryR2("SturtPlains","SP")

SP_CABLE = MemoryR2_CABLE("SturtPlains","SP")

HS_CABLE = MemoryR2_CABLE("HowardSprings","HS")

Sites = list("HS"=HS,"SP"=SP,"SP_CABLE"=SP_CABLE,"HS_CABLE"=HS_CABLE)
source("MemoryPlot.R")
Plot = MemoryPlot(Sites,"LAT")

save(Plot, file=paste('CABLE_Plot_Output_', Sys.Date(),'.rda', sep = ''))