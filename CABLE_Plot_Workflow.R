rm(list=ls())
# Source functions
source("MemoryR2.R")
source("MemoryR2_CABLE.R")

# Run functions for each site and CABLE/Obs
# Note: I had issues with the size of the variables being assigned - the order 
# below SHOULD work as it runs the largest files first (and the functions then
# bin the variables)
HS_CABLE = MemoryR2_CABLE("HowardSprings","HS")

HS = MemoryR2("HowardSprings","HS")

SP = MemoryR2("SturtPlains","SP")

SP_CABLE = MemoryR2_CABLE("SturtPlains","SP")

# Pop them into a list and plot with a function!
Sites = list("HS"=HS,"SP"=SP,"SP_CABLE"=SP_CABLE,"HS_CABLE"=HS_CABLE)
source("MemoryPlot.R")
Plot = MemoryPlot(Sites,"LAT")

save(Plot, file=paste('CABLE_Plot_Output_', Sys.Date(),'.rda', sep = ''))


# Manual code 
# The below code plots a slightly nicer graph with the values hard-coded
Site = rep(c("HS","SP"),6)
Type = rep(c("Obs","CABLE"),3,each=2)
Model = rep(c("Current Environmental","Environmental Memory","Biological Memory"),each=4)
Model = factor(Model,levels=c("Biological Memory","Environmental Memory","Current Environmental"))

# HARD CODE VALUES HERE
SAM = c(0.44,0.70,0.79,0.92)
CUR = c(0.40,0.65,0.78,0.89)
AR1 = c(0.68,0.84,0.84,0.95)
Value = c(CUR,SAM-CUR,AR1-SAM)

Fig = data.frame(Site, Type, Model, Value)


Plot = ggplot(Fig,aes(fill=Model,y=Value,x=Type)) +
  geom_bar(position="stack",stat="identity") +
  geom_bar(stat = "identity", color = "black",size = 1) +
  coord_flip(ylim=c(0,1)) +
  scale_fill_manual(values=c("darkgreen","royalblue4","skyblue"),
                    guide = guide_legend(reverse = TRUE)) +
  facet_grid(Site ~ .) +
  theme_bw() +
  theme(legend.position = "bottom") +
  ggtitle(paste0("Memory of Sites in CABLE"))+
  xlab("Sites") +
  ylab(parse(text="R^2"))
