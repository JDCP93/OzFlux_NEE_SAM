MemoryPlot = function(Sites,VAR="MAP"){

# This is a function which plots the various memory components of a list of 
# sites once the R2 values have been extracted using the MemoryR2 function
# 
# INPUTS:
#   Sites - A named list of outputs from the MemoryR2 function. e.g. 
#           list("HS"=HS,"SP"=SP,"TT"=TT)
#           where HS = MemoryR2("HowardSprings","HS")
#           It's not pretty but it is what it is
# 
# OUTPUTS: 
#   Plot - A stacked bar chart of the memory components for each site
# 

  
# ******************************************************************************
# Order Sites by specified metric
# ******************************************************************************  
Name = names(Sites)
MAP = unlist(lapply(Sites, function(x) x$MAP))
MAT = unlist(lapply(Sites, function(x) x$MAT))
LAT = unlist(lapply(Sites, function(x) x$LAT))
  
data = data.frame(Name,MAP,MAT,LAT)

if (VAR == "MAT"){
  data = data[order(MAT),]
} else if (VAR == "LAT"){
  data = data[order(LAT),]
} else { 
  data = data[order(MAP),]
}
# ******************************************************************************
# Load model outputs  
# ******************************************************************************

Site = rep(names(Sites),3)
Site = factor(Site, levels = data$Name)
Model = rep(c("Current Environmental","Environmental Memory","Biological Memory"),each=length(Sites))
Model = factor(Model,levels=c("Biological Memory","Environmental Memory","Current Environmental"))

SAM = unlist(lapply(Sites, function(x) x$SAM.R2))
CUR = unlist(lapply(Sites, function(x) x$CUR.R2))
AR1 = unlist(lapply(Sites, function(x) x$AR1.R2))
Value = c(CUR,SAM-CUR,AR1-SAM)

Fig = data.frame(Site, Model, Value)


Plot = ggplot(Fig,aes(fill=Model,y=Value,x=Site)) +
  geom_bar(position="stack",stat="identity") +
  geom_bar(stat = "identity", color = "black",size = 1) +
  coord_flip(ylim=c(0,1)) +
  scale_fill_manual(values=c("darkgreen","royalblue4","skyblue"),
                    guide = guide_legend(reverse = TRUE)) +
  ylab(parse(text="R^2")) +
  theme_bw() +
  theme(legend.position = "bottom", 
        text = element_text(size = 20),
        legend.title = element_blank())
}