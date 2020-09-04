### Schematic Plots
### 

# This script is to create basic plots for use in the schematic of the SAM
# method

# Clean up
rm(list=ls())

# Source libraries
library(ggplot2)

## *****************************************************************************
## Laplace Plot

# Create data frame
x = seq(-8,8, by = 0.01)
y = 0.5*exp(-(abs(x)))
df = data.frame("x"=x,"y"=y)
# Create plot
LaplacePlot = ggplot(df) +
  geom_vline(xintercept=0,size=0.5,color="gray",linetype='dashed') +
  geom_line(aes(x,y),size= 2, color = "blue") +
  theme_classic() +
  xlab(expression(mu)) +
  ylab("") +
  coord_cartesian(xlim=c(-8,8),ylim=c(0,0.55),expand = 0) +
  theme(axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  text = element_text(size=20))

# Save as 300x300
LaplacePlot


## *****************************************************************************
## Normal Plot

# Create data frame
x = seq(-4,4, by = 0.01)
y = (1/sqrt(2*pi))*exp(-0.5*x^2)
df = data.frame("x"=x,"y"=y)
# Create plot
NormalPlot = ggplot(df) +
  geom_line(aes(x,y),size= 2, color = "blue") +
  theme_classic() +
  xlab("") +
  ylab("") +
  coord_cartesian(xlim=c(-4,4),ylim=c(0,0.45),expand = 0) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=20))

# Save as 300x300
NormalPlot


## *****************************************************************************
## Uniform Plot

# Create data frame
x = seq(-4,4, by = 0.01)
y = c(rep(0,length(x)/4),rep(1/(max(x)-min(x)),length(x)/2+1),rep(0,length(x)/4))
df = data.frame("x"=x,"y"=y)
# Create plot
UniformPlot = ggplot(df) +
  geom_line(aes(x,y),size= 2, color = "blue") +
  theme_classic() +
  xlab("") +
  ylab("") +
  coord_cartesian(xlim=c(-4,4),ylim=c(0,0.4),expand = 0) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=20))

# Save as 300x300
UniformPlot



## *****************************************************************************
## Omega Plot

# Create data frame
x = 1:8
y = c(0,1,4,10,15,6,3,2)
df = data.frame("x"=x,"y"=y)
# Create plot
OmegaPlot = ggplot(df) +
  geom_bar(aes(x,y),stat="identity", fill = "blue") +
  theme_classic() +
  xlab("Lag") +
  ylab(expression(omega)) +
  coord_cartesian(xlim=c(1,9),ylim=c(0,18),expand = 0) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=20))

# Save as 300x300
OmegaPlot


## *****************************************************************************
## Phi Plot

# Create data frame
x = seq(0,1,by=0.01)
ymin = (1+1-x)*x
y = x
ymax = (1-1+x)*x 
df = data.frame("x"=x,"ymin"=ymin,"y"=y,"ymax"=ymax)
# Create plot
PhiPlot = ggplot(df) +
  geom_line(aes(x,ymin),color="lightblue",size=2) +
  geom_line(aes(x,y),color="blue",size=2) +
  geom_line(aes(x,ymax),color="darkblue",size=2) +
  theme_classic() +
  xlab("NDVI") +
  ylab(expression(phi)) +
  coord_cartesian(xlim=c(0,1),ylim=c(0,1),expand = 0) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=20))

# Save as 300x300
PhiPlot
