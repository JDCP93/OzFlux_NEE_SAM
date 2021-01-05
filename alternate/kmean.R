
load("AU-Wom_Input.Rdata")
input = `AU-Wom_Input`
climate = cbind(input$clim,input$ppt_multiscale)
climate = climate[-(1:365),]


df = data.frame("nCluster" = seq(5,50, by = 5),
                "withinss" = rep(NA,10))
for (i in 1:10){
df[i,2] = kmeans(climate,i*5,iter.max = 25, nstart = 25)$tot.withinss
}

plot(df[,1],df[,2])


kmean.output = kmeans(climate,20,iter.max = 25, nstart = 25)

for (i in 1:20){
  cluster = climate[kmean.output$cluster==i,]
  
}
