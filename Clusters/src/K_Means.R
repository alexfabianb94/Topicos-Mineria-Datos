# Inicializacion de Paquetes ------------------------------------------------------------------------------------------------------
# install.packages(c("ggplot2", "plyr", "gridExtra", "plyr"))

library(plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)

# Carga de datos  -------------------------------------------------------------------------------------------------------------------

grade=read.csv("../data/grades_km_input.csv",sep = ",",row.names = 1)
str(grade)
summary(grade)

# K Mean ----------------------------------------------------------------------------------------------------------------------------

wss=numeric(15)
for (i in 1:15) {
  data_out_kmeans <- kmeans(grade,centers=i,nstart = 25)
  wss[i] <- data_out_kmeans$tot.withinss
}

data_out_kmeans <- kmeans(grade,centers=3,nstart = 25)
plot(1:15, wss, "b")

df <- grade

df$Cluster=factor(data_out_kmeans$cluster)
Centers=as.data.frame(data_out_kmeans$centers)

# Graphics ----------------------------------------------------------------------------------------------------------------------------

g1=ggplot(data=df, aes(x=English, y=Math, color=Cluster))+geom_point()+theme(legend.position = "right")+geom_point(data=Centers, aes(x=English, y=Math, color=as.factor(c(1,2,3))), size=10, alpha=.3, show.legend =FALSE)
g2=ggplot(data=df, aes(x=English, y=Science, color=Cluster))+geom_point()+theme(legend.position = "right")+geom_point(data=Centers, aes(x=English, y=Science, color=as.factor(c(1,2,3))), size=10, alpha=.3, show.legend =FALSE)
g3=ggplot(data=df, aes(x=Math, y=Science, color=Cluster))+geom_point()+theme(legend.position = "right")+geom_point(data=Centers, aes(x=Math, y= Science, color=as.factor(c(1,2,3))), size=10, alpha=.3, show.legend =FALSE)
tmp=ggplot_gtable(ggplot_build(g1))

#grid.arrange(arrangeGrob(g1+theme(legend.position ="none"), g2+theme(legend.position ="none"), g3+theme(legend.position ="none"),main="High School Students Cluster Analysys",ncol=1))

grid.arrange(g1, g2, g3,top="High School Students Cluster Analysys",ncol=3)
