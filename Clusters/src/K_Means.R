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
head(grade)
str(grade)
summary(grade)

# K Mean ----------------------------------------------------------------------------------------------------------------------------

data_out_kmeans=kmeans(grade,centers=3,nstart = 25)
data_out_kmeans

centroides=data_out_kmeans$centers#en la variable centroides llamo a centers del cluster anterior
centroides

distancia_inter_cluster=data_out_kmeans$tot.withinss#distancia entre clusters
distancia_inter_cluster

plot(grade$English,grade$Science)

wss=numeric(15)
