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

plot(1:15, wss, "b")

