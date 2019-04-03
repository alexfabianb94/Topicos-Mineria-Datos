install.packages(&quot;ggplot2&quot;)#este comando es para trabajar con gráficos.
install.packages(&quot;plyr&quot;)
install.packages(&quot;gridExtra&quot;)

library(plyr)
library(ggplot2)
library(cluster)
library (lattice)
library(graphics)
library(grid)
library(gridExtra)

grade_input=read.csv(&quot;C:/Users/Dr. Troncoso/Documents/grades_km_input.csv&quot;, sep = &quot;;&quot;,
row.names = &quot;Student&quot;)

grade_input
head(grade_input)
str(grade_input)
summary(grade_input)

#kmdata_orig= as.matrix(grade_input[,c(&quot;Student&quot;,&quot;English&quot;,&quot;Math&quot;,&quot;Science&quot;)])

#head(kmdata_orig)

#kmdata&lt;-kmdata_orig[,2:4]#solo los atributos, sin el id de los estudiantes

#head(kmdata)

#Aplicando un cluster de 3

data_out_kmeans&lt;-kmeans(grade_input, centers=3, nstart = 25)
data_out_kmeans
CENTROIDES&lt;-data_out_kmeans$centers
CENTROIDES

distancia_inter_cluste&lt;-data_out_kmeans$tot.withinss
distancia_inter_cluste

plot(grade_input$Math,grade_input$Science)

wss&lt;-numeric(15)

#ahora, apliquemos con un ciclo for una serie de clusters y seleccionemos el número optimo

for (k in 1:15)
wss[k]&lt;-kmeans(grade_input, centers=k, nstart = 25)$tot.withinss
wss

plot(1:15, wss, type=&quot;b&quot;, xlab=&quot;Número de Clusters&quot;, ylab=&quot;Suma de Cuadrados Inter clusters&quot;)

df=grade_input
df
df$Cluster=factor(data_out_kmeans$cluster)#asigna a cada registro el cluster al que pertenece
df

Centers=as.data.frame(data_out_kmeans$centers)#registra los centros en un dataframe

g1=ggplot(data=df, aes(x=English, y=Math, color=Cluster))+geom_point()+theme(legend.position =
&quot;right&quot;)+geom_point(data=Centers, aes(x=English, y=Math, color=as.factor(c(1,2,3))), size=10,
alpha=.3, show.legend =FALSE)

g2=ggplot(data=df, aes(x=English, y=Science,
color=Cluster))+geom_point()+theme(legend.position = &quot;right&quot;)+geom_point(data=Centers,
aes(x=English, y=Science, color=as.factor(c(1,2,3))), size=10, alpha=.3, show.legend =FALSE)

g3=ggplot(data=df, aes(x=Math, y=Science, color=Cluster))+geom_point()+theme(legend.position
= &quot;right&quot;)+geom_point(data=Centers, aes(x=Math, y= Science, color=as.factor(c(1,2,3))), size=10,
alpha=.3, show.legend =FALSE)

tmp=ggplot_gtable(ggplot_build(g1))
tmp
#grid.arrange(arrangeGrob(g1+theme(legend.position =&quot;none&quot;), g2+theme(legend.position
=&quot;none&quot;), g3+theme(legend.position =&quot;none&quot;),main=&quot;High School Students Cluster
Analysys&quot;,ncol=1))

grid.arrange(g1, g2, g3,top=&quot;High School Students Cluster Analysys&quot;,ncol=3)

g1
g2
g3