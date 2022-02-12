##instalacion y llamado de librerias
##install.packages("ggplot2")
##install.packages("factoextra")
##install.packages("dplyr")
##install.packages("FactoMineR")
##install.packages("corrplot")
##install.packages("corrr")
##install.packages("ggcorrplot")
library(ggplot2)
library(readxl)
library(factoextra)
library(ggplot2)
library(FactoMineR)
library(dplyr)
library(corrplot)
library(ggcorrplot)
library(corrr)

##Cargar archivo caso 4
Metricas <- read_excel("Metricas.xlsx", sheet = "caso 4")

## PREPARACION DE DATOS
caso4 <- Metricas
caso4[caso4 == " " ] <- NA
caso4[caso4== "NA" ] <- NA
caso4[caso4== "N.A" ] <- NA
caso4 <- caso4[stats::complete.cases(caso4),]
View(caso4)
Metricas <- caso4
caso4$Departamento <-NULL 

## correlacion
##correlacion <- cor(caso4)
##ggcorrplot(correlacion, method = "circle", lab = FALSE)

##pairs.panels(caso4,ellipses = FALSE, smooth = FALSE)

## etiqueta de departamentos
rownames(caso4) <- Metricas$Departamento

## distancia entre departamentos
caso4 <- scale(caso4)
caso4 <- as.data.frame(caso4)
res.dist <- get_dist(caso4, method = "euclidean")
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

## optimo de k
## metodo wss
fviz_nbclust(x = caso4, FUNcluster = kmeans, method = "wss", k.max = 8, 
             diss = get_dist(caso4, method = "euclidean"), nstart = 50)
#Metodo  gap_stat
fviz_nbclust(x =datos1, FUNcluster = kmeans, method = "gap_stat", k.max = 15, 
             diss = get_dist(datos1, method = "euclidean"), nstart = 50)
#Metodo  silhouette
fviz_nbclust(x =caso4, FUNcluster = kmeans, method = "silhouette", k.max = 8, 
             diss = get_dist(caso4, method = "euclidean"), nstart = 50)
#Modelo con 2 clusters 
set.seed(123)
km_clusters <- kmeans(x = caso4, centers = 2, nstart = 50)
fviz_cluster(object = km_clusters, data = caso4, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")
#modelo con 3 clusters
set.seed(123)
km_clusters2 <- kmeans(x = caso4, centers = 3, nstart = 50)
fviz_cluster(object = km_clusters2, data = caso4, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")
# modelo cluster con 4
set.seed(123)
km_clusters3 <- kmeans(x = caso4, centers = 4, nstart = 50)
fviz_cluster(object = km_clusters3, data = caso4, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")

## metricas 
#coordenadas del centro de los clusters
km_clusters2$centers
View(km_clusters3$centers)
# tama?o cluster
km_clusters2$size
km_clusters3$size
km_clusters3$centers
