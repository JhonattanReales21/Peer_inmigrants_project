library(tidyverse)
library(lpSolve)
library(networkD3)

setwd("C:/Users/Uninorte/Dropbox/ASPECTOS MAESTRIA/ENTREGABLE 5/Acercamiento con modelo unico (Departamento)")

mat_familias <- read.csv2("Datos_familia_asignación_DEP.csv") %>% select(-1)
matrix_salud <- read.csv2("Matriz_penalizaciones_salud.csv") %>% select(-1) # PENALIZACIONES DE SALUD/DISTANCIA

setwd("C:/Users/Uninorte/Dropbox/ASPECTOS MAESTRIA/ENTREGABLE 5")

dep_familias <- read.csv("dep_familias.csv", header = T, stringsAsFactors = T, fileEncoding = "Latin1") %>% select(-1)
data_Dane <- read.csv("Dane_etapa_mapeo.csv", header = T, stringsAsFactors = T, fileEncoding = "Latin1") %>% select(-1)

setwd("C:/Users/Uninorte/Dropbox/ASPECTOS MAESTRIA/ENTREGABLE 5/Acercamiento con modelo unico (Departamento)")

Departamentos <- c("Antioquia", "Atlantico", "Bogota", "Bolivar", "Boyaca", "Caldas",
                   "Caqueta", "Cauca", "Cesar",  "Choco", "Cordoba", "Cundinamarca",
                   "Huila", "La.Guajira", "Magdalena", "Meta", "Narino", "Norte.de.Santander",
                   "Quindio", "Risaralda", "Santander", "Sucre", "Tolima", "Valle")

dep_familias$Departamento <- as_factor(dep_familias$Departamento)
dep_familias$Departamento <- fct_collapse(dep_familias$Departamento, "Narino"="Nariño","Norte.de.Santander"="Norte de Santander",
                                          "La.Guajira"="La Guajira")

mat_familias$Departamento <- dep_familias$Departamento

colnames(matrix_salud) <- Departamentos
rownames(matrix_salud) <- Departamentos


# Construcción matriz de costos
probs_familias <- select(mat_familias, 6:29)
probs_familias <- (1- probs_familias) %>% cbind(Familia=mat_familias$Familia) %>% select(Familia, everything())
probs_familias <- probs_familias %>% select(c(1,"Antioquia", "Atlantico", "Bogota", "Bolivar", "Boyaca", "Caldas",
                                              "Caqueta", "Cauca", "Cesar",  "Choco", "Cordoba", "Cundinamarca",
                                              "Huila", "La.Guajira", "Magdalena", "Meta", "Narino", "Norte.de.Santander",
                                              "Quindio", "Risaralda", "Santander", "Sucre", "Tolima", "Valle"))
#Se hizo para reordenar las columnas y que coincidan con los siguientes pasos



#### Análisis de mejora en probabilidades ###########
probs_analisis <- probs_familias
probs_analisis <- data.frame(probs_analisis, "Departamento Origen"=as.character(dep_familias$Departamento))
colnames(probs_analisis) <- c("Familia", Departamentos, "Departamento Origen")
probs_analisis <- select(probs_analisis, Familia, `Departamento Origen`, everything())

for(i in 1:nrow(probs_analisis)){
      for( j in 3:ncol(probs_analisis)){
            region_actual <- as.character(probs_analisis[i, 2])
            prob_actual <- probs_analisis[i ,region_actual ]
            region_destino <- colnames(probs_analisis)[j]
            prob_destino <- probs_analisis[i, j]
            diferencia <- prob_actual - prob_destino
            
            if (diferencia < 0.08 && diferencia>=0 && region_actual != region_destino){
                  probs_analisis[i, j] <- 1
            }
      }
}

##### Analisis de estar en mejor departamento #####

conteo_dep=as_tibble(matrix(0,1,24))
colnames(conteo_dep) <- Departamentos

familias_eliminar = c()  # Vector de familias que se encuentran en su mejor ubicación

for (i in 1:dim(probs_analisis)[1]){ #Para todos los casos
      Mejor <-  TRUE   # Switch de que es la mejor probabilidad
      dep_origen <- probs_analisis[i,2]
      for (j in Departamentos) {  # Le revisamos la probabilidad en cada departamento
            if (dep_origen != j){
                  if (probs_analisis[i,dep_origen]<probs_analisis[i,j] & probs_analisis[i,j] != 1){ #Si la prob es menor que en la ub. actual
                        probs_analisis[i,j] <- 2  # Penalización mas fuerte para que no considere esta locación
                  } else {
                        Mejor <- FALSE # Ya no se cuenta como mejor probabilidad
                  }
            }
      }
      if (Mejor == TRUE){
            conteo_dep[1,dep_origen] <-  conteo_dep[1,dep_origen] + 1
            familias_eliminar <- c(familias_eliminar, probs_analisis[i,1])
      }
}


objetivos <- as.matrix(probs_analisis[,3:26]) # Matriz de costos inicial






#objetivos <- as.matrix(probs_familias[,])

####### Construcción de matrices de penalizaciones ######

penalizaciones_vulnerables=matrix(0,11044,24)
penalizaciones_educacion=matrix(0,11044,24)
colnames(penalizaciones_educacion) <- Departamentos
colnames(penalizaciones_vulnerables) <- Departamentos

# PENALIZACIÓN DE CASOS CON NIÑOS 
for (i in 1:dim(objetivos)[1]) {
      if (mat_familias$n_niños[i] >= 1) {
         penalizaciones_educacion[i,"Choco"]=penalizaciones_educacion[i,"Choco"]+1
         penalizaciones_educacion[i,"Narino"]=penalizaciones_educacion[i,"Narino"]+1  
         penalizaciones_educacion[i,"Caqueta"]=penalizaciones_educacion[i,"Caqueta"]+1  
         penalizaciones_educacion[i,"La.Guajira"]=penalizaciones_educacion[i,"La.Guajira"]+1  
         penalizaciones_educacion[i,"Sucre"]=penalizaciones_educacion[i,"Sucre"]+1
         penalizaciones_educacion[i,"Cordoba"]=penalizaciones_educacion[i,"Cordoba"]+1
         penalizaciones_educacion[i,"Cauca"]=penalizaciones_educacion[i,"Cauca"]+1  
            
      }
}

# PENALIZACIÓN DE CASOS CON PERSONAS VULNERABLES

for (i in 1:dim(objetivos)[1]) {
      if (mat_familias$n_vulnerables[i] > 0) {
            ubi <- mat_familias$Departamento[i]
            for (col in colnames(objetivos)){
                  penalizaciones_vulnerables[i,col]=penalizaciones_vulnerables[i,col]+matrix_salud[ubi,col]
            }
      }
}


########## Continuación del modelo ########

#!!!!!!! Coeficientes matriz objetivo (Probabilidades y pensalizaciones) !!!!!!
alpha=0.7
beta=0.15
gamma=0.15
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#   MATRIZ OBJETIVO
objetivos <-  alpha*objetivos + beta*penalizaciones_vulnerables + gamma*penalizaciones_educacion
sum(is.na(objetivos))
sum(is.na(penalizaciones_vulnerables))
sum(is.na(penalizaciones_educacion))


# Metodologia para restricción de casos en regiones
Num_venezolanos_total=dim(data_Dane)[1]
Num_casos_total=dim(mat_familias)[1]

Promedio_miembros_por_caso=Num_venezolanos_total/Num_casos_total

Porcentaje_ven_nacional=0.0342
Muestra_Col=Num_venezolanos_total/Porcentaje_ven_nacional
Porcentaje_ven_por_dep=c(0.1334,0.0516, 0.16325, 0.04325, 0.02515, 0.0195, 0.0099, 0.02804, 0.02335,
                         0.01024, 0.03579, 0.05619, 0.02387,0.02121, 0.02585, 0.02049,0.03607,
                         0.02932, 0.01135, 0.01910, 0.04255, 0.01855, 0.02789, 0.0946)


Pob_aprox_por_dep=Porcentaje_ven_por_dep*Muestra_Col
cluster_cada_dep=c(1,2,1,3,2,2,3,3,3,4,3,2,3,4,3,2,3,3,2,2,2,3,2,1)
Aprox_num_venezolanos_por_dep=c()


#!!!!!!!!! Variables porcentaje de venezolanos según cluster de regiones !!!!!!!!!! 
cluster1=0.06
cluster2=0.04
cluster3=0.025
cluster4=0.01
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

i=1
for (var in cluster_cada_dep){
      if (var == 1){
            Aprox_num_venezolanos_por_dep[i]=Pob_aprox_por_dep[i]*cluster1
      } else if (var==2){
            Aprox_num_venezolanos_por_dep[i]=Pob_aprox_por_dep[i]*cluster2
      }else if (var==3){
            Aprox_num_venezolanos_por_dep[i]=Pob_aprox_por_dep[i]*cluster3
      }else{
            Aprox_num_venezolanos_por_dep[i]=Pob_aprox_por_dep[i]*cluster4
      } 
      i=i+1
}

# Calculo de Rj definitivo
Aprox_num_CASOS_por_dep=round(Aprox_num_venezolanos_por_dep/Promedio_miembros_por_caso,0)


rest.filas <- rep(1, nrow(objetivos))
rest.columnas <- Aprox_num_CASOS_por_dep
signos.fila <- rep("=", nrow(objetivos))
signos.columna <- rep("<=", 24)

# Construimos y evaluamos modelo
modelo <- lp.transport(objetivos, direction = "min", row.signs = signos.fila, row.rhs = rest.filas, 
                       col.signs = signos.columna, col.rhs = rest.columnas)

apply(modelo$solution, 2, sum)

## COMPARACION

matriz_flujo <- matrix(0,24,24) %>% as.data.frame()
colnames(matriz_flujo) <- names(as.data.frame(objetivos))
rownames(matriz_flujo) <- names(as.data.frame(objetivos))

asignacion <- as.data.frame(modelo$solution)
asignacion$Departamento <- dep_familias$Departamento
asignacion$Familia <- dep_familias$Familia
asignacion <- select(asignacion, Familia,Departamento, everything())

for (i in 1:ncol(matriz_flujo)){
      region_i <- asignacion %>% filter(Departamento==Departamentos[i]) %>% select(c(-1,-2))
      suma <- apply(region_i, 2, sum)
      matriz_flujo[i, ] <- suma
}

setwd("C:/Users/Uninorte/Dropbox/ASPECTOS MAESTRIA/ENTREGABLE 6")

write.csv(matriz_flujo, "Matriz_flujo_departamentos_vol4.csv")

### Analisis de mejora de probabilidad de empleo

colnames(asignacion) <- c("Familia", "Departamento",colnames(matriz_flujo))
matriz_mejora <- as_tibble(matrix(0,dim(mat_familias)[1],7))
colnames(matriz_mejora) <- c("Familia","Dep_actual","Dep_asignado","Prob_actual","Prob_nueva","Diferencia_absoluta", "diferencia_porcentual")
matriz_mejora$Familia <- mat_familias$Familia
matriz_mejora$Dep_actual <- mat_familias$Departamento
matriz_mejora$Dep_asignado <- "N"

for (i in 1:dim(mat_familias)[1]){
      dep_actual <- as.character(mat_familias[i,"Departamento"])
      prob_actual <- mat_familias[i,dep_actual]
      matriz_mejora[i,"Prob_actual"] <- prob_actual
      for (j in Departamentos){
            if (asignacion[i,j] ==1 ){
                  matriz_mejora[i,"Dep_asignado"] <- j
                  matriz_mejora[i,"Prob_nueva"] <- mat_familias[i,j]
            }
      }
}

matriz_mejora$Diferencia_absoluta <-matriz_mejora$Prob_nueva- matriz_mejora$Prob_actual
matriz_mejora$diferencia_porcentual <-100*((matriz_mejora$Prob_nueva-matriz_mejora$Prob_actual)/matriz_mejora$Prob_actual)

# Prob Actual
mean(matriz_mejora$Prob_actual)
quantile(matriz_mejora$Prob_actual)

quantile(matriz_mejora$Prob_actual,0.7)
quantile(matriz_mejora$Prob_actual,0.8)
quantile(matriz_mejora$Prob_actual,0.9)

# Prob Destino
mean(matriz_mejora$Prob_nueva)
quantile(matriz_mejora$Prob_nueva)

quantile(matriz_mejora$Prob_nueva,0.7)
quantile(matriz_mejora$Prob_nueva,0.8)
quantile(matriz_mejora$Prob_nueva,0.9)

#Mejora Absoluta
mean(matriz_mejora$Diferencia_absoluta)
quantile(matriz_mejora$Diferencia_absoluta)

quantile(matriz_mejora$Diferencia_absoluta,0.7)
quantile(matriz_mejora$Diferencia_absoluta,0.8)
quantile(matriz_mejora$Diferencia_absoluta,0.9)

#Mejora Porcentual
mean(matriz_mejora$diferencia_porcentual)
quantile(matriz_mejora$diferencia_porcentual)

quantile(matriz_mejora$diferencia_porcentual,0.7)
quantile(matriz_mejora$diferencia_porcentual,0.8)
quantile(matriz_mejora$diferencia_porcentual,0.9)


#write.csv2(matriz_mejora, "Matriz_p9.csv")

# Histograma prob actual
histograma <- matriz_mejora %>% ggplot(aes(x= Prob_actual )) + geom_histogram(bins = 20, fill="deepskyblue1", color="black" )+
      ggplot2::theme_bw()+xlab("Probabilidad actual") + ylab("Frecuencia") %>% 
      geom_vline(xintercept=mean(matriz_mejora$Prob_actual), color="red", lwd=1.3) 
histograma <- histograma+geom_vline(xintercept=quantile(matriz_mejora$Prob_actual, 0.5), color="green", lwd=1.3) 
histograma <- histograma+geom_vline(xintercept=quantile(matriz_mejora$Prob_actual, 0.8), color="gray", lwd=1.3)
histograma+geom_vline(xintercept=quantile(matriz_mejora$Prob_actual, 0.25), color="gray", lwd=1.3)

# Histograma prob destino
histograma <- matriz_mejora %>% ggplot(aes(x= Prob_nueva )) + geom_histogram(bins = 20, fill="deepskyblue1", color="black" )+
      ggplot2::theme_bw()+xlab("Probabilidad en ubicación de destino") + ylab("Frecuencia") %>% 
      geom_vline(xintercept=mean(matriz_mejora$Prob_nueva), color="red", lwd=1.3) 
histograma <- histograma+geom_vline(xintercept=quantile(matriz_mejora$Prob_nueva, 0.5), color="green", lwd=1.3) 
histograma <- histograma+geom_vline(xintercept=quantile(matriz_mejora$Prob_nueva, 0.8), color="gray", lwd=1.3)
histograma+geom_vline(xintercept=quantile(matriz_mejora$Prob_nueva, 0.25), color="gray", lwd=1.3)

# Histograma Mejora prob absoluta
histograma <- matriz_mejora %>% ggplot(aes(x= Diferencia_absoluta )) + geom_histogram(bins = 25, fill="deepskyblue1", color="black" )+
      ggplot2::theme_bw()+xlab("Mejora en probabilidad absoluta") + ylab("Frecuencia") %>% 
      geom_vline(xintercept=mean(matriz_mejora$Diferencia_absoluta), color="red", lwd=1.3) 
histograma <- histograma+geom_vline(xintercept=quantile(matriz_mejora$Diferencia_absoluta, 0.5), color="green", lwd=1.3) 
histograma <- histograma+geom_vline(xintercept=quantile(matriz_mejora$Diferencia_absoluta, 0.8), color="gray", lwd=1.3)
histograma+geom_vline(xintercept=quantile(matriz_mejora$Diferencia_absoluta, 0.25), color="gray", lwd=1.3)

# Histograma Mejora prob porcentual
histograma <- matriz_mejora %>% ggplot(aes(x= diferencia_porcentual )) + geom_histogram(bins = 25, fill="coral3", color="black" )+
      ggplot2::theme_bw()+xlab("Mejora en probabilidad porcentual") + ylab("Frecuencia") %>% 
      geom_vline(xintercept=mean(matriz_mejora$diferencia_porcentual), color="red", lwd=1.3) 
histograma <- histograma+geom_vline(xintercept=quantile(matriz_mejora$diferencia_porcentual, 0.5), color="green", lwd=1.3) 
histograma <- histograma+geom_vline(xintercept=quantile(matriz_mejora$diferencia_porcentual, 0.8), color="gray", lwd=1.3)
histograma+geom_vline(xintercept=quantile(matriz_mejora$diferencia_porcentual, 0.25), color="gray", lwd=1.3)



#