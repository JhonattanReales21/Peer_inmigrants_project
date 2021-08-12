library(tidyverse)
library(lpSolve)
library(networkD3)

#setwd("C:/Users/Uninorte/Dropbox/ASPECTOS MAESTRIA/ENTREGABLE 5/Acercamiento con modelo unico (Departamento)")

mat_familias <- read.csv2("Datos_familia_asignación_DEP.csv", fileEncoding = "Latin1") %>% select(-1)
matrix_salud <- read.csv2("Matriz_penalizaciones_salud.csv") %>% select(-1) # PENALIZACIONES DE SALUD/DISTANCIA

#setwd("C:/Users/Uninorte/Dropbox/ASPECTOS MAESTRIA/ENTREGABLE 5")

dep_familias <- read.csv("dep_familias.csv", header = T, stringsAsFactors = T, fileEncoding = "Latin1") %>% select(-1)
data_Dane <- read.csv("Dane_TOTAL_etapa_mapeo.csv", header = T, stringsAsFactors = T, fileEncoding = "Latin1") %>% select(-1)

#setwd("C:/Users/Uninorte/Dropbox/ASPECTOS MAESTRIA/ENTREGABLE 5/Acercamiento con modelo unico (Departamento)")

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
apply(matriz_flujo, 2, sum)

asignacion <- as.data.frame(modelo$solution)
asignacion$Departamento <- dep_familias$Departamento
asignacion$Familia <- dep_familias$Familia
asignacion <- select(asignacion, Familia,Departamento, everything())

for (i in 1:ncol(matriz_flujo)){
      region_i <- asignacion %>% filter(Departamento==Departamentos[i]) %>% select(c(-1,-2))
      suma <- apply(region_i, 2, sum)
      matriz_flujo[i, ] <- suma
}

#setwd("C:/Users/Uninorte/Dropbox/ASPECTOS MAESTRIA/ENTREGABLE 6")
colnames(asignacion) <- c("Familia", "Departamento", Departamentos)

write.csv(asignacion, "Matriz_asignación_V4_VF.csv")

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
quantile(matriz_mejora$Prob_actual, 0.25)
quantile(matriz_mejora$Prob_actual, 0.5)
quantile(matriz_mejora$Prob_actual,0.7)
quantile(matriz_mejora$Prob_actual,0.8)
quantile(matriz_mejora$Prob_actual,0.9)

# Prob Destino
mean(matriz_mejora$Prob_nueva)
quantile(matriz_mejora$Prob_nueva, 0.25)
quantile(matriz_mejora$Prob_nueva, 0.5)
quantile(matriz_mejora$Prob_nueva,0.7)
quantile(matriz_mejora$Prob_nueva,0.8)
quantile(matriz_mejora$Prob_nueva,0.9)


#Mejora Absoluta
mean(matriz_mejora$Diferencia_absoluta)
quantile(matriz_mejora$Diferencia_absoluta, 0.25)
quantile(matriz_mejora$Diferencia_absoluta, 0.5)
quantile(matriz_mejora$Diferencia_absoluta,0.7)
quantile(matriz_mejora$Diferencia_absoluta,0.8)
quantile(matriz_mejora$Diferencia_absoluta,0.9)

#Mejora Porcentual
mean(matriz_mejora$diferencia_porcentual)
quantile(matriz_mejora$diferencia_porcentual, 0.25)
quantile(matriz_mejora$diferencia_porcentual, 0.5)
quantile(matriz_mejora$diferencia_porcentual,0.7)
quantile(matriz_mejora$diferencia_porcentual,0.8)
quantile(matriz_mejora$diferencia_porcentual,0.9)


#write.csv2(matriz_mejora, "Matriz_p9.csv")

pdf("histogramas_probabilidades.pdf")
# Histograma prob actual
ggplot(matriz_mejora, aes(x=Prob_actual)) + geom_histogram(color="dodgerblue3", fill="dodgerblue", alpha=0.7) +
   geom_vline(aes(xintercept = mean(Prob_actual)),col='red', size=2) + 
   geom_vline(aes(xintercept =quantile(Prob_actual, 0.25)),col='slategray', size=1.5) + 
   geom_vline(aes(xintercept =quantile(Prob_actual, 0.5)),col='limegreen', size=2) + 
   geom_vline(aes(xintercept =quantile(Prob_actual, 0.70)),col='slategray', size=1.5) + 
   geom_vline(aes(xintercept =quantile(Prob_actual, 0.80)),col='slategray', size=1.5) + 
   geom_vline(aes(xintercept =quantile(Prob_actual, 0.90)),col='slategray', size=1.5) + 
   theme_bw() + 
   labs(x="Current probability", y="Frequency") + 
   ylim(0,4000) + 
   xlim(-0.05,1.05)

# Histograma prob destino
ggplot(matriz_mejora, aes(x=Prob_nueva)) + geom_histogram(color="dodgerblue3", fill="dodgerblue", alpha=0.7) +
   geom_vline(aes(xintercept = mean(Prob_nueva)),col='red', size=2) + 
   geom_vline(aes(xintercept =quantile(Prob_nueva, 0.25)),col='slategray', size=1.5) + 
   geom_vline(aes(xintercept =quantile(Prob_nueva, 0.5)),col='limegreen', size=2) + 
   geom_vline(aes(xintercept =quantile(Prob_nueva, 0.70)),col='slategray', size=1.5) + 
   geom_vline(aes(xintercept =quantile(Prob_nueva, 0.80)),col='slategray', size=1.5) + 
   geom_vline(aes(xintercept =quantile(Prob_nueva, 0.90)),col='slategray', size=1.5) + 
   theme_bw() + 
   labs(x="Probability in assigned location", y="Frequency") + 
   ylim(0,4000)+ 
   xlim(-0.05,1.05)

# Histograma Mejora prob absoluta
ggplot(matriz_mejora, aes(x=Diferencia_absoluta)) + geom_histogram(color="dodgerblue3", fill="dodgerblue", alpha=0.7) +
   geom_vline(aes(xintercept = mean(Diferencia_absoluta)),col='red', size=2) + 
   geom_vline(aes(xintercept =quantile(Diferencia_absoluta, 0.25)),col='slategray', size=1.5) + 
   geom_vline(aes(xintercept =quantile(Diferencia_absoluta, 0.5)),col='limegreen', size=2) + 
   geom_vline(aes(xintercept =quantile(Diferencia_absoluta, 0.70)),col='slategray', size=1.5) + 
   geom_vline(aes(xintercept =quantile(Diferencia_absoluta, 0.80)),col='slategray', size=1.5) + 
   geom_vline(aes(xintercept =quantile(Diferencia_absoluta, 0.90)),col='slategray', size=1.5) + 
   theme_bw() + 
   labs(x="Absolute difference probability", y="Frequency") + 
   ylim(0,4000)+ 
   xlim(-0.05,1.05)

# Histograma Mejora prob porcentual
ggplot(matriz_mejora, aes(x=diferencia_porcentual)) + geom_histogram(color="dodgerblue3", fill="dodgerblue", alpha=0.7) +
   geom_vline(aes(xintercept = mean(diferencia_porcentual)),col='red', size=2) + 
   geom_vline(aes(xintercept =quantile(diferencia_porcentual, 0.25)),col='slategray', size=1.5) + 
   geom_vline(aes(xintercept =quantile(diferencia_porcentual, 0.5)),col='limegreen', size=2) + 
   geom_vline(aes(xintercept =quantile(diferencia_porcentual, 0.70)),col='slategray', size=1.5) + 
   geom_vline(aes(xintercept =quantile(diferencia_porcentual, 0.80)),col='slategray', size=1.5) + 
   geom_vline(aes(xintercept =quantile(diferencia_porcentual, 0.90)),col='slategray', size=1.5) + 
   theme_bw() + 
   labs(x="Porcentual difference probability", y="Frequency") + 
   ylim(0,4000)


estaticos <- filter(matriz_mejora, Diferencia_absoluta == 0) # 3705 familias


dev.off()
#### ANÁLISIS VULNERABLES

vulnerables <- filter(matriz_mejora, Prob_nueva <= 0.15) # 970 hogares vulnerables
vulnerables_estaticos <- filter(vulnerables, diferencia_porcentual == 0) # 645 hogares se quedan
vulnerables_dinamicos <- filter(vulnerables, diferencia_porcentual > 0) # 467 hogares se mueven
table(vulnerables_dinamicos$Dep_asignado)
table(vulnerables$Dep_actual)
mean(vulnerables_dinamicos$diferencia_porcentual)
mean(vulnerables_dinamicos$Diferencia_absoluta)

ss <- vulnerables_dinamicos %>% group_by(Dep_asignado) %>% summarise("freq"=n()) %>% arrange(desc(freq))


### ANÁLISIS DE MEJORA POR INDIVIDUOS
library(stringr)

Dane_paramodelos <- read.csv("Dane_paramodelos.csv") %>% select(-c(1, 13,14,16:18))
Dane_paramodelos <- rename(Dane_paramodelos, "Familia" = "Identificacion")
Dane_paramodelos$Familia <- str_sub(Dane_paramodelos$Familia,1,9)
Dane_paramodelos$Familia <- gsub(" ", "", Dane_paramodelos$Familia, fixed = TRUE)

modelado_ranger <- read.csv("OneModel_RANGER.Y2_Modeling.csv", sep=";")
modelado_ranger <- rename(modelado_ranger, "Familia" = "Families")
modelado_ranger$Familia <- gsub(" ", "", modelado_ranger$Familia, fixed = TRUE)

modelado_por_individuos <- left_join(Dane_paramodelos, modelado_ranger, by = "Familia")


vector_bool <- c()
for (i in 1:nrow(Dane_paramodelos)){
   if(Dane_paramodelos[i, "Familia"] == modelado_ranger[i, "Familia"]){
      vector_bool <- c(vector_bool, TRUE)
   }else{
      vector_bool <- c(vector_bool, FALSE)
   }
}

matriz_mejora$Familia <- gsub(" ", "", matriz_mejora$Familia)
matriz_union <- read.csv("union_Daneparamodelos_modelado.csv", sep=";") %>% select(-c(1, 3, 14,15, 17,18,19))
matriz_union <- rename(matriz_union, "Familia" = "Families")
modelado_por_individuos <- left_join(matriz_union, matriz_mejora, by="Familia")
write.csv2(modelado_por_individuos, "probabilidades_modelado_por_individuos.csv")

data_Dane$Familia <- gsub(" ", "", data_Dane$Familia)
mejora_por_individuos <- left_join(data_Dane, matriz_mejora, by = "Familia")
write.csv2(mejora_por_individuos, "matriz_mejora_probabilidades_matching_individuos.csv")


### ANÁLISIS ADULTOS MAYORES

adultos_mayores <-  filter(mejora_por_individuos, Edad == "(59,100]")
sum(adultos_mayores$Diferencia_absoluta >0)/nrow(adultos_mayores) # numero de adultos mayores reubicados
adultos_mayores %>% filter(Diferencia_absoluta>0) %>% summarise("mean_prob"=mean(Diferencia_absoluta))
adultos_mayores %>% filter(Diferencia_absoluta>0) %>% summarise("mean_prob"=mean(diferencia_porcentual))
deptos_adultos_mayores <- as.data.frame(table(adultos_mayores$Dep_asignado))

### ANÁLISIS MUJERES
mujeres <-  filter(mejora_por_individuos, Genero == "Femenino")
mean(mujeres$Prob_nueva)
sum(mujeres$Diferencia_absoluta >0)/nrow(mujeres)
mujeres %>% filter(Diferencia_absoluta>0) %>% summarise("mean_prob"=mean(Diferencia_absoluta))
mujeres %>% filter(Diferencia_absoluta>0) %>% summarise("mean_prob"=mean(diferencia_porcentual))
deptos_mujeres <- mujeres %>% group_by(Departamento) %>% summarise("freq"=n(), "mean_probs"=mean(Prob_nueva))
deptos_mujeres <- as.data.frame(table(mujeres$Dep_asignado))
deptos_actuales_mujeres <- as.data.frame(table(mujeres$Dep_actual))





mat_familias_pequeña <- select(mat_familias, Familia, n_vulnerables, n_niños, Num_miembros)
mat_familias_pequeña$Familia <- gsub(" ", "", mat_familias_pequeña$Familia)
matriz_mejora$Familia <- gsub(" ", "", matriz_mejora$Familia)
mat_mejora_familias_vulnerables <- left_join(matriz_mejora, mat_familias_pequeña, by="Familia")
write.csv2(mat_mejora_familias_vulnerables, "mejora_asignacion_familias.csv")
