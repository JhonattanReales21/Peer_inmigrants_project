library(tidyr)
library(readxl)
library(readr)
library(dplyr)
library(funModeling)


#### PARA EFECTUAR LA LECTURA DE LOS DATOS DE CADA MES, SE HACE NECESARIO CAMBIAR LA 
#### CARPETA DE CADA MES (DIRECTORIO) DADO QUE TIENEN LOS MISMOS NOMBRES SOLO QUE EN
#### CARPETAS DIFERENTES.




##### Unión csv - Enero ######

setwd("~/ASPECTOS MAESTRIA/DATOS GEIH 2019 -- Enero - diciembre/Enero.csv")

Carac.enero <- read.csv("Cabecera - Caracteristicas generales (Personas).csv", header = T, sep = ";", dec = ",")
desoc.enero <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
F_trabajo.enero <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.enero <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Ocupa.enero <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Otros_ing.enero <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Otras_act.enero <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Vivienda.enero <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

Enero <- Carac.enero %>% left_join(F_trabajo.enero, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Ocupa.enero, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(desoc.enero, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.enero, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otras_act.enero, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otros_ing.enero, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))


##### Unión csv - Febrero ######

setwd("~/ASPECTOS MAESTRIA/DATOS GEIH 2019 -- Enero - diciembre/Febrero.csv")


Carac.febrero <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
desoc.febrero <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
F_trabajo.febrero  <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.febrero  <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Ocupa.febrero  <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Otros_ing.febrero  <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Otras_act.febrero  <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Vivienda.febrero  <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

Febrero <- Carac.febrero %>% left_join(F_trabajo.febrero, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Ocupa.febrero, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(desoc.febrero, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.febrero, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otras_act.febrero, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otros_ing.febrero, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))

##### Unión csv - Marzo ######

setwd("~/ASPECTOS MAESTRIA/DATOS GEIH 2019 -- Enero - diciembre/Marzo.csv")

Carac.Marzo <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
desoc.Marzo <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
F_trabajo.Marzo <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.Marzo <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Ocupa.Marzo <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Otros_ing.Marzo <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Otras_act.Marzo <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Vivienda.Marzo <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

Marzo <- Carac.Marzo %>% left_join(F_trabajo.Marzo, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Ocupa.Marzo, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(desoc.Marzo, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.Marzo, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otras_act.Marzo, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otros_ing.Marzo, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))


##### Unión csv - Abril ######

setwd("~/ASPECTOS MAESTRIA/DATOS GEIH 2019 -- Enero - diciembre/Abril.csv")

Carac.Abril <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
desoc.Abril <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
F_trabajo.Abril <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.Abril<- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Ocupa.Abril<- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Otros_ing.Abril<- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Otras_act.Abril<- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Vivienda.Abril<- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

Abril <- Carac.Abril %>% left_join(F_trabajo.Abril, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Ocupa.Abril, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(desoc.Abril, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.Abril, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otras_act.Abril, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otros_ing.Abril, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))


##### Unión csv - Mayo ######

setwd("~/ASPECTOS MAESTRIA/DATOS GEIH 2019 -- Enero - diciembre/Mayo.csv")

Carac.Mayo <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
desoc.Mayo <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
F_trabajo.Mayo <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.Mayo <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Ocupa.Mayo <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Otros_ing.Mayo <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Otras_act.Mayo <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Vivienda.Mayo <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

Mayo <- Carac.Mayo %>% left_join(F_trabajo.Mayo, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Ocupa.Mayo, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(desoc.Mayo, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.Mayo, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otras_act.Mayo, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otros_ing.Mayo, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))


##### Unión csv - Junio ######

setwd("~/ASPECTOS MAESTRIA/DATOS GEIH 2019 -- Enero - diciembre/Junio.csv")


Carac.Junio <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
desoc.Junio<- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
F_trabajo.Junio <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.Junio <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Ocupa.Junio <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Otros_ing.Junio<- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Otras_act.Junio <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Vivienda.Junio <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

Junio <- Carac.Junio %>% left_join(F_trabajo.Junio, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Ocupa.Junio, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(desoc.Junio, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.Junio, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otras_act.Junio, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otros_ing.Junio, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))


##### Unión csv - Julio ######

setwd("~/ASPECTOS MAESTRIA/DATOS GEIH 2019 -- Enero - diciembre/Julio.csv")


Carac.Julio <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
desoc.Julio <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
F_trabajo.Julio <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.Julio <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Ocupa.Julio <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Otros_ing.Julio <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Otras_act.Julio <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Vivienda.Julio <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

Julio <- Carac.Julio %>% left_join(F_trabajo.Julio, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Ocupa.Julio, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(desoc.Julio, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.Julio, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otras_act.Julio, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otros_ing.Julio, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))


##### Unión csv - Agosto ######

setwd("~/ASPECTOS MAESTRIA/DATOS GEIH 2019 -- Enero - diciembre/Agosto.csv")

Carac.Agosto <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
desoc.Agosto <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
F_trabajo.Agosto <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.Agosto <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Ocupa.Agosto <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Otros_ing.Agosto <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Otras_act.Agosto <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Vivienda.Agosto<- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

Agosto <- Carac.Agosto %>% left_join(F_trabajo.Agosto, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Ocupa.Agosto, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(desoc.Agosto, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.Agosto, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otras_act.Agosto, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otros_ing.Agosto, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))

##### Unión csv - Septiembre ######

setwd("~/ASPECTOS MAESTRIA/DATOS GEIH 2019 -- Enero - diciembre/Septiembre.csv")

Carac.Septiembre <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
desoc.Septiembre <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
F_trabajo.Septiembre <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.Septiembre <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Ocupa.Septiembre <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Otros_ing.Septiembre <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Otras_act.Septiembre <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Vivienda.Septiembre<- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

Septiembre <-Carac.Septiembre %>% left_join(F_trabajo.Septiembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Ocupa.Septiembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(desoc.Septiembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.Septiembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otras_act.Septiembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otros_ing.Septiembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))



##### Unión csv - Octubre ######

setwd("~/ASPECTOS MAESTRIA/DATOS GEIH 2019 -- Enero - diciembre/Octubre.csv")

Carac.Octubre <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
desoc.Octubre <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
F_trabajo.Octubre <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.Octubre <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Ocupa.Octubre <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Otros_ing.Octubre <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Otras_act.Octubre <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
ViviendaOctubre <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

Octubre<- Carac.Octubre %>% left_join(F_trabajo.Octubre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Ocupa.Octubre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(desoc.Octubre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.Octubre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otras_act.Octubre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otros_ing.Octubre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))



##### Unión csv - Noviembre ######

setwd("~/ASPECTOS MAESTRIA/DATOS GEIH 2019 -- Enero - diciembre/Noviembre.csv")

Carac.Noviembre <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
desoc.Noviembre <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
F_trabajo.Noviembre<- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.Noviembre <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Ocupa.Noviembre <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Otros_ing.Noviembre <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Otras_act.Noviembre <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Vivienda.Noviembre <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

Noviembre <- Carac.Noviembre %>% left_join(F_trabajo.Noviembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Ocupa.Noviembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(desoc.Noviembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.Noviembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otras_act.Noviembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otros_ing.Noviembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))



##### Unión csv - Diciembre ######

setwd("~/ASPECTOS MAESTRIA/DATOS GEIH 2019 -- Enero - diciembre/Diciembre.csv")

Carac.Diciembre <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
desoc.Diciembre <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
F_trabajo.Diciembre <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.Diciembre <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Ocupa.Diciembre <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Otros_ing.Diciembre <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Otras_act.Diciembre <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Vivienda.Diciembre <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

Diciembre <- Carac.Diciembre %>% left_join(F_trabajo.Diciembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Ocupa.Diciembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(desoc.Diciembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.Diciembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otras_act.Diciembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Otros_ing.Diciembre, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))




###############-I-##############
##### UNION DE MESES 2019 #######



Dane_2019 <- rbind(Enero, Febrero, Marzo, Abril, Mayo, Junio, Julio, Agosto,
                   Septiembre, Octubre, Noviembre, Diciembre)


Dane_2019 <- Dane_2019 %>% mutate("id_personal"= paste0(Dane_2019$ï..DIRECTORIO, "-", Dane_2019$SECUENCIA_P, "-", Dane_2019$ORDEN)) %>% 
      select(id_personal, everything())

Dane_2019$id_personal <- as.factor(Dane_2019$id_personal)


###############-I-##############
##### UNION DE MESES DE MIGRACIÓN ##########

#   !!! CAMBIAR DIRECTORIO !!!!   #
setwd("~/ASPECTOS MAESTRIA/DATOS GEIH 2019 (migración) -- Enero - diciembre")

Migra1 <- read.csv("Enero.csv", header = T, sep = ";", dec = ",")
Migra2 <- read.csv("Febrero.csv", header = T, sep = ";", dec = ",")
Migra3 <- read.csv("Marzo.csv", header = T, sep = ";", dec = ",")
Migra4 <- read.csv("Abril.csv", header = T, sep = ";", dec = ",")
Migra5 <- read.csv("Mayo.csv", header = T, sep = ";", dec = ",")
Migra6 <- read.csv("Junio.csv", header = T, sep = ";", dec = ",")
Migra7<- read.csv("Julio.csv", header = T, sep = ";", dec = ",")
Migra8 <- read.csv("Agosto.csv", header = T, sep = ";", dec = ",")
Migra9 <- read.csv("Septiembre.csv", header = T, sep = ";", dec = ",")
Migra10 <- read.csv("Octubre.csv", header = T, sep = ";", dec = ",")
Migra11<- read.csv("Noviembre.csv", header = T, sep = ";", dec = ",")
Migra12 <- read.csv("Diciembre.csv", header = T, sep = ";", dec = ",")



Migracion_2019 <- rbind(Migra1, Migra2, Migra3, Migra4, Migra5, Migra6, Migra7, Migra8,
                        Migra9, Migra10, Migra11, Migra12)

Migracion_2019 <- Migracion_2019 %>% mutate("id_personal"= paste0(Migracion_2019$ï..Directorio, "-", Migracion_2019$Secuencia_p, "-", Migracion_2019$Orden)) %>% 
      select(id_personal, everything())

Migracion_2019$id_personal <- as.factor(Migracion_2019$id_personal)



###############-I-##############
setwd("~/ASPECTOS MAESTRIA")

##### BASE DE DATOS COMPLETA 2019  ##########

Dane_2019_completa <- left_join(Dane_2019, Migracion_2019, by = "id_personal")

#write.csv(Dane_2019_completa, "Dane_2019 (Todas las personas).csv")


###############-I-##############
##### Filtrado de venezolanos y refugiados ##########

Dane2019 <- filter(Dane_2019_completa, ((P753S3=="3") | (is.na(P753S3)==T & P755S3=="3") | 
                                           (is.na(P753S3)==T & is.na(P755S3)==T & P756S3== "3")) )

write.csv(Dane2019, "Dane2019_cabecera-migrantes.csv")













