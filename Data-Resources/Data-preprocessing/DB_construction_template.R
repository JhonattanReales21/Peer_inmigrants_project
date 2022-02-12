library(tidyr)
library(readxl)
library(readr)
library(dplyr)
library(funModeling)

## This Database construction template works for data from the DANE's GEIH survey.
## Take in consideration that for each year the name and code of some variables could change

##### Union csv - January ######

setwd("~/path/to/your/january/data")

Carac.january <- read.csv("Cabecera - Caracteristicas generales (Personas).csv", header = T, sep = ";", dec = ",")
Unemployed.january <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
Work_force.january <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.january <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Employed.january <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Other_incomes.january <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Other_activities.january <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Living_place.january <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

January <- Carac.january %>% left_join(Work_force.january, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Employed.january, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Unemployed.january, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.january, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_activities.january, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_incomes.january, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))


##### Union csv - February ######

setwd("~/path/to/your/February/data")


Carac.february <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
Unemployed.february <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
Work_force.february  <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.february  <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Employed.february  <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Other_incomes.february  <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Other_activities.february  <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Living_place.february  <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

February <- Carac.february %>% left_join(Work_force.february, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Employed.february, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Unemployed.february, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.february, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_activities.february, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_incomes.february, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))

##### Union csv - March ######

setwd("~/path/to/your/March/data")

Carac.March <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
Unemployed.March <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
Work_force.March <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.March <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Employed.March <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Other_incomes.March <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Other_activities.March <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Living_place.March <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

March <- Carac.March %>% left_join(Work_force.March, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Employed.March, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Unemployed.March, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.March, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_activities.March, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_incomes.March, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))


##### Union csv - April ######

setwd("~/path/to/your/April/data")

Carac.April <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
Unemployed.April <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
Work_force.April <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.April<- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Employed.April<- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Other_incomes.April<- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Other_activities.April<- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Living_place.April<- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

April <- Carac.April %>% left_join(Work_force.April, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Employed.April, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Unemployed.April, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.April, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_activities.April, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_incomes.April, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))


##### Union csv - May ######

setwd("~/path/to/your/May/data")

Carac.May <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
Unemployed.May <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
Work_force.May <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.May <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Employed.May <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Other_incomes.May <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Other_activities.May <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Living_place.May <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

May <- Carac.May %>% left_join(Work_force.May, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Employed.May, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Unemployed.May, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.May, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_activities.May, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_incomes.May, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))


##### Union csv - June ######

setwd("~/path/to/your/June/data")


Carac.June <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
Unemployed.June<- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
Work_force.June <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.June <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Employed.June <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Other_incomes.June<- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Other_activities.June <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Living_place.June <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

June <- Carac.June %>% left_join(Work_force.June, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Employed.June, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Unemployed.June, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.June, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_activities.June, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_incomes.June, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))


##### Union csv - July ######

setwd("~/path/to/your/July/data")


Carac.July <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
Unemployed.July <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
Work_force.July <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.July <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Employed.July <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Other_incomes.July <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Other_activities.July <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Living_place.July <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

July <- Carac.July %>% left_join(Work_force.July, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Employed.July, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Unemployed.July, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.July, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_activities.July, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_incomes.July, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))


##### Union csv - August ######

setwd("~/path/to/your/August/data")

Carac.August <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
Unemployed.August <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
Work_force.August <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.August <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Employed.August <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Other_incomes.August <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Other_activities.August <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Living_place.August<- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

August <- Carac.August %>% left_join(Work_force.August, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Employed.August, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Unemployed.August, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.August, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_activities.August, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_incomes.August, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))

##### Union csv - September ######

setwd("~/path/to/your/September/data")

Carac.September <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
Unemployed.September <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
Work_force.September <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.September <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Employed.September <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Other_incomes.September <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Other_activities.September <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Living_place.September<- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

September <-Carac.September %>% left_join(Work_force.September, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Employed.September, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Unemployed.September, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.September, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_activities.September, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_incomes.September, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))



##### Union csv - October ######

setwd("~/path/to/your/October/data")

Carac.October <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
Unemployed.October <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
Work_force.October <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.October <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Employed.October <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Other_incomes.October <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Other_activities.October <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Living_place.October <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

October<- Carac.October %>% left_join(Work_force.October, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Employed.October, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Unemployed.October, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.October, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_activities.October, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_incomes.October, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))



##### Union csv - November ######

setwd("~/path/to/your/November/data")

Carac.November <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
Unemployed.November <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
Work_force.November<- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.November <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Employed.November <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Other_incomes.November <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Other_activities.November <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Living_place.November <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

November <- Carac.November %>% left_join(Work_force.November, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Employed.November, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Unemployed.November, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.November, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_activities.November, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_incomes.November, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))



##### Union csv - December ######

setwd("~/path/to/your/December/data")

Carac.December <- read.csv("Cabecera - Características generales (Personas).csv", header = T, sep = ";", dec = ",")
Unemployed.December <- read.csv("Cabecera - Desocupados.csv", header = T, sep = ";", dec = ",")
Work_force.December <- read.csv("Cabecera - Fuerza de trabajo.csv", header = T, sep = ";", dec = ",")
Inac.December <- read.csv("Cabecera - Inactivos.csv", header = T, sep = ";", dec = ",")
Employed.December <- read.csv("Cabecera - Ocupados.csv", header = T, sep = ";", dec = ",")
Other_incomes.December <- read.csv("Cabecera - Otros ingresos.csv", header = T, sep = ";", dec = ",")
Other_activities.December <- read.csv("Cabecera - Otras actividades y ayudas en la semana.csv", header = T, sep = ";", dec = ",")
Living_place.December <- read.csv("Cabecera - Vivienda y Hogares.csv", header = T, sep = ";", dec = ",")

December <- Carac.December %>% left_join(Work_force.December, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Employed.December, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Unemployed.December, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Inac.December, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_activities.December, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN")) %>% 
      left_join(Other_incomes.December, by = c("ï..DIRECTORIO", "SECUENCIA_P", "ORDEN"))




###############-I-##############
##### UNION OF MONTHS #######



Dane_2019 <- rbind(January, February, March, April, May, June, July, August,
                   September, October, November, December)


Dane_2019 <- Dane_2019 %>% mutate("id_personal"= paste0(Dane_2019$ï..DIRECTORIO, "-", Dane_2019$SECUENCIA_P, "-", Dane_2019$ORDEN)) %>% 
      select(id_personal, everything())

Dane_2019$id_personal <- as.factor(Dane_2019$id_personal)


###############-I-##############
##### UNION OF MIGRATION DATA ##########

setwd("~/path/to/your/migration/data/folder")

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



Migration_2019 <- rbind(Migra1, Migra2, Migra3, Migra4, Migra5, Migra6, Migra7, Migra8,
                        Migra9, Migra10, Migra11, Migra12)

Migration_2019 <- Migration_2019 %>% mutate("id_personal"= paste0(Migration_2019$ï..Directorio, "-", Migration_2019$Secuencia_p, "-", Migration_2019$Orden)) %>% 
      select(id_personal, everything())

Migration_2019$id_personal <- as.factor(Migration_2019$id_personal)



###############-I-##############
setwd("~/path/where/you/want/your/output")

##### DATABASE COMPLETE  ##########

Dane_2019_complete <- left_join(Dane_2019, Migration_2019, by = "id_personal")

write.csv(Dane_2019_complete, "<Name of your DataBase complete (All the surveyed)>.csv")


###############-I-##############
##### FILTER ONLY THE VENEZOLAN INMIGRANTS ##########

Dane2019 <- filter(Dane_2019_complete, ((P753S3=="3") | (is.na(P753S3)==T & P755S3=="3") | 
                                           (is.na(P753S3)==T & is.na(P755S3)==T & P756S3== "3")) )

#This variables above can be found in the DANE migration page 

write.csv(Dane2019, "<Name of your INMIGRANTS DataBase complete>.csv")













