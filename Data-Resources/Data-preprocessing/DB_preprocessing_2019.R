library(funModeling)
library(tidyverse)
library(readxl)
library(readr)


setwd("~/ASPECTOS MAESTRIA")

#Leemos los datos
Dane_2019 <- read.csv("Dane2019-migrantes.csv", header = T, sep = ",", dec = ".")
#View(df_status(Dane_2019))
# 32,815 individuos

# Variable ocupacion ( 5 niveles)

      #P6870 -> Numero de personas en la empresa

Dane_2019$ocupacion[(!(is.na(Dane_2019$P6765))) & (Dane_2019$P6870 >=4)] = "Independiente formal"

Dane_2019$ocupacion[ (!(is.na(Dane_2019$P6765))) & (Dane_2019$P6870 <4) |
                           (is.na(Dane_2019$P6765) & (Dane_2019$P6870 <4)) |
                           Dane_2019$P6430==7] = "Empleo informal"

Dane_2019$ocupacion[ Dane_2019$P6440 %in% (1:2) & (is.na(Dane_2019$P6765)) & (Dane_2019$P6870 >=4)] = "Empleo formal"

# Niños e inactivos no aplican
Dane_2019$ocupacion[Dane_2019$P6040 <= 10 | !(is.na(Dane_2019$P7430))]="No aplica"

 #
Dane_2019$ocupacion[is.na(Dane_2019$ocupacion)] = "Desempleado"

#
frec_ocupaciones <- Dane_2019%>%group_by(ocupacion)%>%tally() 

###
#Variable Grupo.de.poblacion ( 4 niveles )
Dane_2019$Grupo.de.poblacion[!(is.na(Dane_2019$P6765))] = "Independiente"

Dane_2019$Grupo.de.poblacion[ Dane_2019$P6440 %in% (1:2) & (is.na(Dane_2019$P6765))] = "Asalariado"

Dane_2019$Grupo.de.poblacion[!(Dane_2019$P6440 %in% (1:2))] = "Desocupado"

Dane_2019$Grupo.de.poblacion[Dane_2019$P6040 <= 10 | !(is.na(Dane_2019$P7430))]="Inactivo y/o nino"


##
#Modelo Binario Ocupado & Desocupado
Dane_2019$Y_modelo1[Dane_2019$ocupacion %in% c("Empleo formal", "Empleo informal","Independiente formal")] = "Ocupado"
Dane_2019$Y_modelo1[Dane_2019$ocupacion=="Desempleado"] = "Desocupado"
Dane_2019$Y_modelo1[Dane_2019$ocupacion=="No aplica"] = "No aplica"

sum(is.na(Dane_2019$Y_modelo1))

#Modelo Binario Empleo & Desocupado-informal
Dane_2019$Y_modelo2[Dane_2019$ocupacion %in% c("Empleo formal","Independiente formal")] = "Ocupado"
Dane_2019$Y_modelo2[Dane_2019$ocupacion %in% c("Desempleado", "Empleo informal")] = "Desocupado/Informal"
Dane_2019$Y_modelo2[Dane_2019$ocupacion=="No aplica"] = "No aplica"
##


###### REALIZAR LOS CAMBIOS PERTINENTES A LA BASE DE DATOS (escoger variables generales y renombrar)
Dane_2019 <- mutate(Dane_2019, "Familia" = paste(Dane_2019$ï..DIRECTORIO, "-", Dane_2019$SECUENCIA_P))
Dane_2019 <- dplyr::select(Dane_2019, -1)
Dane_2019 <- dplyr::select(Dane_2019, 1, Familia, everything())

Dane_2019 <- dplyr::select(Dane_2019, id_personal, Familia, DPTO.x, AREA.x, MES.x, P6020, P6040, P6050, P6070, P6090, P6110, P6160, P6170, P6210, P6220, ESC,
                           P6240, P6280, P6290, P6310, P6260, P6320, P6340, P6350, P6430, P6440, P6450, P6460, P6424S3, P6426, P6480, P9440, P6500,
                           P6510, P6510S2, P6630S1, P6630S1A1, P6765, P6772, P6775, P6750, P6780, P1800, P1800S1, P1802, P1879, P6800, P6870,
                           P6880, P6915, P6920, P6940, P6990, P9450, P7020, P760, P7026, P7028, P1880, P7130, P7100,P7120, P514, P515, P7240, 
                           OFICIO, RAMA2D, RAMA4D, INGLABO, P7250, P7280, P7310, P7360 ,P7260, OFICIO2, RAMA2D_D, RAMA4D_D, P7430, P7450, P7458,P1884, P1807, P7495, P7505,
                           P1661S4, P1661S4A1, P1661S4A2,P753S3, P753, P756S3,P755, ocupacion,Grupo.de.poblacion, fex_c_2011.x, Y_modelo1, Y_modelo2,
                           P7454,P6125,P6140,P6300,P7090,P7170S1,P1881,P1806,P7390,
                           P6585S1A1, P6585S1A2, P6585S2A1, P6585S2A2,
                           P6585S3A1, P6585S3A2, P6585S4A1, P6585S4A2, P6545S1, P6545S2, P6580S1, P6580S2, P7070, P7422S1, P7510S3A1,P750S3A1)


Dane_2019 <- dplyr::rename(Dane_2019, Identificacion=id_personal, Departamento=DPTO.x, Area=AREA.x, Mes=MES.x, "Anios.de.escolaridad" = ESC, "Genero"=P6020,
                           Edad=P6040, Parentesco=P6050, "Estado.civil" = P6070, "P6090 Cuenta con seguridad social en salud?" = P6090)
Dane_2019 <- Dane_2019%>%dplyr::rename("P6110 Quien paga por la salud?" = P6110,  "P6160 Sabe leer y escribir?" = P6160, "P6170 Asiste a una entidad educativa?" =P6170, "P6426 Tiempo trabajando de manera continua (meses)"=P6426,
                                       "Nivel.educativo.alcanzado" = P6210, "P6220 Titulo o diploma de mayor nivel" = P6220, "P6290 Que ha hecho para conseguir trabajo" = P6290, "P6280 Ha intentado conseguir trabajo en este mes?" = P6280,
                                       "P6240 Que fue lo que mas hizo la semana pasada?" = P6240, "P6310 Por que no hizo diligencias para trabajar?" = P6310, "P6260 Aunque no trabajo la semana pasada, tuvo ingresos?" = P6260,
                                       "P6320 El ultimo año ha trabajado por lo menos 2 semanas?" = P6320, "P6340 En el ultimo año ha buscado trabajado?" = P6340, "P6350 Cuantos meses lleva sin buscar trabajo?" = P6350, "P6440 Como -Asalariado-, cuenta con algun tipo de contrato?" = P6440, "Cuales fueron sus ingresos como empleado en el ultimo mes?" =P6500,
                                       "P6765 Que forma de trabajo -independiente- realizó?" =P6765, "P6750 Ganancia neta por honorarios o negocio" =P6750, "P1800 Como -Independiente-, tiene empleados o personas que lo ayuden?" =P1800, "P6800 Cuantas horas a la semana trabaja?" = P6800,
                                       "P6870 Numero de personas en la empresa o negocio donde labora" = P6870, "P6880 Donde realiza su trabajo?" =P6880, "P6915 En caso de enfermedad, como cubre los costos?" =P6915, 
                                       "P6920 Cotiza a pension?" =P6920, "P6940 Quien paga el fondo de pensiones?" =P6940,"P6990 Cuenta con ARL?"=P6990,
                                       "P7020 Tuvo otro trabajo antes del actual?" =P7020, "P760 Meses desempleado antes de conseguir trabajo?" =P760, "P7026 Tiempo en empleo anterior (meses)"=P7026,"P7028 En su empleo anterior usted era?"=P7028,
                                       "P1880 Por que dejó su empleo anterior?" = P1880, "P7130 Desea cambiar su trabajo actual?" =P7130, "P7100 Horas adicionales que puede trabajar en la semana"=P7100, 
                                       "P7120 Disponible para trabajar mas horas a la semana?"=P7120, "P514 Considera que su trabajo es estable?"=P514, "P515 Su horario de trabajo y responsabilidades familiares son compatibles?"=P515, "P7240 Si no tiene empleo de donde obtendria los recursos?"=P7240,
                                       "P7280 Estando -Desempleado- que tipo de trabajo ha buscado?"=P7280,
                                       "P6430 En este trabajo usted es"=P6430, "Rama de actividad de la empresa donde labora(2digitos)"=RAMA2D, "Rama de actividad de la empresa donde labora(4digitos)"=RAMA4D, "Ingresos laborales"=INGLABO, "P7250 Semanas que lleva -Desocupado- buscando trabajo, o que estuvo buscando"=P7250,
                                       "P7310 Buscando trabajo por primera vez?"=P7310, "P7260 Cuantas horas a la semana podría trabajar?"=P7260,"Rama de la actividad que realiza la ultima empresa donde laboró (2 digitos)?"=RAMA2D_D, "P7430 Es -Inactivo- pero, ha trabajado alguna vez?"=P7430, "Rama de la actividad que realiza la ultima empresa deonde laboró (4 digitos)"=RAMA4D_D,
                                       "OFICIO codigo de su oficio actual"=OFICIO, "OFICIO2 Codigo del ultimo oficio que realiz?"=OFICIO2, "P7450 Por que dejo de trabajar?"=P7450, "P1884 Cuantas horas a la semana puede trabajar?"=P1884, "P1807 -inactivo-, Salario minimo que aceptaria"=P1807,
                                       "P7495 Recibio pagos por pensiones o arriendos?"=P7495, "P7505 Recibio dinero de otras persons o instituciones?"=P7505,"P1661S4 Recibio ayuda de entidades diferentes al gobierno?"=P1661S4, 
                                       "P1661S4A1 Cual entidad no gubernamental?"=P1661S4A1, "P1661S4A2 Monto recibido"=P1661S4A2, "P7360 Numero de personas en la empresa o negocio donde laboraba" = P7360, "P755 Donde vivia hace 5 años?"=P755,
                                       "P753 Donde vivia hace 12 meses?" = P753, "P753S3 En que pais vivia hace 12 meses?"=P753S3, "Factor.de.expansion"=fex_c_2011.x,
                                       "P7454 Ha buscado trabajo alguna vez?"= P7454, "P7458 Por que dejo de buscar trabajo?"=P7458, "P6125 En los últimos doce meses dejó de asistir al médico por no tener con que pagar estos servicios?"=P6125, 
                                       "P6140 Anteriormente contó con seguridad social en salud?"=P6140, "P6480 Por cual medio consiguio su trabajo actual?"=P6480,
                                       "P6300 Desea conseguir un trabajo remunerado o instalar un negocio?"= P6300, "P7090 Trabaja menos horas de las que puede o quisiera?"=P7090, "P7170S1 Esta satisfecho con su trabajo actual?"=P7170S1,
                                       "P1881 Que medio de transporte utiliza para dirigirse a su sitio de trabajo?"= P1881, "P1806 Como -Desocupado-, cual seria la remuneracion minima mensual por la cual aceptaria un trabajo?"=P1806, 
                                       "P7390 Como -Desocupado-, en caso de enfermedad, como cubre los costos?"=P7390, "P9440 Su trabajo actual lo consiguio a traves de internet?"=P9440,
                                       "P1879 Por que trabaja como independiente?"=P1879, "P6510S2 Incluyo horas extras?"=P6510S2, "P6585S1A1 Recibio auxilio de alimentacion?"=P6585S1A1,
                                       "P6585S1A2 Incluyo sub de alimentacion?"=P6585S1A2, "P6585S2A1 Recibio auxilio de transporte?"=P6585S2A1, "P6585S2A2 Incluyo sub de transporte?"=P6585S2A2,
                                       "P6585S3A1 Recibio auxilio familiar?"=P6585S3A1, "P6585S3A2 Incluyo sub familiar?"=P6585S3A2,
                                       "P6585S4A1 Recibio auxilio educativo?"=P6585S4A1, "P6585S4A2 Incluyo sub educativo?"=P6585S4A2,
                                       "P6545S1 Recibio primas(Tecnica, de antiguedad, etc)?"=P6545S1, "P6545S2 Incluyo primas(Tecnica, de antiguedad, etc)?"=P6545S2,
                                       "P6580S1 Recibio algun tipo de bonificacion?"=P6580S1, "P6580S2 Incluyo algun tipo de bonificacion?"=P6580S2,
                                       "P7070 Cuanto fue su ingreso por un segundo trabajo?"=P7070, "P7422S1 Como -Desempleado- cuanto recibio de ingresos el mes pasado"=P7422S1,
                                       "P7510S3A1 Cuanto recibio por ayudas en dinero de instituciones en el pais?"=P7510S3A1,
                                       "P750S3A1 Cuanto recibio por ayudas en dinero de instituciones fuera del pais?"=P750S3A1, "P6510 Cuanto recibio por horas extras"=P6510)



   
   
            ##  Construir variables nuevas, combinando preguntas de cada modulo  ##


#Horas que puede trabajar en la semana o las que trabaja
Dane_2019$Horas.disponibles= 0

for (i in 1:nrow(Dane_2019)) {
      if (!(is.na(Dane_2019$`P7260 Cuantas horas a la semana podría trabajar?`[i])) == T) {
            Dane_2019$Horas.disponibles[i] <- Dane_2019$`P7260 Cuantas horas a la semana podría trabajar?`[i]
      }
      
      if (!(is.na(Dane_2019$`P6800 Cuantas horas a la semana trabaja?`[i])) == T) {
            Dane_2019$Horas.disponibles[i] <- Dane_2019$`P6800 Cuantas horas a la semana trabaja?`[i] 
      }
      if (!(is.na(Dane_2019$`P7100 Horas adicionales que puede trabajar en la semana`[i])) == T){
            Dane_2019$Horas.disponibles[i] <- Dane_2019$Horas.disponibles[i] + Dane_2019$`P7100 Horas adicionales que puede trabajar en la semana`[i]
      }
}

#Rama de la empresa donde trabaja o donde trabajo
Dane_2019$Rama.actividad= 0

for (i in 1:nrow(Dane_2019)) {
      if (!(is.na(Dane_2019$`Rama de la actividad que realiza la ultima empresa donde laboró (2 digitos)?`[i])) == T) {
            Dane_2019$Rama.actividad[i] <- Dane_2019$`Rama de la actividad que realiza la ultima empresa donde laboró (2 digitos)?`[i]
      }
      if (!(is.na(Dane_2019$`Rama de actividad de la empresa donde labora(2digitos)`[i])) == T) {
            Dane_2019$Rama.actividad[i] <- Dane_2019$`Rama de actividad de la empresa donde labora(2digitos)`[i]
      }
}

#Variable tiempo en Colombia
Dane_2019$tiempo.en.colombia= 0

for (i in 1:nrow(Dane_2019)) {
      if(!(is.na(Dane_2019$`P753S3 En que pais vivia hace 12 meses?`[i])) == T){
            Dane_2019$tiempo.en.colombia[i] <- 1
      }
      if (Dane_2019$`P753 Donde vivia hace 12 meses?`[i] %in% (2:3)) {
            Dane_2019$tiempo.en.colombia[i] <- 2
      }
      if(!(is.na(Dane_2019$P756S3[i])) == T & Dane_2019$`P755 Donde vivia hace 5 años?`[i] %in% (2:3) & Dane_2019$`P753 Donde vivia hace 12 meses?`[i] %in% (2:3)) {
            Dane_2019$tiempo.en.colombia[i] <- 3
      }
      if (Dane_2019$tiempo.en.colombia[i]==0) {
            Dane_2019$tiempo.en.colombia[i] <- 1
      }
}

# Indicativo de año
Dane_2019$anio <- 2019


# Inactivos - variables
Dane_2019$`Siendo inactivo, por que dejo de buscar trabajo?`="No aplica"

for (i in 1:nrow(Dane_2019)){
      if (is.na(Dane_2019$`P7454 Ha buscado trabajo alguna vez?`[i])){
            
      } else if (Dane_2019$`P7454 Ha buscado trabajo alguna vez?`[i] == 2){
            Dane_2019$`Siendo inactivo, por que dejo de buscar trabajo?`[i]="Nunca ha buscado"
      }
      
      if (!(is.na(Dane_2019$`P7458 Por que dejo de buscar trabajo?`[i]))) {
            Dane_2019$`Siendo inactivo, por que dejo de buscar trabajo?`[i]=Dane_2019$`P7458 Por que dejo de buscar trabajo?`[i]
      }
}
class(Dane_2019$`P7454 Ha buscado trabajo alguna vez?`)

# General - variable
Dane_2019$`Es o ha sido afiliado, cotizante o beneficiario de EPS en Colombia`="No, nunca"

for (i in 1:nrow(Dane_2019)){
      if (Dane_2019$`P6090 Cuenta con seguridad social en salud?`[i] == 1){
            Dane_2019$`Es o ha sido afiliado, cotizante o beneficiario de EPS en Colombia`[i]="Si, actualmente"
      }
      
      if (Dane_2019$`P6090 Cuenta con seguridad social en salud?`[i] == 2 &  Dane_2019$`P6140 Anteriormente contó con seguridad social en salud?`[i]==1 ){
            Dane_2019$`Es o ha sido afiliado, cotizante o beneficiario de EPS en Colombia`[i]="No, actualmente"
                  
      }
}


# Fuerza de trabajo - variable
Dane_2019$`En el ultimo mes ha buscado trabajo? por que no?`="No aplica"

for (i in 1:nrow(Dane_2019)){
      if (!(is.na(Dane_2019$`P6290 Que ha hecho para conseguir trabajo`[i]))){
            Dane_2019$`En el ultimo mes ha buscado trabajo? por que no?`[i]="Si, he buscado"
            
      } 
      if (!(is.na(Dane_2019$`P6310 Por que no hizo diligencias para trabajar?`[i]))){
            Dane_2019$`En el ultimo mes ha buscado trabajo? por que no?`[i]=Dane_2019$`P6310 Por que no hizo diligencias para trabajar?`[i]
            
      } 
      if (!(is.na(Dane_2019$`P6440 Como -Asalariado-, cuenta con algun tipo de contrato?`[i]))){
            Dane_2019$`En el ultimo mes ha buscado trabajo? por que no?`[i]="Me encuentro trabajando"
            
      } 
      
      if (is.na(Dane_2019$`P6300 Desea conseguir un trabajo remunerado o instalar un negocio?`[i])){
            
      } else if (Dane_2019$`P6300 Desea conseguir un trabajo remunerado o instalar un negocio?`[i] == 2){
            Dane_2019$`En el ultimo mes ha buscado trabajo? por que no?`[i]="No deseo trabajar"
            
      }
      
}

            ## Nivel <<No aplica>> para valores <<NA>> (para variables de interes)
Dane_2019$`P6170 Asiste a una entidad educativa?`[is.na(Dane_2019$`P6170 Asiste a una entidad educativa?`)]="No aplica (<3 años)"
Dane_2019$Nivel.educativo.alcanzado[is.na(Dane_2019$Nivel.educativo.alcanzado)]="No aplica (<3 años)"
Dane_2019$Estado.civil[is.na(Dane_2019$Estado.civil)]="No aplica (<10 años)"
Dane_2019$`P6240 Que fue lo que mas hizo la semana pasada?`[is.na(Dane_2019$`P6240 Que fue lo que mas hizo la semana pasada?`)]="No aplica (<12 años)"
Dane_2019$`P6440 Como -Asalariado-, cuenta con algun tipo de contrato?`[is.na(Dane_2019$`P6440 Como -Asalariado-, cuenta con algun tipo de contrato?`)]="No aplica"
Dane_2019$`P6430 En este trabajo usted es`[is.na(Dane_2019$`P6430 En este trabajo usted es`)]="No aplica (No ocupado)"
Dane_2019$`P6426 Tiempo trabajando de manera continua (meses)`[is.na(Dane_2019$`P6426 Tiempo trabajando de manera continua (meses)`)]=0
Dane_2019$`P6480 Por cual medio consiguio su trabajo actual?`[is.na(Dane_2019$`P6480 Por cual medio consiguio su trabajo actual?`)]="No aplica"
Dane_2019$`P9440 Su trabajo actual lo consiguio a traves de internet?`[is.na(Dane_2019$`P9440 Su trabajo actual lo consiguio a traves de internet?`)]="No aplica"
Dane_2019$`Cuales fueron sus ingresos como empleado en el ultimo mes?`[is.na(Dane_2019$`Cuales fueron sus ingresos como empleado en el ultimo mes?`)]=0
Dane_2019$`P1800 Como -Independiente-, tiene empleados o personas que lo ayuden`[is.na(Dane_2019$`P1800 Como -Independiente-, tiene empleados o personas que lo ayuden`)]="No aplica"
Dane_2019$`P6750 Ganancia neta por honorarios o negocio`[is.na(Dane_2019$`P6750 Ganancia neta por honorarios o negocio`)]=0
Dane_2019$`P1807 -inactivo-, Salario minimo que aceptaria`[is.na(Dane_2019$`P1807 -inactivo-, Salario minimo que aceptaria`)]=0
Dane_2019$`P1879 Por que trabaja como independiente?`[is.na(Dane_2019$`P1879 Por que trabaja como independiente?`)]="No aplica"
Dane_2019$`P6880 Donde realiza su trabajo?`[is.na(Dane_2019$`P6880 Donde realiza su trabajo?`)]="No aplica"
Dane_2019$`P6915 En caso de enfermedad, como cubre los costos?`[is.na(Dane_2019$`P6915 En caso de enfermedad, como cubre los costos?`)]="No aplica"
Dane_2019$`P7090 Trabaja menos horas de las que puede o quisiera?`[is.na(Dane_2019$`P7090 Trabaja menos horas de las que puede o quisiera?`)]="No aplica"
Dane_2019$`P7130 Desea cambiar su trabajo actual?`[is.na(Dane_2019$`P7130 Desea cambiar su trabajo actual?`)]="No aplica"
Dane_2019$`P7170S1 Esta satisfecho con su trabajo actual?`[is.na(Dane_2019$`P7170S1 Esta satisfecho con su trabajo actual?`)]="No aplica"
Dane_2019$`P514 Considera que su trabajo es estable?`[is.na(Dane_2019$`P514 Considera que su trabajo es estable?`)]="No aplica"
Dane_2019$`P515 Su horario de trabajo y responsabilidades familiares son compatibles?`[is.na(Dane_2019$`P515 Su horario de trabajo y responsabilidades familiares son compatibles?`)]="No aplica"
Dane_2019$`P1881 Que medio de transporte utiliza para dirigirse a su sitio de trabajo?`[is.na(Dane_2019$`P1881 Que medio de transporte utiliza para dirigirse a su sitio de trabajo?`)]="No aplica"
Dane_2019$`P7240 Si no tiene empleo de donde obtendria los recursos?`[is.na(Dane_2019$`P7240 Si no tiene empleo de donde obtendria los recursos?`)]="No aplica"
Dane_2019$`P7250 Semanas que lleva -Desocupado- buscando trabajo, o que estuvo buscando`[is.na(Dane_2019$`P7250 Semanas que lleva -Desocupado- buscando trabajo, o que estuvo buscando`)]=0
Dane_2019$`P1806 Como -Desocupado-, cual seria la remuneracion minima mensual por la cual aceptaria un trabajo?`[is.na(Dane_2019$`P1806 Como -Desocupado-, cual seria la remuneracion minima mensual por la cual aceptaria un trabajo?`)]=0
Dane_2019$`P7390 Como -Desocupado-, en caso de enfermedad, como cubre los costos?`[is.na(Dane_2019$`P7390 Como -Desocupado-, en caso de enfermedad, como cubre los costos?`)]="No aplica"
Dane_2019$`P6765 Que forma de trabajo -independiente- realizó?`[is.na(Dane_2019$`P6765 Que forma de trabajo -independiente- realizó?`)]="No aplica"
Dane_2019$`P6160 Sabe leer y escribir?`[is.na(Dane_2019$`P6160 Sabe leer y escribir?`)]="No aplica (<3 años)"



            ##  Modificando clase de las variables de interes ##

#`P1807 -inactivo-, Salario minimo que aceptaria` (numerica)
# `Cuales fueron sus ingresos como empleado en el ultimo mes?` (numerica)
#`P6750 Ganancia neta por honorarios o negocio` (numerica)
#`P7250 Semanas que lleva -Desocupado- buscando trabajo, o que estuvo buscando` (numerica)
#`P1806 Como -Desocupado-, cual seria la remuneracion minima mensual por la cual aceptaria un trabajo?` (numerica)
#`P6426 Tiempo trabajando de manera continua (meses)` (numerica)
Dane_2019$`P6170 Asiste a una entidad educativa?` <- as.factor(Dane_2019$`P6170 Asiste a una entidad educativa?`)
Dane_2019$`P6240 Que fue lo que mas hizo la semana pasada?` <- as.factor(Dane_2019$`P6240 Que fue lo que mas hizo la semana pasada?`)
Dane_2019$`P6440 Como -Asalariado-, cuenta con algun tipo de contrato?` <- as.factor(Dane_2019$`P6440 Como -Asalariado-, cuenta con algun tipo de contrato?`)
Dane_2019$`P6430 En este trabajo usted es` <- as.factor(Dane_2019$`P6240 Que fue lo que mas hizo la semana pasada?`)
Dane_2019$`P6480 Por cual medio consiguio su trabajo actual?` <- as.factor(Dane_2019$`P6480 Por cual medio consiguio su trabajo actual?`)
Dane_2019$`P9440 Su trabajo actual lo consiguio a traves de internet?` <- as.factor(Dane_2019$`P9440 Su trabajo actual lo consiguio a traves de internet?`)
Dane_2019$`P1800 Como -Independiente-, tiene empleados o personas que lo ayuden` <- as.factor(Dane_2019$`P1800 Como -Independiente-, tiene empleados o personas que lo ayuden`)
Dane_2019$`P1879 Por que trabaja como independiente?` <- as.factor(Dane_2019$`P1879 Por que trabaja como independiente?`)
Dane_2019$`P6880 Donde realiza su trabajo?` <- as.factor(Dane_2019$`P6880 Donde realiza su trabajo?`)
Dane_2019$`P6915 En caso de enfermedad, como cubre los costos?` <- as.factor(Dane_2019$`P6915 En caso de enfermedad, como cubre los costos?`)
Dane_2019$`P7090 Trabaja menos horas de las que puede o quisiera?` <- as.factor(Dane_2019$`P7090 Trabaja menos horas de las que puede o quisiera?`)
Dane_2019$`P7130 Desea cambiar su trabajo actual?` <- as.factor(Dane_2019$`P7130 Desea cambiar su trabajo actual?`)
Dane_2019$`P6125 En los últimos doce meses dejó de asistir al médico por no tener con que pagar estos servicios?` <- as.factor(Dane_2019$`P6125 En los últimos doce meses dejó de asistir al médico por no tener con que pagar estos servicios?`)
Dane_2019$`P7170S1 Esta satisfecho con su trabajo actual?` <- as.factor(Dane_2019$`P7170S1 Esta satisfecho con su trabajo actual?`)
Dane_2019$`P514 Considera que su trabajo es estable?` <- as.factor(Dane_2019$`P514 Considera que su trabajo es estable?`)
Dane_2019$`P515 Su horario de trabajo y responsabilidades familiares son compatibles?` <- as.factor(Dane_2019$`P515 Su horario de trabajo y responsabilidades familiares son compatibles?`)
Dane_2019$`P1881 Que medio de transporte utiliza para dirigirse a su sitio de trabajo?` <- as.factor(Dane_2019$`P1881 Que medio de transporte utiliza para dirigirse a su sitio de trabajo?`)
Dane_2019$`P7240 Si no tiene empleo de donde obtendria los recursos?` <- as.factor(Dane_2019$`P7240 Si no tiene empleo de donde obtendria los recursos?`)
Dane_2019$`P7390 Como -Desocupado-, en caso de enfermedad, como cubre los costos?` <- as.factor(Dane_2019$`P7390 Como -Desocupado-, en caso de enfermedad, como cubre los costos?`)
Dane_2019$`En el ultimo mes ha buscado trabajo? por que no?` <- as.factor(Dane_2019$`En el ultimo mes ha buscado trabajo? por que no?`)
Dane_2019$`Siendo inactivo, por que dejo de buscar trabajo?` <- as.factor(Dane_2019$`Siendo inactivo, por que dejo de buscar trabajo?`)
Dane_2019$`P6765 Que forma de trabajo -independiente- realizó?` <- as.factor(Dane_2019$`P6765 Que forma de trabajo -independiente- realizó?`)
Dane_2019$`P6160 Sabe leer y escribir?` <- as.factor(Dane_2019$`P6160 Sabe leer y escribir?`)
Dane_2019$Mes <- as.factor(Dane_2019$Mes)

Dane_2019$Departamento <-  as.factor(Dane_2019$Departamento)
Dane_2019$Genero <- as.factor(Dane_2019$Genero)
Dane_2019$Parentesco <- as.factor(Dane_2019$Parentesco)
Dane_2019$Estado.civil <- as.factor(Dane_2019$Estado.civil)
Dane_2019$Nivel.educativo.alcanzado<- as.factor(Dane_2019$Nivel.educativo.alcanzado)
Dane_2019$Rama.actividad <- as.factor(Dane_2019$Rama.actividad)
Dane_2019$tiempo.en.colombia <- as.factor(Dane_2019$tiempo.en.colombia)
Dane_2019$Y_modelo1 <- as.factor(Dane_2019$Y_modelo1)
Dane_2019$Y_modelo2 <- as.factor(Dane_2019$Y_modelo2)


# Cambiar niveles variables
Dane_2019$Parentesco <- fct_collapse(Dane_2019$Parentesco, "Jefe de hogar"="1", "Nucleo familiar"=c("2","3"), "Otros parientes"=c("4","5"), "Otros No parientes"=c("6","7","8","9"))
Dane_2019$Genero <- fct_collapse(Dane_2019$Genero, "Masculino"="1", "Femenino"=c("2"))
Dane_2019$tiempo.en.colombia <- fct_collapse(Dane_2019$tiempo.en.colombia, "Menos de 1 año"="1", "Entre 1 y 5 años"=c("2"), "Mas de 5 años" ="3")
Dane_2019$Estado.civil <- fct_collapse(Dane_2019$Estado.civil, "Soltero"="6", "Casado y/o union libre"=c("1","2","3"), "Separado y/o viudo"=c("4","5"))
Dane_2019$Nivel.educativo.alcanzado <- fct_collapse(Dane_2019$Nivel.educativo.alcanzado, "Ninguno/No_sabe"=c("1","9"), "Preescolar"="2","Básica primaria"="3","Básica secundaria"="4","Media (10o -13o)"="5","Superior o universitaria"="6",)
Dane_2019$Mes <- fct_collapse(Dane_2019$Mes, "Enero"="1", "Febrero"="2", "Marzo"="3", "Abril"="4", "Mayo"="5", "Junio"="6", "Julio"="7", "Agosto"="8", "Septiembre"="9", "Octubre"="10", "Noviembre"="11", "Diciembre"="12")
Dane_2019$`P6160 Sabe leer y escribir?` <- fct_collapse(Dane_2019$`P6160 Sabe leer y escribir?`, "Si"="1", "No"="2")
Dane_2019$`P6125 En los últimos doce meses dejó de asistir al médico por no tener con que pagar estos servicios?` <- fct_collapse(Dane_2019$`P6125 En los últimos doce meses dejó de asistir al médico por no tener con que pagar estos servicios?`, "Si"="1","No"="2","No sabe, no informa"="9")

Dane_2019$`P6170 Asiste a una entidad educativa?` <- fct_collapse(Dane_2019$`P6170 Asiste a una entidad educativa?`, "Si"="1", "No"="2")

Dane_2019$`P6240 Que fue lo que mas hizo la semana pasada?` <- fct_collapse(Dane_2019$`P6240 Que fue lo que mas hizo la semana pasada?`,
                                                                  "Trabajando"="1","Buscando Trabajo"="2","Estudiando"="3",
                                                                  "Oficios del hogar"="4","Incapacitado permanentemente para trabajar"="5","Otra actividad"="6")

Dane_2019$`P6440 Como -Asalariado-, cuenta con algun tipo de contrato?` <- fct_collapse(Dane_2019$`P6440 Como -Asalariado-, cuenta con algun tipo de contrato?`,
                                                                            "Si"="1","No"="2")

Dane_2019$`P6430 En este trabajo usted es` <- fct_collapse(Dane_2019$`P6430 En este trabajo usted es`,
                                                           "Obrero o empleado de empresa particular"="1","Obrero o empleado del gobierno"="2","Empleado doméstico"="3",
                                                           "Trabajador por cuenta propia"="4","Patrón o empleador"="5","Trabajador familiar sin remuneración"="6",
                                                           "Trabajador sin remuneración en empresas o negocios de otros hogares"="7","Jornalero o peón"="8","Otro"="9")
                                                                                        

Dane_2019$`P6480 Por cual medio consiguio su trabajo actual?`<- fct_collapse(Dane_2019$`P6480 Por cual medio consiguio su trabajo actual?`,
                                                                             "Pidió ayuda a familiares, amigos, colegas"="1","Visitó, llevó o envió hojas de vida a empresas o empleadores"="2","Visitó, llevó o envió hojas de vida a bolsas de empleo o intermediarios"="3",
                                                                             "Puso o consultó avisos clasificados"="4","Por convocatorias"="5","Por el sistema de información sena"="6",
                                                                             "Otro medio"="7","No sabe, no informa"="9")

Dane_2019$`P9440 Su trabajo actual lo consiguio a traves de internet?` <- fct_collapse(Dane_2019$`P9440 Su trabajo actual lo consiguio a traves de internet?`, "Si"="1", "No"="2")

Dane_2019$`P1800 Como -Independiente-, tiene empleados o personas que lo ayuden` <- fct_collapse(Dane_2019$`P1800 Como -Independiente-, tiene empleados o personas que lo ayuden`, "Si"="1", "No"="2")

Dane_2019$`P1879 Por que trabaja como independiente?` <- fct_collapse(Dane_2019$`P1879 Por que trabaja como independiente?`,
                                                                            "No encontró trabajo como asalariado"="1","Por despido"="2","Más independencia"="3",
                                                                            "Horario de trabajo más flexible"="4","Se considera muy joven o muy viejo"="5","Empezó su empresa o negocio"="6",
                                                                            "Trabaja en un negocio familiar"="7","Mayor nivel de ingreso"="8","Por tradición familiar"="9",
                                                                            "Considera que no tiene los estudios necesarios"="10","	Otro"="11")

Dane_2019$`P6880 Donde realiza su trabajo?` <- fct_collapse(Dane_2019$`P6880 Donde realiza su trabajo?`,
                                                                      "En esta vivienda"="1","En otras viviendas"="2","En kiosco - caseta"="3",
                                                                      "En un vehículo"="4","De puerta en puerta"="5","Sitio al descubierto en la calle (ambulante y estacionario)"="6",
                                                                      "Local fijo, oficina, fábrica, etc."="7","En el campo o área rural, mar o río"="8","En una obra en construcción"="9",
                                                                      "En una mina o cantera"="10","	Otro"="11")

Dane_2019$`P6915 En caso de enfermedad, como cubre los costos?` <- fct_collapse(Dane_2019$`P6915 En caso de enfermedad, como cubre los costos?`,
                                                            "Afiliado a eps"="1","Afiliado a regimen subsidiado de salud"="2","Es beneficiario de un afiliado"="3",
                                                            "Con ahorros personales"="4","Con ayudas de los hijos o familiares"="5","Con otro tipo de seguro o cubrimiento"="6",
                                                            "Pidiendo dinero prestado"="7","Vendería su vivienda o bienes del hogar"="8","Empeñaría bienes del hogar"="9",
                                                            "No lo ha considerado"="10","No tiene recursos"="11","Otro"="12")

Dane_2019$`P7090 Trabaja menos horas de las que puede o quisiera?` <- fct_collapse(Dane_2019$`P7090 Trabaja menos horas de las que puede o quisiera?`, "Si"="1", "No"="2")
Dane_2019$`P7130 Desea cambiar su trabajo actual?` <- fct_collapse(Dane_2019$`P7130 Desea cambiar su trabajo actual?`, "Si"="1", "No"="2")
Dane_2019$`P7170S1 Esta satisfecho con su trabajo actual?` <- fct_collapse(Dane_2019$`P7170S1 Esta satisfecho con su trabajo actual?`, "Si"="1", "No"="2")
Dane_2019$`P514 Considera que su trabajo es estable?` <- fct_collapse(Dane_2019$`P514 Considera que su trabajo es estable?`, "Si"="1", "No"="2")
Dane_2019$`P515 Su horario de trabajo y responsabilidades familiares son compatibles?` <- fct_collapse(Dane_2019$`P515 Su horario de trabajo y responsabilidades familiares son compatibles?`, "Si"="1", "No"="2")

Dane_2019$`P1881 Que medio de transporte utiliza para dirigirse a su sitio de trabajo?` <- fct_collapse(Dane_2019$`P1881 Que medio de transporte utiliza para dirigirse a su sitio de trabajo?`,
                                                                                "Bus intermunicipal"="1","Bus urbano"="2","A pie"="3",
                                                                                "Metro"="4","Transporte articulado"="5","Taxi"="6",
                                                                                "Transportede la empresa"="7","Automovil de uso particular"="8","Lancha, planchón, canoa"="9",
                                                                                "Caballo"="10","Moto"="11","Mototaxi"="12","Bicicleta"="13","No se desplaza"="14", "Otro"="15")

Dane_2019$`P7240 Si no tiene empleo de donde obtendria los recursos?` <- fct_collapse(Dane_2019$`P7240 Si no tiene empleo de donde obtendria los recursos?`,
                                                                                                        "Cesantías"="1","Ahorros personales"="2","Ayudas de hijos o familiares"="3",
                                                                                                        "Indemnización o similar"="4","No lo ha considerado"="5","Vendería su vivienda o bienes del hogar"="6",
                                                                                                        "Empeñaría bienes del hogar"="7","No tendría recursos"="8","	Solicitaría dinero prestado"="9",
                                                                                                         "Otro"="10")

Dane_2019$`P7390 Como -Desocupado-, en caso de enfermedad, como cubre los costos?` <- fct_collapse(Dane_2019$`P7390 Como -Desocupado-, en caso de enfermedad, como cubre los costos?`,
                                                                                "Afiliado a eps o regimen subsidiado"="1","Es beneficiario de un afiliado"="2",
                                                                                "Con ahorros personales"="3","Con ayudas de los hijos o familiares"="4","Con otro tipo de seguro o cubrimiento"="5",
                                                                                "Pidiendo dinero prestado"="6", "No lo ha considerado"="7","No tiene recursos"="8","Otro"="9")

Dane_2019$`En el ultimo mes ha buscado trabajo? por que no?` <- fct_collapse(Dane_2019$`En el ultimo mes ha buscado trabajo? por que no?`,
                                                                                "Ya encontre trabajo"="1","No hay trabajo disponible en la ciudad ó región / no encuentra trabajo en su oficio o profesión"="2",
                                                                                "Está esperando que lo llamen o esperando temporada alta"="3",
                                                                                "No sabe como buscarlo"="4","Está cansado de buscar"="5","Carece de la experiencia necesaria"="6",
                                                                                "No tiene recursos para instalar un negocio"="7","Los empleadores lo consideran muy joven o muy viejo"="8","Usted se considera muy joven o muy viejo"="9",
                                                                                "Responsabilidades familiares"="10","Problemas de salud"="11","Está estudiando"="12","Otro"="13")

Dane_2019$`Siendo inactivo, por que dejo de buscar trabajo?` <- fct_collapse(Dane_2019$`Siendo inactivo, por que dejo de buscar trabajo?`,
                                                                             "No hay trabajo disponible en la ciudad o región"="1","Para dedicarse a estudiar"="2",
                                                                             "No sabe como buscarlo"="3","	Por enfermedad"="4","Está cansado de buscar"="5","No encuentra el trabajo apropiado"="6",
                                                                             "Considera que no está calificado"="7","Por la edad"="8","Responsabilidades familiares"="9",
                                                                             "Jubilación o retiro"="10","No desea trabajar"="11","Otro"="12")


Dane_2019$`P6765 Que forma de trabajo -independiente- realizó?`<- fct_collapse(Dane_2019$`P6765 Que forma de trabajo -independiente- realizó?`,
                                                                             "Trabajo por honorarios o prestación de servicios"="1","Trabajo por obra"="2","Trabajó por piezas o a destajo (Satelite, maquila, etc)"="3",
                                                                             "Trabajó por comisión únicamente"="4","Trabajó vendiendo por catálogo"="5","Trabajó en su oficio (plomero, taxista, domestica por días etc)"="6",
                                                                             "Tiene un negocio de industria, comercio, servicios o una finca"="7","Otro"="8")

Dane_2019$Departamento <- fct_collapse(Dane_2019$Departamento, "Antioquia"="5",
                                           "Atlantico"="8", "Bogota"="11", "Bolivar"="13", 
                                           "Boyaca"="15", "Caldas"="17", "Caqueta"="18",
                                           "Cauca"="19", "Cesar"="20", "Cordoba"="23", 
                                           "Cundinamarca"="25", "Choco"="27",
                                           "Huila"="41", "La Guajira"="44", "Magdalena"="47", "Meta"="50",
                                           "Nariño"="52", "Norte de Santander"="54",
                                           "Quindio"="63", "Risaralda"="66", "Santander"="68",
                                           "Sucre"="70", "Tolima"="73", "Valle"="76")


#!!!!!!!!!!  RENOMBRAR VARIABLE RAMA.ACTIVIDAD
Dane_2019$Rama.actividad <-fct_collapse(Dane_2019$Rama.actividad,"No aplica"="0", "Agricultura, ganadería y caza"= "1" , "Silvicultura y extracción de madera "= "2",
                                               "Actividades de servicios relacionadas con la pesca"="5", "Extracción de minerales, petroleo, gas y carbon"=c("10","11","14"),"Elaboración de productos alimenticios y bebidas"="15",
                                                 "Fabricación de productos de tabaco"="16",
                                               "Fabricación de productos textiles"="17", "Fabricación de prendas de vestir"="18", "Curtido y preparado de cueros, fabricacion articulos de cuero"="19",
                                               "Transformacion y fabricación de productos de madera"="20","Fabricación de productos de papel y cartón"="21", "Actividades de edicion e impresion"="22",
                                               "Fabricación de sustancias y productos químicos"="24", "Fabricación de productos de caucho y de plástico"="25", "Fabricación de productos minerales no metálicos"="26",
                                               "Fabricación de productos metalúrgicos básicos"="27", "Fabricación de productos de metal, excepto maquinaria"="28", "Fabricación de maquinaria y equipo ncp"="29",
                                               "Fabricación de maquinaria y aparatos eléctricos ncp"="31", "Fabricación de equipo  de radio y televisión"="32", "Fabricación de instrumentos médicos // relojes"="33",
                                               "Fabricación de vehículos automotores"="34", "Fabricación de otros tipos de equipo de transporte"="35", "Fabricación de muebles // industrias manufactureras ncp"="36",
                                               "Reciclaje de metalicos y no metalicos"="37", "Suministro de electricidad, gas y vapor"="40", "Captación, depuración y distribución de agua"="41",
                                               "Construcción"="45", "Comercio y reparacion de veh. automotores y motocicletas; Comercio de combustible"="50", "Comercio al por mayor y en comision o por contrata"="51", 
                                                "Comercio al por menor / reparacion de efectos personales y enseres domesticos"="52",
                                                "Hoteles, restaurantes, bares y similares"="55", "Transporte por vía terrestre; transporte por tuberías"="60","Transporte por vía acuática"="61",
                                               "Transporte por vía aérea"="62", "Actividades complementarias y auxiliares al transporte / agencias de viajes"="63", "Correo y telecomunicaciones"="64", "Intermediación financiera"="65",
                                               "Financiacion de planes de seguros y pensiones"="66", "Actividades auxiliares de la intermediación financiera"="67", "Actividades inmobiliarias"="70",
                                               "Alquiler de maquinaria y equipo sin operarios y enseres domesticos"="71", "Informática y actividades conexas"="72", "Otras actividades empresariales"="74","Administración pública y defensa"="75",
                                               "Educación"="80", "Servicios sociales y de salud"="85", "Saneamiento y eliminación de desperdicios"="90", "Actividades de asociaciones ncp"="91",
                                               "Actividades culturales y deportivas"="92", "Otras actividades de servicios"="93", "Hogares privados con servicio doméstico"="95", "Organizaciones extraterritoriales"="99")

#freq(Dane_2019$Rama.actividad)
#levels(Dane_2019$Rama.actividad)
Dane_2019$Rama.actividad <- as.factor(Dane_2019$Rama.actividad)

#Esta funcion escoge como target el nivel con menos frecuencia de la variable RESPUESTA BINARIA 
#categ_analysis(data=Dane_2019, input="Rama.actividad", target = "Y_modelo1")




#-------    Construcción de variables de ingresos ("Ingresos totales en el ultimo mes")   -------

# En las variables de ingresos los NA los convertimos en cero
Dane_2019$`Cuales fueron sus ingresos como empleado en el ultimo mes?`[is.na(Dane_2019$`Cuales fueron sus ingresos como empleado en el ultimo mes?`)]=0
Dane_2019$`P6510 Cuanto recibio por horas extras`[is.na(Dane_2019$`P6510 Cuanto recibio por horas extras`)]=0
Dane_2019$`P6585S1A1 Recibio auxilio de alimentacion?`[is.na(Dane_2019$`P6585S1A1 Recibio auxilio de alimentacion?`)]=0
Dane_2019$`P6585S2A1 Recibio auxilio de transporte?`[is.na(Dane_2019$`P6585S2A1 Recibio auxilio de transporte?`)]=0
Dane_2019$`P6585S3A1 Recibio auxilio familiar`[is.na(Dane_2019$`P6585S3A1 Recibio auxilio familiar`)]=0
Dane_2019$`P6585S4A1 Recibio auxilio educativo?`[is.na(Dane_2019$`P6585S4A1 Recibio auxilio educativo?`)]=0
Dane_2019$`P6545S1 Recibio primas(Tecnica, de antiguedad, etc)?`[is.na(Dane_2019$`P6545S1 Recibio primas(Tecnica, de antiguedad, etc)?`)]=0
Dane_2019$`P6580S1 Recibio algun tipo de bonificacion?`[is.na(Dane_2019$`P6580S1 Recibio algun tipo de bonificacion?`)]=0
Dane_2019$`P6750 Ganancia neta por honorarios o negocio`[is.na(Dane_2019$`P6750 Ganancia neta por honorarios o negocio`)]=0
Dane_2019$`P7070 Cuanto fue su ingreso por un segundo trabajo?`[is.na(Dane_2019$`P7070 Cuanto fue su ingreso por un segundo trabajo?`)]=0
Dane_2019$`P7422S1 Como -Desempleado- cuanto recibio de ingresos el mes pasado`[is.na(Dane_2019$`P7422S1 Como -Desempleado- cuanto recibio de ingresos el mes pasado`)]=0
Dane_2019$`P7510S3A1 Cuanto recibio por ayudas en dinero de instituciones en el pais?`[is.na(Dane_2019$`P7510S3A1 Cuanto recibio por ayudas en dinero de instituciones en el pais?`)]=0
Dane_2019$`P750S3A1 Cuanto recibio por ayudas en dinero de instituciones fuera del pais?`[is.na(Dane_2019$`P750S3A1 Cuanto recibio por ayudas en dinero de instituciones fuera del pais?`)]=0



# En las variables de tipo Si o No los NA los convertimos en "No responde"
Dane_2019$`P6510S2 Incluyo horas extras?`[is.na(Dane_2019$`P6510S2 Incluyo horas extras?`)]="No responde"
Dane_2019$`P6585S1A2 Incluyo sub de alimentacion?`[is.na(Dane_2019$`P6585S1A2 Incluyo sub de alimentacion?`)]="No responde"
Dane_2019$`P6585S2A2 Incluyo sub de transporte?`[is.na(Dane_2019$`P6585S2A2 Incluyo sub de transporte?`)]="No responde"
Dane_2019$`P6585S3A2 Incluyo sub familiar?`[is.na(Dane_2019$`P6585S3A2 Incluyo sub familiar?`)]="No responde"
Dane_2019$`P6585S4A2 Incluyo sub educativo?`[is.na(Dane_2019$`P6585S4A2 Incluyo sub educativo?`)]="No responde"
Dane_2019$`P6545S2 Incluyo primas(Tecnica, de antiguedad, etc)?`[is.na(Dane_2019$`P6545S2 Incluyo primas(Tecnica, de antiguedad, etc)?`)]="No responde"
Dane_2019$`P6580S2 Incluyo algun tipo de bonificacion?`[is.na(Dane_2019$`P6580S2 Incluyo algun tipo de bonificacion?`)]="No responde"


Dane_2019$`P6510 Cuanto recibio por horas extras` <- as.integer(Dane_2019$`P6510 Cuanto recibio por horas extras`)
Dane_2019$`P7422S1 Como -Desempleado- cuanto recibio de ingresos el mes pasado` <- as.integer(Dane_2019$`P7422S1 Como -Desempleado- cuanto recibio de ingresos el mes pasado`)


##### 1. ASALARIADOS #####


#inicilizamos a variable
Dane_2019$Ingresos_total=Dane_2019$`Cuales fueron sus ingresos como empleado en el ultimo mes?`

# Comprobamos si la persona incluyó o no el valor de los subsidios en el
# ingreso declarado anteriormente (P6500). Si no lo incluyó, se suman.

#!!! Opté por ciclo while porque el ciclo for no me estaba actualizando los valores 

i <- 1
while (i <= nrow(Dane_2019)) {
   
   if (Dane_2019$`P6510S2 Incluyo horas extras?`[i] == "2") { # Horas extras
      
      Dane_2019$Ingresos_total[i] <- Dane_2019$Ingresos_total[i] + Dane_2019$`P6510 Cuanto recibio por horas extras`[i]
      
   } 
   if (Dane_2019$`P6585S1A2 Incluyo sub de alimentacion?`[i] == "2") { # Subsidio de alimentación
      
      Dane_2019$Ingresos_total[i] <- Dane_2019$Ingresos_total[i] + Dane_2019$`P6585S1A1 Recibio auxilio de alimentacion?`[i]
      
   } 
   if (Dane_2019$`P6585S2A2 Incluyo sub de transporte?`[i] == "2") { # Subsidio de transporte
      
      Dane_2019$Ingresos_total[i] <- Dane_2019$Ingresos_total[i] + Dane_2019$`P6585S2A1 Recibio auxilio de transporte?`[i]
      
   } 
   if (Dane_2019$`P6585S3A2 Incluyo sub familiar?`[i] == "2") { # Subsidio familiar
      
      Dane_2019$Ingresos_total[i] <- Dane_2019$Ingresos_total[i] + Dane_2019$`P6585S3A1 Recibio auxilio familiar`[i]
      
   } 
   if (Dane_2019$`P6585S4A2 Incluyo sub educativo?`[i] == "2") { # Subsidio educativo
      
      Dane_2019$Ingresos_total[i] <- Dane_2019$Ingresos_total[i] + Dane_2019$`P6585S4A1 Recibio auxilio educativo?`[i]
      
   } 
   if (Dane_2019$`P6545S2 Incluyo primas(Tecnica, de antiguedad, etc)?`[i] == "2") { # Ingresos por primas (tecnica, antiguedad)
      
      Dane_2019$Ingresos_total[i] <- Dane_2019$Ingresos_total[i] + Dane_2019$`P6545S1 Recibio primas(Tecnica, de antiguedad, etc)?`[i]
      
   } 
   if (Dane_2019$`P6580S2 Incluyo algun tipo de bonificacion?`[i] == "2") { # Por algún tipo de bonificación mensual
      
      Dane_2019$Ingresos_total[i] <- Dane_2019$Ingresos_total[i] + Dane_2019$`P6580S1 Recibio algun tipo de bonificacion?`[i]
   }
   i <- i+1
}

##### 2. NO ASALARIADOS #####

# Todos los que no son asalariados tienen un cero en la variable p6750, por lo tanto no afecta sumar los valores.
Dane_2019$Ingresos_total <- Dane_2019$Ingresos_total + Dane_2019$`P6750 Ganancia neta por honorarios o negocio`


##### 3. DESEMPLEADOS ####

# Todos los que no son asalariados tienen un cero en la variable p6750, por lo tanto no afecta sumar los valores.
Dane_2019$Ingresos_total <- Dane_2019$Ingresos_total + Dane_2019$`P7422S1 Como -Desempleado- cuanto recibio de ingresos el mes pasado`


##### 4. PARA DOS O LAS 3 OCUPACIONES  #####

# Ingresos por trabajos secundarios y por ayudas de entidades nacionales o internacionales
Dane_2019$Ingresos_total <- Dane_2019$Ingresos_total + Dane_2019$`P7070 Cuanto fue su ingreso por un segundo trabajo?`
Dane_2019$Ingresos_total <- Dane_2019$Ingresos_total + Dane_2019$`P7510S3A1 Cuanto recibio por ayudas en dinero de instituciones en el pais?`
Dane_2019$Ingresos_total <- Dane_2019$Ingresos_total + Dane_2019$`P750S3A1 Cuanto recibio por ayudas en dinero de instituciones fuera del pais?`

#Dane_2019_2 <- filter(Dane_2019, !(ocupacion%in%c("Empleo informal", "Empleo formal", "Desempleado") & Ingresos_total %in% 1:200 |
 #                                   Edad > 60 & Ingresos_total %in% 1:200)) #Eliminamos personas que no saben sus ingresos

write.csv2(Dane_2019, "Dane_2019_reducida.csv")

#describe(Dane_2019$Ingresos_total)
#ggplot(Dane_2019, aes(x=ocupacion, y=Ingresos_total, fill=ocupacion)) + geom_boxplot() + 
#   ggtitle("Ingresos totales por sector de población")+
#   xlab("")+ ylab("Monto $") + theme_bw()




#-------.-----     
#--------------------   Selección final de variables multiproposito   -----------------------



Dane_2019_final <-  Dane_2019

# Seleccionamos las variables de interes
Dane_2019_final  <- Dane_2019_final %>% select(Identificacion,Departamento,Grupo.de.poblacion, Genero, Edad, Parentesco, Estado.civil, Nivel.educativo.alcanzado, 
                                                Rama.actividad,tiempo.en.colombia,Horas.disponibles, ocupacion, Y_modelo1, Y_modelo2,Mes, anio,
                                               `Siendo inactivo, por que dejo de buscar trabajo?`,
                                               `En el ultimo mes ha buscado trabajo? por que no?`, `P1807 -inactivo-, Salario minimo que aceptaria`,
                                               `Cuales fueron sus ingresos como empleado en el ultimo mes?`, `P6750 Ganancia neta por honorarios o negocio`,
                                               `P7250 Semanas que lleva -Desocupado- buscando trabajo, o que estuvo buscando`, `P1806 Como -Desocupado-, cual seria la remuneracion minima mensual por la cual aceptaria un trabajo?`,
                                               `P6426 Tiempo trabajando de manera continua (meses)`,`P6125 En los últimos doce meses dejó de asistir al médico por no tener con que pagar estos servicios?`,
                                               `P6170 Asiste a una entidad educativa?`, `P6240 Que fue lo que mas hizo la semana pasada?`, `P6440 Como -Asalariado-, cuenta con algun tipo de contrato?`,
                                               `P6430 En este trabajo usted es`, `P6480 Por cual medio consiguio su trabajo actual?`, `P9440 Su trabajo actual lo consiguio a traves de internet?`,
                                               `P1800 Como -Independiente-, tiene empleados o personas que lo ayuden`, `P1879 Por que trabaja como independiente?`, `P6880 Donde realiza su trabajo?`,
                                               `P6915 En caso de enfermedad, como cubre los costos?`, `P7090 Trabaja menos horas de las que puede o quisiera?`,
                                               `P7130 Desea cambiar su trabajo actual?`, `P7170S1 Esta satisfecho con su trabajo actual?`,
                                               `P514 Considera que su trabajo es estable?`, `P515 Su horario de trabajo y responsabilidades familiares son compatibles?`,
                                               `P1881 Que medio de transporte utiliza para dirigirse a su sitio de trabajo?`,`P7240 Si no tiene empleo de donde obtendria los recursos?`,
                                               `P7390 Como -Desocupado-, en caso de enfermedad, como cubre los costos?`,`P6765 Que forma de trabajo -independiente- realizó?`,
                                               `P6160 Sabe leer y escribir?`, Ingresos_total)


#Edad y horas disponibles modo categoria
Dane_2019_final$Edad_categ <-  cut(Dane_2019_final$Edad,  breaks = c(0,11,14,18, 26, 59, 100), include.lowest = T )
Dane_2019_final$Horas.disponibles_categ <-  cut(Dane_2019_final$Horas.disponibles,  breaks = c(0,1,39, 48, 130), include.lowest = T )
Dane_2019_final$Horas.disponibles_categ <- as.factor(Dane_2019_final$Horas.disponibles_categ)
Dane_2019_final$Horas.disponibles_categ <- fct_collapse(Dane_2019_final$Horas.disponibles_categ,"0"="[0,1]" )

View(df_status(Dane_2019_final))

            #-------  Para analisis de regresion   -----------
#-------  Todos Los ingresos los traemos a pesos del 2021   -----------

# Los ingresos del 2018, 2019 y 2020 se encuentran en pesos nominales o corrientes
# Para hacerlos comparables con los ingresos declarados del 2021
# es necesario hacer la conversión a valores reales, y de esta manera
# tener en cuenta la inflación de un año al otro


#!!!!!!!!!!!   Escogemos como periodo base el periodo de Marzo de 2021   !!!!!!!!!!!!!!!

# Los IPC (indices) los obtenemos del DANE, mes tras mes, año tras año

anio <- c("2018", "2019","2020","2021")
IPC_marzo <- c(98.45,101.62,105.53,107.12)

tabla_conversion <- as_tibble(cbind(anio, IPC_marzo))
tabla_conversion$IPC_marzo <-  as.numeric(tabla_conversion$IPC_marzo)

# para calcular el valor real de cada individuo en comparación con el mes de marzo 
# de 2021, realizamos una regla de 3: Multiplicamos el valor nominal con el IPC de 2021
# y lo dividimos por el IPC del año del individuo.

i <- 1
while (i<=nrow(Dane_2019_final)) {
   
   if (Dane_2019_final$anio[i] == "2019") {
      
      Dane_2019_final$Ingresos_total[i] <- (Dane_2019_final$Ingresos_total[i]/tabla_conversion$IPC_marzo[2])*tabla_conversion$IPC_marzo[4]
      
   }
   else if (Dane_2019_final$anio[i] == "2018") {
      
      Dane_2019_final$Ingresos_total[i] <- (Dane_2019_final$Ingresos_total[i]/tabla_conversion$IPC_marzo[1])*tabla_conversion$IPC_marzo[4]
      
   }
   
   else if (Dane_2019_final$anio[i] == "2020") {
      
      Dane_2019_final$Ingresos_total[i] <- (Dane_2019_final$Ingresos_total[i]/tabla_conversion$IPC_marzo[3])*tabla_conversion$IPC_marzo[4]
      
   }
   i <- i+1
}



write.csv2(Dane_2019_final, "Dane_2019_Para_analisis.csv")




































