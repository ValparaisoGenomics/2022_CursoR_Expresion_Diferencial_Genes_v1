# ------------------------------------------------------------------------------
# Clase 08 - Calculo de la expresion genica
# Dra. Debora Torrealba Sandoval
# 29 de octubre de 2022
# Analisis de expresion diferencial de genes e investigacion reproducible con R.
# ------------------------------------------------------------------------------

#Introduccion

# Delta-Delta CT o metodo de Livak

# Este método asume que tanto los genes de interes como los de referencia son 
# amplificados con eficiencias cercanas al 100% y dentro del 5% de diferencia entre si. 
# Antes de usar el metodo Delta-delta CT, es esencial verificar los supuestos de  
# eficiencias de amplificacion de los cebadores de los genes de interes y de 
# referencia. 
  

# Actividad 1: Realizar analisis del set de datos con el metodo Delta-Delta CT


# Remover objetos de la sesion de trabajo

rm(list = ls())

# Instalacion de librerias

library(readxl)
library(ggplot2)
library(dplyr)
library(cowplot)

# Analisis con metodo Delta Delta CT

# Exportacion de datos

Ctdata <- read_excel("Datos.xlsx")

Ctdata

# Analisis Gen 1

# Promedio de los Ct del gen de referencia para las muestras control 

Prom_Ref_C<-Ctdata%>%
            select(Replica,Treatment,Tissue,Gen_ref)%>%
            filter(Treatment=="Control", Tissue=="Cerebro")%>%
            summarize(Prom_Ref_C=mean(Gen_ref))

class(Prom_Ref_C)

Prom_Ref_C<-as.numeric(Prom_Ref_C)

#  Promedio de los Ct del gen de interes para las muestras control

Prom_Int_C<-Ctdata%>%
  select(Replica,Treatment,Tissue,Gen1, Eficiencia)%>%
  filter(Treatment=="Control", Tissue=="Cerebro")%>%
  summarize(Prom_Int_C=mean(Gen1))

class(Prom_Int_C)

Prom_Int_C<-as.numeric(Prom_Int_C)

# Normalizar el CT del gen de interes al del gen de referencia para muestra control  
GR_GI<-(Prom_Int_C-Prom_Ref_C)
GR_GI


# Calculo Expresion
ExpresionG1<-Ctdata%>%
             select(Replica,Treatment,Tissue,Gen_ref, Gen1, Eficiencia)%>%  #Selecciona Gen 1
             filter(Treatment %in% c("Control","A","B","C"))%>% # Selecciona tratamientos, el control queda con valor 1 
             mutate(GR_GI_T=Gen1-Gen_ref)%>% # GR_GI_T= Resta del Ct del gen de referncia al Ct del gen de interes por cada muestra
             mutate(Trat_ctrl=GR_GI_T-GR_GI)%>% # Resta de GR_GI de los controles al GR_GI_T de los tratamientos por cada muestra            
             mutate(Expresion=2^-(Trat_ctrl)) # 2^Delta-Delta CT= Relacion de expresion normalizada

# Promedios y desviaciones estandar por tratamiento

Resumen_G1 <- ExpresionG1 %>% select(Treatment, Expresion) %>% group_by(Treatment) %>%
            summarize(n=n(),
                      Mean_Expresion=mean(Expresion),
                      sd_Expresion=sd(Expresion))

# Grafico de barras Fold-change

Gen1_ggplot(Resumen_G1, aes(x=Treatment, y=Mean_Expresion, fill=Treatment)) + 
           geom_bar(stat="identity", color="black", position=position_dodge()) +
           geom_errorbar(aes(ymin=Mean_Expresion-sd_Expresion, ymax=Mean_Expresion+sd_Expresion), width=.2,
                position=position_dodge(.9))+
           theme(legend.position="none")+
           labs(y="Fold Change (2^ddCt)",title="Gen 1")


# Repetir este procedimiento para Gen 2 y Gen 3




# Unificar graficos
#Utilizar el sicguiente codigo para unificar las graficas de los genes 1, 2 y 3.

plot_grid(Gen1, Gen2, Gen3,labels=c("A", "B", "C"), nrow = 3)



# Método Pfaffl

# El método Pfaffl solo es válido cuando las eficiencias de amplificación de los 
# genes de ineterés y de referencia son similares. Si las eficiencias de amplificación 
# de los cebadores no son similares se debe usar una fórmula alternativa 
# para determinar la expresión relativa del gen de interes. 


# Actividad 2: Realizar analisis del set de datos con el metodo Pfaffl

# Eficiencia gen de referencia: 2.05 ((2.05-1)*100 =105%)
# Eficiencia gen de interes: 1.98 ((1.98-1)*100 =98%)

# Promedio de los controles del gen de referencia

Prom_Ref_CP<-Ctdata%>%
  select(Replica,Treatment,Tissue,Gen_ref)%>%
  filter(Treatment=="Control", Tissue=="Cerebro")%>%
  summarize(Prom_Ref_CP=mean(Gen_ref))

class(Prom_Ref_CP)

Prom_Ref_CP<-as.numeric(Prom_Ref_CP)

# Promedio control del gen de interes
Prom_Int_CP<-Ctdata%>%
  select(Replica,Treatment,Tissue,Gen1, Eficiencia2)%>%
  filter(Treatment=="Control", Tissue=="Cerebro")%>%
  summarize(Prom_Int_CP=mean(Gen1))

class(Prom_Int_CP)

Prom_Int_CP<-as.numeric(Prom_Int_CP)


# Calculo de expresion
Expresion_Pfaffl_G1<-Ctdata%>%
  select(Replica,Treatment,Tissue,Gen_ref, Gen1, Eficiencia2)%>%
  mutate(DeltaGR_CP=Prom_Ref_CP-Gen_ref, E_DeltaGR_CP=Eficiencia2^(DeltaGR_CP))%>%
  mutate(DeltaGI_IP=Prom_Int_CP-Gen1, E_DeltaGI_CP=Eficiencia2^(DeltaGI_IP))%>%
  mutate(ExpresionP=E_DeltaGI_CP/E_DeltaGR_CP)


# Promedios y desviaciones estandar por tratamiento
Resumen_Pfaffl_G1 <- Expresion_Pfaffl_G1 %>% select(Treatment, ExpresionP) %>% group_by(Treatment) %>%
                      summarize(n=n(),
            Mean_ExpresionP=mean(ExpresionP),
            sd_ExpresionP=sd(ExpresionP))

# Grafico de barras Fold-change

Pfaffl_Gen1<- ggplot(Resumen_Pfaffl_G1, aes(x=Treatment, y=Mean_ExpresionP, fill=Treatment)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Mean_ExpresionP-sd_ExpresionP, ymax=Mean_ExpresionP+sd_ExpresionP),
                width=.2, position=position_dodge(.9))+
  theme(legend.position="none")+
  labs(y="Fold Change (Pfaffl method)",title="Gen 1")

Pfaffl_Gen1

# Repetir para Gen2



# Repetir analisis de Gen 1 y Gen 2, pero usando eficiencias de cebadores menos optimos
# usar los datos de la columna Eficiencia2

# Calcular porcentaje de eficiencia de los cebadores utilizados
# Eficiencia 2 gen de referencia: 
# Eficiencia 2 gen de interes: 

# Analisis Gen 1

# Analisis Gen 2



# Unificar graficos
#Utilizar el siguiente codigo para unificar las graficas de los genes 1, 2 y 3.

plot_grid(Pfaffl_Gen1, Pfaffl_Gen1b, Pfaffl_Gen2, Pfaffl_Gen2b, labels=c("A", "B", "C", "D"), ncol= 2, nrow = 2)




# Responder

# a) ¿Que diferencias visualiza en la expresión genica obtenida entre los metodos 
# Delta-Delta CT y Pfaffl para los genes analizados?

# b) ¿Que diferencia observa en los genes 1 y 2 analizados con diferente eficencia de
# cebadores?
