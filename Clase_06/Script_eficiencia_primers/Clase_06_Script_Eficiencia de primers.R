# ------------------------------------------------------------------------------
# Clase 06 - Evaluación de la eficiencia de primers
# Dra. Debora Torrealba Sandoval
# 22 de octubre de 2022
# Análisis de expresión diferencial de genes e investigación reproducible con R.
# ------------------------------------------------------------------------------

# Remover objetos de la sesión de trabajo
rm(list = ls())

# Instalar librerías
library(readxl)
library(ggplot2)
library(ggpubr)

# Importar y explorar datos
Data <- read_excel("Eficiencia_cebadores.xlsx", sheet = 1)
head(Data)
summary(Data)
Data$Gen <- as.factor(Data$Gen)
Data$Replica <- as.factor(Data$Replica)
summary(Data)

# Histograma
ggplot(Data, aes(Ct)) +
  geom_histogram(color="white", fill="blue", bins = 10)

# Grafica de puntos
ggplot(Data, aes(x=Dilucion, y= Ct))+
  geom_point(color="blue", fill = "white", shape = 21)

# Grafica de puntos con logaritmo de dilución
ggplot(Data, aes(x=log10(Dilucion), y= Ct))+
  geom_point(color="blue", fill = "white", shape = 21)

# Grafica de puntos con logaritmo de dilución por gen
ggplot(Data, aes(x=log10(Dilucion), y= Ct))+
  geom_point(color="blue", fill = "white", shape = 21)+
  facet_wrap( ~ Gen)

# Grafica + linea de regresión
ggplot(Data, aes(x=log10(Dilucion), y= Ct))+
  geom_point(color="blue", fill = "white", shape = 21)+
  geom_smooth(method = "lm")+
  facet_wrap( ~ Gen)

# Grafica + linea de regresion + ecuación + R2
ggplot(Data, aes(x=log10(Dilucion), y= Ct))+
  geom_point(color="blue", fill = "white", shape = 21)+
  geom_smooth(method = "lm")+
  stat_regline_equation(label.y = 29, label.x = -5.5, aes(label = ..rr.label..)) + 
  stat_regline_equation(label.y = 35, label.x = -5.5, aes(label = ..eq.label..))+
  facet_wrap( ~ Gen)

# Calculo de eficiencia genes individuales
# Crear segundo objeto con valores promedio
Data2 = aggregate(Data$Ct, by = list(Data$Dilucion, Data$Gen), FUN = mean, na.rm = TRUE)
colnames(Data2)<- c("Dilucion","Gen","Promct")

# Calcular el logaritmo 
Data2$Logsamplequantity = log10(Data2$Dilucion)

# Evaluación del gen CD4

CD4Data <- Data2[1:6,]

# Generación de gráfico
PlotCD4 <- qplot(Logsamplequantity, Promct, data=CD4Data) + geom_smooth(method = "lm") +  
      stat_regline_equation(label.y = 29, label.x = -5.5, aes(label = ..rr.label..)) + 
      stat_regline_equation(label.y = 35, label.x = -5.5, aes(label = ..eq.label..))

# Visualizar gráfico

PlotCD4

# Regresión lineal

x1 <- CD4Data$Logsamplequantity
y1 <- CD4Data$Promct
plot(x1, y1)
CD4Amodel <- lm(y1~x1)
CD4Amodel
CD4Amodel_summary <- summary(CD4Amodel)
slopeCD4value <- CD4Amodel_summary$coefficients[2,1]

# Pendiente de la curva

slopeCD4value

# Eficiencia del cebador en porcentaje

(10^(-1/slopeCD4value)-1)*100


# Evaluación del gen CD56

CD56Data <- Data2[7:12,]

# Generación de gráfico
PlotCD56 <- qplot(Logsamplequantity, Promct, data=CD4Data) + geom_smooth(method = "lm") +  
  stat_regline_equation(label.y = 29, label.x = -5.5, aes(label = ..rr.label..)) + 
  stat_regline_equation(label.y = 35, label.x = -5.5, aes(label = ..eq.label..))

# Visualizar gráfico

PlotCD56

# Regresión lineal

x1 <- CD56Data$Logsamplequantity
y1 <- CD56Data$Promct
plot(x1, y1)
CD56Amodel <- lm(y1~x1)
CD56Amodel
CD56Amodel_summary <- summary(CD56Amodel)
slopeCD56value <- CD56Amodel_summary$coefficients[2,1]

# Pendiente de la curva

slopeCD56value 

# Eficiencia del cebador en porcentaje

(10^(-1/slopeCD56value)-1)*100

# Repetir para los genes cd56, CD8, IFNy, IL-10 y MHC-I.

# Responder

# a) ¿Cuales son los porcentajes de eficiencia de los cebadores evaluados?
  
# b) ¿Que par de cebadores se obtuvo el porcentaje más alto y bajo de eficiencia? Explique que factores pueden influir en este porcentaje.

# c) ¿Que primers efectivamente pueden ser usados para un análisis de expresión génica?






















































# Respuesta a ejercicios

# a) Eficiencia de los cebadores evaluados:
# cd4   = 94,93%
# cd56  = 73,18%
# cd8   = 87,98%
# INFy  = 102,12%
# IL-10 = 282,59%
# MHCI  = 108,74%


# b) El porcentaje más alto de eficiencia fue de 282,59% de los cebadores del gen IL-10, 
# el porcentaje de eficiencia más bajo fue de 73,18% de los cebadores del gen cd56.

# Eficiencias sobre el 110% se pueden deber a errores de pipeteo en las diluciones 
# seriadas. La presencia de inhibidores de la PCR en uno o más de los reactivos puede 
# producir eficiencias de más del 110%. Esto puede resultar en un aumento aparente en la 
# eficiencia, ya que las muestras con mayor concentración de ADNc también tienen el nivel 
# más alto de inhibidores, que causan un retraso en la CT, mientras que las muestras con 
# concentraciones de ADNc más bajas tienen niveles más bajos de inhibidores, por lo que 
#la CT es minimamente retrasada. Como resultado, el valor absoluto de la pendiente
# disminuye y la eficiencia calculada parece aumentar. 

# Eficiencias de los cebadores por bajo el 90% se puede deber a diversos factores como la
# longitud, la estructura secundaria y el contenido de GC del cebador. Otras razones que 
# pueden influir en la eficiencia son la din?mica de la propia reacción, el uso de 
# reactivos no óptimos, concentraciones y la calidad de la enzima, lo que puede resultar 
# en eficiencias por debajo del 90%. Tambi?n se puede deber a la coamplificación de 
# productos no específicos, como los dímeros de cebadores. 


# c) Se pueden utilizar los cebadores que obtuvieron un porcentaje de eficiencia de entre 
# un 90 y 110%, estos son cd4, INFy y MHCI.

