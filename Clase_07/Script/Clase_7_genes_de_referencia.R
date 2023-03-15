# ------------------------------------------------------------------------------
# Clase 07 - Genes de referencia
# Dra. Débora Torrealba Sandoval
# 27 de octubre de 2022
# Análisis de expresión diferencial de genes e investigación reproducible con R.
# ------------------------------------------------------------------------------


# Remover objetos de la sesión de trabajo
rm(list = ls())

# Instalar librerías
library(readxl)
library(ggplot2)
library(ggpubr)

# Importar y explorar set de datos llamado "Datos_a"
Datos_a <- read_excel("Datos_a.xlsx", sheet = 1)
head(Datos_a)
summary(Datos_a)
Datos_a$Gen <- as.factor(Datos_a$Gen)
Datos_a$Replica <- as.factor(Datos_a$Replica)
Datos_a$Tipo <- as.factor(Datos_a$Tipo)
summary(Datos_a)

# Histograma datos
ggplot(Datos_a, aes(Ct)) +
  geom_histogram(color="white", fill="blue", bins = 10)+
  facet_wrap(~Tipo)

# Bloxplot por tipo de muestra
Blox1<- Datos_a %>%
        ggplot(aes(x=Tipo, y= Ct, fill=Tipo)) +
        geom_boxplot() +
        labs(y="Ct", x="Tipo de muestra", 
            subtitle="Variación de Ct por tipo de muestra")

Blox1

# Bloxplot por gen
Blox2<- Datos_a %>%
  ggplot(aes(x=Gen, y= Ct, fill=Gen)) +
  geom_boxplot() +
  labs(y="Ct", x="Genes", 
       subtitle="Variación de Ct por gen y tipo de muestra")+
  facet_wrap(~Tipo)

Blox2

# Instalar NormFinder para R

source("r.NormOldStab5.txt")

# Guardar los datos como datos_a.txt (Texto delimitado por tabulaciones)

# Analizar set de datos "Datos_a.txt" con NormFinder

Results=Normfinder("Datos_a.txt")

# Visualizar resultados

Results

# Interpretación de los datos

# De los resultados se obtiene una primera tabla de 4 columnas:
# 1) Nombre de los genes
# 2) GroupDif:Es una medida de la diferencia entre los grupos
# 3) GroupSD: Desviación estandar entre grupos
# 4) Stanility: Contiene la estabilidad de los genes

# La segunda tabla que arroja los resultados contiene las columnas mencionadas anteriormente,
# e incluye las desviaciones estándar individuales para cada grupo (*IGroupSD*) y las 
# diferencias de grupos individuales (*IGroupDif*).

# La tercera tabla de los resultados muestra la mejor combinación de genes de referencia 
# en el caso que se quieran usar dos genes en un mismo estudio.

# Responder:

# a) ¿Cuál es el gen de referencia más estable y menos estable en el análisis?
# b) ¿Cuál es la mejor combinación de genes de referencia en base a los datos analizados? 
#    ¿Son los dos mejores genes obtenidos en el análisis?






# Importar y explorar set de datos llamado "Datos_b"

# Set de datos del artículo "Reference genes for real-time RT-PCR expression
# studies in an Antarctic Pseudomonas exposed to different temperature
# conditions" García-Laviña et al., 2019.


# Histograma datos


# Bloxplot por tipo de muestra


# Bloxplot por gen


# Instalar NormFinder para R



# Guardar los datos como datos_b.txt (Texto delimitado por tabulaciones)

# Analizar set de datos "Datos_b.txt" con NormFinder



# Visualizar resultados



# Interpretación de los datos

# De los resultados se obtiene una primera tabla de 4 columnas:
# 1) Nombre de los genes
# 2) GroupDif:Es una medida de la diferencia entre los grupos
# 3) GroupSD: Desviación estandar entre grupos
# 4) Stanility: Contiene la estabilidad de los genes

# La segunda tabla que arroja los resultados contiene las columnas mencionadas anteriormente,
# e incluye las desviaciones estándar individuales para cada grupo (*IGroupSD*) y las 
# diferencias de grupos individuales (*IGroupDif*).

# La tercera tabla de los resultados muestra la mejor combinación de genes de referencia 
# en el caso que se quieran usar dos genes en un mismo estudio.

# Responder:

# a) ¿Cuál es el gen de referencia más estable y menos estable en el análisis?
# b) ¿Cuál es la mejor combinación de genes de referencia en base a los datos analizados? 
#    ¿Son los dos mejores genes obtenidos en el análisis?

# Repetir con set de datos "Datos_b".

































































# Respuesta a ejercicios

# Set de Datos_a

# a) El gen que muestra mayor estabilidad es B2M con valor de estabilidad de 0.35. 
#    El gen que muestra menor estabilidad es TBP con un valor de estabilidad de 1.52. 

# b) El mejor par de genes de referencias es B2M y ACTB con un valor de estabilidad de 0.52.
#    No son los dos mejores genes de referencia, estos eran B2M y r28s.


# Set de Datos_b

# a) El gen que muestra mayor estabilidad es rpoD con valor de estabilidad de 0.89. 
#    El gen que muestra menor estabilidad es gap con un valor de estabilidad de 2.75. 

# b) El mejor par de genes de referencias es 16SrRNA y ftsZ con un valor de estabilidad de 0.55.
#    No son los dos mejores genes de referencia, estos eran rpoD y ftsZ.


