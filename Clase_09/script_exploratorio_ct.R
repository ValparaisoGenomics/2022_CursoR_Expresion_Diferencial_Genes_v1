# ----------------------------------------------------------
# Clase 09 - Script Análisis exploratorio de datos de Ct
# Dr. José Gallardo Matus
# 29 octubre 2022
# Curso Análisis de expresión diferencial de genes con R
# ----------------------------------------------------------

# Remover objetos de la sesi?n de trabajo
rm(list = ls())

# habilitar paquetes ggplot2
library(readxl) # Paquete para importar datos desde excel
library(tidyr) # Paquete para manipular datos
library(dplyr) # Paquete para manipular datos
library(ggplot2) # Paquete para hacer lindos gráficos en R
library(psych) # Graficas de correlación

# Importa base de datos de CT en formato MESSY
messy <- read_excel("Delta-delta-method-template.xlsx")
head(messy)  # Muestra los primeros datos del data set
summary(messy)

# Correlación entre muestras
cor(messy[3:6], method = "pearson")
pairs.panels(messy[3:6])

# Colapsar columnas con Ct
messy %>% gather("Gen","Ct",3:6)

tidy <- messy %>% gather("Gen","Ct",3:6)

# Las variables de clasificación se deben transformar a factores.
tidy$Tratamiento <- as.factor(tidy$Tratamiento)
tidy$Sample <- as.factor(tidy$Sample)
tidy$Gen <- as.factor(tidy$Gen)
summary(tidy)

# Crear histograma con ggplot2
ggplot(tidy, aes(Ct))+
  geom_histogram(color="white", fill="blue", bins = 10)+
  labs(title="Histograma", x="Ct", 
       y="Frecuencia")

# Modificamos tamaño de etiquetas
My_Theme = theme(
  plot.title = element_text(size = 18),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 16))

ggplot(tidy, aes(Ct))+
  geom_histogram(color="white", fill="blue", bins = 10)+
  labs(title="Histograma de CT", x="Ct", 
       y="Frecuencia") +
  My_Theme

# Crear boxplot
ggplot(tidy, aes(x=Gen, y= Ct))+
  geom_boxplot(color="blue")+
  labs(title="Boxplot de ct", x="TGen", y="Ct") +
  My_Theme

# Crear boxplot por tratamiento
ggplot(tidy, aes(x=Gen, y= Ct))+
  geom_boxplot(color="blue")+
  labs(title="Boxplot de ct", x="Gen", y="Ct") +
  facet_wrap(~ Tratamiento)+
  My_Theme

# Crear boxplot por gen
ggplot(tidy, aes(x=Tratamiento, y= Ct))+
  geom_boxplot(color="blue")+
  labs(title="Boxplot de ct", x="Tratamiento", y="Ct") +
  facet_wrap(~ Gen)+
  My_Theme

# Crear boxplot por gen con escala independiente
ggplot(tidy, aes(x=Tratamiento, y= Ct))+
  geom_boxplot(color="blue")+
  labs(title="Boxplot de ct", x="Tratamiento", y="Ct") +
  facet_wrap(~ Gen, scales = "free")+
  My_Theme
