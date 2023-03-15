# ----------------------------------------------------------
# Clase 09 - Script Análisis exploratorio hkg
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

# Importa base de datos de CT de HK gene de Nogal en formato MESSY
messy <- hkg_nogal <- read_excel("hkg_nogal.xlsx", 
                                 col_types = c("text", "text", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric"))

head(messy)  # Muestra los primeros datos del data set
summary(messy)

# Correlación entre muestras
cor(messy[3:9], method = "pearson")
pairs.panels(messy[3:9])

# Colapsar columnas con Ct
messy %>% gather("Gen","Ct",3:9)

tidy <- messy %>% gather("Gen","Ct",3:9)

# Las variables de clasificación se deben transformar a factores.
tidy$Sample <- as.factor(tidy$Sample)
tidy$Tissue <- as.factor(tidy$Tissue)
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
My_Theme = theme(
  plot.title = element_text(size = 18),
  axis.title.x = element_text(size = 10),
  axis.text.x = element_text(size = 10),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 16))

ggplot(tidy, aes(x=Gen, y= Ct, fill=Gen))+
  geom_boxplot()+
  labs(title="Boxplot de ct", x="TGen", y="Ct") +
  My_Theme

# Crear boxplot por tejido
ggplot(tidy, aes(x=Gen, y= Ct, fill=Gen))+
  geom_boxplot()+
  labs(title="Boxplot de ct", x="HK Gene", y="Ct") +
  facet_wrap(~ Tissue)+
  My_Theme

# Crear boxplot por gen
ggplot(tidy, aes(x=Tissue, y= Ct, fill=Tissue))+
  geom_boxplot()+
  labs(title="Boxplot de ct", x="Tissue", y="Ct") +
  facet_wrap(~ Gen)+
  My_Theme

# Crear boxplot por gen con escala independiente
ggplot(tidy, aes(x=Tissue, y= Ct, fill=Tissue))+
  geom_boxplot()+
  labs(title="Boxplot de ct", x="Tissue", y="Ct") +
  facet_wrap(~ Gen, scales="free")+
  My_Theme

# Crear boxplot por gen con escala independiente y borrando etiquetas del eje x para que no solapen
My_Theme = theme(
  plot.title = element_text(size = 18),
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 16))

ggplot(tidy, aes(x=Tissue, y= Ct, fill=Tissue))+
  geom_boxplot()+
  labs(title="Boxplot de ct", x="Tissue", y="Ct") +
  facet_wrap(~ Gen, scales="free")+
  My_Theme
