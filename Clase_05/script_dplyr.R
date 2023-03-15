# ----------------------------------------------------------
# Clase 05 - Script Manipulación de datos con tidyr y dplyr
# Dr. José Gallardo Matus
# 22 octubre 2022
# Curso Análisis de expresión diferencial de genes con R
# ----------------------------------------------------------

# Limpia objetos del ambiente de trabajo
ls()
rm(list=ls())

# Habilita librerías
library(readxl) # Para importar datos desde excel a R

library(tidyr) # Para manipular datos

library(dplyr) # Para manipular datos

library(ggplot2) # Para hacer gráficos


# LIBRERÍA DPLYR: EL OPERADOR PIPE (TUBERÍA).

# dplyr usa el operador pipe %>% como una tubería para enlazar un data.frame con una o más funciones.

x <- rnorm(5)
y <- rnorm(5)
dat <- data.frame(x,y)
dat
max(dat) 
dat %>% max

# Importa base de datos en formato .xlsx
RNA_sample_NA <- read_excel("RNA_sample_NA.xlsx",
                         col_types = c("text","text", "numeric", "numeric",
                                       "numeric", "numeric", "numeric"))

# Las variables de clasificacion se deben transformar a factores.
RNA_sample$Type <- as.factor(RNA_sample$Type)
RNA_sample$Sample <- as.factor(RNA_sample$Sample)
summary(RNA_sample_NA)

# Seleccionar columnas
RNA_sample_NA %>% select(Type, RIN)
RNA_sample_NA %>% select(Sample, `260_280`, `260_230`, RIN)

# Filtrar variable peso con tubería por categoría
RNA_sample_NA %>% filter(Type=="Human")

# MÚLTIPLES FUNCIONES Y TUBERÍAS
RNA_sample_NA %>% select(Type, RIN) %>% filter(Type=="Human")

# Eliminar muestras con baja calidad
RNA_sample_NA %>% filter(RIN>7)
RNA_sample_NA %>% filter(`260_230`>1)

# datos filtrados
dat_qc <- RNA_sample_NA %>% filter(RIN>7) %>% filter(`260_230`>1)
dat_qc

# FUNCIÓN SUMMARIZE()
dat_qc %>% summarize(n = n(), 
                Promedio_RNA = mean(RNA_ug), 
                Promedio_RIN = mean(RIN))

# FUNCIÓN GROUP_BY() + SUMMARIZE()
# Permite agrupar filas con base a los niveles de alguna variable o factor.
dat_qc %>% group_by(Type) %>% 
  summarize(n = n(), 
            Promedio_RIN = mean(RIN), 
            sd_RIN = sd(RIN))

RIN_table <- dat_qc %>% group_by(Type) %>% 
  summarize(n = n(), 
            Promedio_RIN = mean(RIN), 
            sd_RIN = sd(RIN))

# FUNCIÓN MUTATE()
# Permite calcular nuevas variables "derivadas", ej. proporciones, tasas, log.
# Calcularemos el coeficiente de variación como  sd / mean * 100
RIN_table %>% mutate(CV_RIN = sd_RIN/Promedio_RIN*100)
