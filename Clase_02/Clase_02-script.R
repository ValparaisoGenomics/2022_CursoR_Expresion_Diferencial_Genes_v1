# ----------------------------------------------------------
# Clase 02 - Programación con R
# Dr. José Gallardo Matus
# 15 octubre 2022
# Curso Análisis de expresión diferencial de genes con R
# ----------------------------------------------------------

# R reconoce funciones matemáticos
29+29
29*29
29==29

# Error en R
29 + diez

# Crear un objeto
diez <- 10
29 + diez

# Crea un objeto llamado cromosomas.
# Agrega en el objeto cromosomas el número 23, para representar el número los cromosomas humanos

# Version R
R.version.string

# ¿Como citar R?
citation()

# En que directorio estoy
getwd()

# Listar paquetes disponibles en mi entorno de trabajo
search()

# Listar archivos en el directorio actual
list.files()

# Crear un objeto
Nombres <- c("José Gallardo", "Debora Torrealba")

# Características de un objeto
class(Nombres)
colnames(Nombres)

Sexo <- c(1,2) # Codificamos 1= varón ; 2= mujer
Estatura <- c(1.73,1.63) # Punto indica decimales.
Albinismo <- c(0,0) # # Codificamos 0= No ; 1= Si
Genotipo <- c("TT","Tt")
Profesores <- data.frame(Nombres,Sexo,Estatura,Albinismo,Genotipo)

# Use los comandos class(), dim(), str() y summary() para identificar atributos de Sexo, Profesores


# Explorar un objeto con [] y con $
Profesores[1,1]
Profesores[1,]
Profesores[2,6] # da NULL
Profesores[c(1:2),"Albinismo"]
Profesores$Nombres
Profesores$Nombres[-1] # excluye datos


# Usando [] y $ extraer y excluir datos de Genotipo desde el data.frame Profesores


# Listar objetos 
ls()

# Obtener ayuda de un comando
help("mean")
mean(Profesores$Estatura)
mean(Profesores$Nombres) # da error porque nombres no es numérico.
Profesores$Sexo <- as.factor(Profesores$Sexo)
mean(Profesores$Sexo)
class(Profesores$Sexo)
Profesores$Sexo

# Librerías e importar datos desde excel 

# ¿Cómo instalar paquetes?
# install.packages("readxl")

# ¿Cómo habilitar paquetes?
library(readxl)

# Importa base de datos en formato .xlsx
RNA_sample <- read_excel("RNA_sample.xlsx",
                        col_types = c("text", "numeric", "numeric",
                        "numeric", "numeric", "numeric"))

# Explorar datos
# Use las funciones summary(), head() y str() para explorar el set de datos
# Calcule la media, desviación estándar, máximo y mínino de algunas de las variables.
# Use los comandos mean(), max(), min(), ds().



# Gráficas sencillas para explorar
hist(RNA_sample$Tota_RNA_ug)
hist(RNA_sample$`260_280`)
hist(RNA_sample$`260_230`)
hist(RNA_sample$RIN_bioanalyzer)

plot(RNA_sample$Tota_RNA_ug, RNA_sample$`260_280`)
cor(RNA_sample$Tota_RNA_ug, RNA_sample$`260_280`)

plot(RNA_sample$Tota_RNA_ug, RNA_sample$`260_230`)
cor(RNA_sample$Tota_RNA_ug, RNA_sample$`260_230`)


plot(RNA_sample$Tota_RNA_ug, RNA_sample$RIN_bioanalyzer)
cor(RNA_sample$Tota_RNA_ug, RNA_sample$RIN_bioanalyzer)

# Remover objetos de la sesión de trabajo
rm(list = ls())

