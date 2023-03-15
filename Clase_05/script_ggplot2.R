# ----------------------------------------------------------
# Clase 05 - Script ggplot2
# Dr. José Gallardo Matus
# 22 octubre 2022
# Curso Análisis de expresión diferencial de genes con R
# ----------------------------------------------------------

# habilitar paquetes ggplot2
library(readxl) # importa datos desde excel
library(ggplot2) # Paquete para hacer lindos gráficos en R

# Importa base de datos en formato .xlsx
RNA_sample <- read_excel("RNA_sample.xlsx",
                         col_types = c("text","text", "numeric", "numeric",
                                       "numeric", "numeric", "numeric"))
# Exploramos el objeto
class(RNA_sample)
dim(RNA_sample)
head(RNA_sample)
head(RNA_sample[,5:7]) 
# pregunta a los alumnos: ¿como interpreta estos criterios de calidad de ARN?

# summary(RNA_sample) muy util para observar variabilidad de nuestros datos
summary(RNA_sample)

# Las variables de clasificacion se deben transformar a factores.
RNA_sample$Type <- as.factor(RNA_sample$Type)
RNA_sample$Sample <- as.factor(RNA_sample$Sample)
summary(RNA_sample)

# Crear histograma por capas con ggplot2
# 1era capa con función ggplot.
# La gráfica queda vacía pues falta indicar el tipo de gráfica que deseamos
ggplot(RNA_sample, aes(RNA_ug))

# Histograma con ggplot. 
ggplot(RNA_sample, aes(RNA_ug)) +
  geom_histogram()

# agregamos estética de color
ggplot(RNA_sample, aes(RNA_ug)) +
  geom_histogram(color="white", fill="blue")

# Reducimos número de barras
# Depende del número de observaciones, se sugiere entre 5 y 15.
ggplot(RNA_sample, aes(RNA_ug)) +
  geom_histogram(color="white", fill="blue", bins = 10)

# Agregamos titulo y nombre de los ejes
ggplot(RNA_sample, aes(RNA_ug))+
  geom_histogram(color="white", fill="blue", bins = 10)+
  labs(title="Histograma", x="ARN total (ug)", 
       y="Frecuencia")

# Modificamos tamaño de etiquetas
# https://ggplot2.tidyverse.org/reference/theme.html
My_Theme = theme(
  plot.title = element_text(size = 20),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 16))

ggplot(RNA_sample, aes(RNA_ug))+
  geom_histogram(color="white", fill="blue", bins = 10)+
  labs(title="Histograma", x="ARN total (ug)", 
       y="Frecuencia") +
  My_Theme

# Crear boxplot
# Primera capa con dos ejes x e y
ggplot(RNA_sample, aes(x=Type, y= RNA_ug))

# Gráfica de boxplot
ggplot(RNA_sample, aes(x=Type, y= RNA_ug))+
  geom_boxplot()

# agregamos estética de color
ggplot(RNA_sample, aes(x=Type, y= RNA_ug))+
  geom_boxplot(color="blue")

# agregamos nombre de ejes y cambiamos tamaño de letra
ggplot(RNA_sample, aes(x=Type, y= RNA_ug))+
  geom_boxplot(color="blue")+
  labs(title="Boxplot", x="Tipo de muestra", y="ARN total (ug)") +
  My_Theme
