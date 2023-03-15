# ----------------------------------------------------------
# Clase 09 - Script Análisis exploratorio Fold change
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
ExpresionG1 <- read_excel("FC_Gen1.xlsx") 

Resumen_G1 <- ExpresionG1 %>% group_by(Treatment) %>%
  summarize(n=n(),
            Mean_Expresion=mean(Expresion),
            sd_Expresion=sd(Expresion))

ggplot(Resumen_G1, aes(x=Treatment, y=Mean_Expresion, fill=Treatment)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=Mean_Expresion-sd_Expresion, ymax=Mean_Expresion+sd_Expresion), width=.2,
                position=position_dodge(.9))+
  theme(legend.position="none")+
  labs(y="Fold Change (2^ddCt)",title="Gen 1")

# Crear histograma con ggplot2
ggplot(ExpresionG1, aes(Expresion))+
  geom_histogram(color="white", fill="blue", bins = 10)+
  labs(title="Histograma de FC", x="Fold Change (2^ddCt)", 
       y="Frecuencia")

# Crear boxplot
ggplot(ExpresionG1, aes(x=Treatment, y=Expresion, fill=Treatment))+
  geom_boxplot()+
  labs(y="Fold Change (2^ddCt)",title="Gen 1")

# Crear boxplot + jitter
ggplot(ExpresionG1, aes(x=Treatment, y=Expresion, fill=Treatment))+
  geom_boxplot()+
  labs(y="Fold Change (2^ddCt)",title="Gen 1")+
  geom_jitter(color="#A52A2A")
