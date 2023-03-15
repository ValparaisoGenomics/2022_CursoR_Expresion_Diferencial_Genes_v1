# ----------------------------------------------------------
# Clase 10 - Script inferencia estadística
# Dr. José Gallardo Matus
# 04 de noviembre 2022
# Curso Análisis de expresión diferencial de genes e investigación reproducible.'
# ----------------------------------------------------------


# Remover objetos de la sesi?n de trabajo
rm(list = ls())


# Paquetes
library(readxl) # Paquete para importar datos desde excel
library(ggplot2)
library(tidyr)
library(dplyr)
library(psych) # Graficas de correlación
library(knitr)
library(broom)


# ESTUDIO DE CASO: HK GENE EN NOGAL
# https://doi.org/10.1371/journal.pone.0209424)
# Correlación entre CT de 18S rRNA y GAPDH en Nogal.

# Importa base de datos de CT de HK gene de Nogal en formato MESSY
nogal <- read_excel("hkg_nogal.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric"))

# Correlación entre muestras
pairs.panels(nogal[3:4])


#  PRUEBA DE CORRELACIÓN DE PEARSON
cor.test(nogal$`18S rRNA`, nogal$GAPDH,
         method = "pearson",
         alternative = "two.sided")

# ESTUDIO DE CASO: ARABIDOPSIS
# Adaptado de Yuan, et al, 2006
# https://doi.org/10.1186/1471-2105-7-85)
# Expresión diferencial del gen MT-7 de Arabidopsis tratada con jasmonato de metilo y con alameticinam, y no tratada (control).      

My_Theme = theme(
  plot.title = element_text(size = 18),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 16),
  legend.position = "none")

Arabidopsis <- read_excel("dat_Arabidopsis.xlsx")

ggplot(Arabidopsis, aes(x=Sample, y=ΔCt , fill=Sample))+
  geom_boxplot()+
  geom_jitter(color="#A52A2A")+
  labs(title="Delta Ct de Arabidopsis tratada y no tratada (control).", x="Sample", y="Delta Ct") +
  My_Theme

# PRUEBA DE T PARA DOS MUESTRAS INDEPENDIENTES

# **$H_0$** : delta ct(tratamiento) = delta ct(control).   
# **$H_1$** : delta ct(tratamiento) $\neq$ delta ct(control).  
# **$H_1$** : delta delta Ct $\neq$  0.  
# 
# delta delta Ct (Tratamiento - control) = `r round(2.955583 - 3.640408,4)  `  

test <- t.test(ΔCt ~ Sample, Arabidopsis,
                alternative = c("two.sided"),
                var.equal=TRUE)


# ESTUDIO DE CASO: HPS70
# Adaptado de Xu, et al, 2018
# https://www.mdpi.com/2073-4425/9/12/590.
# Expresión diferencial del gen hps-70 en peces expuestos a tratamiento de frío o calor.  

dat_hps70 <- read_excel("dat_hps70.xlsx")

ggplot(dat_hps70, aes(x=Treatment, y=deltaCt , fill=Treatment))+
  geom_boxplot()+
  geom_jitter(color="#A52A2A")+
  labs(title="Boxplot delta Ct peces expuestos a estres por calor o frío.", x="Treatment", y="delta Ct")+
  My_Theme

# PRUEBA DE ANOVA

# **$H_0$** : delta ct(hot) = delta ct(cold) = delta ct(control).   
# **$H_1$** : No todas las medias son iguales.  

res.aov <- lm(deltaCt ~ Treatment, data = dat_hps70)
anova(res.aov)

# Imprime resultado en formato tabla.
anova(res.aov) %>% 
  kable(caption = "Anova de una vía.",
                        digits=2)

# COMPARACIONES MÚLTIPLES

fit_anova <- aov(res.aov)
tk <- TukeyHSD(fit_anova)

tidy(tk) %>% kable(caption = "Prueba de Tukey.", digits=2,
                   col.names=c("Trat.","Contraste", "H0",
                               "Dif.", "IC-bajo","IC-alto",
                               "p-ajus"))
