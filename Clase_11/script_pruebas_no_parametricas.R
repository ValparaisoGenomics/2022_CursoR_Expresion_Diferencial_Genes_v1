# ----------------------------------------------------------
# Clase 11 - Script pruebas no paramétricas
# Dr. José Gallardo Matus
# 04 de noviembre 2022
# 'Curso Análisis de expresión diferencial de genes e investigación reproducible.'
# ----------------------------------------------------------

# Remover objetos de la sesi?n de trabajo
rm(list = ls())

# Paquetes
library(ggplot2)
library(tidyr)
library(dplyr)
library(FSA) #perform Dunn's Test with Bonferroni correction for p-values
library(pander) # da formato a tablas estadisticas
library(ggpubr) # estética y  estadistica
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/

# ESTUDIO DE CASO: CORRELACIÓN COPIAS Y EXPRESIÓN GEN DEFENSINA

# [Schmitt et al. 2013](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0075900)
# Relación lineal entre número de copias del gen y su expresión.

# Crea objetos X (Nº copias gen) e Y (Expresión relativa) 
gene_number <- c(13,25,34,46,44,36)
expression <- c(0.5,0.2,1.4,2.4,1.7,0.8)

# Realiza test de correlación
cor.test(gene_number,expression, method = "spearman",
         alternative = "two.sided") %>% pander()

# ESTUDIO DE CASO: EXPRESIÓN CDK18 EN SANGRE
# [Simonovic eyt al. 2020](https://doi.org/10.1186/s12894-019-0542-9)
# Comparación de expresión relativa de CDK18 en sangre de pacientes sanos (N) y con tumor (T) renal. 

# Hace el ejemplo reproducible
set.seed(0)

# Crea objetos tratamiento y control
cdk18 <- data.frame(Tratamiento = rep(c("normal", "tumoral"), each = 20),
                   FC = c(runif(20, 0.8,3.9),
                          runif(20, 0.4, 1.6)))

myplot <-  ggplot(cdk18, aes(x=Tratamiento, y=FC, color=Tratamiento))+
  geom_jitter(width = 0.10)+
  labs(title="FC cdk18 normal v/s tumoral.", x="Tratamiento", y="FC") +
  stat_summary(fun= median, geom="crossbar", width=0.5, color="black")

myplot

# Realiza prueba de Mann-Whitney
wilcox.test(FC ~ Tratamiento, data = cdk18, alternative = "two.sided",
            paired = FALSE) %>% pander()

# Agrega significancia al plot
myplot + stat_compare_means(method = "wilcox.test", label.y = 3.7, label.x = 1.2)

myplot + stat_compare_means(comparisons = list(c("normal","tumoral")), label = "p.signif")

myplot + stat_compare_means(method = "wilcox.test", label.y = 4.0, label.x = 1.2)+
  stat_compare_means(comparisons = list(c("normal","tumoral")), label = "p.signif")


# ESTUDIO DE CASO: BIOMARCADOR PARA TERAPIA DE CANCER COLORRECTAL

# [**Li et al. 2018**](https://doi.org/10.2147/OTT.S163883)
# Expresion del gen SPINT1-AS1 (A) y SPINT1 (B) en 45 pares de muestras pre y post-operados de cancer colorrectal.

# Hace el ejemplo reproducible
set.seed(0)

# Crea objetos pre y post
data <- data.frame(Tratamiento = rep(c("pre", "post"), each = 10),
                   FC = c(runif(10, 0.5, 6),
                          runif(10, 0.1, 1.9)))


# Realiza prueba de Wilcoxon
wilcox.test(FC ~ Tratamiento, data = data,
            alternative = "two.sided",
            paired = TRUE) %>% pander()

# ESTUDIO DE CASO: EXPRESIÓN SLC6A14 EN RESPUESTA A ISQUEMIA.

# [**Lange et al. 2015**](doi:10.1371/journal.pone.0133987.g002)
# Respuesta a isquemia en tejido normal (N) y tumoral (T) sometido a isquemia por 0, 10, 20 y 45 min.

# Simula datos
data <- data.frame(Time = rep(c("N0", "N10", "N20", "N45"), each = 20),
                   FC = c(runif(20, 1,110),
                          runif(20, 0.1, 2),
                          runif(20, 0.3, 2.3),
                          runif(20, 0.2, 2.2)))


myplot <-  ggplot(data, aes(x=Time, y=FC, color=Time))+
  geom_jitter(width = 0.10)+
  labs(title="FC SLC6A14 normal v/s tumoral.", x="Tiempo", y="FC") +
  stat_summary(fun= median, geom="crossbar", width=0.5, color="black")
myplot 


myplot_log <-  ggplot(data, aes(x=Time, y=log(FC), color=Time))+
  geom_jitter(width = 0.10)+
  labs(title="FC SLC6A14 normal v/s tumoral.", x="Tiempo", y="FC") +
  stat_summary(fun= median, geom="crossbar", width=0.5, color="black")
myplot_log 


# Realiza prueba de kruskal
kruskal.test(FC ~ Time, data=data) %>% pander()

# Realice prueba de dunn
dunnTest(FC ~ Time,
         data=data,
         method="bonferroni")

# Agrega significancia al plot
myplot_log + stat_compare_means(method = 'kruskal.test', label.y = 5.5, label.x = 1)

myplot_log + stat_compare_means(comparisons = list(c("N0", "N10"),c("N0", "N20"), c("N0", "N45")),
                                label = "p.signif")

myplot_log + stat_compare_means(method = 'kruskal.test', label.y = 7.5, label.x = 1) +
  stat_compare_means(comparisons = list(c("N0", "N10"),c("N0", "N20"), c("N0", "N45")),
                                label = "p.signif")


# ESTUDIO DE CASO: PREDICCIÓN MOLECULAR DE EMBRIONES ANEUPLOIDES

# [**Lal et al. 2022**](https://doi.org/10.1007/s10815-022-02510-3).
# Expresión del gen BCL2L13 en embiones euploides y anauploides.

# Crea matriz de datos
datos <- c(9, 1, 2, 7)
dim(datos) <- c(2,2)
rownames(datos) <- c('Euploides','Aneuploides')
colnames(datos) <- c('Expresado','No expresado')
datos

# Test de Chi-squared en R (chisq.test)
test<-chisq.test(datos, correct = FALSE)

test %>% pander()