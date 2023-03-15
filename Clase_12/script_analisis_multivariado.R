# ----------------------------------------------------------
# Clase 12 - Script análisis multivariado
# Dr. José Gallardo Matus
# 10 de noviembre 2022
# 'Curso Análisis de expresión diferencial de genes e investigación reproducible.'
# ----------------------------------------------------------

# Remover objetos de la sesi?n de trabajo
rm(list = ls())

# Paquetes
library(readxl)
library(ggplot2)
library(dplyr)
library(knitr)
library(psych) # Graficas de correlación
library(factoextra) # distancia euclideana
library(vegan) # 	Community Ecology Package: Ordination, Diversity and Dissimilarities

# ESTUDIO DE CASO: BIOMARCADORES DE GENOTOXICIDAD.
# [Cho et al. 2018](https://doi.org/10.1002/em.22257)
# Validación de un panel de expresión de genes como biomarcadores de genotoxicidad en modelo *in vitro*.
# - Objetivo: Reducir el uso de animales en estudios de genotixicidad.
# - Células TK6 se exponen a 14 agentes que inducen daños en el ADN (DDI) y a 14 agentes que no indicen daño (non-DDI) por 4 h.

# ETAPA 1 - Explorar correlación de expresión de genes (Ct o FC).
# Si no están correlacionadas el PCA no es de utilidad.  
FC_dat <- read_excel("PCA_dat.xlsx", sheet = "Fold_Change")
class(FC_dat)
# Transformar a matriz
FC_mat <- as.matrix(FC_dat)
class(FC_mat)
row.names(FC_mat) <- FC_dat$ID_REF
FC_mat <- FC_mat[,-1]
FCt <- t(FC_mat)
class(FCt) <- "numeric"
FCt

# Realiza gráfica de correlación
pairs.panels(FCt[,1:7], method = "spearman")

# Etapa 2 - Explorar matriz de distancia (euclideana) con log(FC).  
dist_euclidea <- dist(log(FCt))
class(dist_euclidea)
# extracto de matriz de distancia
dist_euclidea <- as.matrix(dist_euclidea)
class(dist_euclidea)
dist_euclidea

# Etapa 3 - Elaborar e interpretar Cluster jerárquico.  

dist_euclidea <- dist(log(FCt))
fit <- hclust(dist_euclidea, method="average")
plot(fit, hang=-1)
rect.hclust(fit, k = 3, border = c(2,2,4))

# Etapa 4 - Elaborar e interpretar PCA.
FC_pca <- prcomp(log(FCt), scale = TRUE)
FC_pca[["rotation"]]

My_Theme = theme(
  plot.title = element_text(size = 16),
  axis.title.x = element_text(size = 14),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 14),
  axis.text.y = element_text(size = 14),
  legend.position = "none")

p <- fviz_eig(FC_pca, addlabels=TRUE)+
  ylim(0, 50)

p + My_Theme

# PCA: GRÁFICA POR MUESTRA
FC_factor <- read_excel("PCA_dat.xlsx", sheet = "Tratamiento_FC")

q <- fviz_pca_ind(FC_pca, repel = TRUE,
                  label = "none",
                  habillage = FC_factor$Treatment,
                  addEllipses = FALSE, pointsize = 3) # añade elipses para identificar factores de clasificación y aumenta tamaño simbolos

q + My_Theme

# PCA: GRÁFICA POR VARIABLE
fviz_pca_var(FC_pca)
fviz_pca_var(FC_pca, select.var=list(name = c("DAAM1","USP41","HIST1H2BI","HIST1H2BM","HIST1H1E","CEBPD", "RAPGEF2","TRIAP1","PRKAB2")))

# PCA: GRÁFICAS BI-PLOT
r <- fviz_pca_biplot(FC_pca,
                     repel = TRUE,
                     label = "var",
                     habillage = FC_factor$Treatment,
                     addEllipses = TRUE,
                     ellipse.level=0.60,
                     pointsize = 3,
                     select.var=list(name = c("DAAM1","USP41","HIST1H2BI","HIST1H2BM","HIST1H1E","CEBPD", "RAPGEF2","TRIAP1","PRKAB2")))

r + My_Theme

# Etapa 5 - Elaborar e interpretar PERMANOVA.

permanova <- adonis(FCt ~ Treatment, method = "bray", data=FC_factor, permutations=999)
permanova[["aov.tab"]]