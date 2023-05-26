#genera el codigo para RMarkdown con las siguentes:library(tidyverse)
library(magrittr)
library(pheatmap)
library(RColorBrewer)
library(rio)
library(readr)
library(tidyverse)

# Hay que poner aqui la dirección donde estan los datos y desde el cual se va a trabajar
setwd("~/Nextcloud/NubeGralCarlos/ElisaProject/NuevoProyecto/HeathMaps") 
# Se lee el archivo que contiene la información, es el que me pasaste, solamente recorte el nombre
EiMybs <- read_csv("~/Nextcloud/NubeGralCarlos/ElisaProject/NuevoProyecto/EiMybs.csv")
# Aqui comienza el codigo
EiMybs_clean = EiMybs %>%
  mutate(gene = paste0(...2, ":", nombre)) %>%
  as.data.frame() %>%
  column_to_rownames("gene")

EiMybs_clean <- EiMybs_clean[,2:9]
colnames(EiMybs_clean) <- c('GenId', "Trophozoites", "8_h_en","24_h_en","48_h_en", "72_h_en", "2_h_ex","8_h_ex")
EiMybs_clean <- EiMybs_clean[,2:8]

#pdf("BoxplotDatosLimpios.#pdf")
boxplot(EiMybs_clean, las = 2)
#dev.off()

EiMybs_log2 = log2(EiMybs_clean + 1)
#pdf("BoxPlotDatosTransformadosLog2.#pdf")
boxplot(EiMybs_log2, las = 3)
#dev.off()

EiMbysOrdered <- EiMybs_clean[order(-EiMybs_clean$Trophozoites),]

top_genes_EiMybs_Trop <- EiMbysOrdered %>% filter(Trophozoites>0) %>% select(Trophozoites)
k <- dim(EiMybs_clean)
proporcion <- 1
NS <- round(k[1]*proporcion)
random_genes_EiMybs = sample(rownames(EiMybs_clean),NS )
head(random_genes_EiMybs)

#pdf("HeatMapDatosLimpios.#pdf")
pheatmap(EiMybs_clean[random_genes_EiMybs, ])
#dev.off()

sampledEiMybs_Log2 <- EiMybs_log2[random_genes_EiMybs, ]; #View(sampledEiMybs_Log2)

#pdf("HeatMapLog2Transformed.#pdf")
pheatmap(EiMybs_log2[random_genes_EiMybs, ])
#dev.off()

EiMybs_log2_filtrado <- EiMybs_log2[rowSums(EiMybs_log2) != 0, ]; #View(EiMybs_log2_filtrado)


#pdf("HeatMapNormalizadosRenglon.#pdf")
pheatmap(EiMybs_log2_filtrado, scale = "row")
#dev.off()

#pdf("HeatMapNormalizadosColumna.#pdf")
pheatmap(EiMybs_log2_filtrado, scale = "column")
#dev.off()

my_colors = c("green", "yellow", "pink")
my_colors = colorRampPalette(my_colors)(50)
#my_colors
#pdf("HeatMapPaletaPersonal.#pdf")
pheatmap(EiMybs_log2_filtrado, scale = "row", color = my_colors)
#dev.off()

my_colors = brewer.pal(n = 11, name = "RdBu")
my_colors = colorRampPalette(my_colors)(50)
my_colors = rev(my_colors)
#my_colors

#pdf("HeatMapLog2Transformed.#pdf")
pheatmap(EiMybs_log2_filtrado, scale = "row", color = my_colors)
#dev.off()

#pdf("HeatMapLog2TransformedLetra4.#pdf")
pheatmap(EiMybs_log2_filtrado, scale = "row",color = my_colors, border_color = NA, fontsize_row = 4)
#dev.off()

#pdf("MapadeCalorFinal.#pdf")
pheatmap(EiMybs_log2_filtrado, scale = "row",color = my_colors, border_color = NA, fontsize_row = 6)
#dev.off()
#
#
# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==
EiMybs_Log2 <- EiMybs_log2_filtrado
# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==


hc <- hclust(dist(EiMybs_Log2))
plot(hc, main = "Dendograma del análisis de clúster jerárquico",
     xlab =  "Genes EiMybs",
     ylab = "Distancias"
     )


plot(hc, main = "Dendograma del análisis de clúster jerárquico",
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = 0
)

plot(hc, main = "Dendograma del análisis de clúster jerárquico",
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1
)


grupos <- cutree(hc, k = 2)  
# Asignar grupos utilizando un umbral o número específico
colores <- c("purple", "blue")  
# Vector de colores para los grupos
plot(hc, col = colores[grupos])



grupos <- cutree(hc, h = 0.5)  
plot(hc, col = colores[grupos])


grupos <- cutree(hc, k = 2, )  
plot(hc, col = colores[grupos])


# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==
# 
colores <- c("purple", "blue")  
hc <- hclust(dist(EiMybs_Log2))
plot(hc, main = "Dendograma del análisis de clúster jerárquico",
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1,
     col=colores[grupos]
)


hc <- hclust(dist(EiMybs_Log2),"ave")
plot(hc, main = "Dendograma del análisis de clúster jerárquico",
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1,
     col=colores[grupos]
)

hc <- hclust(dist(EiMybs_Log2),"cen")
plot(hc, main = "Dendograma del análisis de clúster jerárquico",
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1,
     col=colores[grupos]
)

# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==

k <- 3
kmeans_result <- kmeans(EiMybs_Log2, centers = k)
plot(EiMybs_Log2, col = kmeans_result$cluster)


# Crear una lista vacía para almacenar las variaciones explicadas
variaciones_explicadas <- vector("numeric", length = 10)

# Calcular la variación explicada para diferentes valores de k
for (k in 1:10) {
  kmeans_result <- kmeans(EiMybs_Log2, centers = k)
  variaciones_explicadas[k] <- kmeans_result$tot.withinss
}

# Graficar la variación explicada en función del número de clústeres
plot(1:10, variaciones_explicadas, type = "b", pch = 19, frame = FALSE,
     xlab = "Número de clústeres", ylab = "Variación explicada",
     main = "Método del codo")

# Agregar líneas para identificar el punto de codo
ss_total <- sum(var(EiMybs_Log2)^2)
variacion_explicada_rel <- 1 - variaciones_explicadas/ss_total
lines(1:10, variacion_explicada_rel, type = "b", pch = 19, col = "red")
abline(v = which.max(variacion_explicada_rel), col = "blue", lty = 2)

# Obtener el número óptimo de clústeres
k_optimo <- which.max(variacion_explicada_rel)
k_optimo



grupos <- cutree(hc, k = 10)  
# Asignar grupos utilizando un umbral o número específico
colores <- c("purple", "blue","yellow", "green","brown","gray","pink","cyan","red","black")  
# Vector de colores para los grupos
plot(hc, col = colores[grupos])


plot(hc, main = "Dendograma del análisis de clúster jerárquico completo",
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1,
     col=colores[grupos]
)
hc <- hclust(dist(EiMybs_Log2),"ave")
plot(hc, main = "Dendograma del análisis de clúster jerárquico Distancia euclideana",
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1,
     col=colores[grupos]
)

hc <- hclust(dist(EiMybs_Log2),"cen")
plot(hc, main = "Dendograma del análisis de clúster jerárquico centroide",
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1,
     col=colores[grupos]
)




grupos <- cutree(hc, k = 1)  
# Asignar grupos utilizando un umbral o número específico
colores <- c("purple", "blue","yellow", "green","brown","gray","pink","cyan","red","black")  
# Vector de colores para los grupos
plot(hc, col = colores[grupos])


plot(hc, main = "Dendograma del análisis de clúster jerárquico completo",
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1,
     col=colores[grupos]
)
hc <- hclust(dist(EiMybs_Log2),"ave")
plot(hc, main = "Dendograma del análisis de clúster jerárquico Distancia euclideana",
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1,
     col=colores[grupos]
)

hc <- hclust(dist(EiMybs_Log2),"cen")
plot(hc, main = "Dendograma del análisis de clúster jerárquico centroide",
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1,
     col=colores[grupos]
)


#
#

# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==

if (!requireNamespace("mclust"))install.packages("mclust")
library(mclust)

# Aplicar el clustering basado en el algoritmo de mezcla de Gaussianas
mclust_result <- Mclust(EiMybs_Log2)
plot(mclust_result, what = "classification")


plot(mclust_result, what = "density")

plot(mclust_result, what = "uncertainty")


plot(mclust_result, what = "BIC")
# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==
if (!requireNamespace("kohonen"))
  install.packages("kohonen")
library(kohonen)

# Crear la red SOM
som_grid <- somgrid(5, 5, "hexagonal")
EiMybs_Log2_list <- as.list(EiMybs_Log2)
head(EiMybs_Log2_list, 5)

num_observaciones <- length(EiMybs_Log2_list[[1]])

# Ajustar el tamaño de la cuadrícula según el número de observaciones
num_filas <- min(sqrt(num_observaciones), 2)
num_columnas <- ceiling(num_observaciones / num_filas)

# Crear la nueva cuadrícula SOM
som_grid <- somgrid(num_filas, num_columnas, "hexagonal")

# Construir la matriz de datos
EiMybs_Log2_matrix <- do.call(rbind, EiMybs_Log2_list)

# Crear el modelo SOM
som_model <- som(EiMybs_Log2_matrix, grid = som_grid)

# Visualizar el mapa SOM
plot(som_model)

# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==

if (!requireNamespace("dbscan"))install.packages("dbscan")
library(dbscan)

# Aplicar el clustering DBSCAN
dbscan_result <- dbscan(EiMybs_Log2, eps = 0.2, minPts = 3)
plot(dbscan_result)
