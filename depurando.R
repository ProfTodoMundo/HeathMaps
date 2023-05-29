library(magrittr)
library(pheatmap)
library(RColorBrewer)
library(rio)
library(readr)
library(tidyverse)

#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
setwd("~/Nextcloud/NubeGralCarlos/ElisaProject/NuevoProyecto/HeathMaps") 
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
EiMybs <- read_csv("~/Nextcloud/NubeGralCarlos/ElisaProject/NuevoProyecto/EiMybs.csv")
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
EiMybs_clean = EiMybs %>%
  mutate(gene = paste0(...2, ":", nombre)) %>%
  as.data.frame() %>%
  column_to_rownames("gene")
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
EiMybs_clean <- EiMybs_clean[,2:9]
colnames(EiMybs_clean) <- c('GenId', "Trophozoites", "8_h_en","24_h_en","48_h_en",
                            "72_h_en", "2_h_ex","8_h_ex")
EiMybs_clean <- EiMybs_clean[,2:8]
EiMybs_log2 = log2(EiMybs_clean + 1)
boxplot(EiMybs_log2, las = 3)
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
EiMbysOrdered <- EiMybs_clean[order(-EiMybs_clean$Trophozoites),]
top_genes_EiMybs_Trop <- EiMbysOrdered %>% filter(Trophozoites>0) %>% select(Trophozoites)
k <- dim(EiMybs_clean); proporcion <- 1; NS <- round(k[1]*proporcion);
random_genes_EiMybs = sample(rownames(EiMybs_clean),NS )
head(random_genes_EiMybs)
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
sampledEiMybs_Log2 <- EiMybs_log2[random_genes_EiMybs, ]; #View(sampledEiMybs_Log2)
pheatmap(EiMybs_log2[random_genes_EiMybs, ])
EiMybs_log2_filtrado <- EiMybs_log2[rowSums(EiMybs_log2) != 0, ]; #View(EiMybs_log2_filtrado)
pheatmap(EiMybs_log2_filtrado, scale = "row")
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
my_colors = brewer.pal(n = 11, name = "RdBu")
my_colors = colorRampPalette(my_colors)(50)
my_colors = rev(my_colors)
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
pheatmap(EiMybs_log2_filtrado, scale = "row",color = my_colors, border_color = NA, fontsize_row = 6)
# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==
EiMybs_Log2 <- EiMybs_log2_filtrado
# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==
hc <- hclust(dist(EiMybs_Log2))
grupos <- cutree(hc, k = 1); colores <- c("purple", "blue");  
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
plot(hc, main = "Dendograma del análisis de clúster jerárquico", 
     col = colores[grupos], 
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1)
#
hc <- hclust(dist(EiMybs_Log2),"ave")
plot(hc, main = "Dendograma del análisis de clúster jerárquico",
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1,
     col=colores[grupos])
#
hc <- hclust(dist(EiMybs_Log2),"cen")
plot(hc, main = "Dendograma del análisis de clúster jerárquico",
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1,
     col=colores[grupos])
# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==
k <- 3
kmeans_result <- kmeans(EiMybs_Log2, centers = k)
plot(EiMybs_Log2, col = kmeans_result$cluster)
variaciones_explicadas <- vector("numeric", length = 10)
for (k in 1:10) {
  kmeans_result <- kmeans(EiMybs_Log2, centers = k)
  variaciones_explicadas[k] <- kmeans_result$tot.withinss}
plot(1:10, variaciones_explicadas, type = "b", pch = 19, frame = FALSE,
     xlab = "Número de clústeres", ylab = "Variación explicada",main = "Método del codo")
ss_total <- sum(var(EiMybs_Log2)^2)
variacion_explicada_rel <- 1 - variaciones_explicadas/ss_total
lines(1:10, variacion_explicada_rel, type = "b", pch = 19, col = "red")
abline(v = which.max(variacion_explicada_rel), col = "blue", lty = 2)
k_optimo <- which.max(variacion_explicada_rel); k_optimo
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
hc <- hclust(dist(EiMybs_Log2))
grupos <- cutree(hc, k = 10); colores <- c("purple", "blue");  
plot(hc, main = "Dendograma del análisis de clúster jerárquico", 
     col = colores[grupos], xlab =  "Genes EiMybs",ylab = "Distancias",hang = -1)
#
hc <- hclust(dist(EiMybs_Log2),"ave")
plot(hc, main = "Dendograma del análisis de clúster jerárquico",
     xlab =  "Genes EiMybs",ylab = "Distancias",hang = -1,col=colores[grupos])
#
hc <- hclust(dist(EiMybs_Log2),"cen")
plot(hc, main = "Dendograma del análisis de clúster jerárquico",xlab =  "Genes EiMybs",
     ylab = "Distancias",hang = -1,col=colores[grupos])
# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==
if (!requireNamespace("mclust"))install.packages("mclust")
library(mclust)
# Aplicar el clustering basado en el algoritmo de mezcla de Gaussianas
mclust_result <- Mclust(EiMybs_Log2)
plot(mclust_result, what = "classification",
     main = "Cluster basado en mezcla de gaussianos",
     hang = -1, col=my_colors)
plot(mclust_result, what = "density",
     main = "Cluster basado en mezcla de gaussianos",
     hang = -1,col=my_colors)
plot(mclust_result, what = "uncertainty",
     main = "Cluster basado en mezcla de gaussianos",
     hang = -1,col=my_colors)
plot(mclust_result, what = "BIC",
     main = "Cluster basado en mezcla de gaussianos",
     hang = -1,col=my_colors)
# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==