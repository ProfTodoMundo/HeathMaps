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

pdf("BoxplotDatosLimpios.pdf")
boxplot(EiMybs_clean, las = 2)
dev.off()

EiMybs_log2 = log2(EiMybs_clean + 1)
pdf("BoxPlotDatosTransformadosLog2.pdf")
boxplot(EiMybs_log2, las = 3)
dev.off()

EiMbysOrdered <- EiMybs_clean[order(-EiMybs_clean$Trophozoites),]

top_genes_EiMybs_Trop <- EiMbysOrdered %>% filter(Trophozoites>0) %>% select(Trophozoites)
k <- dim(EiMybs_clean)
proporcion <- 1
NS <- round(k[1]*proporcion)
random_genes_EiMybs = sample(rownames(EiMybs_clean),NS )
head(random_genes_EiMybs)

pdf("HeatMapDatosLimpios.pdf")
pheatmap(EiMybs_clean[random_genes_EiMybs, ])
dev.off()

sampledEiMybs_Log2 <- EiMybs_log2[random_genes_EiMybs, ]; #View(sampledEiMybs_Log2)

pdf("HeatMapLog2Transformed.pdf")
pheatmap(EiMybs_log2[random_genes_EiMybs, ])
dev.off()

EiMybs_log2_filtrado <- EiMybs_log2[rowSums(EiMybs_log2) != 0, ]; #View(EiMybs_log2_filtrado)


pdf("HeatMapNormalizadosRenglon.pdf")
pheatmap(EiMybs_log2_filtrado, scale = "row")
dev.off()

pdf("HeatMapNormalizadosColumna.pdf")
pheatmap(EiMybs_log2_filtrado, scale = "column")
dev.off()

my_colors = c("green", "yellow", "pink")
my_colors = colorRampPalette(my_colors)(50)
#my_colors
pdf("HeatMapPaletaPersonal.pdf")
pheatmap(EiMybs_log2_filtrado, scale = "row", color = my_colors)
dev.off()

my_colors = brewer.pal(n = 11, name = "RdBu")
my_colors = colorRampPalette(my_colors)(50)
my_colors = rev(my_colors)
#my_colors

pdf("HeatMapLog2Transformed.pdf")
pheatmap(EiMybs_log2_filtrado, scale = "row", color = my_colors)
dev.off()

pdf("HeatMapLog2TransformedLetra4.pdf")
pheatmap(EiMybs_log2_filtrado, scale = "row",color = my_colors, border_color = NA, fontsize_row = 4)
dev.off()

pdf("MapadeCalorFinal.pdf")
pheatmap(EiMybs_log2_filtrado, scale = "row",color = my_colors, border_color = NA, fontsize_row = 6)
dev.off()