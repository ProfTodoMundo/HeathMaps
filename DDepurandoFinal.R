#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
if (!requireNamespace("magrittr"))install.packages("magritt");           library(magrittr)
if (!requireNamespace("pheatmap"))install.packages("pheatmap");          library(pheatmap)
if (!requireNamespace("RColorBrewer"))install.packages("RcolorBrewer");  library(RColorBrewer)
if (!requireNamespace("Rio"))install.packages("rio");                    library(rio)
if (!requireNamespace("readr"))install.packages("readr");                library(readr)
if (!requireNamespace("tidyverse"))install.packages("");                 library(tidyverse)
if (!requireNamespace("mclust"))install.packages("mclust");              library(mclust)
if (!requireNamespace("venn"))install.packages("venn");                  library(venn)
if (!requireNamespace("dplyr"))install.packages("dplyr");                library(dplyr)
if (!requireNamespace("ggplot2"))install.packages("ggplot2");            library(ggplot2)
if (!requireNamespace("cowplot"))install.packages("cowplot");            library(cowplot)
if (!requireNamespace("RColorBrewer"))install.packages("RColorBrewer");  library(RColorBrewer)
if (!requireNamespace("ggVennDiagram"))install.packages("ggVennDiagram");library(ggVennDiagram)
if (!requireNamespace("VennDiagram"))install.packages("VennDiagram");    library(VennDiagram)
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
write.csv(EiMybs_log2,"EiMybsLog.csv")
pdf("Boxplot.pdf")
boxplot(EiMybs_log2, las = 3)
dev.off()
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
EiMbysOrdered <- EiMybs_clean[order(-EiMybs_clean$Trophozoites),]
top_genes_EiMybs_Trop <- EiMbysOrdered %>% filter(Trophozoites>0) %>% select(Trophozoites)
k <- dim(EiMybs_clean); proporcion <- 1; NS <- round(k[1]*proporcion);
random_genes_EiMybs = sample(rownames(EiMybs_clean),NS )
head(random_genes_EiMybs)
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
sampledEiMybs_Log2 <- EiMybs_log2[random_genes_EiMybs, ]; #View(sampledEiMybs_Log2)
write.csv(sampledEiMybs_Log2,"SampledEiMybsLog.csv")
pdf("HeathMapEiMybsDef.pdf")
pheatmap(EiMybs_log2[random_genes_EiMybs, ])
dev.off()
EiMybs_log2_filtrado <- EiMybs_log2[rowSums(EiMybs_log2) != 0, ]; #View(EiMybs_log2_filtrado)
pdf("HeathMapLog2EiMybsDef.pdf")
pheatmap(EiMybs_log2_filtrado, scale = "row")
dev.off()
write.csv(EiMybs_log2_filtrado,"EiMybsLogFiltrado.csv")
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
my_colors = brewer.pal(n = 11, name = "RdBu")
my_colors = colorRampPalette(my_colors)(50)
my_colors = rev(my_colors)
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
pdf("HeathMapLog2EiMybsfiltrado.pdf")
pheatmap(EiMybs_log2_filtrado, scale = "row",color = my_colors, border_color = NA, fontsize_row = 6)
dev.off()
# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==
EiMybs_Log2 <- EiMybs_log2_filtrado
# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==
hc <- hclust(dist(EiMybs_Log2))
grupos <- cutree(hc, k = 1); colores <- c("purple", "blue");  
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
pdf("DendogramEiMybsJerarquico.pdf")
plot(hc, main = "Dendograma del análisis de clúster jerárquico", 
     col = colores[grupos], 
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1)
dev.off()
#

hc <- hclust(dist(EiMybs_Log2),"ave")
pdf("DendogramEiMybsJerarquicoAve.pdf")
plot(hc, main = "Dendograma del análisis de clúster jerárquico",
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1,
     col=colores[grupos])
dev.off()
#
hc <- hclust(dist(EiMybs_Log2),"cen")
pdf("DendogramEiMybsJerarquicoCen.pdf")
plot(hc, main = "Dendograma del análisis de clúster jerárquico",
     xlab =  "Genes EiMybs",
     ylab = "Distancias",
     hang = -1,
     col=colores[grupos])
dev.off()
# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==
k <- 3
kmeans_result <- kmeans(EiMybs_Log2, centers = k)
pdf("KMeansGraph.pdf")
plot(EiMybs_Log2, col = kmeans_result$cluster)
dev.off()
variaciones_explicadas <- vector("numeric", length = 10)

for (k in 1:10) {
  kmeans_result <- kmeans(EiMybs_Log2, centers = k)
  variaciones_explicadas[k] <- kmeans_result$tot.withinss}
pdf("Kmeansto10.pdf")
plot(1:10, variaciones_explicadas, type = "b", pch = 19, frame = FALSE,
     xlab = "Número de clústeres", ylab = "Variación explicada",main = "Método del codo")
ss_total <- sum(var(EiMybs_Log2)^2)
variacion_explicada_rel <- 1 - variaciones_explicadas/ss_total
lines(1:10, variacion_explicada_rel, type = "b", pch = 19, col = "red")
abline(v = which.max(variacion_explicada_rel), col = "blue", lty = 2)
k_optimo <- which.max(variacion_explicada_rel); k_optimo
dev.off()
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
hc <- hclust(dist(EiMybs_Log2))
grupos <- cutree(hc, k = 10); colores <- c("purple", "blue");  
pdf("DendogramEiMybsJerarquicoJerarq.pdf")
plot(hc, main = "Dendograma del análisis de clúster jerárquico", 
     col = colores[grupos], xlab =  "Genes EiMybs",ylab = "Distancias",hang = -1)
dev.off()
hc <- hclust(dist(EiMybs_Log2),"ave")
pdf("DendogramEiMybsJerarquicoAve.pdf")
plot(hc, main = "Dendograma del análisis de clúster jerárquico",
     xlab =  "Genes EiMybs",ylab = "Distancias",hang = -1,col=colores[grupos])
dev.off()
hc <- hclust(dist(EiMybs_Log2),"cen")
pdf("DendogramEiMybsJerarquicoCen.pdf")
plot(hc, main = "Dendograma del análisis de clúster jerárquico",xlab =  "Genes EiMybs",
     ylab = "Distancias",hang = -1,col=colores[grupos])
dev.off()
# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==
library(mclust)
# Aplicar el clustering basado en el algoritmo de mezcla de Gaussianas
mclust_result <- Mclust(EiMybs_Log2)
pdf("ClusterGaussClass.pdf")
plot(mclust_result, what = "classification",
     main = "Cluster basado en mezcla de gaussianos",
     hang = -1, col=my_colors)
dev.off()
pdf("ClusterGaussDensity.pdf")
plot(mclust_result, what = "density",
     main = "Cluster basado en mezcla de gaussianos",
     hang = -1,col=my_colors)
dev.off()
pdf("ClusterGaussUncertainty.pdf")
plot(mclust_result, what = "uncertainty",
     main = "Cluster basado en mezcla de gaussianos",
     hang = -1,col=my_colors)
dev.off()
pdf("ClusterGaussBIC.pdf")
plot(mclust_result, what = "BIC",
     main = "Cluster basado en mezcla de gaussianos",
     hang = -1,col=my_colors)
dev.off()

# == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> == << >> ==  == << >> ==

# when x is a dataframe
set.seed(12345)
x <- as.data.frame(matrix(sample(0:1, 210, replace = TRUE), ncol = 7))
colnames(x) <- c("Set2","Set2","Set3","Set4","Set5","Set6","Set7")
# with dashed lines
venn(x, ggplot = TRUE, linetype = "dashed",
     snames = c("A", "B", "C", "D", "E", "F", "G"),
     size = 0.5,
     ellipse = TRUE,
     zcolor = c("red", "blue", "yellow", "green", "gray", "orange", "purple"),
     col = c("red", "blue", "yellow", "green", "gray", "orange", "purple"))
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 

setwd("~/Nextcloud/nubegeneral/ElisaProject/NuevoProyecto")
GenesEiMybs <- read_csv("GenesEiMybs.csv")
DataSet <- GenesEiMybs[,2:9]; #View(DataSet)
DataSet <- as.data.frame(DataSet)
typeof(DataSet)
View(DataSet)
DDataSet <- ifelse(DataSet!=0,1,0); 
View(DDataSet)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# hay que sustituir los valores del dataframe ejemplo con los valores de
# los genes
set.seed(12345)
x <- as.data.frame(matrix(sample(0:1, 329, replace = TRUE), ncol = 7))
#colnames(x) <- c("Set2","Set2","Set3","Set4","Set5","Set6","Set7")
colnames(x) <- c("Trophozoites","en_8h",
                 "en_24h", "en_48h", "en_72h",
                 "ex_2h","ex_8h")
#length(x$Trophozoites)
Set1 <- DDataSet[,2]; 
Set2 <- DDataSet[,3]; 
Set3 <- DDataSet[,4]; 
Set4 <- DDataSet[,5]; 
Set5 <- DDataSet[,6]; 
Set6 <- DDataSet[,7]; 
Set7 <- DDataSet[,8]; 
x$Trophozoites <- Set1
x$en_8h <- Set2
# with dashed lines
venn(x, ggplot = TRUE, linetype = "dashed",
     snames = c("A", "B", "C", "D", "E", "F", "G"),
     size = 0.5,
     ellipse = TRUE,
     zcolor = c("red", "blue", "yellow", "green", "gray", "orange", "purple"),
     col = c("red", "blue", "yellow", "green", "gray", "orange", "purple"))

x$en_24h<- Set3
# with dashed lines
venn(x, ggplot = TRUE, linetype = "dashed",
     snames = c("A", "B", "C", "D", "E", "F", "G"),
     size = 0.5,
     ellipse = TRUE,
     zcolor = c("red", "blue", "yellow", "green", "gray", "orange", "purple"),
     col = c("red", "blue", "yellow", "green", "gray", "orange", "purple"))

x$en_48h<- Set4
# with dashed lines
venn(x, ggplot = TRUE, linetype = "dashed",
     snames = c("A", "B", "C", "D", "E", "F", "G"),
     size = 0.5,
     ellipse = TRUE,
     zcolor = c("red", "blue", "yellow", "green", "gray", "orange", "purple"),
     col = c("red", "blue", "yellow", "green", "gray", "orange", "purple"))

x$en_72h<- Set5
# with dashed lines
venn(x, ggplot = TRUE, linetype = "dashed",
     snames = c("A", "B", "C", "D", "E", "F", "G"),
     size = 0.5,
     ellipse = TRUE,
     zcolor = c("red", "blue", "yellow", "green", "gray", "orange", "purple"),
     col = c("red", "blue", "yellow", "green", "gray", "orange", "purple"))

x$ex_2h<- Set6
# with dashed lines
venn(x, ggplot = TRUE, linetype = "dashed",
     snames = c("A", "B", "C", "D", "E", "F", "G"),
     size = 0.5,
     ellipse = TRUE,
     zcolor = c("red", "blue", "yellow", "green", "gray", "orange", "purple"),
     col = c("red", "blue", "yellow", "green", "gray", "orange", "purple"))

x$ex_8h<- Set7
# with dashed lines
venn(x, ggplot = TRUE, linetype = "dashed",
     snames = c("A", "B", "C", "D", "E", "F", "G"),
     size = 0.5,
     ellipse = TRUE,
     zcolor = c("red", "blue", "yellow", "green", "gray", "orange", "purple"),
     col = c("red", "blue", "yellow", "green", "gray", "orange", "purple"))
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
# --------------------------------------------------------------------
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 

setwd("~/Nextcloud/nubegeneral/ElisaProject/NuevoProyecto")
GenesEiMybs <- read_csv("GenesEiMybs.csv")
DataSet <- GenesEiMybs[,2:9]; #View(DataSet)
DataSet <- as.data.frame(DataSet)
DDataSet <- ifelse(DataSet!=0,1,0); 
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
x <- as.data.frame(matrix(sample(0:1, 329, replace = TRUE), ncol = 7))
colnames(x) <- c("Trophozoites","en_8h",
                 "en_24h", "en_48h", "en_72h",
                 "ex_2h","ex_8h")
Set1 <- DDataSet[,2];  Set2 <- DDataSet[,3]; 
Set3 <- DDataSet[,4];  Set4 <- DDataSet[,5]; 
Set5 <- DDataSet[,6];  Set6 <- DDataSet[,7]; 
Set7 <- DDataSet[,8]; 
x$Trophozoites <- Set1
x$en_8h <- Set2
x$en_24h<- Set3
x$en_48h<- Set4
x$en_72h<- Set5
x$ex_2h<- Set6
x$ex_8h<- Set7
# with dashed lines
venn(x, ggplot = TRUE, linetype = "dashed",
     size = 0.5,
     ellipse = TRUE,
     zcolor = c("red", "blue", "yellow", "green", "gray", "orange", "purple"),
     col = c("red", "blue", "yellow", "green", "gray", "orange", "purple"))


venn_plot <- venn(x, ggplot = TRUE, linetype = "dashed",
                  size = 0.5,
                  ellipse = TRUE,
                  zcolor = c("red", "blue", "yellow", "green", "gray", "orange", "purple"),
                  col = c("red", "blue", "yellow", "green", "gray", "orange", "purple"))

venn_plot <- venn_plot+ggtitle("EiMybs Genns's Venn Diagram")

print(venn_plot)
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
#<< == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> 
# << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >>

# << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >>
setwd("~/Nextcloud/NubeGralCarlos/ElisaProject/NuevoProyecto")
#setwd("~/Nextcloud/nubegeneral/ElisaProject/NuevoProyecto")
# << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >>
GenesEiMybs <- read_csv("GenesEiMybs.csv")
DataSet <- GenesEiMybs[,2:9];
DataSet <- as.data.frame(DataSet)
DIMENSION <- dim(DataSet); Renglones <- DIMENSION[1];Columnas  <- DIMENSION[2]; listando <- c()
nmatriz <- matrix(0,Renglones,Columnas)
for(i in 1:Renglones){
  for(j in 2:Columnas){
    if(DataSet[i,j]!=0){
      nmatriz[i,j] <- DataSet[i,1]
    }
  }
}
nmatriz <- as.data.frame(nmatriz); View(nmatriz); MatrizTW <- nmatriz[,2:8]
r1 <- MatrizTW[,1]; Set1 <- subset(r1,r1!=0); print(Set1); write.csv(Set1,"Intersecciones/Set1.csv")
r2 <- MatrizTW[,2]; Set2 <- subset(r2,r2!=0); print(Set2); write.csv(Set2,"Intersecciones/Set2.csv")
r3 <- MatrizTW[,3]; Set3 <- subset(r3,r3!=0); print(Set3); write.csv(Set3,"Intersecciones/Set3.csv")
r4 <- MatrizTW[,4]; Set4 <- subset(r4,r4!=0); print(Set4); write.csv(Set4,"Intersecciones/Set4.csv")
r5 <- MatrizTW[,5]; Set5 <- subset(r5,r5!=0); print(Set5); write.csv(Set5,"Intersecciones/Set5.csv")
r6 <- MatrizTW[,6]; Set6 <- subset(r6,r6!=0); print(Set6); write.csv(Set6,"Intersecciones/Set6.csv")
r7 <- MatrizTW[,7]; Set7 <- subset(r7,r7!=0); print(Set7); write.csv(Set7,"Intersecciones/Set7.csv")
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
Sets <- list(Set1,Set2,Set3,Set4,Set5,Set6,Set7); NumSets <- length(Sets)
print(Sets)
# << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >>  << == >> 
IR2 <- function(conjunto1, conjunto2) {
  if (length(conjunto1) == 0 || length(conjunto2) == 0) {return(NULL)}
  return(intersect(conjunto1, conjunto2))
}
IR3 <- function(conjunto1, conjunto2, conjunto3) {
  interseccion_12 <- intersect(conjunto1, conjunto2)
  return(IR2(interseccion_12, conjunto3))
}
IR4 <- function(conjunto1, conjunto2, conjunto3, conjunto4) {
  interseccion_123 <- IR3(conjunto1, conjunto2, conjunto3)
  return(IR2(interseccion_123, conjunto4))
}

IR5 <- function(conjunto1, conjunto2, conjunto3, conjunto4, conjunto5) {
  interseccion_1234 <- IR4(conjunto1, conjunto2, conjunto3, conjunto4)
  return(IR2(interseccion_1234, conjunto5))
}

IR6 <- function(conjunto1, conjunto2, conjunto3, conjunto4, conjunto5, conjunto6) {
  interseccion_12345 <- IR5(conjunto1, conjunto2, conjunto3, conjunto4, conjunto5)
  return(IR2(interseccion_12345, conjunto6))
}

IR7 <- function(conjunto1, conjunto2, conjunto3, conjunto4, conjunto5, conjunto6, conjunto7) {
  interseccion_123456 <- IR6(conjunto1, conjunto2, conjunto3, conjunto4, conjunto5, conjunto6)
  return(IR2(interseccion_123456, conjunto7))
}
# <<== >>  <<== >>  <<== >>  <<== >>  <<== >> 
# INTERSECCION DE DOS CONJUNTOS
i <- 1; j <- 0; 
while(i <= NumSets){
  j <- i+1
  while(j <= NumSets){
    Listas <- list()
    SS1 <- Sets[[i]]
    SS2 <- Sets[[j]]
    interseccion <- IR2(SS1, SS2)
    inter <- as.data.frame(interseccion)
    nombre <- paste0("Intersecciones/","Interseccion","_Set",i,"_Set",j,".csv")
    write.csv(inter,nombre)
    j <- j+1;
  }
  i <- i+1; print(i)
}
# <<== >>  <<== >>  <<== >>  <<== >>  <<== >> 
# <<== >>  <<== >>  <<== >>  <<== >>  <<== >> 
# INTERSECCION DE TRES CONJUNTOS
i <- 1; j <- 0; k <- 0; 
while(i <= NumSets){
  j <- i+1
  while(j <= NumSets){
    k <- j+1
    while (k <= NumSets) {
      Listas <- list()
      SS1 <- Sets[[i]]
      SS2 <- Sets[[j]]
      SS3 <- Sets[[k]]
      interseccion <- IR3(SS1, SS2, SS3)
      inter <- as.data.frame(interseccion)
      nombre <- paste0("Intersecciones/","Interseccion","_Set",i,"_Set",j,"_Set",k,".csv")
      write.csv(inter,nombre)
      k <- k+1
    }
    j <- j+1; print(j)
  }
  i <- i+1; print(i)
}
# <<== >>  <<== >>  <<== >>  <<== >>  <<== >> 
# AHORA CUATRO INTERSECCIONES
i<- 1; j <- 0; k <- 0; l <- 0;
while (i <= NumSets) {
  j <- i+1
  while(j <= NumSets){
    k <- j+1
    while(k <= NumSets){
      l <- k+1
      while (l <= NumSets ) {
        Listas <- list()
        SS1 <- Sets[[i]]
        SS2 <- Sets[[j]]
        SS3 <- Sets[[k]]
        SS4 <- Sets[[l]]
        interseccion <- IR4(SS1, SS2, SS3, SS4)
        inter <- as.data.frame(interseccion)
        nombre <- paste0("Intersecciones/","Interseccion","_Set",i,"_Set",j,"_Set",k,"_Set",l,".csv")
        write.csv(inter,nombre)
        l <- l+1
      }
      k <- k+1
    }
    j <- j+1
  }
  i <- i+1
}
# <<== >>  <<== >>  <<== >>  <<== >>  <<== >> 
# AHORA CINCO INTERSECCIONES
i<- 1; j <- 0; k <- 0; l <- 0;m <- 0
while (i <= NumSets) {
  j <- i+1
  while(j <= NumSets){
    k <- j+1
    while(k <= NumSets){
      l <- k+1
      while (l <= NumSets ) {
        m <- l+1
        while (m <= NumSets) {
          Listas <- list()
          SS1 <- Sets[[i]]
          SS2 <- Sets[[j]]
          SS3 <- Sets[[k]]
          SS4 <- Sets[[l]]
          SS5 <- Sets[[m]]
          interseccion <- IR5(SS1, SS2, SS3, SS4,SS5)
          inter <- as.data.frame(interseccion)
          nombre <- paste0("Intersecciones/","Interseccion","_Set",i,"_Set",j,"_Set",k,"_Set",l,"_Set",m,".csv")
          write.csv(inter,nombre)
          m <- m+1
        }
        l <- l+1
      }
      k <- k+1
    }
    j <- j+1
  }
  i <- i+1
}
# <<== >>  <<== >>  <<== >>  <<== >>  <<== >> 
# <<== >>  <<== >>  <<== >>  <<== >>  <<== >> 
# AHORA SEIS INTERSECCIONES
i<- 1; j <- 0; k <- 0; l <- 0;m <- 0;n <- 0;
while (i <= NumSets) {
  j <- i+1
  while(j <= NumSets){
    k <- j+1
    while(k <= NumSets){
      l <- k+1
      while (l <= NumSets ) {
        m <- l+1
        while (m <= NumSets) {
          n <- m+1
          while (n <= NumSets) {
            Listas <- list()
            SS1 <- Sets[[i]]
            SS2 <- Sets[[j]]
            SS3 <- Sets[[k]]
            SS4 <- Sets[[l]]
            SS5 <- Sets[[m]]
            SS6 <- Sets[[n]]
            interseccion <- IR6(SS1, SS2, SS3, SS4, SS5, SS6)
            inter <- as.data.frame(interseccion)
            nombre <- paste0("Intersecciones/","Interseccion","_Set",i,"_Set",j,"_Set",k,"_Set",l,"_Set",m,"_Set",n,".csv")
            write.csv(inter,nombre)
            n <- n+1
          }
          m <- m+1
        }
        l <- l+1
      }
      k <- k+1
    }
    j <- j+1
  }
  i <- i+1
}
# <<== >>  <<== >>  <<== >>  <<== >>  <<== >> 
# <<== >>  <<== >>  <<== >>  <<== >>  <<== >> 
# AHORA SIETE INTERSECCIONES
i<- 1; j <- 0; k <- 0; l <- 0;m <- 0;n <- 0;r <- 0
while (i <= NumSets) {
  j <- i+1
  while(j <= NumSets){
    k <- j+1
    while(k <= NumSets){
      l <- k+1
      while (l <= NumSets ) {
        m <- l+1
        while (m <= NumSets) {
          n <- m+1
          while (n <= NumSets) {
            r <- n+1
            while(r <= NumSets){
              Listas <- list()
              SS1 <- Sets[[i]]
              SS2 <- Sets[[j]]
              SS3 <- Sets[[k]]
              SS4 <- Sets[[l]]
              SS5 <- Sets[[m]]
              SS6 <- Sets[[n]]
              SS7 <- Sets[[n+1]]
              interseccion <- IR7(SS1, SS2, SS3, SS4, SS5, SS6,SS7)
              inter <- as.data.frame(interseccion)
              nombre <- paste0("Intersecciones/","Interseccion","_Set",i,"_Set",j,"_Set",k,"_Set",l,"_Set",m,"_Set",n,"_Set",r,".csv")
              write.csv(inter,nombre)
              r <- r+1
            }
            n <- n+1
          }
          m <- m+1
        }
        l <- l+1
      }
      k <- k+1
    }
    j <- j+1
  }
  i <- i+1
}
# <<== >>  <<== >>  <<== >>  <<== >>  <<== >> 
# <<== >>  <<== >>  <<== >>  <<== >>  <<== >> 
