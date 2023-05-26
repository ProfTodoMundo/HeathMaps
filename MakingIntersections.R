# << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >>
library(ggplot2)
library(dplyr)
library(readr)
library(cowplot)
library(RColorBrewer)
#library(ggVennDiagram)
library(VennDiagram)
library(venn)
# << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >> << == >>
#setwd("~/Nextcloud/NubeGralCarlos/ElisaProject/NuevoProyecto")
setwd("~/Nextcloud/nubegeneral/ElisaProject/NuevoProyecto")
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
# 
