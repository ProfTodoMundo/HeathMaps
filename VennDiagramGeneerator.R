library(venn)
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
library(readr)
library(dplyr)
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
library(readr)
library(dplyr)
library(ggplot2)
library(venn)
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