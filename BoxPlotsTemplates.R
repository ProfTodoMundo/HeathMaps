load("~/Desktop/TemporalEnsamble/HeathMaps-Principal/SiSirve2.RData")
#pdf("Boxplot.pdf")
boxplot(EiMybs_log2, las = 3)
stripchart(EiMybs_log2, method = "jitter", pch = 19, add = TRUE, col = "blue")
#dev.off()
colnames(EiMybs_log2) <- c("Trophozoites","en_8h",
                           "en_24h", "en_48h", "en_72h",
                           "ex_2h","ex_8h")

boxplot(EiMybs_log2, las = 3)
stripchart(EiMybs_log2, method = "jitter", pch = 19, add = TRUE, col = "blue")


boxplot(EiMybs_log2, col = rainbow(ncol(EiMybs_log2)))
boxplot(EiMybs_log2, # Datos
        horizontal = FALSE, # Horizontal o vertical
        lwd = 2, # Lines width
        col = rainbow(ncol(EiMybs_log2),alpha = 0.35), # Color
        xlab = "Etiqueta eje X",  # Etiqueta eje X
        ylab = "Etiqueta eje Y",  # Etiqueta eje Y
        main = "Boxplot personalizado en R base", # Título
        notch = FALSE, # Añade intervalos de confianza para la mediana
        border = "black",  # Color del borde del boxplot
        outpch = 20,       # Símbolo para los outliers
        outbg = "green",   # Color de los datos atípicos
        whiskcol = "blue", # Color de los bigotes
        whisklty = 2,      # Tipo de línea para los bigotes
        lty = 2) # Tipo de línea (caja y mediana)

# Agregamos una leyenda
legend("topright", legend = "Boxplot", # Posición y título
       fill = rgb(1, 0, 0, alpha = 0.4),  # Color
       inset = c(0.03, 0.05), # Cambiamos los márgenes
       bg = "white") # Color de fondo de la leyenda


par(mfrow = c(1, 2))

# << == >> << == >>  << == >> << == >>  << == >> << == >> 

# Generar una matriz de 47x7 con valores aleatorios entre 0 y 16.3
set.seed(42)  # Establecer una semilla para reproducibilidad

EiMybs <- matrix(runif(47*7, min = 0, max = 16.3), nrow = 47, ncol = 7)

# Cambiar los nombres de las columnas
colnames(EiMybs) <- c("Trophozoites", "en_8h", "en_24h", "en_48h", "en_72h", "ex_2h", "ex_8h")

# Generar una secuencia de palabras tipo "gen1", "gen2", ..., "gen100"
secuencia_palabras <- vector("character", 100)

for (i in 1:100) {
  secuencia_palabras[i] <- paste0("gen", i)
}

# Tomar una muestra de tamaño 47 de la secuencia de palabras
muestra <- sample(secuencia_palabras, size = 47)

# Agregar una nueva columna a la matriz EiMybs
EiMybs_con_muestra <- cbind(EiMybs, muestra)

# Asignar los valores de la columna agregada a los renglones de EiMybs
EiMybs[, "NuevaColumna"] <- EiMybs[, ncol(EiMybs)]

# Asignar nombres a los renglones de EiMybs utilizando la última columna agregada
rownames(EiMybs) <- EiMybs[, "NuevaColumna"]

# Transformar la matriz aplicando el logaritmo en base dos a cada entrada más uno
EiMybs_log2 <- log2(EiMybs + 1)

View(EiMybs_log2)

# 
# Establecer la cuadrícula de visualización
rows <- 2  # Número de filas de la cuadrícula
columns <- 3  # Número de columnas de la cuadrícula
par(mfrow = c(rows, columns))

# Generar los boxplots comparando con la primera columna
for (i in 2:ncol(EiMybs_log2)) {
  boxplot(EiMybs_log2[, i], col = "lightblue", main = colnames(EiMybs_log2)[i],
          ylab = "Log2 Transform", xlab = colnames(EiMybs_log2)[1])
}
par
# Generar el boxplot de EiMybs_log2
par(mfrow = c(1,1))
boxplot(EiMybs_log2, col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightcyan", "lightgray", "lightcoral"),
        main = "Boxplot de EiMybs_log2", xlab = "Columnas", ylab = "Log2 Transform")

# Crear la leyenda con los nombres y colores asignados
nombres_columnas <- colnames(EiMybs_log2)
colores <- c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightcyan", "lightgray", "lightcoral")
leyenda <- legend("right", legend = nombres_columnas, fill = colores, bty = "n", title = "Columnas")

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)

# Generar el boxplot de EiMybs_log2
boxplot(EiMybs_log2, col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightcyan", "lightgray", "lightcoral"),
        main = "Boxplot de EiMybs_log2", xlab = "Columnas", ylab = "Log2 Transform")

# Crear la leyenda con los nombres y colores asignados
nombres_columnas <- colnames(EiMybs_log2)
colores <- c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightcyan", "lightgray", "lightcoral")
leyenda <- legend("right", legend = nombres_columnas, fill = colores, bty = "n", title = "Columnas", cex = 0.8)

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)

# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>
# Generar el boxplot de EiMybs_log2
boxplot(EiMybs_log2, col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightcyan", "lightgray", "lightcoral"),
        main = "Boxplot de EiMybs_log2", xlab = "", ylab = "Log2 Transform")

# Crear la leyenda con los nombres y colores asignados
nombres_columnas <- colnames(EiMybs_log2)
colores <- c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightcyan", "lightgray", "lightcoral")
leyenda <- legend("topright", legend = nombres_columnas, fill = colores, bty = "n", cex = 0.8,
                  xjust = 1, yjust = 1)

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)

# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>

# Generar el boxplot de EiMybs_log2 con colores más claros
boxplot(EiMybs_log2, col = c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999"),
        main = "Boxplot de EiMybs_log2", xlab = "", ylab = "Log2 Transform")

# Crear la leyenda con los nombres y colores asignados
nombres_columnas <- colnames(EiMybs_log2)
colores <- c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999")
leyenda <- legend("topright", legend = nombres_columnas, fill = colores, bty = "n", cex = 0.8,
                  xjust = 1, yjust = 1)

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)


# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>

# Generar el boxplot de EiMybs_log2 con colores más claros
boxplot(EiMybs_log2, col = c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999"),
        main = "Boxplot de EiMybs_log2", xlab = "", ylab = "Log2 Transform")

# Establecer la orientación vertical de las etiquetas del eje x
par(las = 2)

# Ajustar el tamaño de las etiquetas del eje x
par(cex.axis = 0.8)

# Crear la leyenda con los nombres y colores asignados
nombres_columnas <- colnames(EiMybs_log2)
colores <- c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999")
leyenda <- legend("topright", legend = nombres_columnas, fill = colores, bty = "n", cex = 0.8,
                  xjust = 1, yjust = 1)

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)

# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>

# Generar el boxplot de EiMybs_log2 con colores más claros
boxplot(EiMybs_log2, col = c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999"),
        main = "Boxplot de EiMybs_log2", xlab = "", ylab = "Log2 Transform")

# Crear la leyenda con los nombres y colores asignados en la parte superior izquierda
nombres_columnas <- colnames(EiMybs_log2)
colores <- c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999")
leyenda <- legend("topleft", legend = nombres_columnas, fill = colores, bty = "n", cex = 0.7)

# Ajustar el tamaño de la letra en las etiquetas del eje x
par(cex.axis = 0.8)

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)

# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>

# Generar el boxplot de EiMybs_log2 con colores más claros
boxplot(EiMybs_log2, col = c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999"),
        main = "Boxplot de EiMybs_log2", xlab = "", ylab = "Log2 Transform")

# Obtener los valores de las medianas y los límites del boxplot
valores_estadisticos <- boxplot(EiMybs_log2, plot = FALSE)

# Agregar las medias y los entornos
points(valores_estadisticos$stats[3, ], pch = 19, col = "red", cex = 1.5)  # Medias
segments(1:ncol(EiMybs_log2), valores_estadisticos$stats[2, ], 1:ncol(EiMybs_log2), valores_estadisticos$stats[4, ],
         lty = "dashed", col = "black", lwd = 0.5)  # Límites del boxplot

# Crear la leyenda con los nombres y colores asignados en la parte superior izquierda
nombres_columnas <- colnames(EiMybs_log2)
colores <- c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999")
leyenda <- legend("topleft", legend = nombres_columnas, fill = colores, bty = "n", cex = 0.7)

# Ajustar el tamaño de la letra en las etiquetas del eje x
par(cex.axis = 0.8)

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)

# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>
# Generar el boxplot de EiMybs_log2 con colores más claros y entornos de cajas azules
boxplot(EiMybs_log2, col = c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999"),
        border = "blue", lwd = 0.5, main = "Boxplot de EiMybs_log2", xlab = "", ylab = "Log2 Transform")

# Obtener los valores de las medianas y los límites del boxplot
valores_estadisticos <- boxplot(EiMybs_log2, plot = FALSE)

# Agregar las medias y los entornos
points(valores_estadisticos$stats[3, ], pch = 19, col = "red", cex = 1.5)  # Medias
segments(1:ncol(EiMybs_log2), valores_estadisticos$stats[2, ], 1:ncol(EiMybs_log2), valores_estadisticos$stats[4, ],
         lty = "dashed", col = "black", lwd = 0.5)  # Límites del boxplot

# Crear la leyenda con los nombres y colores asignados en la parte superior izquierda
nombres_columnas <- colnames(EiMybs_log2)
colores <- c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999")
leyenda <- legend("topleft", legend = nombres_columnas, fill = colores, bty = "n", cex = 0.7)

# Ajustar el tamaño de la letra en las etiquetas del eje x
par(cex.axis = 0.8)

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)


# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>
# Generar el boxplot de EiMybs_log2 con colores más claros y entornos de cajas azules
boxplot(EiMybs_log2, col = c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999"),
        border = "blue", lwd = 0.5, pch = "*", main = "Boxplot de EiMybs_log2", xlab = "", ylab = "Log2 Transform")

# Obtener los valores de las medianas y los límites del boxplot
valores_estadisticos <- boxplot(EiMybs_log2, plot = FALSE)

# Agregar las medias y los entornos
points(valores_estadisticos$stats[3, ], pch = 19, col = "red", cex = 1.5)  # Medias
segments(1:ncol(EiMybs_log2), valores_estadisticos$stats[2, ], 1:ncol(EiMybs_log2), valores_estadisticos$stats[4, ],
         lty = "dashed", col = "black", lwd = 0.5)  # Límites del boxplot

# Crear la leyenda con los nombres y colores asignados en la parte superior izquierda
nombres_columnas <- colnames(EiMybs_log2)
colores <- c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999")
leyenda <- legend("topleft", legend = nombres_columnas, fill = colores, bty = "n", cex = 0.7)

# Ajustar el tamaño de la letra en las etiquetas del eje x
par(cex.axis = 0.8)

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)


# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>

# Generar el boxplot de EiMybs_log2 con colores más claros y entornos de cajas azules
boxplot(EiMybs_log2, col = c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999"),
        border = "blue", lwd = 0.5, pch = "", main = "Boxplot de EiMybs_log2", xlab = "", ylab = "Log2 Transform")

# Obtener los valores de las medianas y los límites del boxplot
valores_estadisticos <- boxplot(EiMybs_log2, plot = FALSE)

# Agregar las medias y los entornos
points(valores_estadisticos$stats[3, ], pch = 19, col = "red", cex = 1.5)  # Medias
segments(1:ncol(EiMybs_log2), valores_estadisticos$stats[2, ], 1:ncol(EiMybs_log2), valores_estadisticos$stats[4, ],
         lty = "dashed", col = "black", lwd = 0.5)  # Límites del boxplot

# Agregar los valores dentro de cada caja como círculos pequeños
for (i in 1:ncol(EiMybs_log2)) {
  text(i, valores_estadisticos$stats[3, i], round(valores_estadisticos$stats[3, i], 2), pos = 3, cex = 0.7)
}

# Crear la leyenda con los nombres y colores asignados en la parte superior izquierda
nombres_columnas <- colnames(EiMybs_log2)
colores <- c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999")
leyenda <- legend("topleft", legend = nombres_columnas, fill = colores, bty = "n", cex = 0.7)

# Ajustar el tamaño de la letra en las etiquetas del eje x
par(cex.axis = 0.8)

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)

# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>

# Generar el boxplot de EiMybs_log2 con colores más claros y entornos de cajas azules
boxplot(EiMybs_log2, col = c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999"),
        border = "blue", lwd = 0.5, main = "Boxplot de EiMybs_log2", xlab = "", ylab = "Log2 Transform")

# Obtener los valores de las medianas y los límites del boxplot
valores_estadisticos <- boxplot(EiMybs_log2, plot = FALSE)

# Agregar las medias y los entornos
points(valores_estadisticos$stats[3, ], pch = 19, col = "red", cex = 1.5)  # Medias
segments(1:ncol(EiMybs_log2), valores_estadisticos$stats[2, ], 1:ncol(EiMybs_log2), valores_estadisticos$stats[4, ],
         lty = "dashed", col = "black", lwd = 0.5)  # Límites del boxplot

# Agregar los valores de la media y los outliers dentro de cada caja
text(1:ncol(EiMybs_log2), valores_estadisticos$stats[3, ],
     labels = round(valores_estadisticos$stats[3, ], 2), pos = 3, col = "red", cex = 0.7)  # Medias
text(1:ncol(EiMybs_log2), valores_estadisticos$out, labels = round(valores_estadisticos$out, 2),
     pos = 1, col = "black", cex = 0.7)  # Outliers

# Crear la leyenda con los nombres y colores asignados en la parte superior izquierda
nombres_columnas <- colnames(EiMybs_log2)
colores <- c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999")
leyenda <- legend("topleft", legend = nombres_columnas, fill = colores, bty = "n", cex = 0.7)

# Ajustar el tamaño de la letra en las etiquetas del eje x
par(cex.axis = 0.8)

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)

# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>
# Generar el boxplot de EiMybs_log2 con colores más claros y entornos de cajas azules
boxplot(EiMybs_log2, col = c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999"),
        border = "blue", lwd = 0.5, main = "Boxplot de EiMybs_log2", xlab = "", ylab = "Log2 Transform")

# Obtener los valores de las medianas y los límites del boxplot
valores_estadisticos <- boxplot(EiMybs_log2, plot = FALSE)

# Agregar las medias y los entornos
points(valores_estadisticos$stats[3, ], pch = 19, col = "red", cex = 1.5)  # Medias
segments(1:ncol(EiMybs_log2), valores_estadisticos$stats[2, ], 1:ncol(EiMybs_log2), valores_estadisticos$stats[4, ],
         lty = "dashed", col = "black", lwd = 0.5)  # Límites del boxplot

# Agregar los valores de la media y los outliers dentro de cada caja
text(1:ncol(EiMybs_log2), valores_estadisticos$stats[3, ],
     labels = round(valores_estadisticos$stats[3, ], 2), pos = 3, col = "red", cex = 0.7)  # Medias
text(1:ncol(EiMybs_log2), rep(valores_estadisticos$out, each = 5), labels = round(valores_estadisticos$out, 2),
     pos = 1, col = "black", cex = 0.7)  # Outliers

# Crear la leyenda con los nombres y colores asignados en la parte superior izquierda
nombres_columnas <- colnames(EiMybs_log2)
colores <- c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999")
leyenda <- legend("topleft", legend = nombres_columnas, fill = colores, bty = "n", cex = 0.7)

# Ajustar el tamaño de la letra en las etiquetas del eje x
par(cex.axis = 0.8)

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)


# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>

# Generar el boxplot de EiMybs_log2 con colores más claros y entornos de cajas azules
boxplot(EiMybs_log2, col = c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999"),
        border = "blue", lwd = 0.5, main = "Boxplot de EiMybs_log2", xlab = "", ylab = "Log2 Transform")

# Obtener los valores de las medianas y los límites del boxplot
valores_estadisticos <- boxplot(EiMybs_log2, plot = FALSE)

# Agregar las medias y los entornos
points(valores_estadisticos$stats[3, ], pch = 19, col = "red", cex = 1.5)  # Medias
segments(1:ncol(EiMybs_log2), valores_estadisticos$stats[2, ], 1:ncol(EiMybs_log2), valores_estadisticos$stats[4, ],
         lty = "dashed", col = "black", lwd = 0.5)  # Límites del boxplot

# Obtener los valores más altos de los outliers
valores_outliers_max <- sapply(valores_estadisticos$out, max)

# Agregar los valores más altos de los outliers dentro de cada caja
text(1:ncol(EiMybs_log2), rep(valores_outliers_max, each = 1), labels = round(valores_outliers_max, 2),
     pos = 1, col = "black", cex = 0.7)  # Outliers

# Crear la leyenda con los nombres y colores asignados en la parte superior izquierda
nombres_columnas <- colnames(EiMybs_log2)
colores <- c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999")
leyenda <- legend("topleft", legend = nombres_columnas, fill = colores, bty = "n", cex = 0.7)

# Ajustar el tamaño de la letra en las etiquetas del eje x
par(cex.axis = 0.8)

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)



# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>
# Generar el boxplot de EiMybs_log2 con colores más claros y entornos de cajas azules
boxplot(EiMybs_log2, col = c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999"),
        border = "blue", lwd = 0.5, main = "Boxplot de EiMybs_log2", xlab = "", ylab = "Log2 Transform")

# Obtener los valores de las medianas y los límites del boxplot
valores_estadisticos <- boxplot(EiMybs_log2, plot = FALSE)

# Agregar las medias y los entornos
points(valores_estadisticos$stats[3, ], pch = 19, col = "red", cex = 1.5)  # Medias
segments(1:ncol(EiMybs_log2), valores_estadisticos$stats[2, ], 1:ncol(EiMybs_log2), valores_estadisticos$stats[4, ],
         lty = "dashed", col = "black", lwd = 0.5)  # Límites del boxplot

# Obtener los valores más altos de los outliers
valores_outliers_max <- sapply(valores_estadisticos$out, max)

# Agregar los valores de las medias dentro de cada caja
text(1:ncol(EiMybs_log2), valores_estadisticos$stats[3, ],
     labels = round(valores_estadisticos$stats[3, ], 2), pos = 3, col = "red", cex = 0.7)  # Medias

# Agregar los valores más altos de los outliers dentro de cada caja
text(1:ncol(EiMybs_log2), rep(valores_outliers_max, each = 1), labels = round(valores_outliers_max, 2),
     pos = 1, col = "black", cex = 0.7)  # Outliers

# Crear la leyenda con los nombres y colores asignados en la parte superior izquierda
nombres_columnas <- colnames(EiMybs_log2)
colores <- c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999")
leyenda <- legend("topleft", legend = nombres_columnas, fill = colores, bty = "n", cex = 0.7)

# Ajustar el tamaño de la letra en las etiquetas del eje x
par(cex.axis = 0.8)

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)

# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>
# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>
load("~/Desktop/TemporalEnsamble/HeathMaps-Principal/SiSirve2.RData")
# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>
# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>
boxplot(EiMybs_log2, 
        col = c("#CCEEFF", "#CCFFCC", "#FFCCE6", 
                "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999"),
        border = "blue", 
        lwd = 0.5, 
        main = "EiMybs", 
        xlab = "", 
        ylab = "Log2(EiMybs)")

# Obtener los valores de las medianas y los límites del boxplot
valores_estadisticos <- boxplot(EiMybs_log2, plot = FALSE)

# Agregar las medias y los entornos
points(valores_estadisticos$stats[3, ], 
       pch = 19, 
       col = "red", 
       cex = 1.5)  # Medias

segments(1:ncol(EiMybs_log2), valores_estadisticos$stats[2, ], 
         1:ncol(EiMybs_log2), valores_estadisticos$stats[4, ],
         lty = "dashed",
         col = "black", 
         lwd = 0.5)  # Límites del boxplot

# Obtener los valores más altos de los outliers
valores_outliers_max <- sapply(valores_estadisticos$out, max)

# Agregar los valores de las medias dentro de cada caja
text(1:ncol(EiMybs_log2), valores_estadisticos$stats[3, ],
     labels = round(valores_estadisticos$stats[3, ], 2), 
     pos = 3, col = "red", cex = 0.7)  # Medias

# Agregar los valores más altos de los outliers dentro de cada caja
text(1:ncol(EiMybs_log2), 
     rep(valores_outliers_max, each = 1), 
     labels = round(valores_outliers_max, 2),
     pos = 1, 
     col = "black", 
     cex = 0.7)  # Outliers

# Crear la leyenda con los nombres y colores asignados en la parte superior izquierda
nombres_columnas <- colnames(EiMybs_log2)
colores <- c("#CCEEFF", "#CCFFCC", "#FFCCE6", "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999")
leyenda <- legend("topright", 
                  legend = nombres_columnas, 
                  fill = colores, 
                  bty = "n", 
                  cex = 0.7)

# Ajustar el tamaño de la letra en las etiquetas del eje x
par(cex.axis = 0.8, las = 2)  # Orientación vertical del texto del eje x

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)
# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>
# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>
# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>

# Generar el boxplot de EiMybs_log2 con colores degradados
boxplot(EiMybs_log2,
        col = sapply(EiMybs_log2, function(x) {
          col <- rainbow(length(x))
          mediana <- median(x)
          porcentaje <- ecdf(x)(mediana)
          col[1:round(length(x) * porcentaje)] <- colorRampPalette(c("white", col[1]))(round(length(x) * porcentaje))
          return(col)
        }),
        border = "blue",
        lwd = 0.5,
        main = "EiMybs",
        xlab = "",
        ylab = "Log2(EiMybs)")

# Obtener los valores de las medianas y los límites del boxplot
valores_estadisticos <- boxplot(EiMybs_log2, plot = FALSE)

# Agregar las medias y los entornos
points(valores_estadisticos$stats[3, ],
       pch = 19,
       col = "red",
       cex = 1.5)  # Medias

segments(1:ncol(EiMybs_log2), valores_estadisticos$stats[2, ],
         1:ncol(EiMybs_log2), valores_estadisticos$stats[4, ],
         lty = "dashed",
         col = "black",
         lwd = 0.5)  # Límites del boxplot

# Obtener los valores más altos de los outliers
valores_outliers_max <- sapply(valores_estadisticos$out, max)

# Agregar los valores de las medias dentro de cada caja
text(1:ncol(EiMybs_log2), valores_estadisticos$stats[3, ],
     labels = round(valores_estadisticos$stats[3, ], 2),
     pos = 3, col = "red", cex = 0.7)  # Medias

# Agregar los valores más altos de los outliers dentro de cada caja
text(1:ncol(EiMybs_log2),
     rep(valores_outliers_max, each = 1),
     labels = round(valores_outliers_max, 2),
     pos = 1,
     col = "black",
     cex = 0.7)  # Outliers

# Crear la leyenda con los nombres y colores asignados en la parte superior izquierda
nombres_columnas <- colnames(EiMybs_log2)
colores <- sapply(EiMybs_log2, function(x) {
  col <- rainbow(length(x))
  mediana <- median(x)
  porcentaje <- ecdf(x)(mediana)
  col[1:round(length(x) * porcentaje)] <- colorRampPalette(c("white", col[1]))(round(length(x) * porcentaje))
  return(col[length(col) %/% 2])
})
leyenda <- legend("topright",
                  legend = nombres_columnas,
                  fill = colores,
                  bty = "n",
                  cex = 0.7)

# Ajustar el tamaño de la letra en las etiquetas del eje x
par(cex.axis = 0.8, las = 2)  # Orientación vertical del texto del eje x

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)


# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>
# Generar el boxplot de EiMybs_log2 con colores degradados
boxplot(EiMybs_log2,
        col = sapply(EiMybs_log2, function(x) {
          col <- c("#CCEEFF", "#CCFFCC", "#FFCCE6", 
                   "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999")
          mediana <- median(x)
          porcentaje <- ecdf(x)(mediana)
          col[1:round(length(col) * porcentaje)] <- colorRampPalette(c("white", col[1]))(round(length(col) * porcentaje))
          return(col)
        }),
        border = "blue",
        lwd = 0.5,
        main = "EiMybs",
        xlab = "",
        ylab = "Log2(EiMybs)")

# Obtener los valores de las medianas y los límites del boxplot
valores_estadisticos <- boxplot(EiMybs_log2, plot = FALSE)

# Agregar las medias y los entornos
points(valores_estadisticos$stats[3, ],
       pch = 19,
       col = "red",
       cex = 1.5)  # Medias

segments(1:ncol(EiMybs_log2), valores_estadisticos$stats[2, ],
         1:ncol(EiMybs_log2), valores_estadisticos$stats[4, ],
         lty = "dashed",
         col = "black",
         lwd = 0.5)  # Límites del boxplot

# Obtener los valores más altos de los outliers
valores_outliers_max <- sapply(valores_estadisticos$out, max)

# Agregar los valores de las medias dentro de cada caja
text(1:ncol(EiMybs_log2), valores_estadisticos$stats[3, ],
     labels = round(valores_estadisticos$stats[3, ], 2),
     pos = 3, col = "red", cex = 0.7)  # Medias

# Agregar los valores más altos de los outliers dentro de cada caja
text(1:ncol(EiMybs_log2),
     rep(valores_outliers_max, each = 1),
     labels = round(valores_outliers_max, 2),
     pos = 1,
     col = "black",
     cex = 0.7)  # Outliers

# Crear la leyenda con los nombres y colores asignados en la parte superior izquierda
nombres_columnas <- colnames(EiMybs_log2)
colores <- sapply(EiMybs_log2, function(x) {
  col <- c("#CCEEFF", "#CCFFCC", "#FFCCE6", 
           "#FFFFCC", "#CCFFFF", "#E6E6E6", "#FF9999")
  mediana <- median(x)
  porcentaje <- ecdf(x)(mediana)
  col[1:round(length(col) * porcentaje)] <- colorRampPalette(c("white", col[1]))(round(length(col) * porcentaje))
  return(col)
})
leyenda <- legend("topright",
                  legend = nombres_columnas,
                  fill = colores,
                  bty = "n",
                  cex = 0.7)

# Ajustar el tamaño de la letra en las etiquetas del eje x
par(cex.axis = 0.8, las = 2)  # Orientación vertical del texto del eje x

# Ajustar los márgenes para incluir la leyenda completa
par(mar = c(5, 5, 4, 10) + 0.1)


# <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>>>>> <<<<<<>

