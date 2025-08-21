`temperatura` <- read.csv("c:/repositorios joana/Met_Est_2025/temperatura (2).csv")
view(`temperatura`)

head(`temperatura`)# primeras 6 filas
dim(`temperatura`) # numero de filas y columnas 
names(`temperatura`) #nombre de las columnas
str(`temperatura`)# estructura del objeto 

# resumen estadistico 
summary(`temperatura`)

# modificacion de nombres de las columnas
name(`temperatura`) <- c("Año", "Ene", "Feb", "Mar",
                         "Abr", "May", "jun", "Jul", "Ago", "Sep",
                         "oct", "Nov", "Dic")

names(`temperatura`)

# crear columna media anual con temperatura mediaanual
`temperatura`$Ene
`temperatura`$media_anual <- rowMeans(`temperatura`[,2:13])

# crear objeto con medias mensuales de temperatura

medias_mensuales <- colMeans(`temperatura`[,2:13])
medias_mensuales
help("boxplot")

boxplot(`temperatura`$Ene,
        ylab="°C",
        col="orange")

datos_meses <- `temperatura`[,2:13]
boxplot(datos_meses,
        main="temperatura de enero",
        ylab= "°C",
        col="blue")
names <- c("Año", "Ene", "Feb", "Mar",
                 "Abr", "May", "jun", "Jul", "Ago", "Sep",
                 "oct", "Nov", "Dic")