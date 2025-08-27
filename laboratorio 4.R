# pruebas de t
# caso de muestra independiente
# MAGT
# 27/08/2025
  
# importar datos indice de calidad
calidad <- read.csv("calidad_planta.csv", header = T )

calidad$Tratamiento <- as.factor(calidad$Tratamiento)

colores <- c("blue", "violet")
boxplot(calidad$IE ~ calidad$Tratamiento,
        col= colores,
        xlab = "tratamientos",
        ylab = "indice de calidad",
        ylin=c(0.4,1.2),
        main="vivero iturbide")

# estadistica descriptiva
# tapply sirve para obtener un valor cuando contamos con dos grupos

tapply(calidad$IE, calidad$Tratamiento, mean)
tapply(calidad$IE, calidad$Tratamiento, var)
 
# revisa el comportamiento de los datos
 
library("ggplot2")

ggplot(calidad, aes(x=IE, color=Tratamiento))+
  geom_density()

ggplot(calidad, aes(x=IE, color=Tratamiento))+
  geom_histogram()