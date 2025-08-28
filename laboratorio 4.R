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

# separar los datos con tratamientos

df_Ctlr <- subset(calidad, Tratamiento == "Ctrl")
df_fert <- subset(calidad, Tratamiento != "Ctrl")

# goplot revisar la 

par(mfrow = c (1,2))
qqnorm(df_Ctlr$IE);qqline(df_Ctlr$IE)
qqnorm(df_fert$IE);qqline(df_fert$IE)


shapiro.test(df_Ctlr$IE)
shapiro.test(df_fert$IE)

# revisar la homogeniedad 

var.test(df_Ctlr$IE, df_fert$IE)
var.test(calidad$IE ~ calidad$Tratamiento)

# aplicar la prueba de t, variansas iguales 
# dos colas = two.sided

t.test(calidad$IE ~ calidad$Tratamiento,
       var.equal = T,
       alternative = "two.sided")

cohens_efecto <- function(x, y) {
  n1 <- length(x); n2 <- length(y)
  s1 <- sd(x); s2 <- sd(y) 
  sp <- sqrt(((n1 -1) * s1^2 + (n2 -1) * s2^2) / (n1 + n2 -2))
  (mean(x) - mean(y)) / sp
}
d_cal <- cohens_efecto(df_Ctlr$IE, df_fert$IE)
