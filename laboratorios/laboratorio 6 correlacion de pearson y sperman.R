# --------------------------------------------------
# correlacion de person
# datos de geyser pld fatihful
# 24/0972025
data("faithful")
plot(faithful$waiting, faithful$eruptions,
     xlab = "tiempo de espera",
     ylab = "erupcion (min)",
     col= "violet",
     pch= 20)
#---------------------------------------------------
# correlacionar las dos variables

shapiro.test(faithful$eruptions)
shapiro.test(faithful$waiting)

cor.test(faithful$waiting, faithful$eruptions, 
         method ="pearson")

#---------------------------------------------------------
#sperman se utiliza como comprobante de datos no normales
#---------------------------------------------------------

cor.test(faithful$waiting, faithful$eruptions,
         method = "spearman")


resp <- data.frame ( Tiempo = c(12, 15,17,18, 20, 21, 22, 26),
                   Edad = c(13, 25, 28, 35, 45, 30, 60, 95))
resp

# crear nueva columna con los rangos (1 a 8)

resp$rango_Tiempo <- rank(resp$Tiempo, ties.method = "first")
resp$rango_Edad <- rank(resp$Edad, ties.method = "first")

resp
     
plot(resp$Tiempo, resp$Edad)  
plot(resp$rango_Tiempo, resp$rango_Edad)

resp$dif <- resp$rango_Tiempo - resp$rango_Edad
resp$dif2 <- resp$dif^2
sum(resp$dif2)

cor.test(resp$rango_Tiempo, resp$rango_Edad, method = "spearman")
cor.test(resp$Tiempo, resp$Edad, method = "spearman")



#------------------------------------------------------------------------



set.seed(123) # para producibilidad
# numero de obserbaciones 
n <- 20
horas_estudio <- sample(1:10, n, replace = TRUE)
resultado <- sappy (horas_estudio, funcion(horas)(ifelse(runif(1)<(horas/10), "aprobado", "reprobado")))

estudio <- data.frame(
  estudiante 1:n,
  horas_estudio,
  resultado
)
estudio$resultado_bin <- ifelse(estudio$resultado==)