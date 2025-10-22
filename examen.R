# irvi joana varela briones
url <-  "https://www.dropbox.com/s/3pi3huovq6qce42/obs.csv?dl=1"
suelo <- read.csv(url)
head (suelo)
suelo$zone <- as.factor(suelo$zone)
suelo$wrb1 <- as.factor(suelo$wrb1)
# act 1
summary_clay1 <- summary(suelo$Clay1)
summary_clay2 <- summary(suelo$Clay2)
summary_clay5 <- summary(suelo$Clay5)

cat("Resumen Clay1 (0-10 cm):\n"); print(summary_clay1)
cat("Resumen Clay2 (10-20 cm):\n"); print(summary_clay2)
cat("Resumen Clay5 (30-50 cm):\n"); print(summary_clay5)
# act 2

boxplot(suelo$Clay1,
        col = "violet",
        main = "Boxplot - Clay1 (0-10 cm)", 
        ylab = "Contenido de arcilla (%)")

cat("\nOutliers detectados en Clay1 (si hay):\n")

Q1 <- quantile(suelo$Clay1, 0.25)
Q3 <- quantile(suelo$Clay1, 0.75)
IQR <- Q3 - Q1
lim_sup <- Q3 + 1.5 * IQR
lim_inf <- Q1 - 1.5 * IQR

# Mostrar los valores fuera del rango
suelo[suelo$Clay1 > lim_sup | suelo$Clay1 < lim_inf, c("Clay1")]
# act 3
 t.test(suelo$Clay1, mu = 30)

# act 4

 plot(suelo$Clay1, suelo$Clay5,
      main = "Relación entre Clay1 y Clay5",
      xlab = "Arcilla 0–10 cm",
      ylab = "Arcilla 30–50 cm",
      col = "violet", pch = 19)
 
 cor.test(suelo$Clay1, suelo$Clay5)
 
 # act 5
 boxplot(suelo$Clay5 ~ suelo$zone,
         main = "Contenido de Arcilla (30–50 cm) por Zona",
         xlab = "Zona",
         ylab = "Clay5 (%)",
         col = "violet")
 
 