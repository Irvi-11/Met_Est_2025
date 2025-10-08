# velocidad vs abundancia
speed <- c(2,3,5,9,14,24,29,34)
abundance <- c(6,3,5,23,16,12,48,43)

# Diagrama de dispersión
plot(speed, abundance,
     xlab="Speed (m/s)", ylab="Abundance (efímeras)",
     main="Speed vs Abundance",
     pch=19)

# Ajustar línea de regresión simple para visualizar
abline(lm(abundance ~ speed), lty=2)

# Correlación de Pearson y prueba
ct <- cor.test(speed, abundance, method="pearson")
ct

# Extraer valores para reporte
r <- ct$estimate          # r
pval <- ct$p.value        # p
n <- length(speed)
df <- n - 2               # grados de libertad
cat("r =", round(r,3), "\n")
cat("df =", df, "\n")
cat("p =", signif(pval,3), "\n")

# -------------------------------------------------------------------------------

# Columnas: Gp, Block, pH, N, Dens, P, Ca, Mg, K, Na, Conduc
suelo <- data.frame(
  Gp = c("T0","T0","T0","T0","T1","T1","T1"),
  Block = c(1,2,3,4,1,2,3),
  pH = c(5.40,5.65,5.14,5.14,5.14,5.10,4.70),
  N  = c(0.188,0.165,0.260,0.169,0.164,0.094,0.100),
  Dens = c(0.92,1.04,0.95,1.10,1.12,1.22,1.52),
  P = c(215,208,300,248,174,129,117),
  Ca = c(16.35,12.25,13.02,11.92,14.17,8.55,8.74),
  Mg = c(7.65,5.15,5.68,7.88,8.12,6.92,8.16),
  K = c(0.72,0.71,0.68,1.09,0.70,0.81,0.39),
  Na = c(1.14,0.94,0.60,1.01,2.17,2.67,3.32),
  Conduc = c(1.09,1.35,1.41,1.64,1.85,3.18,4.16)
)

# variables numéricas a correlacionar
vars <- suelo[, c("pH","N","Dens","P","Ca","Mg","K","Na","Conduc")]

# Matriz de correlación
R <- cor(vars, use="pairwise.complete.obs", method="pearson")
round(R,3)

# p-values para cada par (usando Hmisc)
if(!requireNamespace("Hmisc", quietly=TRUE)) install.packages("Hmisc")
library(Hmisc)
rcorr_res <- Hmisc::rcorr(as.matrix(vars), type="pearson")
R_values <- rcorr_res$r
P_values <- rcorr_res$P

# Mostrar tabla combinada (r y p)
pairs <- expand.grid(row=colnames(R_values), col=colnames(R_values), stringsAsFactors=FALSE)
pairs$cor <- as.vector(R_values)
pairs$pval <- as.vector(P_values)
# Filtrar sólo la mitad superior (opcional)
pairs

# matriz con r y p separados
round(R_values,3)
round(P_values,4)

# Correlograma (visual)
if(!requireNamespace("corrplot", quietly=TRUE)) install.packages("corrplot")
library(corrplot)
corrplot(R_values, method = "circle", type = "upper", tl.col = "black", tl.srt = 45) 

report <- data.frame(
  Var1 = rep(colnames(R_values), each=ncol(R_values)),
  Var2 = rep(colnames(R_values), ncol(R_values)),
  r = as.vector(round(R_values,3)),
  p = as.vector(round(P_values,4))
)

report <- report[as.character(report$Var1) < as.character(report$Var2), ]
report

#---------------------------------------------------------------------------------------------------------------------

# anscombe tiene x1..x4 y y1..y4

par(mfrow=c(2,2))
for(i in 1:4){
  x <- anscombe[[paste0("x",i)]]
  y <- anscombe[[paste0("y",i)]]
  plot(x,y, main=paste("Anscombe I",i), pch=19, xlab="x", ylab="y")
  abline(lm(y ~ x), col="blue", lty=2)
  # imprimir r y p en la gráfica
  ct <- cor.test(x,y)
  txt <- paste0("r=", round(ct$estimate,3), "\n p=", signif(ct$p.value,3))
  mtext(txt, side=3, adj=1, cex=0.8)
}
par(mfrow=c(1,1))

# Tabla con estadísticas resumidas para los 4 conjuntos
res <- data.frame(
  conjunto = 1:4,
  r = sapply(1:4, function(i) cor(anscombe[[paste0("x",i)]], anscombe[[paste0("y",i)]])),
  lm_intercept = sapply(1:4, function(i) coef(lm(anscombe[[paste0("y",i)]] ~ anscombe[[paste0("x",i)]]))[1]),
  lm_slope = sapply(1:4, function(i) coef(lm(anscombe[[paste0("y",i)]] ~ anscombe[[paste0("x",i)]]))[2])
)
round(res,3)