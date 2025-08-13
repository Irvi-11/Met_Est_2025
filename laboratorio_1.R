# Gastos mensuales
300 + 240 + 1527 + 400 + 1500 + 1833
celular <- 300
celular
transporte <- 240
comestibles <- 1527
gimnacio <- 400
alquiler <- 1500
otros <- 1833
total <- celular+transporte+comestibles+gimnacio+alquiler+otros
semestre <- total*5
anual <- total*10

# valor absoluto
abs(10)
abs(-4)

# raiz cuadrada
sqrt(9)

# logaritmo natural
log(2)

2*9
4+5 # comentarios

celular <- 300
Celular <--300
CELULAR <- 300

celular+Celular
CELULAR-celular
help(abs)
help(mean)
?abs
?mean
help.search("absolute")

gastos <- c(celular, transporte, comestibles, gimnacio, alquiler, otros)
gastos
barplot(gastos)
?sort
sort(gastos, decreasing = TRUE)
gastos_ord <- sort(gastos,decreasing = TRUE)

barplot(gastos_ord)
help("barplot")
barplot(gastos_ord, main = "Gastos mensuales", names.arg = c("otros", "comestibles", "alquiler", "gimnacio", "celular", "transporte"))


