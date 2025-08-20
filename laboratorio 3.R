# laboratorio semana 3
# 20/08/2025
# Irvi joana varela briones



# importar datos -----------------------------------------
  

temp <- read.csv("temperatura.csv", header = TRUE)
temp <- read.csv("data/medias_temp.csv" ,header = T)

# ingregresar datos de manera manual
 
edad <- c(18,19,18,18,25,19,18,18,18,17,19,
          19,18,17,19,18,19,19)

alumno <- seq(1,18,1)
 


info$altura <- c(174,174,170,160,158,155,
          188,170,175,170,172,170,174,161,
          180,158,188,164)

# grafica datos -----------------------------------------

boxplot(info$altura,
        #col sirve para colorear la grafica
        col = "indianred", 
        # main sirve para poner titulo
        main = "clase 3 semestre")

colores = c("navajowhite", "indianred", "skyblue" )
boxplot(datos_meses, col=colores)
        
datos_meses <- 
# estadistica descriptivas        



















