setwd("S:/FCEN-UBA/Introduccion a la Estadistica y Ciencia de Datos")

#Ejercicio 1

cancer <- read.csv("Practicas/Data/Debernardi.csv")

#Tabla con los valores totales (absolutos)
diag_abs <- table(cancer$diagnosis)

#Tabla de las proporciones
diag_rel <- prop.table(diag_abs)

print(paste(diag_abs,diag_rel))

#No es necesario pero asi se armaria la tabla conjunta:
diag_tabla <- data.frame(
  Diagnosis = names(diag_abs),
  Frecuencia = as.vector(diag_abs),
  Frecuencia_Relativa = as.vector(diag_rel)
)

print(diag_tabla)

#Grafico
barplot(diag_rel,
        main = "Frecuencia de Diagnosis",
        xlab = "Diagnosis",
        ylab = "Frecuencia",
        col = c("skyblue","red","green"),
        ylim = c(0,1)
)


#Ejercicio 2

