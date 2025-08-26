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

titanic <- read.csv("Practicas/Data/datos_titanic.csv")
names(titanic)

#a

#proba de ser mujer y sobrevivir 

sum(titanic$Sex == 'female' & titanic$Survived == 1) / sum(titanic$Survived==1)

#proba de ser mujer

sum(titanic$Sex == 'female') / nrow(titanic)


#b
abs <- table(as.factor(titanic$Survived), as.factor(titanic$Pclass))

rel <-prop.table(abs, margin = 2)

#c

barplot(rel,
        beside = TRUE,      
        col = c("tomato", "steelblue"),
        legend = rownames(rel),      
        names.arg = c("1ª Clase", "2ª Clase", "3ª Clase"),
        xlab = "Clase de Pasajero",
        ylab = "Cantidad de pasajeros",
        main = "Supervivencia por Clase en el Titanic"
        )


#Ejercicio 3

iridio <- read.csv("Practicas/Data/iridio.txt", sep = " ")
rodio <- read.csv("Practicas/Data/rodio.txt", sep = " ")

iridio <- iridio[[1]]
rodio  <- rodio[[1]]

#a

par(mfrow = c(1,2))

hist(iridio, main = "Iridio", col = 'tomato')

hist(rodio, main = "Rodio", col = 'steelblue')

par(mfrow = c(1,1))

boxplot(iridio, rodio,
        names=c("Iridio","Rodio"),
        col=c("lightblue","lightgreen"),
        main="Boxplots comparativos",
        ylab="Valores"
        )
#b

print(paste("Media Iridio", mean(iridio), "Media Rodio" , mean(rodio)))

print(paste("Mediana Iridio", median(iridio), "Mediana Rodio" , median(rodio)))

print(paste("0.2 Media Iridio", mean(iridio, trim = 0.2), "0.2 Media Rodio" , median(rodio, trim = 0.2)))

print(paste("0.1 Media Iridio", mean(iridio, trim = 0.1), "0.1 Media Rodio" , median(rodio, trim = 0.1)))

#c

#Desvio estandar es sd
print(paste("Desvio Estandar Iridio", sd(iridio), "Desvio Estandar Rodio" , sd(rodio)))

#Rango Intercualificado iqr es la diferencia entre el cuartil 75 y 25
print(paste("IQR Iridio", IQR(iridio), "IQR Rodio" , IQR(rodio)))

#MAD
print(paste("MAD Iridio", mad(iridio), "MAD Rodio" , mad(rodio)))

#d

cuantiles <- c(0.9,0.75,0.5,0.25,0.1)

iri_cuan <- quantile(iridio,cuantiles)
rad_cuan <- quantile(rodio,cuantiles)

#Ejercicio 4

sa <- read.csv("Practicas/Data/salchichas_A.txt",sep = "")
sb <- read.csv("Practicas/Data/salchichas_B.txt",sep = "")
sc <- read.csv("Practicas/Data/salchichas_C.txt",sep = "")


#a
colnames(sa) <- c("Calorias", "Sodio")
colnames(sb) <- c("Calorias", "Sodio")
colnames(sc) <- c("Calorias", "Sodio")


sa$tipo <- 'A'
sb$tipo <- 'B'
sc$tipo <- 'C'

salchichas <- rbind(sa,sb,sc)

write.table(salchichas, "Practicas/Data/salchichas.txt", row.names = TRUE)

#b

par(mfrow = c(1,3))

hist(salchichas$Calorias[salchichas$tipo == "A"],
     main = "Histograma Calorías - Salchicha A",
     xlab = "Calorías", col = "steelblue", breaks = 10)

# Histograma para la salchicha B
hist(salchichas$Calorias[salchichas$tipo == "B"],
     main = "Histograma Calorías - Salchicha B",
     xlab = "Calorías", col = "lightgreen", breaks = 10)

# Histograma para la salchicha C
hist(salchichas$Calorias[salchichas$tipo == "C"],
     main = "Histograma Calorías - Salchicha C",
     xlab = "Calorías", col = "tomato", breaks = 10)

par(mfrow = c(1,1))

#c
