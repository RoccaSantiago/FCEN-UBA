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
colnames(sa) <- c("calorias", "Sodio")
colnames(sb) <- c("calorias", "Sodio")
colnames(sc) <- c("calorias", "Sodio")


sa$tipo <- 'A'
sb$tipo <- 'B'
sc$tipo <- 'C'

salchichas <- rbind(sa,sb,sc)

write.table(salchichas, "Practicas/Data/salchichas.txt", row.names = TRUE)

#b

par(mfrow = c(1,3))

# Histograma para la salchicha A
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

boxplot(sa$calorias,sb$calorias,sc$calorias, 
        names = c("Calorias A","Calorias B","Calorias C"),
        col = c("steelblue","lightgreen","tomato"),
        main = "Calorias")

#d

par(mfrow = c(1,3))

# Histograma para la salchicha A
hist(salchichas$Sodio[salchichas$tipo == "A"],
     main = "Histograma Sodio - Salchicha A",
     xlab = "Sodio", col = "steelblue", breaks = 10)

# Histograma para la salchicha B
hist(salchichas$Sodio[salchichas$tipo == "B"],
     main = "Histograma Sodio - Salchicha B",
     xlab = "Calorías", col = "lightgreen", breaks = 10)

# Histograma para la salchicha C
hist(salchichas$Sodio[salchichas$tipo == "C"],
     main = "Sodio Sodio - Salchicha C",
     xlab = "Sodio", col = "tomato", breaks = 10)

par(mfrow = c(1,1))

boxplot(sa$Sodio,sb$Sodio,sc$Sodio, 
        names = c("Sodio A","Sodio B","Sodio C"),
        col = c("steelblue","lightgreen","tomato"),
        main = "Sodio")

#Ejercicio 5

estudiantes <- read.csv("Practicas/Data/estudiantes.txt",sep="")
grupo1 <- estudiantes$GRUPO1
grupo2 <- estudiantes$GRUPO2
#a

hist(estudiantes$GRUPO1, probability = TRUE, col = 'steelblue',main = 'Estudiantes del grupo 1',xlab = "Valor",ylab = "Densidad")

curve(dnorm(x, mean=mean(estudiantes$GRUPO1), sd=sd(estudiantes$GRUPO1)), add=TRUE, col="blue", lwd=2)

hist(estudiantes$GRUPO2, probability=TRUE, col="tomato", main="Grupo 2", xlab="Valor", ylab = "Densidad")
curve(dnorm(x, mean=mean(estudiantes$GRUPO2), sd=sd(estudiantes$GRUPO2)), add=TRUE, col="red", lwd=2)

qqnorm(grupo1, main="QQ-Plot Grupo 1")
qqline(grupo1, col="tomato", lwd=2)

qqnorm(grupo2, main="QQ-Plot Grupo 2")
qqline(grupo2, col="blue", lwd=2)

#b

boxplot(grupo1,grupo2)


#Ejercicio 6

nubes <- read.csv("Practicas/Data/nubes.txt", sep = "")
tratadas <- nubes$TRATADAS
controles <- nubes$CONTROLES

#a

boxplot(nubes$TRATADAS,nubes$CONTROLES, col = c('tomato','steelblue'), names = c('Tratadas','Controles'))

#b

hist(tratadas, probability=TRUE, col="tomato", main="tratadas", xlab="Valor", ylab = "Densidad")
curve(dnorm(x, mean=mean(tratadas), sd=sd(tratadas)), add=TRUE, col="red", lwd=2)

hist(controles, probability=TRUE, col="steelblue", main="controles", xlab="Valor", ylab = "Densidad")
curve(dnorm(x, mean=mean(controles), sd=sd(controles)), add=TRUE, col="blue", lwd=2)

qqnorm(tratadas, main="QQ-Plot tratadas")
qqline(tratadas, col="tomato", lwd=2)

qqnorm(controles, main="QQ-Plot controles")
qqline(controles, col="blue", lwd=2)

#c

log_tratadas <- log(tratadas)
log_controles <- log(controles)

hist(log_tratadas, probability=TRUE, col="tomato", main="log tratadas", xlab="Valor", ylab = "Densidad")
curve(dnorm(x, mean=mean(log_tratadas), sd=sd(log_tratadas)), add=TRUE, col="red", lwd=2)

hist(log_controles, probability=TRUE, col="steelblue", main="log controles", xlab="Valor", ylab = "Densidad")
curve(dnorm(x, mean=mean(log_controles), sd=sd(log_controles)), add=TRUE, col="blue", lwd=2)

qqnorm(log_tratadas, main="QQ-Plot log tratadas")
qqline(log_tratadas, col="tomato", lwd=2)

qqnorm(log_controles, main="QQ-Plot log controles")
qqline(log_controles, col="blue", lwd=2)

#d
boxplot(log_tratadas,log_controles, col = c("tomato","steelblue"), names = c('Log Tratadas', 'Log Controles'))

#Ejercicio 7

tarjeta <- read.csv("Practicas/Data/data_credit_Card.csv")

vars <- c("purchases", "credit_limit", "purchases_freq", "tenure")

par(mfrow=c(2,2))

#a
for (v in vars){
  plot(ecdf(tarjeta[[v]]), main=paste("ECDF de", v), xlab=v, ylab="F(x)")
}

par(mfrow=c(1,1))

#b

hist(tarjeta$credit_limit,main="Histograma de Credit Limit",xlab="Límite de crédito",col="steelblue", freq=FALSE)   

lines(density(tarjeta$credit_limit), col="blue", lwd=2)


#c
ternure_rel <- prop.table(table(tarjeta$tenure))

barplot(ternure_rel,col = "tomato",xlab="Meses restantes (tenure)", ylab="Frecuencia relativa",)

#d

for (v in vars) {
  x <- tarjeta[[v]]
  cat("\nVariable:", v, "\n")
  cat(" Media:", mean(x), "\n")
  cat(" Mediana:", median(x), "\n")
  cat(" Media podada (10%):", mean(x, trim = 0.1), "\n")
  x
}

#e

