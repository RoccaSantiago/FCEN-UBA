setwd("S:/FCEN-UBA/Introduccion a la Estadistica y Ciencia de Datos")

# Ejercicio 1

#a
sum_a <- 0

for (i in 1:1000){
  sum <- sum + i
}

print(sum)

#b

i <- 0
sum_b <- 0

while (sum_b <= 1000){
  sum_b <- sum_b + i
  i <- i+1
}

print(i)

#c

coor_sum <- function(vector){
  sum <- 0
  n <- length(vector)
  for (i in 1:n){
    v_i = vector[i]
      if (v_i > 0){
        sum <- sum + v_i
      }
  }
 return(sum)
}
 
v <- c(1,2,3,4,-1,-3)

print(coor_sum(v))


#Ejercicio 2

xs <- seq(0, 1, by=0.01)
cuadratica <- function(x){
  x*(1-x)
}
ys <- cuadratica(xs)
plot(xs, ys) # Tengo un scatter plot :(
plot(xs, ys, type = "l")

#Ejercicio 3

xs <- seq(0,2*pi,length.out = 100)
ys_1 <- sin(xs)
ys_2 <- cos(xs)
ys_3 <- cos(xs^2)
plot(xs, ys_1, , type = "l", xlim=c(0, 2*pi),xlab="Eje X", ylab="Eje Y")
lines(xs, ys_2)
lines(xs, ys_3)
title("Grafico")

#Ejercicio 4

autos <- read.csv("Clases/Data/autos.txt", sep = ' ')

#a
print(autos[3,])

#b
autos$calidad

#c

autos$calidad[autos$precio == min(autos$precio)]

#d

sum(autos$precio[1:4])

#e
apply(autos,2,sum) # columnas

apply(autos,1,sum) # filas

#f
plot(autos$precio,autos$calidad, xlab = "precio", ylab = "calidad", xlim=c(min(autos$precio),max(max(autos$precio))))

#g
head(autos[order(autos$precio),])

#Ejercicio 5


names(mtcars)
mtcars[ ,]

#a
rownames(mtcars[mtcars$gear == 4,])

#b
rownames(mtcars[mtcars$gear == 4 & mtcars$am ==1,])

#c
rownames(mtcars[mtcars$gear == 4 | mtcars$am ==1,])

#d
print(class(mtcars$am))
mtcars$am <- as.factor(mtcars$am)
print(class(mtcars$am))

#Ejercicio 6

grafico <- function(x){
  hist(x)
  boxplot(x) 
  qqnorm(x)
  qqline(x)
}

#x <- rnorm(1000)
#x <-rbinom(1000,10,0.4)
#x <-runif(1000, 4,8)
#x <-  rt(1000,10)
#x <- rchisq(1000,50)
#x <- rf(1000,90,40)
x <- rgamma(1000,0.7)

grafico(x)

