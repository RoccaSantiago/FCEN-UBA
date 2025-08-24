setwd("S:/FCEN-UBA/Introduccion a la Estadistica y Ciencia de Datos")

encuestas <- read.csv("Clases/Data/ENNyS_menorA2.txt", sep = '')
encuestas_continuas <- c("Edad","Peso","Perim_encef","Talla")
#Ejercicio 1

#a

encuestas$Sexo <- as.factor(encuestas$Sexo)
encuestas$Tipo_embarazo <- as.factor(encuestas$Tipo_embarazo)

tabla_abs_sexo <- table(encuestas$Sexo)
tabla_abs_tipo_embarazo <- table(encuestas$Tipo_embarazo)

tabla_abs_sexo
tabla_abs_tipo_embarazo


tabla_rel_sexo <- prop.table(tabla_abs_sexo)
tabla_rel_tipo_embarazo <- prop.table(tabla_abs_tipo_embarazo)

tabla_rel_sexo
tabla_rel_tipo_embarazo

#b

bp_sex <- barplot(tabla_rel_sexo,main = "Proporcion de Hombres vs Mujeres", ylab = "Proporcion", ylim = c(0,1), col = c("pink",'skyblue'))
bp_type <- barplot(tabla_rel_tipo_embarazo,main = "Tipo de Embarazo", ylab = "Proporcion", ylim = c(0,1), col = c("red",'orange'))

#c

pie_sex <- pie(tabla_rel_sexo,main = "Proporcion de Hombres vs Mujeres", col = c("pink",'skyblue'))
pie_type <- pie(tabla_rel_tipo_embarazo,main = "Tipo de Embarazo", col = c("red",'orange'))

#Ejercicio 2

#a

tabla_contigencia <- table(encuestas$Sexo,encuestas$Tipo_embarazo)
tabla_contigencia
bp_contigencia <- barplot(tabla_contigencia)

#b
hist(encuestas$Edad, xlab = "Edad", ylab= "Frecuencia Absoluta", main = " ")
boxplot(encuestas$Edad)

hist(encuestas$Peso, xlab = "Peso", ylab= "Frecuencia Absoluta", main = " ", xlim = c(0,30))
boxplot(encuestas$Peso)

hist(encuestas$Perim_encef, xlab = "Perimetro Encefalico", ylab= "Frecuencia Absoluta", main = " ")
boxplot(encuestas$Perim_encef)

hist(encuestas$Talla, xlab = "Talla", ylab= "Frecuencia Absoluta", main = " ")
boxplot(encuestas$Talla)

#c

for (var in encuestas_continuas) {
  print("--------------------------------------------------------")
  datos <- encuestas[[var]]
  
  print(paste("Resumen de", var))
  print(summary(datos))
  
  print(paste("Media 0,1 podada", var, ":", mean(datos, trim = 0.1, na.rm = TRUE)))
  print(paste("Media 0,2 podada", var, ":", mean(datos, trim = 0.2, na.rm = TRUE)))
}

#d

for(var in encuestas_continuas){
  print("--------------------------------------------------------")
  datos <- encuestas[[var]]
  print(paste("Medidas de dispersion de", var))
  
  print(paste("Desvío estándar", var, ":", sd(datos, na.rm = TRUE)))
  print(paste("Distancia intercuartil", var, ":", IQR(datos, na.rm = TRUE)))
  print(paste("MAD", var, ":", mad(datos, na.rm = TRUE)))
}


#e
for(var in encuestas_continuas){
  print("--------------------------------------------------------")
  datos <- encuestas[[var]]
  percentiles <- quantile(datos, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm = TRUE)
  
  print(paste("Percentiles", var, ":"))
  print(percentiles)
}

#f
for(var in names(encuestas_continuas)){
  p_qqnorm <- ggplot(encuestas_continuas, aes(sample = .data[[var]])) +
    stat_qq(color = "blue", size = 1.5) +              # Puntos de los cuantiles muestrales
    stat_qq_line(color = "red", size = 1.2) +          # Línea de referencia para ajuste normal (cuantiles teóricos)
    labs(title = paste("QQ-plot para", var, "- Ajuste Normal"),
         x = "Cuantiles Teóricos (Normal)",
         y = "Cuantiles Muestrales") +
    theme_minimal()
  
  # Imprimir el gráfico inmediatamente
  print(p_qqnorm)
}

#Ejercicio 4

bagplot(encuestas_continuas$Peso, encuestas_continuas$Perim_encef,
        xlab = 'Peso',                   
        ylab = 'Perímetro cefálico',              
        main = "Bagplot de Peso vs. Perímetro cefálico", 
        #col = c("lightblue", "salmon")
)

