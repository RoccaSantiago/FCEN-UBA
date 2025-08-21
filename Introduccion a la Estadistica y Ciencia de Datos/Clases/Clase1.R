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