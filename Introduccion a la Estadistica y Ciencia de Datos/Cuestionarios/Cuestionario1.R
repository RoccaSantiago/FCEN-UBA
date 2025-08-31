install.packages("Lock5Data")
library(Lock5Data)
data("FloridaLakes")
help("FloridaLakes")

aa <- hist(FloridaLakes$Alkalinity, freq = FALSE)
names(aa)

max(aa$density)

mean(FloridaLakes$Alkalinity, trim = 0.2)
median(FloridaLakes$Alkalinity)

mean(FloridaLakes$Alkalinity <= 40)

count <- 0

for (i in 1:length(FloridaLakes$Alkalinity)){
  if (FloridaLakes$Alkalinity[i] <= 40){
    count <- count + 1
  }
}

prob <- count / length(FloridaLakes$Alkalinity)

prob

boxplot(FloridaLakes$Alkalinity)


plot(density(FloridaLakes$Alkalinity), xlim = c(0, max(FloridaLakes$Alkalinity)))
