data <- read.csv("zika.csv", header=TRUE, sep=",")
png("p5r2.png", width=600, height=300, units="px")
plot(data$sem, data$casos, xlab="Semana", ylab="Casos nuevos", xlim=c(1,34))
lines(data$sem, data$casos, type="l")
graphics.off()

hist(data[,2])
datos=(data[,2])^(1/3)
hist(datos)

shapiro.test(resid(lm(datos~seq(1,34))))


