inicio <- -6
final <- -inicio
paso <- 0.25
x <- seq(inicio, final, paso)
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
png("p5f.png") # dibujamos f(x) para ver como es
plot(x,  (2/pi) * (1/(exp(x)+exp(-x))))
lines(x,  (2/pi) * (1/(exp(x)+exp(-x))), type="l")
graphics.off()
suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
library(distr)
generador  <- r(AbscontDistribution(d = g)) # creamos un generador
muestra <- generador(50000) # sacamos una muestra
png("p5m.png") # validamos con un dibujo
hist(muestra, freq=F, breaks=50,
     main="Histograma de g(x) comparado con g(x)",
     xlim=c(inicio, final), ylim=c(0, 0.4))
lines(x, g(x), col="red") # dibujamos g(x) encima del histograma
graphics.off()
desde <- 3
hasta <- 7
wolfram=0.048834
pedazo <- 500000
cuantos <- 500
parte <- function() {
  valores <- generador(pedazo)
  return(sum(valores >= desde & valores <= hasta))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
datos=data.frame()
for(pedazo in seq(1000,10000,1000)){ #subirle al experimento
  for(i in 1:50){
      start.time <- Sys.time()
        montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
        integral <- (pi / 2) *sum(montecarlo) / (cuantos * pedazo)
        gap=abs(wolfram- integral)/((wolfram+integral)/2)*100
      end.time <- Sys.time()
      time.taken <- (end.time - start.time)/cuantos
    
      datos=rbind(datos,c(pedazo,gap,time.taken))
  }
}
stopImplicitCluster()

names(datos)=c("Corridas","GAP","Tiempo")
datos$Corridas=as.factor(datos$Corridas)

boxplot(data=datos,GAP~Corridas,xlab="Numero de corridas",ylab="GAP (%)",main="GAP wolfram vs aproximación")
boxplot(data=datos,Tiempo~Corridas,xlab="Numero de corridas",ylab="Tiempo (s)",main="Tiempo para calcular aproximación")

library(ggplot2)
png("P5_Violines_GAP.png",width=1200,height=1000)
dodge <- position_dodge(width = 1)
ggplot(data=datos,aes(x=Corridas,y=GAP))+
  geom_violin(position = dodge)+
  geom_boxplot(position=dodge,width=0.1)+
  stat_summary(fun.y=median, geom="smooth", aes(group=1))+
  xlab("Tamaño de muestras")+
  ylab("GAP (%)")+
  labs(title="GAP wolfram vs aproximación")+
  theme(text = element_text(size=36),
        axis.text.x = element_text(size=36,angle = 90, hjust = 1),
        axis.text.y = element_text(size=36),
        plot.title = element_text(size=36))+
  theme(plot.title = element_text(hjust = 0.5))
graphics.off()

png("P5_Violines_Tiempo.png",width=1200,height=1000)  #recuerda acomodar ylim
ggplot(data=datos,aes(x=Corridas,y=Tiempo))+
  geom_violin(position = dodge)+
  geom_boxplot(position=dodge,width=0.1)+
  stat_summary(fun.y=median, geom="smooth", aes(group=1))+
  xlab("Tamaño de muestra")+
  ylab("Tiempo de ejecución (s)")+
  labs(title="Tiempo para calcular aproximación")+theme(text = element_text(size=36),
                                                        axis.text.x = element_text(size=36,angle = 90, hjust = 1),
                                                        axis.text.y = element_text(size=36),
                                                        plot.title = element_text(size=36))+
  theme(plot.title = element_text(hjust = 0.5))
graphics.off()

kruskal.test(datos$GAP~datos$Corridas)
kruskal.test(datos$Tiempo~datos$Corridas)

library(FSA)
PT = dunnTest(datos$GAP~datos$Corridas,method="bh") 
PT = PT$res
#Resultado de prueba Dunn, comparacion entre cada par de dimensiones
print(PT)

#Pares no significtaivos al 99.9% de confianza
print(PT[PT$P.adj>=0.001,]) #estos son estadisticamente equivalentes
