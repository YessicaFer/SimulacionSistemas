library(parallel)

datos <-  data.frame(stringsAsFactors=FALSE)
temp=c()

experimento <- function(replica) {
  pos <- rep(0, dimension)
  ceros <- 0
  for (t in 1:duracion) {
    cambiar <- sample(1:dimension, 1)
    cambio <- 1
    if (runif(1) < 0.5) {
      cambio <- -1
    }
    pos[cambiar] <- pos[cambiar] + cambio
    if(all(pos==0)){
      ceros<-ceros+1
    }
  }
  return(ceros)
}

dimensiones<-1:8
replicas<-seq(50,300,50)
duraciones<-seq(100,400,50)

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "experimento")
for(replica in replicas){
  for (dimension in dimensiones) {
    for (duracion in duraciones){
      clusterExport(cluster, "replica")
      clusterExport(cluster, "dimension")
      clusterExport(cluster, "duracion")
      resultado <- parSapply(cluster, 1:replica, experimento)
      for(i in 1:replica){
        datos <- rbind(datos, c(replica,dimension,duracion,resultado[i]))
        temp<-c(temp,paste(replica,"_",dimension,"_",duracion))
      }
    }
  }
}
stopCluster(cluster)



#graphics.off()
names(datos)<-c("repeticiones","dimension","duracion","cruces")
datos$repeticiones<-as.factor(datos$repeticiones)
datos$dimension<-as.factor(datos$dimension)
datos$duracion<-as.factor(datos$duracion)
datos$unidos<-as.factor(temp)

#Revisar normalidad


lin<-lm(datos$cruces~datos$repeticiones+datos$dimension+datos$duracion)
residuales<-resid(lin)
histograma<-hist(residuales)
mult <- histograma$counts / histograma$density
densidad <- density(resid(lin))
densidad$y <- densidad$y * mult[1]

#sacar una muestra de datos para hacer la prueba de normalidad (las pruebas de R soportan solo hasta 5000 datos)
muestra=datos[sample(nrow(datos),5000),]
linM=lm(muestra$cruces~muestra$repeticiones+muestra$dimension+muestra$duracion)
residualesM<-resid(linM)
histogramaM<-hist(residualesM)
multM <- histogramaM$counts / histogramaM$density
densidadM <- density(resid(linM))
densidadM$y <- densidadM$y * mult[1]

#para graficar las dos densidades de los residuales sobrepuestas
png("T1_densidades.png")
plot(histograma,main="Densidad de residuales",xlab="Residuales de cruces",ylab="Frecuencias")
polygon(densidad, col=rgb(0,1,0,0.7), border=NA)
polygon(densidadM, col=rgb(0,0,1,0.4), border=NA)
legend("topright", legend = c("Original", "Muestra"), pch = 22, col = c("green","blue"))
graphics.off()

#Para graficar las dos qqnorm sobrepuestas 
png("T1_qqplot.png")
qqnorm(residuales,col=rgb(0,1,0,0.5))
points(qqnorm(residualesM,plot=FALSE),col=rgb(0,0,1,0.5),pch=4)
abline(a = 0, b = 1,col="red")
legend("bottomright", legend = c("Original", "Muestra"), pch = c(1,4), col = c("green","blue"))
graphics.off()

ss<-shapiro.test(residualesM)
print(ss)
#Los datos no provienen de una distribución normal, aplicar pruebas no parametricas

#Prueba de kruskal y Wallis para determiniar si hay diferencia significativa entre las medianas de las configuraciones
#Una configuracion es una terna (repeticiones,dimension,duracion)
kw<-kruskal.test(datos$cruces~datos$unidos,data=datos)
print(kw)
#Existe diferencia entre las configuraciones

#Ahora revisemos cada factor por separado

#Repeticiones
kw<-kruskal.test(datos$cruces~datos$repeticiones,data=datos)
print(kw)
#No hay diferencia signifcativa entre los niveles del factor repeticiones; es decir, el número 
#de cruces no depende del numero derepeticiones

#Duracion
kw<-kruskal.test(datos$cruces~datos$duracion,data=datos)
print(kw)
#No hay diferencia signifcativa entre los niveles del factor duracion; es decir, el número 
#de cruces no depende de la duración de la caminata

#Diagrama de bigotes para duracion
png("T1_boxplot_duracion.png")
boxplot(datos$cruces~datos$duracion,xlab="Duracion",ylab="Cruces con el origen")
graphics.off()

#Dimension
kw<-kruskal.test(datos$cruces~datos$dimension,data=datos)
print(kw)
#Si hay diferencia signifcativa entre los niveles del factor dimensión; de acuerdo a lo anterior, el número
#de cruces solo depende de la duración

#Diagrama de bigotes para dimension
png("T1_boxplot_dimension.png")
boxplot(datos$cruces~datos$dimension,xlab="Dimension",ylab="Cruces con el origen")
graphics.off()

#Revisemos si todos los niveles son significativos

  library(FSA)

PT = dunnTest(datos$cruces~datos$dimension,data=datos,
              method="bh") 
PT = PT$res
#Resultado de prueba Dunn, comparacion entre cada par de dimensiones
print(PT)

#Pares no significtaivos al 99.9% de confianza
print(PT[PT$P.adj>=0.00001,]) #estos son estadisticamente equivalentes
