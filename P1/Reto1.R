library(parallel)

datos <-  data.frame(stringsAsFactors=FALSE)
temp=c()
normal=FALSE
alfa=0.001

tiempo<-function(replica){
  r=system.time(experimento(replica))
  return(r[3])
}

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
    
  }
  return(ceros)
}

dimensiones=1:8
duraciones=seq(100,500,50)
replica=200
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster,"tiempo")
clusterExport(cluster, "experimento")
clusterExport(cluster, "replica")
for (dimension in dimensiones) {
  for (duracion in duraciones){
    clusterExport(cluster, "dimension")
    clusterExport(cluster, "duracion")
    resultado <- parSapply(cluster, 1:replica, tiempo)
    for(i in 1:replica){
      datos <- rbind(datos, c(dimension,duracion,resultado[i]))
      temp<-c(temp,paste(dimension,"_",duracion))
    }
  }
}

stopCluster(cluster)

names(datos)<-c("dimension","duracion","tiempo")
datos$dimension<-as.factor(datos$dimension)
datos$duracion<-as.factor(datos$duracion)
datos$unidos<-as.factor(temp)

#Revisar normalidad
lin<-lm(datos$tiempo~datos$dimension+datos$duracion)
residuales<-resid(lin)
histograma<-hist(residuales)
mult <- histograma$counts / histograma$density
densidad <- density(resid(lin))
densidad$y <- densidad$y * mult[1]

if(nrows(datos)>5000){
  #sacar una muestra de datos para hacer la prueba de normalidad (las pruebas de R soportan solo hasta 5000 datos)
  muestra=datos[sample(nrow(datos),5000),]
  linM=lm(muestra$tiempo~muestra$dimension+muestra$duracion)
  residualesM<-resid(linM)
  histogramaM<-hist(residualesM)
  multM <- histogramaM$counts / histogramaM$density
  densidadM <- density(resid(linM))
  densidadM$y <- densidadM$y * mult[1]
  
  #para graficar las dos densidades de los residuales sobrepuestas
  png("densidades.png")
  plot(histograma,main="Densidad de residuales",xlab="Residuales de tiempo",ylab="Frecuencias")
  polygon(densidad, col=rgb(0,1,0,0.7), border=NA)
  polygon(densidadM, col=rgb(0,0,1,0.4), border=NA)
  legend("topright", legend = c("Original", "Muestra"), pch = 22, col = c("green","blue"))
  graphics.off()
  
  #Para graficar las dos qqnorm sobrepuestas 
  png("qqplot.png")
  qqnorm(residuales,col=rgb(0,1,0,0.5))
  points(qqnorm(residualesM,plot=FALSE),col=rgb(0,0,1,0.5),pch=4)
  abline(a = 0, b = 1,col="red")
  legend("bottomright", legend = c("Original", "Muestra"), pch = c(1,4), col = c("green","blue"))
  graphics.off()
  
  ss<-shapiro.test(residualesM)
  print(ss)
  
  if(ss$p.value<alfa){
    print("Los datos no provienen de una distribución normal, aplicar pruebas no parametricas")
  }
  else{
    print("Los datos provienen de una distribución normal, aplicar ANOVA")
    normal=TRUE
  }
}else{
  #para graficar la densidad de los residuales 
  png("R1_densidad.png")
  plot(histograma,main="Densidad de residuales",xlab="Residuales de cruces",ylab="Frecuencias")
  polygon(densidad, col=rgb(0,1,0,0.7), border=NA)
  graphics.off()
  
  #Para graficar qqnorm
  png("R1_qqplot.png")
  qqnorm(residuales,col=rgb(0,1,0,0.5))
  abline(a = 0, b = 1,col="red")
  graphics.off()
  
  ss<-shapiro.test(residuales)
  print(ss)
  
  if(ss$p.value<alfa){
    print("Los datos no provienen de una distribución normal, aplicar pruebas no parametricas")
  }
  else{
    print("Los datos provienen de una distribución normal, aplicar ANOVA")
    normal=TRUE
  }
}

#Aplicar pruebas
if(normal){
  #Aplicar ANOVA
  a=aov(datos$tiempo~datos$dimension+datos$duracion)
  print(a)
  m=anova(a)
  print(m)
  
  #Prueba de Tukey a pares para dimensión
  #idea de https://stackoverflow.com/questions/33644034/how-to-visualize-pairwise-comparisons-with-ggplot2
  tky = as.data.frame(TukeyHSD(a)$'datos$dimension')
  tky$pares = rownames(tky)
  library(ggplot2)
  # Plot pairwise TukeyHSD comparisons and color by significance level
  png("R1_pairwise_dimension.png")
  ggplot(tky, aes(colour=cut(`p adj`, c(0, 0.0001, 0.001, 1), 
                             label=c("p<0.0001","p<0.001","No significativo")))) +
    geom_hline(yintercept=0, lty="11", colour="grey30") +
    geom_errorbar(aes(pares, ymin=lwr, ymax=upr), width=0.4) +
    geom_point(aes(pares, diff)) +
    labs(colour="")+
    theme(text = element_text(size=10),
          axis.text.x = element_text(angle=90, hjust=1)) 
  graphics.off()
  if(unlist(m$`Pr(>F)`[1]<alfa)){
    print("El factor dimensión es significativo; es decir, el tiempo de ejecución depende de la dimensión de la particula")
    print("Los pares con diferencia son:")
    print(tky$pares[tky$`p adj`<alfa])
  }else{
    print("El factor dimensión no es significativo; es decir, el tiempo de ejecución no depende de la dimensión de la particula")
  }
  
  
  
  #Prueba de Tukey a pares para duración
  if(unlist(m$`Pr(>F)`[2]<alfa)){
    print("El factor duracion es significativo; es decir, el tiempo de ejecución depende de la duración de la caminata")
    print("Los pares con diferencia son:")
    print(tky$pares[tky$`p adj`<alfa])
  }else{
    print("El factor duracion  no es significativo; es decir, el tiempo de ejecución no depende de la duración de la caminata")
 }
  #idea de https://stackoverflow.com/questions/33644034/how-to-visualize-pairwise-comparisons-with-ggplot2
  tky = as.data.frame(TukeyHSD(a)$'datos$duracion')
  tky$pares = rownames(tky)
  library(ggplot2)
  # Plot pairwise TukeyHSD comparisons and color by significance level
  png("R1_pairwise_duracion.png")
  ggplot(tky,aes(colour=cut(`p adj`, c(0, 0.0001, 0.001, 1), 
                             label=c("p<0.0001","p<0.001","No significativo")))) +
    geom_hline(yintercept=0, lty="11", colour="grey30") +
    geom_errorbar(aes(pares, ymin=lwr, ymax=upr), width=0.4) +
    geom_point(aes(pares, diff)) +
    labs(colour="")+
    theme(text = element_text(size=10),
          axis.text.x = element_text(angle=90, hjust=1)) 
  graphics.off() 
  
  
  
}else{
  #Prueba de kruskal y Wallis para determinar si hay diferencia significativa entre las medianas de las configuraciones
  #Una configuracion es un par (dimension,duracion)
  kw<-kruskal.test(datos$tiempo~datos$unidos,data=datos)
  print(kw)
  if(kw$p.value<alfa){
    print("Existe diferencia entre las configuraciones, revisemos cada factor por separado")
    #Ahora revisemos cada factor por separado
    
    #Duracion
    kw<-kruskal.test(datos$tiempo~datos$duracion,data=datos)
    print(kw)
    png("R1_boxplot_duracion.png")
    boxplot(datos$tiempo~datos$duracion,xlab="Duración de la caminata",ylab="Tiempo")
    graphics.off()
    if(kw$p.value<alfa){
      print("Existe diferencia signifcativa entre los niveles del factor duracion; es decir, el tiempo de ejecucion depende de la duración de la caminata")
      #Revisemos si todos los niveles son significativos
      library(FSA)
      PT = dunnTest(datos$tiempo~datos$duracion,data=datos,method="bh") 
      PT = PT$res
      #Resultado de prueba Dunn, comparacion entre cada par de dimensiones
      print(PT)
      
      #Pares no significtaivos al 99.99% de confianza
      print(PT[PT$P.adj>=0.001,]) #estos son estadisticamente equivalentes
    }else{
      print("No existe diferencia signifcativa entre los niveles del factor duracion; es decir, el tiempo de ejecucion no depende de la duración de la caminata")
    }
    
    
    #Dimension
    kw<-kruskal.test(datos$tiempo~datos$dimension,data=datos)
    print(kw)
    png("R1_boxplot_dimension.png")
    boxplot(datos$tiempo~datos$dimension,xlab="Dimension de la particula",ylab="Tiempo")
    graphics.off()
    if(kw$p.value<alfa){
      print("Existe diferencia signifcativa entre los niveles del factor dimension; es decir, el tiempo de ejecucion depende de la dimension de la caminata")
      #Revisemos si todos los niveles son significativos
      library(FSA)
      PT = dunnTest(datos$tiempo~datos$dimension,data=datos,method="bh") 
      PT = PT$res
      #Resultado de prueba Dunn, comparacion entre cada par de dimensiones
      print(PT)
      
      #Pares no significtaivos al 99.99% de confianza
      print(PT[PT$P.adj>=0.001,]) #estos son estadisticamente equivalentes
    }else{
      print("No existe diferencia signifcativa entre los niveles del factor dimension; es decir, el tiempo de ejecucion  no depende de la dimension de la caminata")
    }
    
  }else{
    print("No hay diferencia entre ls configuraciones, el tiempo de ejecución no depende ni de la duración de la caminata ni de la dimensión de la particula")
  }
  
  
}


