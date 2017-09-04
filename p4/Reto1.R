

celda <-  function(pos) {
  fila <- floor((pos - 1) / n) + 1
  columna <- ((pos - 1) %% n) + 1
  if (zona[fila, columna] > 0) { # es una semilla
    return(zona[fila, columna])
  } else {
    cercano <- NULL # sin valor por el momento
    menor <- n * sqrt(2) # mayor posible para comenzar la busqueda
    for (semilla in 1:k) {
      dx <- columna - x[semilla]
      dy <- fila - y[semilla]
      dist <- sqrt(dx^2 + dy^2)
      if (dist < menor) {
        cercano <- semilla
        menor <- dist
      }
    }
    return(cercano)
  }
}

inicio <- function() {
  direccion <- sample(1:4, 1)
  xg <- NULL
  yg <- NULL
  if (direccion == 1) { # vertical
    xg <- 1
    yg <- sample(1:n, 1)
  } else if (direccion == 2) { # horiz izr -> der
    xg <- sample(1:n, 1)
    yg <- 1
  } else if (direccion == 3) { # horiz der -> izq
    xg <- n
    yg <- sample(1:n, 1)
  } else { # vertical al reves
    xg <- sample(1:n, 1)
    yg <- n
  }
  return(c(xg, yg))
}

vp <- data.frame(numeric(), numeric()) # posiciones de posibles vecinos
for (dx in -1:1) {
  for (dy in -1:1) {
    if (dx != 0 | dy != 0) { # descartar la posicion misma
      vp <- rbind(vp, c(dx, dy))
    }
  }
}
names(vp) <- c("dx", "dy")
vc <- dim(vp)[1]
grafica=numeric()

propaga <- function(replica) {
  # probabilidad de propagacion interna
  prob <- 1
  dificil <- 0.99
  grieta <- voronoi # marcamos la grieta en una copia
  i <- inicio() # posicion inicial al azar
  xg <- i[1]
  yg <- i[2]
  largo <- 0
  while (TRUE) { # hasta que la propagacion termine
    grieta[yg, xg] <- 0 # usamos el cero para marcar la grieta
    largo <-  largo + 1
    frontera <- numeric()
    interior <- numeric()
    for (v in 1:vc) {
      vecino <- vp[v,]
      xs <- xg + vecino$dx # columna del vecino potencial
      ys <- yg + vecino$dy # fila del vecino potencial
      if (xs > 0 & xs <= n & ys > 0 & ys <= n) { # no sale de la zona
        if (grieta[ys, xs] > 0) { # aun no hay grieta ahi
          if (voronoi[yg, xg] == voronoi[ys, xs]) {
            interior <- c(interior, v)
          } else { # frontera
            frontera <- c(frontera, v)
          }
        }
      }
    }
    elegido <- 0
    if (length(frontera) > 0) { # siempre tomamos frontera cuando haya
      if (length(frontera) > 1) {
        elegido <- sample(frontera, 1)
      } else {
        elegido <- frontera # sample sirve con un solo elemento
      }
      prob <- 1 # estamos nuevamente en la frontera
    } else if (length(interior) > 0) { # no hubo frontera para propagar
      if (runif(1) < prob) { # intentamos en el interior
        if (length(interior) > 1) {
          elegido <- sample(interior, 1)
        } else {
          elegido <- interior
        }
        prob <- dificil * prob # mas dificil a la siguiente
      }
    }
    if (elegido > 0) { # si se va a propagar
      vecino <- vp[elegido,]
      xg <- xg + vecino$dx
      yg <- yg + vecino$dy
    } else {
      break # ya no se propaga
    }
  }
  #png(paste("Graficas/","grieta_n",n,"_k",k,".png",sep=""))
  #par(mar = c(0,0,0,0))
  #image(rotate(grieta), col=rainbow(k+1), xaxt='n', yaxt='n')
  #graphics.off()
  
  return(largo)
}


#Experimento
suppressMessages(library(parallel))
cluster=makeCluster(detectCores() - 1)
clusterExport(cluster,"vp")
clusterExport(cluster,"vc")
clusterExport(cluster,"celda")
clusterExport(cluster,"inicio")
clusterExport(cluster,"propaga")
kas=c(0.5,1,2,4,8,16,32)
ns=c(50,100,500)
datos=data.frame()
factores=data.frame()
for(n in ns){
  #vector de probabilidades tipo bañera
  t=pexp(q=seq(0,10,10/(n/2)))
  ex=sapply(2:length(t),function(x) t[x]-t[x-1])
  ex=c(ex,rev(ex))/2
  png("Banera.png",width=500,height=400)
  par(mar=c(5,6,4,0))
  plot(ex,type="l",xlab="Índices del vector",ylab="Probabilidad de selección",cex.lab=1.7, cex.axis=1.5)
  graphics.off()
  
  #vector de probabilidades normal
  t=pnorm(q=seq(-3,3,6/n),sd=0.5)
  nor=sapply(2:length(t),function(x) t[x]-t[x-1])
  png("Normal.png",width=500,height=400)
  par(mar=c(5,6,4,0))
  plot(nor,type="l",xlab="Índices del vector",ylab="Probabilidad de selección",cex.lab=1.7, cex.axis=1.5)
  graphics.off()
  
  #vector de probabilidades normal para la mitad
  t=pnorm(q=seq(-3,3,6/(n/2)),sd=0.5)
  nor2=sapply(2:length(t),function(x) t[x]-t[x-1])
  nor2=rep(nor2,2)
  png("DobleNormal.png",width=500,height=400)
  par(mar=c(5,6,4,0))
  plot(nor2,type="l",xlab="Índices del vector",ylab="Probabilidad de selección",cex.lab=1.7, cex.axis=1.5)
  graphics.off()
  
  #vector de probabilidades tipo gamma
  t=pgamma(q=seq(0,10,10/n),shape=15,rate=5)
  gam=sapply(2:length(t),function(x) t[x]-t[x-1])
  png("Gamma.png",width=500,height=400)
  par(mar=c(5,6,4,0))
  plot(gam,type="l",xlab="Índices del vector",ylab="Probabilidad de selección",cex.lab=1.7, cex.axis=1.5)
  graphics.off()
  
  for(tipo in 1:6){
    for(k in kas){
      ka=k
      k=floor(k*n)
      grieta=matrix()
      mayor_largo=0
      zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
      x <- rep(0, k) # ocupamos almacenar las coordenadas x de las semillas
      y <- rep(0, k) # igual como las coordenadas y de las semillas
      
      for (semilla in 1:k) {
        while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
          if(tipo==1){ #las semillas se distribuyen sobre la orilla
            if(runif(1)<0.5){
              fila <- sample(1:n, 1,prob=ex)
              columna <- sample(1:n, 1)
            }else{
              fila <- sample(1:n, 1)
              columna <- sample(1:n, 1,prob=ex)
            }
            nombre="orillas"
          }else if(tipo==2){#las semillas se distribuyen en el centro
            fila <- sample(1:n, 1,prob=nor)
            columna <- sample(1:n, 1,prob=nor)
            nombre="centro"
          }else if(tipo==3){#las emillas se distribuyen sólo en las esquinas
            fila <- sample(1:n, 1,prob=ex)
            columna <- sample(1:n, 1,prob=ex)
            nombre="esquinas"
          }else if(tipo==4){#las semillas forman en 4 grupos
            fila <- sample(1:n, 1,prob=nor2)
            columna <- sample(1:n, 1,prob=nor2)
            nombre="grupos"
          }
          else if(tipo==5){
            fila <- sample(1:n, 1,prob=gam)
            columna <- sample(1:n, 1,prob=gam)
            nombre="monton"
          }else if(tipo==6){
            fila <- sample(1:n, 1)
            columna <- sample(1:n, 1)
            nombre="normal"
          }
          if (zona[fila, columna] == 0) {
            zona[fila, columna] = semilla
            x[semilla] <- columna
            y[semilla] <- fila
            break
          }
        }
      }
      clusterExport(cluster,"zona")
      
      clusterExport(cluster,"k")
      clusterExport(cluster,"x")
      clusterExport(cluster,"y")
      clusterExport(cluster,"n")
      celdas=parSapply(cluster,1:(n * n),celda)
      voronoi <- matrix(celdas, nrow = n, ncol = n, byrow=TRUE)
      clusterExport(cluster,"voronoi")
      largos <- parSapply(cluster, 1:100, propaga)
      
      
      
      rotate <- function(x) t(apply(x, 2, rev))
      png(paste("Graficas/",nombre,"_inicio_n",n,"_k",k,".png",sep=""))
      par(mar = c(0,0,0,0))
      image(rotate(zona), col=rainbow(k+1), xaxt='n', yaxt='n')
      graphics.off()
      png(paste("Graficas/",nombre,"_voronoi_n",n,"_k",k,".png",sep=""))
      par(mar = c(0,0,0,0))
      image(rotate(voronoi), col=rainbow(k+1), xaxt='n', yaxt='n')
      graphics.off()
      
      datos=rbind(datos,largos)
      factores=rbind(factores,c(n,ka,tipo))
    }
  }
}

stopCluster(cluster)
boxplot(t(datos ),col=rainbow(length(kas)),outline=F)
matplot(apply(datos,1,sort) ,type='l',col=rainbow(length(kas)))


wilcox.test(datos[1,],datos[5,])

datos2=data.frame()
for(i in 1:nrow(datos)){
  for(j in 1:100){
    datos2=rbind(datos2,c(as.numeric(factores[i,]),as.numeric(datos[i,j])))
  }
}

names(datos2)=c("tamaño","semillas","tipo","largo")
datos2$tamaño=as.factor(datos2$tamaño)
datos2$semillas=as.factor(datos2$semillas)
datos2$tipo=as.factor(datos2$tipo)
levels(datos2$tipo)=c("orillas","centro","esquinas","grupos","descentrado","uniforme")
datos2$tipo=factor(datos2$tipo,levels=c("orillas","uniforme","esquinas","grupos","descentrado","centro"))

#https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
require(ggplot2)
png("R1_boxplotLargos.png",width=1000,height=600)
dodge <- position_dodge(width = 3)
ggplot(data = datos2, aes(x=tipo, y=largo)) +
  geom_violin(position = dodge)+
  geom_boxplot(width=.1, outlier.colour=NA,position = dodge)+
  xlab("Distribución inicial de semillas")+
  ylab("Largos de las grietas")+
  labs(title="Grietas para diversos esquemas iniciales de semillas")+
  guides(fill=guide_legend(title="Distribuciones"))+
  scale_fill_manual(labels=levels(tipo),values=1:6)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0,200)+
  stat_summary(fun.y=median, geom="smooth", aes(group=1),show.legend = T)+
  stat_summary(fun.y=mean, geom="smooth", aes(group=1),col='red')+
  theme(text = element_text(size=21),
        axis.text.x = element_text(size=21)) 
graphics.off()

library(FSA)
PT = dunnTest(datos2$largo~datos2$tipo,data=datos2, method="bh") 
PT = PT$res
#Resultado de prueba Dunn, comparacion entre cada par de dimensiones
print(PT)

#Pares no significtaivos al 99.9% de confianza
print(PT[PT$P.adj>=0.001,]) #estos son estadisticamente equivalentes



