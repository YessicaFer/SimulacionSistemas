

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
ns=c(50,100,500,1000)
datos=data.frame()
factores=data.frame()
for(n in ns){
  for(k in kas){
    ka=k
    k=floor(k*n)
    zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
    x <- rep(0, k) # ocupamos almacenar las coordenadas x de las semillas
    y <- rep(0, k) # igual como las coordenadas y de las semillas
    for (semilla in 1:k) {
      while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
        fila <- sample(1:n, 1)
        columna <- sample(1:n, 1)
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
    
    
    datos=rbind(datos,largos)
    factores=rbind(factores,c(n,ka))
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

names(datos2)=c("tamaño","semillas","largo")
datos2$tamaño=as.factor(datos2$tamaño)
datos2$semillas=as.factor(datos2$semillas)

#https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
require(ggplot2)
library(RColorBrewer)
png("T1_boxplotLargos.png",width=800,height=1000)
dodge <- position_dodge(width = 1)
ggplot(data = datos2, aes(x=tamaño, y=largo)) +
  geom_violin(aes(fill=semillas),position = dodge)+
  geom_boxplot(aes(fill=semillas),width=.1, outlier.colour=NA,position = dodge)+
  facet_wrap( ~ tamaño, scales="free",ncol=2)+
  xlab("Tamaño de la cuadricula")+
  ylab("Largos de las grietas")+
  labs(title="Distribución de las grietas")+
  guides(fill=guide_legend(title="Porcentaje\n de semillas"))+
  scale_fill_manual(labels=paste(levels(datos2$semillas),"n",sep=""),values=brewer.pal(length(levels(datos2$semillas)),"Paired"))+
  ylim(0,350)+
  theme(plot.title = element_text(hjust = 0.5))
graphics.off()

library(FSA)
PT = dunnTest(datos2$largo~datos2$tamaño,data=datos2,
              method="bh") 
PT = PT$res
#Resultado de prueba Dunn, comparacion entre cada par de dimensiones
print(PT)

#Pares no significtaivos al 99.9% de confianza
print(PT[PT$P.adj>=0.001,]) #estos son estadisticamente equivalentes
