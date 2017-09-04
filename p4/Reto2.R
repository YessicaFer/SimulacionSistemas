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

x=vector()
y=vector()
voronoi=matrix()


celda <-  function(pos) {
  fila <- floor((pos - 1) / n) + 1
  columna <- ((pos - 1) %% n) + 1
  if (voronoi[fila, columna] > 0) { # ya tiene  una semilla
    return(voronoi[fila, columna])
  } else {
    for (semilla in sample(1:length(radio))) {
      dx <- columna - x[semilla]
      dy <- fila - y[semilla]
      dist <- sqrt(dx^2 + dy^2)
      if (dist <= radio[semilla]) {
        return(semilla)
      }
    }
    return(voronoi[fila, columna])
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
  #if (largo >= limite) {
  # png(paste("p4g_", replica, ".png", sep=""))
  #par(mar = c(0,0,0,0))
  #image(rotate(grieta), col=rainbow(k+1), xaxt='n', yaxt='n')
  #graphics.off()
  #grafica=c(grafica,replica)
  
  #}
  return(largo)
}
#for (r in 1:10) { # para pruebas sin paralelismo
#    propaga(r)
#}

acomoda_semilla=function(){
  #vector de probabilidades tipo bañera
  t=pexp(q=seq(0,10,10/(n/2)))
  ex=sapply(2:length(t),function(x) t[x]-t[x-1])
  ex=c(ex,rev(ex))/2
  #plot(ex,type="l")
  
  #vector de probabilidades normal
  t=pnorm(q=seq(-3,3,6/n),sd=0.5)
  nor=sapply(2:length(t),function(x) t[x]-t[x-1])
  #plot(nor,type="l")
  
  #vector de probabilidades normal para la mitad
  t=pnorm(q=seq(-3,3,6/(n/2)),sd=0.5)
  nor2=sapply(2:length(t),function(x) t[x]-t[x-1])
  nor2=rep(nor2,2)
  #plot(nor2,type="l")
  
  #vector de probabilidades tipo gamma
  t=pgamma(q=seq(0,10,10/n),shape=15,rate=5)
  gam=sapply(2:length(t),function(x) t[x]-t[x-1])
  #plot(gam,type="l")
  it=1
  while (it<n*n) { # hasta que hallamos una posicion vacia para la semilla
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
    }else if(tipo==5){
      fila <- sample(1:n, 1)
      columna <- sample(1:n, 1)
      nombre="uniforme"
    }
    if (voronoi[fila, columna] == 0) {
      #print(voronoi[fila, columna])
      return(c(fila,columna))
    }
    it=it+1
  }
  return(c(0,0))
}


#Experimento
graphics=T
datos=data.frame()
factores=data.frame()
ns=c(250,500,1000)
if (!file.exists("R2Gifs")){
  dir.create("R2Gifs")
}
suppressMessages(library(parallel))
cluster=makeCluster(detectCores() - 1)
clusterExport(cluster,"vp")
clusterExport(cluster,"vc")
clusterExport(cluster,"celda")
clusterExport(cluster,"inicio")
clusterExport(cluster,"propaga")
for(n in ns){
  tasa=max(floor(0.015*n),1)
  for(prob_nueva in c(0.2,0.4,0.6)){
    for(tipo in 1:5){
      #crear carpetas para imagenes
      carpeta=paste("R2Graficas_n",n,"_tipo",tipo,"_prob",100*prob_nueva,sep="")
      if (!file.exists(carpeta)){
        dir.create(carpeta)
      }
      
      
      #Para hacer voronoi
      clusterExport(cluster,"n")
      k=1
      it=1
      radio=c(1)
      x=vector()
      y=vector()
      zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
      voronoi=zona
      temp=acomoda_semilla()
      voronoi[temp[1],temp[2]]=k
      x=c(x,temp[2])
      y=c(y,temp[1])
      table(voronoi)
      while(length(which(voronoi==0))>0){
        
        if(runif(1)<prob_nueva){
          k=k+1
          temp=acomoda_semilla()
          if(all(temp==0)){
            k=length(radio)
          }
          
          voronoi[temp[1],temp[2]]=k
          x=c(x,temp[2])
          y=c(y,temp[1])
          radio=c(radio,1)
        }
        
        clusterExport(cluster,"zona")
        clusterExport(cluster,"voronoi")
        clusterExport(cluster,"k")
        clusterExport(cluster,"radio")
        clusterExport(cluster,"x")
        clusterExport(cluster,"y")
        #celdas <- foreach(p = 1:(n * n), .combine=c) %dopar% celda(p)
        celdas=parSapply(cluster,1:(n * n),celda)
        voronoi <- matrix(celdas, nrow = n, ncol = n, byrow=TRUE)
        rotate <- function(x) t(apply(x, 2, rev))
        
        if(graphics){
          png(paste(carpeta,"/grafica_n",n,"_tipo",tipo,"_prob",100*prob_nueva,"_",it,".png",sep=""))
          par(mar = c(0,0,0,0))
          image(rotate(voronoi), col=c( '#FFFFFFFF',rainbow(k)), xaxt='n', yaxt='n')
          graphics.off()
        }
        it=it+1
        radio=radio+tasa
        print(radio)
      }
      
      if(graphics){
        #hacer gif
        library(magick)
        it=it-1
        frames=lapply(1:(it-1),function(x) image_read(paste(carpeta,"/grafica_n",n,"_tipo",tipo,"_prob",100*prob_nueva,"_",x,".png",sep="")))
        animation <- image_animate(image_join(frames),fps=100)
        #print(animation)
        image_write(animation, paste("R2Gifs/bolitasNieve_n",n,"_tipo",tipo,"_prob",100*prob_nueva,".gif",sep=""))
        #sapply(0:iteracion,function(x) file.remove(paste("p2_t",x,".png",sep="")))
      }
      largos <- parSapply(cluster,1:200, propaga)
      datos=rbind(datos,largos)
      factores=rbind(factores,c(n,tipo,prob_nueva))
      
    }
  }
}

stopCluster(cluster)



datos2=data.frame()
for(i in 1:nrow(datos)){
  for(j in 1:100){
    datos2=rbind(datos2,c(as.numeric(factores[i,]),as.numeric(datos[i,j])))
  }
}

names(datos2)=c("tamaño","tipo","prob_nueva","largo")
datos2$tamaño=as.factor(datos2$tamaño)
datos2$tipo=as.factor(datos2$tipo)
levels(datos2$tipo)=c("orillas","centro","esquinas","grupos","uniforme")
#datos2$tipo=factor(datos2$tipo,levels=c("orillas","uniforme","esquinas","grupos","descentrado","centro"))
datos2$prob_nueva=as.factor(datos2$prob_nueva)

#https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
require(ggplot2)
library(RColorBrewer)
png("R2_boxplotLargos.png",width=1200,height=1000)
dodge <- position_dodge(width = 1)
ggplot(data = datos2, aes(x=interaction(tamaño,tipo), y=largo)) +
  geom_violin(aes(fill=prob_nueva),position = dodge)+
  geom_boxplot(aes(fill=prob_nueva),width=.1, outlier.colour=NA,position = dodge)+
  facet_wrap( ~ tipo, scales="free",ncol=2)+
  xlab("Tamaño de cuadricula vs tipo de distribución de semillas")+
  ylab("Largos de las grietas")+
  labs(title="Distribución de las grietas")+
  guides(fill=guide_legend(title="Probabilidad de \n nueva semilla"))+
  #scale_fill_manual(labels=paste(levels(datos2$prob_nueva),"n",sep=""),values=brewer.pal(length(levels(datos2$tipo)),"Paired"))+
  ylim(0,100)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=24),
        axis.text.x = element_text(size=24)) +
  theme(legend.text=element_text(size=20),legend.key = element_rect(size = 3),
        legend.key.size = unit(1.5, 'lines'),legend.title=element_text(size=20))
graphics.off()


kruskal.test(datos2$largo~datos2$tipo)
kruskal.test(datos2$largo~datos2$tamaño)
kruskal.test(datos2$largo~datos2$prob_nueva)

library(FSA)
PT = dunnTest(datos2$largo~datos2$tipo,data=datos2, method="bh") 
PT = PT$res
#Resultado de prueba Dunn, comparacion entre cada par de dimensiones
print(PT)

#Pares no significtaivos al 99.9% de confianza
print(PT[PT$P.adj>=0.05,]) #estos son estadisticamente equivalentes




