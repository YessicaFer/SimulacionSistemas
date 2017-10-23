library(parallel)

pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}

poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}


dist <- function(i,j){
  return (sqrt((val[i,1]-val[j,1])^2+(val[i,2]-val[j,2])^2))
}

manhattan <- function(i,j){
  return (abs(val[i,1]-val[j,1])+abs(val[i,2]-val[j,2]))
}

#distancia hacinamiento ---- 
crowding.distance <- function (frente,nf){
  #nf=dim(frente)[1]
  #k=dim(frente)[2]
  cluster2 <- makeCluster(detectCores() - 1)
  clusterExport(cluster2,c("frente","nf"))
  d=parSapply(cluster2,1:k,function(m){
    distancia=rep(0,nf)
    orden=sort(frente[,m],index.return=T)
    distancia[orden$ix[c(1,nf)]]=Inf
    if (nf<=2){
      return (distancia)
    }
    for (i in 2:(nf-1)){
      distancia[i]=distancia[i]+(orden$x[i+1]-orden$x[i-1])/(orden$x[nf]-orden$x[1])
    }
    
    return(distancia)
  })
  stopCluster(cluster2)
  if(dim(d)[1]==1){
    return (sum(d))
  }
  return (rowSums(d)) 
  
}

medida <- function (frente){
  nf=length(frente)
  
  orden=sort(frente,index.return=T)
  posibles=frente[orden$ix]
  suma=0
  while(length(posibles)>1){
    x=posibles[1]
    posibles=posibles[2:length(posibles)]
    suma=suma+min(sapply(posibles,function(y){return(manhattan(x,y))}))
  }
  
  return(suma)
}

diversidad <- function(seleccion){
  if(length(which(no.dom))==1){
    return (0)
  }else{
    return(100*medida(seleccion)/(medida(which(no.dom) )*length(seleccion)))
  }
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster,c("poli","pick.one","eval","domin.by","dist"))

vc <- 4
md <- 3
tc <- 5

clusterExport(cluster,c("vc","md","tc"))

k <- 2 # cuantas funciones objetivo
n <- 200 # cuantas soluciones aleatorias
datos=data.frame()
for(k in 2:4){
  for(n in c(100,200,300)){
    for(rep in 1:30){
      
      sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
      
      #Creacion de funciones objetivo
      obj <- parLapply(cluster,1:k,function(k){return (poli( md, vc, tc))})
      
      minim <- (runif(k) > 0.5)
      sign <- (1 + -2 * minim)
      
      
      #evaluación de soluciones
      clusterExport(cluster,c("k","n","obj","sol","sign"))
      val=matrix(parSapply(cluster,1:(k*n),function(pos){
        i <- floor((pos - 1) / k) + 1
        j <- ((pos - 1) %% k) + 1
        return(eval(obj[[j]], sol[i,], tc))
      }), nrow=n, ncol=k, byrow=TRUE)
      
      #Buscando soluciones no dominadas
      clusterExport(cluster,"val")
      temp=parSapply(cluster,1:n,function(i){
        d <- logical()
        for (j in 1:n) {
          d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
        }
        cuantos <- sum(d)
        return(c(cuantos,cuantos==0))
      })
      dominadores = temp[1,]
      no.dom = as.logical(temp[2,])
      rm(temp)
      
      frente <- subset(val, no.dom) # solamente las no dominadas
      
      
      ##Seleccionar soluciones no dominadas...
      
      #Usando distancia Manhattan y seleccionando en orden
      startM=Sys.time()
      if(dim(frente)[1]<=2){
        seleccionM=seleccionC=which(no.dom)
        startM=startC=endM=endC=Sys.time()
      }  else{
        
        seleccionM <- parSapply(cluster,1:k,function(i){return(which.max(sign[i] * val[,i]))})
        posibles=setdiff(which(no.dom),seleccionM)
        orden=sort(val[posibles,1],index.return=T)$ix
        umbral=manhattan(seleccionM[1],seleccionM[2])/10
        #Eliminar cercanos
        i=1
        j=1
        quitar=c()
        while(TRUE){
          if(length(orden)<j)break
          if (manhattan(seleccionM[i],posibles[orden[j]])<umbral){ #lEiminar
            quitar=c(quitar,orden[j])
            j=j+1
          }else{
            seleccionM=c(seleccionM,posibles[orden[j]])
            quitar=c(quitar,orden[j])
            orden=setdiff(orden,quitar)
            i=length(seleccionM)
            j=1
            quitar=c()
          }
        }
        
        endM=Sys.time()
        
        startC=Sys.time()
        #distancia hacinamiento
        distancia=crowding.distance(frente,length(frente))
        temp=sort(distancia,index.return=T,decreasing = T)
        seleccionC=which(no.dom)[temp$ix[1:length(seleccionM)]]
        endC=Sys.time()
        
      }
      datos=rbind(datos,c(k,n,0,diversidad(seleccionM),difftime(endM,startM,units='secs')))
      datos=rbind(datos,c(k,n,1,diversidad(seleccionC),difftime(endC,startC,units='secs')))
      
      
    }
  }
}

names(datos)=c("k","n","tipo","diversidad","tiempo")
datos$tipo=as.factor(datos$tipo)
levels(datos$tipo)=c("Manhattan","Hacinamiento")
datos$k=as.factor(datos$k)

library(ggplot2)
png("DiversidadDiversidad.png",width=1000,height=800)
ggplot(data=datos,aes(x=k,y=diversidad,fill=tipo))+geom_boxplot()+
  ylab('Diversidad relativa')+
  xlab('Número de objetivos')+
  labs(title='Diversidad')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=30),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size=26),
        plot.title = element_text(size=32))+
  scale_fill_discrete("Método de\nselección:", 
                      labels=c("Nichos", "Hacinamiento"))
graphics.off()

png("DiversidadTiempo.png",width=1000,height=800)
ggplot(data=datos,aes(x=k,y=tiempo,fill=tipo))+geom_boxplot()+
  ylab('Tiempo de ejecución (s)')+
  xlab('Número de objetivos')+
  labs(title='Tiempo')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=30),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size=26),
        plot.title = element_text(size=32))+
  scale_fill_discrete("Método de\nselección:", 
                      labels=c("Nichos", "Hacinamiento"))
graphics.off()

cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")

png("diversidad.png",width=1200,height=600)
par(mfrow=c(1,2))
plot(val[,1], val[,2],xaxt='n',yaxt='n',xlab='',ylab='',main='Selección descartando nichos',cex=1.5,cex.main=2)
points(frente[,1], frente[,2], col="green", pch=16,cex=1.5)
points(val[seleccionM,1],val[seleccionM,2],col='red', pch=16,cex=1.5)
mtext(side=1,text=xl,line=0.7,cex=2)
mtext(side=2,text=yl,line=0.5,cex=2)
legend("bottomright",paste("Diversidad: ",format(diversidad(seleccionM),digits=3,format='f'),"%"))

plot(val[,1], val[,2],xaxt='n',yaxt='n',xlab='',ylab='',main='Selección basada en distancia de hacinamiento',cex=1.5,cex.main=2)
points(frente[,1], frente[,2], col="green", pch=16,cex=1.5)
points(val[seleccionC,1],val[seleccionC,2],col='red', pch=16,cex=1.5)
mtext(side=1,text=xl,line=0.7,cex=2)
mtext(side=2,text=yl,line=0.5,cex=2)
legend("bottomright",paste("Diversidad: ",format(diversidad(seleccionC),digits=3,format='f'),"%"))

graphics.off()









#}

stopCluster(cluster)

