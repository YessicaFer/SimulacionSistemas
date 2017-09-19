library(parallel)

cluster <- makeCluster(detectCores() - 1)


l <- 1.5
n <- 50
pi <- 0.05
pr <- 0.02
pv <- 0.05
v <- l / 30

clusterExport(cluster,"l")
clusterExport(cluster,"pi")
clusterExport(cluster,"pr")

clusterExport(cluster,"v")


agente=function(i){
  e <- 5
  if (runif(1) < pi) {
    e <- 1
  }else if(runif(1)<pv){
    e <- 4
  }
  
  return(c(x = runif(1, 0, l), y = runif(1, 0, l),
           dx = runif(1, -v, v), dy = runif(1, -v, v),
           estado = e))
}


contagiar=function(i){
  a1 <- agentes[i, ]
  if (a1$estado == 5) {# desde los susceptibles
    for (j in 1:n) {
      a2 <- agentes[j, ]
      if (a2$estado == 1) { # hacia los infectados
        dx <- a1$x - a2$x
        dy <- a1$y - a2$y
        d <- sqrt(dx^2 + dy^2)
        if (d < r) { # umbral
          p <- (r - d) / r
          if (runif(1) < p) {
            return(TRUE)
          }
        }
      }
    }
    return(FALSE)
  }else{
    return(FALSE)
  }
  
}

mov_act=function(i) { # movimientos y actualizaciones
  a <- agentes[i, ]
  if (contagios[i]) {
    a$estado <- 1
  } else if (a$estado == 1) { # ya estaba infectado
    if (runif(1) < pr) {
      a$estado <- 4 # recupera
    }
  }
  a$x <- a$x + a$dx
  a$y <- a$y + a$dy
  if (a$x > l) {
    a$x <- a$x - l
  }
  if (a$y > l) {
    a$y <- a$y - l
  }
  if (a$x < 0) {
    a$x <- a$x + l
  }
  if (a$y < 0) {
    a$y <- a$y + l
  }
  return(c(x=a$x,y=a$y,dx=a$dx,dy=a$dy,estado=a$estado))
}

clusterExport(cluster,"agente")
clusterExport(cluster,"contagiar")
clusterExport(cluster,"mov_act")

datos=data.frame()

  clusterExport(cluster,"n")
for(pv in seq(0,1,0.05)){
  clusterExport(cluster,"pv")
  for(rep in 1:10){
    #Version paralela
    
    #creacion de agentes
    agentes=parSapply(cluster,1:n,agente)
    agentes=data.frame(t(agentes))
    #levels(agentes$estado) <- c(5, 1, 4)
    
    epidemia <- integer()
    r <- 0.1
    tmax <- 100
    clusterExport(cluster,"r")
    for (tiempo in 1:tmax) {
      infectados <- dim(agentes[agentes$estado == 1,])[1]
      epidemia <- c(epidemia, infectados)
      if (infectados == 0) {
        break
      }
      
      #contagios
      clusterExport(cluster,"agentes")
      contagios=parSapply(cluster,1:n,contagiar)
      
      #actualización
      clusterExport(cluster,"contagios")
      agentes=data.frame(t(parSapply(cluster,1:n,mov_act)))
      
    }
    datos=rbind(datos,epidemia)
  } 
}
 
  
stopCluster()

c0=colSums(datos[1:10,],1)/10 #pv=0
c1=colSums(datos[11:20,],1)/10 #pv=0.05
c2=colSums(datos[21:30,],1)/10 #pv=0.1
c3=colSums(datos[101:110,],1)/10 #pv=0.5
c4=colSums(datos[201:210,],1)/10 #pv=0.95
c5=colSums(datos[161:170,],1)/10 #pv=0.8

png("comparacion.png", width=600, height=300)
par(mar=c(5.1, 4.1, 4.1, 9), xpd=TRUE)
plot(1:100,c0,type='l',col='blue',xlab='Tiempo',ylab='Porcentaje de infectados',main='Simulación de epidemia con vacunación')
lines(1:100,c1,col='red')
lines(1:100,c2,col='green')
lines(1:100,c3,col='purple')
lines(1:100,c5,col='black')
#lines(1:100,c4,col='orange')
legend("topright",legend=c(0,0.05,0.1,0.5,0.8),fill=c('blue','red','green','purple','black'),inset=c(-0.3,0),title='probabilidad vacuna')
graphics.off()

png("p6e.png", width=600, height=300)
plot(1:100,c0,type='l',xlab='Tiempo',ylab='Porcentaje de infectados',main='Simulación de epidemia')
graphics.off()
