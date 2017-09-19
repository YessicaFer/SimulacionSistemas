library(parallel)

cluster <- makeCluster(detectCores() - 1)


l <- 1.5
n <- 50
pi <- 0.05
pr <- 0.02
v <- l / 30

clusterExport(cluster,"l")
clusterExport(cluster,"pi")
clusterExport(cluster,"pr")
clusterExport(cluster,"v")


agente=function(i){
  e <- 5
  if (runif(1) < pi) {
    e <- 1
  }
  
  return(c(x = runif(1, 0, l), y = runif(1, 0, l),
                    dx = runif(1, -v, v), dy = runif(1, -v, v),
                    estado = e))
}


contagiar=function(i){
  a1 <- agentes[i, ]
  if (a1$estado == 5) {# desde los susceptibles
    for (j in which(agentes$estado==1)) {
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
for(n in c(100,200,300)){
  clusterExport(cluster,"n")
  for(rep in 1:10){
  #Version paralela
    startP=Sys.time()
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
    endP=Sys.time()
    
  #version secuencial
    startS=Sys.time()
    #creacion de agentes
    agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(), estado  = character())
    for (i in 1:n) {
      e <- "S"
      if (runif(1) < pi) {
        e <- "I"
      }
      agentes <- rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                                           dx = runif(1, -v, v), dy = runif(1, -v, v),
                                           estado = e))
    }
    
    levels(agentes$estado) <- c("S", "I", "R")
    
    epidemia <- integer()
    r <- 0.1
    tmax <- 100
    digitos <- floor(log(tmax, 10)) + 1
    for (tiempo in 1:tmax) {
      infectados <- dim(agentes[agentes$estado == "I",])[1]
      epidemia <- c(epidemia, infectados)
      if (infectados == 0) {
        break
      }
      #contagios
      contagios <- rep(FALSE, n)
      for (i in 1:n) { # posibles contagios
        a1 <- agentes[i, ]
        if (a1$estado == "I") { # desde los infectados
          for (j in 1:n) {
            if (!contagios[j]) { # aun sin contagio
              a2 <- agentes[j, ]
              if (a2$estado == "S") { # hacia los susceptibles
                dx <- a1$x - a2$x
                dy <- a1$y - a2$y
                d <- sqrt(dx^2 + dy^2)
                if (d < r) { # umbral
                  p <- (r - d) / r
                  if (runif(1) < p) {
                    contagios[j] <- TRUE
                  }
                }
              }
            }
          }
        }
      }
      
      #actualización
      for (i in 1:n) { # movimientos y actualizaciones
        a <- agentes[i, ]
        if (contagios[i]) {
          a$estado <- "I"
        } else if (a$estado == "I") { # ya estaba infectado
          if (runif(1) < pr) {
            a$estado <- "R" # recupera
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
        agentes[i, ] <- a
      }
    }
    endS=Sys.time()
    
    datos=rbind(datos,c(n,endP-startP,endS-startS))
  }
}

names(datos)=c("n","paralelo","secuencial")
datos1=datos[,1:2]
names(datos1)=c("n","tiempo")
datos1$metodo='paralelo'
datos2=data.frame(datos$n)
names(datos2)='n'
datos2$tiempo=datos$secuencial
datos2$metodo='secuencial'
datos=rbind(datos1,datos2)
datos$n=as.factor(datos$n)

require(ggplot2)


png("secuencialParalelo.png",width = 1200,height = 800)
ggplot(datos, aes(factor(n), tiempo)) + 
  geom_boxplot() + facet_grid(. ~ metodo)+
  stat_summary(fun.y=median, geom="smooth", aes(group=1))+
  ylab('Tiempo de ejecución (s)')+
  xlab('Número de agentes')+
  labs(title='Comparacion secuencial vs paralelo')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=30),
        axis.text.x = element_text(size=26,angle = 90, hjust = 1),
        axis.text.y = element_text(size=26),
        plot.title = element_text(size=32))


graphics.off()
