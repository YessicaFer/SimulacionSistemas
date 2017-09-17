library(foreach)
library(doParallel)

cl<-makeCluster(detectCores()-1)
registerDoParallel(cl)

l <- 1.5
n <- 50
pi <- 0.05
pr <- 0.02
v <- l / 30

agente=function(i){
  e <- "S"
  if (runif(1) < pi) {
    e <- "I"
  }
  
  return(data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                    dx = runif(1, -v, v), dy = runif(1, -v, v),
                    estado = e))
}


agentes=foreach(i = 1:n, .combine = rbind)  %dopar%  agente(i)
levels(agentes$estado) <- c("S", "I", "R")

epidemia <- integer()
r <- 0.1
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
nombres=c()
for (tiempo in 1:tmax) {
  infectados <- dim(agentes[agentes$estado == "I",])[1]
  epidemia <- c(epidemia, infectados)
  if (infectados == 0) {
    break
  }
  
  
  
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
  
  
  mov_act=function(i) { # movimientos y actualizaciones
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
    return( a )
    
    
  }
  
  agentes=foreach(i=1:n,.combine=rbind) %dopar% mov_act(i)
  aS <- agentes[agentes$estado == "S",]
  aI <- agentes[agentes$estado == "I",]
  aR <- agentes[agentes$estado == "R",]
  tl <- paste(tiempo, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  salida <- paste("Imagenes_P6/p6_t", tl, ".png", sep="")
  nombres=c(nombres,salida)
  tiempo <- paste("Paso", tiempo)
  png(salida)
  par(mar=c(0.2,0.2,0.2,0.2))
  plot(l, type="n", main='', xlim=c(0, l), ylim=c(0, l),xaxt='n',yaxt='n',xlab='',ylab='')
  if (dim(aS)[1] > 0) {
    points(aS$x, aS$y, pch=15, col="blue", bg="blue",cex=1.5)
  }
  if (dim(aI)[1] > 0) {
    points(aI$x, aI$y, pch=16, col="red", bg="red",cex=2)
  }
  if (dim(aR)[1] > 0) {
    points(aR$x, aR$y, pch=17, col="green", bg="green",cex=2)
  }
  graphics.off()
}
png("p6e.png", width=600, height=300)
plot(1:length(epidemia), 100 * epidemiaNoR / (4*n), xlab="Tiempo", ylab="Porcentaje de infectados",main="Simulación de epidemia")
graphics.off()
stopImplicitCluster()

#hacer gif
library(magick)
frames=lapply(nombres,function(x) image_read(x))
animation <- image_animate(image_join(frames),fps=100)
print(animation)
image_write(animation, "automatas.gif")
#sapply(nombres,function(x) file.remove(x))


#epidemiaNoR=epidemiaNoR+epidemia