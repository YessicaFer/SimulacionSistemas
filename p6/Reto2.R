library(foreach)
library(doParallel)

cl<-makeCluster(detectCores()-1)
registerDoParallel(cl)

l <- 1.5
n <- 500
#pi <- 0.05
pr <- 0.02
v <- l / 30
pv <- 0.01

agente=function(i){
  e <- "S"
  if (runif(1) < pi) {
    e <- "I"
  }
  if (runif(1) < pv){
    e <- "R"
  }
  return(data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                    dx = runif(1, -v, v), dy = runif(1, -v, v),
                    estado = e))
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

contagiar=function(i){
  contagios=c()
  a1 <- agentes[i, ]
  print(a1)
  if (a1$estado == "I") { # desde los infectados
    for (j in 1:n) {
      a2 <- agentes[j, ]
      if (a2$estado == "S") { # hacia los susceptibles
        dx <- a1$x - a2$x
        dy <- a1$y - a2$y
        d <- sqrt(dx^2 + dy^2)
        if (d < r) { # umbral
          p <- (r - d) / r
          if (runif(1) < p) {
            contagios=c(contagios,j)
          }
        }
      }
    }
    
  }
  return(contagios)
}

datos=data.frame()
for(pi in seq(0.05,0.95,0.05)){
  for(rep in 1:1){
    #Creacion de agentes
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
      
      #contagiar
      temp=foreach(i=1:n,.combine=c)%dopar% contagiar(i)
      contagios=rep(FALSE,n)
      contagios[temp]=TRUE
      
      #Actualizacion y movimientos
      agentes=foreach(i=1:n,.combine=rbind) %dopar% mov_act(i)
      
      
    }
    datos=rbind(datos,c(pi,100*max(epidemia)/n))
  }
}
stopImplicitCluster()


names(datos)=c("ProbabilidadInfeccion","MaximoPorcentaje")
datos$ProbabilidadInfeccion=as.factor(datos$ProbabilidadInfeccion)
boxplot(datos$MaximoPorcentaje~datos$ProbabilidadInfeccion,xlab="Probabilidad de infecci?n",ylab="Porcentaje m?ximo de infecci?n")

library(ggplot2)
png("P6_Violines_Infeccion.png",width=1200,height=1000)  #recuerda acomodar ylim
dodge <- position_dodge(width = 1)
ggplot(data=datos,aes(x=ProbabilidadInfeccion,y=MaximoPorcentaje))+
  geom_violin(position = dodge)+
  geom_boxplot(position=dodge,width=0.1)+
  stat_summary(fun.y=median, geom="smooth", aes(group=1))+
  xlab("Probabilidad de infecci?n")+
  ylab("Porcentaje m?ximo de infecci?n")+
  labs(title="Variando la probabilidad de infecci?n")+
  theme(text = element_text(size=36),
                                                        axis.text.x = element_text(size=30,angle = 90, hjust = 1),
                                                        axis.text.y = element_text(size=30),
                                                        plot.title = element_text(size=36))+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(38,100)
graphics.off()

kruskal.test(datos$MaximoPorcentaje~datos$ProbabilidadInfeccion)
