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
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster,c("poli","pick.one","eval","domin.by"))

vc <- 4
md <- 3
tc <- 5

clusterExport(cluster,c("vc","md","tc"))

k <- 2 # cuantas funciones objetivo
n <- 200 # cuantas soluciones aleatorias
datos=data.frame()
for(k in c(2,4,8,16)){
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
      
      datos=rbind(datos,c(k=k,n=n,porcentaje=dim(frente)[1]*100/n))
    }
  }
}
stopCluster(cluster)

names(datos)=c("k","n","porcentaje")
datos$k=as.factor(datos$k)
library(ggplot2) # recordar instalar si hace falta
library(beeswarm)



png("abejitas.png",width=1200,height=800)
par(mar=c(5, 5, 4, 2))
beeswarm(data=datos,porcentaje~k,col=rainbow(15),pch=19,method = "square",log = FALSE,corral="gutter",corralWidth=1,cex=1.5,cex.axis=2,cex.lab=2,ylab='Porcentaje de soluciones no dominadas',xaxt='n',xlab='Número de funciones objetivo')
axis(1,at=1:15,labels = rep('',15), cex.axis=2)
boxplot(data=datos,porcentaje~k,add=TRUE,col= rgb(0,0,1.0,alpha=0),outline=F,xaxt='n',yaxt='n')
text(labels=2:16, col=rainbow(15),x=1:15+0.25,y=-8.5,srt = 0, pos = 2, xpd = TRUE,cex=2)
graphics.off()


png("p11_violinObjetivos1.png",width=1200,height=800)
gr <- ggplot(subset(datos,datos$k<=4), aes(x=factor(k), y=porcentaje)) + 
  geom_violin(fill="orange", color="red",scale="count")
gr + geom_boxplot(width=0.05, fill="blue", color="white", lwd=1.2) +
  xlab("Número de objetivos") +
  ylab("Porcentaje de soluciones no dominadas") +
  ggtitle("Cantidad de soluciones dominantes")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=30),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size=26),
        plot.title = element_text(size=32))
graphics.off()



kruskal.test(data=datos,porcentaje~k)

library(FSA)
PT = dunnTest(data=datos,porcentaje~factor(k),method="bh") 
PT = PT$res
#Resultado de prueba Dunn, comparacion entre cada par de dimensiones
print(PT)

#Pares no significtaivos al 99.99% de confianza
print(PT[PT$P.adj>=0.001,]) #estos son estadisticamente equivalentes
