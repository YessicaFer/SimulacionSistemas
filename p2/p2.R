library(parallel)
dim <- 20
num <-  dim^2
prob_vivo=0.5
tope=1.5*dim
#actual <- matrix(round(runif(num)), nrow=dim, ncol=dim)

#suppressMessages(library("sna"))
#png("p2_t0.png")
#plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
#graphics.off()

paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                      max(columna - 1, 1): min(columna + 1, dim)]
  return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}

cluster <- makeCluster(detectCores() - 1)

clusterExport(cluster, "dim")
clusterExport(cluster, "paso")
probabilidades=seq(0,1,0.05)
datos=data.frame()
num_rep=50
for(prob_vivo in probabilidades){
  clusterExport(cluster,"prob_vivo")
  for(i in 1:num_rep){
    #iniciar actual
    actual <- matrix(1*(runif(num)<=prob_vivo), nrow=dim, ncol=dim)
    iteracion=1
    for(j in 1:tope) {
      clusterExport(cluster, "actual")
      siguiente <- parSapply(cluster, 1:num, paso)
      actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
      
      if (sum(siguiente) == 0) { # todos murieron
        #print("Ya no queda nadie vivo.")
        break;
      }
      iteracion=iteracion+1
    }
    if(iteracion>tope){
      datos<-rbind(datos,c(prob_vivo,iteracion,1))
    }else{
      datos<-rbind(datos,c(prob_vivo,iteracion,0))
    }
    
  }
}
stopCluster(cluster)




names(datos)=c("prob_vivo","iteracion","infinito")
datos$prob_vivo=as.factor(datos$prob_vivo)
datos$infinito=as.factor(datos$infinito)
boxplot(datos$iteracion[datos$infinito==0]~datos$prob_vivo[datos$infinito==0])

if(sum(as.numeric(as.vector(datos$infinito)))/length(datos$infinito)<0.1){
  print("bajar la cota")
}else{
  print("subir la cota")
  print(sum(as.numeric(as.vector(datos$infinito))))
}

png("T1_boxplot.png",width=1200,height=600)
boxplot(datos$iteracion[datos$infinito==0]~datos$prob_vivo[datos$infinito==0], data = datos,ylab="Numero de iteraciones",xlab="Probabilidad de distribucion inicial") 
lines(as.factor(probabilidades),sapply(levels(datos$prob_vivo),function(x) median(datos$iteracion[datos$prob_vivo==x&datos$infinito==0])),col="red",lwd=3)
axis(side=2,at=1:max(datos$iteracion[datos$infinito==0]),labels=1:max(datos$iteracion[datos$infinito==0]))
abline(h=mean(datos$iteracion),col="green")
abline(h=mean(datos$iteracion)+sqrt(var(datos$iteracion)),col="blue")
segments(1,mean(datos$iteracion)+1,1,mean(datos$iteracion)+sqrt(var(datos$iteracion))-1,col="blue")
segments(0.8,mean(datos$iteracion)+1,1.2,mean(datos$iteracion)+1,col="blue")
segments(0.8,mean(datos$iteracion)+sqrt(var(datos$iteracion))-1,1.2,mean(datos$iteracion)+sqrt(var(datos$iteracion))-1,col="blue")
text(x=as.factor(1),y=mean(datos$iteracion)-1, label=paste("Media: ", format(mean(datos$iteracion),digits=2)), col = "green")
text(x=1.5,y=mean(datos$iteracion)+3,label=paste("sd: ",format(sqrt(var(datos$iteracion)),digits=2)),srt=90,col="blue")
#axis(1,at=factor(seq(0,2,0.2)),las=2)
graphics.off()

#Hay diferencia estadistica en las iteraciones del intervalo [0.3,0.45]?
kruskal.test(datos$iteracion[datos$infinito==0&(datos$prob_vivo==0.3|datos$prob_vivo==0.35|datos$prob_vivo==0.4|datos$prob_vivo==0.45)]~datos$prob_vivo[datos$infinito==0&(datos$prob_vivo==0.3|datos$prob_vivo==0.35|datos$prob_vivo==0.4|datos$prob_vivo==0.45)])


#Hay diferencia estadistica en las iteraciones del intervalo [0.3,0.45]?
kruskal.test(datos$iteracion[datos$infinito==0&(datos$prob_vivo==0.95|datos$prob_vivo==1)]~datos$prob_vivo[datos$infinito==0&(datos$prob_vivo==0.95|datos$prob_vivo==1)])




