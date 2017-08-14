library(parallel)

datos <-  data.frame(stringsAsFactors=FALSE)

temp=c()
normal=FALSE
alfa=0.001



experimento <- function(replica) {
  
  pos <- rep(0, dimension)
  ceros <- 0
  for (t in 1:duracion) {
    cambiar <- sample(1:dimension, 1)
    cambio <- 1
    if (runif(1) < 0.5) {
      cambio <- -1
    }
    pos[cambiar] <- pos[cambiar] + cambio
    
  }
  return(ceros)
}
replicas=10
dimensiones=3
duracion=300
repeticiones=seq(100,1000,100)
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "experimento")
clusterExport(cluster, "dimension")
clusterExport(cluster, "duracion")
clusterExport(cluster,"replicas")
for (repetir in repeticiones){
    for(i in 1:replicas){
      clusterExport(cluster, "repetir")
      resultado_P <- system.time(parSapply(cluster, 1:repetir, experimento))[3]
      resultado_NP<- system.time(sapply(1:repetir,experimento))[3]
      datos <- rbind(datos, c(repetir,resultado_P,resultado_NP))
      
  }
}

stopCluster(cluster)

names(datos)<-c("replicas","paralelo","no_paralelo")
datos$replicas<-as.factor(datos$replicas)
r=c(as.vector(datos$paralelo),as.vector(datos$no_paralelo))
datos2<- data.frame(id=c(rep("Paralelo",nrow(datos)),rep("No paralelo",nrow(datos))),
replicas=rep(as.vector(datos$replicas),2),
Tiempo=c(as.vector(datos$paralelo),as.vector(datos$no_paralelo))
)

png("R2_boxplot.png")
ggplot(datos2,aes(x=replicas, y=Tiempo)) +
  geom_boxplot(aes(fill=id)) +
  scale_x_discrete(limits=paste(repeticiones),labels=paste(repeticiones))+
  theme(legend.position = "bottom")
graphics.off()


