library(parallel)
library(magick)
dim <- 50
num <-  dim^2


#actual <- matrix(round(runif(num)), nrow=dim, ncol=dim)
suppressMessages(library("sna"))
#png("p2_t0.png")
#plot.sociomatrix(actual, diaglab=FALSE,drawlab=FALSE, main="Inicio")
#graphics.off()

paso <- function(pos){
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  if(actual[fila,columna]>0){ #si ya tiene un nucleo asignado... dejar igual
    return(actual[fila,columna])
  }else{
    vecindad <-  actual[max(fila - 1, 0) : min(fila + 1, dim),
                        max(columna - 1, 0): min(columna + 1, dim)]
    vecindad=sample(vecindad)
    for(v in vecindad){ #desordenar vecindad
      if(v>0){#tiene un vecino en algun nucleo
        if(runif(1)<tasa){#si pasa la tasa de crecimiento
          return(v) #cambiar a vecino seleccionado
        }
      }
    }
    return(actual[fila,columna]) #si no se pudo asignar a algun nucleo... dejar igual
  }
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")
kas=c(5,10,20,30,40,50,100,500)
tasa=0.5
num_rep=50
datos=data.frame()
datos_O=data.frame()
centro=data.frame()
clusterExport(cluster,"tasa")
for(k in kas){
  clusterExport(cluster,"k")
  tam=vector()
  tam_orilla=vector()
  numero=vector()
  for(i in 1:num_rep){
    actual <- matrix(rep(0, dim * dim), nrow = dim, ncol = dim)
    
    for (semilla in 1:k) {
      while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
        fila <- sample(1:dim, 1)
        columna <- sample(1:dim, 1)
        if (actual[fila, columna] == 0) {
          actual[fila, columna] = semilla
          break
        }
      }
    }
    #png("p2_t0.png")
    #plot.sociomatrix(actual, diaglab=FALSE,drawlab=FALSE, main="Inicio")
    #graphics.off()
    
    while(TRUE){
      clusterExport(cluster, "actual")
      siguiente <- parSapply(cluster,1:num, paso)
      actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
      
      #grafica matriz
      #salida = paste("p2_t", iteracion, ".png", sep="")
      #tiempo = paste("Paso", iteracion)
      #png(salida)
      #plot.sociomatrix(actual, diaglab=FALSE ,drawlab=FALSE,main=tiempo)
      #graphics.off()
      
      if(length(which(actual==0))==0){
        #evalua
        s=setdiff(1:k,union(union(union(actual[1,],actual[dim,]),actual[,1]),actual[,dim]))
        tam=c(tam,sapply(s,function(x) length(which(actual==x))))
        tam_orilla=c(tam_orilla,sapply(union(union(union(actual[1,],actual[dim,]),actual[,1]),actual[,dim]),function(x) length(which(actual==x))))
        numero=c(numero,length(s))
        break
      }
      
    }
    
  }
  datos=rbind(datos,c(tam,rep(NA,30000-length(tam))))
  datos_O=rbind(datos_O,c(tam_orilla,rep(NA,30000-length(tam_orilla))))
  centro=rbind(centro,numero)
  centro=rbind(centro,k-numero)
}
stopCluster(cluster)


#Graficas
  quitaNA<-function(fila){
    return(as.integer(fila)[which(as.numeric(fila)!='NA')])
  }
  
  f5=quitaNA(datos[1,])
  f10=quitaNA(datos[2,])
  f20=quitaNA(datos[3,])
  f30=quitaNA(datos[4,])
  f40=quitaNA(datos[5,])
  f50=quitaNA(datos[6,])
  f100=quitaNA(datos[7,])
  f500=quitaNA(datos[8,])
  
  o5=quitaNA(datos_O[1,])
  o10=quitaNA(datos_O[2,])
  o20=quitaNA(datos_O[3,])
  o30=quitaNA(datos_O[4,])
  o40=quitaNA(datos_O[5,])
  o50=quitaNA(datos_O[6,])
  o100=quitaNA(datos_O[7,])
  o500=quitaNA(datos_O[8,])
  
  png("R1_distribucionNucleosInternos.png",width=1200,height=600)
  layout(matrix(1:2,nrow=1))
  par(mar=c(4,4,4,0),bty="c")
  plot(sort(f10,decreasing = T),type="l",col="red",xaxt='n',xlab='',ylab='',yaxt='n')
  lines(sort(f5,decreasing = T),col="gray")
  lines(sort(f20,decreasing = T),col="black")
  lines(sort(f30,decreasing = T),col="green")
  lines(sort(f40,decreasing = T),col="blue")
  lines(sort(f50,decreasing = T),col="yellow")
  lines(sort(f100,decreasing = T),col="purple")
  lines(sort(f500,decreasing = T),col="orange")
  mtext(2, text = "Tamaño de los nucleos", line = 0.5)
  mtext(1, text = "Nucleos interiores", line = 0.5)
  legend("topright",bty='n',pch=20,legend=kas, ncol = 2,
        col=c("gray","red","black","green","blue","yellow","purple","orange"),title = "Numero de nucleos iniciales")
  
  
  par(mar=c(4,0,4,4),bty="]")
  plot(sort(o10,decreasing = T),col="red",lty="twodash",type="l",xaxt='n',xlab='',ylab="Tamaños de nucleos",yaxt='n',ylim=c(min(datos,na.rm = T),max(datos,na.rm = T)))
  lines(sort(o5,decreasing = T),col="gray",lty="twodash")
  lines(sort(o20,decreasing = T),col="black",lty="twodash")
  lines(sort(o30,decreasing = T),col="green",lty="twodash")
  lines(sort(o40,decreasing = T),col="blue",lty="twodash")
  lines(sort(o50,decreasing = T),col="yellow",lty="twodash")
  lines(sort(o100,decreasing = T),col="purple",lty="twodash")
  lines(sort(o500,decreasing = T),col="orange",lty="twodash")
  axis(4,at=c(seq(0,1200,50)))
  mtext(1, text = "Nucleos limitados por la orilla", line = 0.5)
  graphics.off()
  
  png("R1_boxplotTamNucleos.png",width=1000,height=600)
  boxplot(f5,o5,f10,o10,f20,o20,f30,o30,f40,o40,f50,o50,f100,o100,f500,o500,xaxt='n',col=rep(c("green","red"),8),ylab="Tamaño de Nucleos",xlab="Número de nucleos iniciales")
  axis(1,at=seq(1.5,15.5,2),labels=kas)
  legend("topright",pch=22,legend=c("Nucleos interiores","Nucleos limitados por la orilla"),col=c("green","red"))
  graphics.off()

#Las pruebas de Wilcoxon muestran que las distribuciones de los tamaños de nucleos nunca son iguales
wilcox.test(f500,o500)

#Numero de nucleos internos vs externos
png("R1_boxplotNumNucleos.png",width=1000,height=600)
boxplot(t(centro),col=rep(c("green","red"),6),xaxt='n',ylab="Nucleos",xlab="Número de nucleos iniciales")
axis(1,at=seq(1.5,15.5,2),labels=kas)
legend("topleft",pch=22,legend=c("Nucleos interiores","Nucleos limitados por la orilla"),col=c("green","red"))
graphics.off()

#hacer gif
#frames=lapply(0:iteracion,function(x) image_read(paste("p2_t",x,".png",sep="")))
#animation <- image_animate(image_join(frames))
#print(animation)
#image_write(animation, "R2_crecimiento.gif")
#sapply(0:iteracion,function(x) file.remove(paste("p2_t",x,".png",sep="")))
