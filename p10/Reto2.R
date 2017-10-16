library(testit)
library(parallel)
knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso) 
  assert(n == length(valor))
  vt <- sum(valor) 
  if (pt < cap) { 
    return(vt)
  } else {
    filas <- cap + 1 
    cols <- n + 1 
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) 
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 
    }
    rownames(tabla) <- 0:cap 
    colnames(tabla) <- c(0, valor) 
    for (objeto in 1:n) { 
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1, media, desv))
  }
  valores <- normalizar(valores) * (max - min) + max
  return(valores)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster,"factible")
clusterExport(cluster,"objetivo")
clusterExport(cluster,"normalizar")
clusterExport(cluster,"poblacion.inicial")
clusterExport(cluster,"mutacion")
clusterExport(cluster,"reproduccion")


datos=data.frame()

#for(init in c(50,100,200)){
 # print(c("init",init))
  #for (pm in c(0.05,0.1,0.2)){
   # print(c("pm",pm))
    #for (rep in c(0.5,1,2)){
     # rep=floor(rep*init)
      #print(c("rep",rep))
      #for(tmax in c(50,100,200)){
       # print(c("tmax",tmax))
        
        
       # for(replica in 1:1){
          #creacion de instancia
          n <- 50
          pesos <- generador.pesos(n, 15, 80)
          valores <- generador.valores(pesos, 10, 500)
          capacidad <- round(sum(pesos) * 0.65)
          optimo <- knapsack(capacidad, pesos, valores)
          clusterExport(cluster,"n")
          clusterExport(cluster,"capacidad")
          clusterExport(cluster,"init")
          clusterExport(cluster,"pesos")
          clusterExport(cluster,"valores")
          
          init <- 3000
          #########################################################################################
          #version con ruleta
          p <- as.data.frame(t(parSapply(cluster,1:init,function(i){return(round(runif(n)))})))
          p0=p
          tam <- dim(p)[1]
          assert(tam == init)
          pm <- 0.05
          rep <- 300
          tmax <- 300
          k=0.1
          mejoresR <- double()
          ruleta=rep(1/tam,tam)
          clusterExport(cluster,"p")
          obj=parSapply(cluster,1:tam,function(i){return(objetivo(unlist(p[i,]), valores))})
          for (iter in 1:tmax) {
            p$obj <- NULL
            p$fact <- NULL
            
            #escalar densidad
            temp=density(obj)
            temp$x=(temp$x-min(temp$x))/(max(temp$x)-min(temp$x))*init
            temp$y=(temp$y-min(temp$y))/(max(temp$y)-min(temp$y))
            png(paste("R2_R_",iter,".png",sep=''),width = 800,height=800)
            plot(temp$x,temp$y,ylim = c(0,1),type='l',xlab="Valores objetivo",
                 ylab="Densidad",main=paste("Con Ruleta en supervivencia\nGeneración",iter),cex.axis=1.5,
                 cex.main=1.5,cex.lab=1.5,xaxt='n')
            
            graphics.off()
            
            
            #mutacion
            clusterExport(cluster,"p")
            mutan=sample(1:tam,round(pm*tam)) #Elegir cuales van a mutar con pm
            p <- rbind(p,(t(parSapply(cluster,mutan,function(i){return(mutacion(unlist(p[i,]), n))}))))
            
            #"reproduccion"
            clusterExport(cluster,"tam")
            clusterExport(cluster,"p") #se actualiza p pero como no lo ha hecho tam la muestra solo es sobre los originales
            clusterExport(cluster,"ruleta")
            padres <- parSapply(cluster,1:rep,function(x){return(sample(1:tam, 2, prob=ruleta,replace=FALSE))}) #selección de padres
            clusterExport(cluster,"padres")
            hijos <- t(parSapply(cluster,1:rep,function(i){return(unlist(reproduccion(p[padres[1,i],], p[padres[2,i],], n)))}))
            p = rbind(p,hijos[,1:n],hijos[,(n+1):(2*n)])
            
            #actualización
            tam <- dim(p)[1]
            clusterExport(cluster,"p")
            obj=parSapply(cluster,1:tam,function(i){return(objetivo(unlist(p[i,]), valores))})
            fact=parSapply(cluster,1:tam,function(i){return(factible(unlist(p[i,]), pesos, capacidad))})
            
            
            p <- cbind(p, obj)
            p <- cbind(p, fact)
            elite <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
            #penalizar a los infactibles
            f.min=min(obj[fact])
            nf.max=max(obj[!fact])
            #se penaliza para que el mejor infactible este al 20% del peor factible
            obj[!fact]=max(obj[!fact]-(nf.max-f.min*0.8),0)
            
            #agregar k mejores
            mantener=elite[1:floor(k*init)]
            
            obj2=obj[setdiff(1:tam,mantener)]
            
            ruleta=obj2/sum(obj2)
            mantener <- c(mantener,sample(setdiff(1:tam,mantener),init-length(mantener),replace = FALSE,prob=ruleta))
            p <- p[mantener,]
            tam <- dim(p)[1]
            assert(tam == init)
            #reactualizar ruleta para la seleccion
            ruleta=obj[mantener]/sum(obj[mantener])
            factibles <- p[p$fact == TRUE,]
            
            mejorR <- max(factibles$obj)
            mejoresR <- c(mejoresR, mejorR)
          }
          
          
          ######################################################
          # version sin ruleta en supervivencia
          
          p=p0
          tam <- dim(p)[1]
          assert(tam == init)
          
          mejores <- double()
          ruleta=rep(1/tam,tam)
          clusterExport(cluster,"p")
          obj=parSapply(cluster,1:tam,function(i){return(objetivo(unlist(p[i,]), valores))})
          for (iter in 1:tmax) {
            p$obj <- NULL
            p$fact <- NULL
            
            #escalar densidad
            temp=density(obj)
            temp$x=(temp$x-min(temp$x))/(max(temp$x)-min(temp$x))*init
            temp$y=(temp$y-min(temp$y))/(max(temp$y)-min(temp$y))
            png(paste("R2_S_",iter,".png",sep=''),width = 800,height=800)
            plot(temp$x,temp$y,ylim = c(0,1),type='l',xlab="Valores objetivo",
                 ylab="Densidad",main=paste("Sin Ruleta en supervivencia\nGeneración",iter),cex.axis=1.5,
                 cex.main=1.5,cex.lab=1.5,xaxt='n')
            
            graphics.off()
            
            
            #mutacion
            clusterExport(cluster,"p")
            mutan=sample(1:tam,round(pm*tam)) #Elegir cuales van a mutar con pm
            p <- rbind(p,(t(parSapply(cluster,mutan,function(i){return(mutacion(unlist(p[i,]), n))}))))
            
            #"reproduccion"
            clusterExport(cluster,"tam")
            clusterExport(cluster,"p") #se actualiza p pero como no lo ha hecho tam la muestra solo es sobre los originales
            clusterExport(cluster,"ruleta")
            padres <- parSapply(cluster,1:rep,function(x){return(sample(1:tam, 2, prob=ruleta,replace=FALSE))}) #selección de padres
            clusterExport(cluster,"padres")
            hijos <- t(parSapply(cluster,1:rep,function(i){return(unlist(reproduccion(p[padres[1,i],], p[padres[2,i],], n)))}))
            p = rbind(p,hijos[,1:n],hijos[,(n+1):(2*n)])
            
            #actualización
            tam <- dim(p)[1]
            clusterExport(cluster,"p")
            obj=parSapply(cluster,1:tam,function(i){return(objetivo(unlist(p[i,]), valores))})
            fact=parSapply(cluster,1:tam,function(i){return(factible(unlist(p[i,]), pesos, capacidad))})
            
            
            p <- cbind(p, obj)
            p <- cbind(p, fact)
            mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
            p <- p[mantener,]
            tam <- dim(p)[1]
            assert(tam == init)
            ruleta=obj[mantener]/sum(obj[mantener])
            factibles <- p[p$fact == TRUE,]
            
            mejor <- max(factibles$obj)
            mejores <- c(mejores, mejor)
          }
          
          
          
          
          datos=rbind(datos,c(init,pm,rep,tmax,gapR=(optimo - mejorR) / optimo))
        #}
      #}
    #}
  #}
#}


stopCluster(cluster)

library(magick)
frames=lapply(1:tmax,function(iter) image_read(paste("R2_R_", iter, ".png", sep="")))
animation <- image_animate(image_join(frames))
image_write(animation, "R2_ConRuleta.gif")
sapply(1:tmax,function(iter) file.remove(paste("R2_R_", iter, ".png", sep="")))

frames=lapply(1:tmax,function(iter) image_read(paste("R2_S_", iter, ".png", sep="")))
animation <- image_animate(image_join(frames))
image_write(animation, "R2_SinRuleta.gif")
sapply(1:tmax,function(iter) file.remove(paste("R2_S_", iter, ".png", sep="")))


png("R2.png", width=1200, height=600)
plot(1:tmax, mejores, xlab="Generación", ylab="Valor Objetivo", type='l', ylim=c(min(mejores), 
                                                                                 1.01*optimo),lwd=2,cex.axis=1.5,cex.lab=1.5,
     main="Efecto de la supervivencia con ruleta",cex.main=1.5)
abline(h=optimo, col="green", lwd=3)
lines(1:tmax, mejoresR,col='red',lwd=2)
legend("bottomright",legend=c("Óptimo","Con ruleta","Sin ruleta"),col=c("green","red","black"),lwd=c(3,2,2),bty='n',cex=1.5)
graphics.off()



library(ggplot2)
png("Supervivencia.png",width = 2000,height = 900)
ggplot( datos2,aes(int, 100*tiempo,fill=metodo)) + 
  geom_boxplot() +
  #facet_grid(. ~ metodo)+
  #stat_summary(fun.y=median, geom="smooth", aes(group=1))+
  ylab('GAP (%)')+
  xlab('Tamaño de población. Probabilidad de mutación. Número de parejas de padres seleccionados. Número de generaciones ')+
  labs(title='Eficacia del método de selección')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=30),
        axis.text.x = element_text(size=20,angle = 90, hjust = 1),
        axis.text.y = element_text(size=26),
        plot.title = element_text(size=32),legend.key.size = unit(1.5, 'lines'))+
  guides(fill=guide_legend(title="Método de\n selección:"))
graphics.off()
