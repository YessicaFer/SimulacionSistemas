library(emoa)
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

mutacion <- function(solu) {
  cuantos=sample(1:vc,1)#cuantos 
  cuales=sample(1:vc,cuantos) #cuales
  
  for(i in cuales){
    solu[i]=solu[i]+runif(1,-0.1,0.1) #como
  }
  
  return(solu)
}

#Reproduccion BLX-alfa
#Eshelman, L. J. (1993). Real coded genetic algorithms and interval-schemata. Foundations of genetic algorithms, 2, 187-202.
reproduccion <- function(x, y,num_hijos=2,alfa=0.05) {
  hijos=c()
  for(i in 1:vc){
    lw=min(x[i],y[i])-alfa*abs(x[i]-y[i])
    up=max(x[i],y[i])+alfa*abs(x[i]-y[i])
    hijos=c(hijos,runif(num_hijos,lw,up))
  }
  return(matrix(hijos,ncol=vc))
}

factible <- function(s){
  suma=0
  for(i in 1:vc){ #medir la distancia que esta dentro del cuadrado unitario en cada variable
    suma=suma+(1*!(0<=s[i] & s[i]<=1))*min(abs(0-s[i]),abs(1-s[i]))
  }
  return(suma)
}



cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster,c("poli","pick.one","eval","domin.by","dist","mutacion","reproduccion","factible"))

vc <- 4
md <- 3
tc <- 5

clusterExport(cluster,c("vc","md","tc"))





#for(init in c(50,100,200)){
 # print(init)
 # for(replica in 1:20){
    ##################################################################################
    ###                         creacion de instancia                               ##
    ##################################################################################
    k <- 2 # cuantas funciones objetivo
    n <- 300 # cuantas soluciones aleatorias
    
    #Creacion de funciones objetivo
    obj <- parLapply(cluster,1:k,function(k){return (poli( md, vc, tc))})
    minim <- (runif(k) > 0.5)
    sign <- (1 + -2 * minim)
    
    
    pm <- 0.05
    rep <- 50
    tmax <- 200
    e=0.05
    #------------------------------------------#
    #             población inicial            #
    #------------------------------------------#
    sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
    
    #evaluación de soluciones
    clusterExport(cluster,c("k","n","obj","sol","sign"))
    val=matrix(parSapply(cluster,1:(k*n),function(pos){
      i <- floor((pos - 1) / k) + 1
      j <- ((pos - 1) %% k) + 1
      return(eval(obj[[j]], sol[i,], tc))
    }), nrow=n, ncol=k, byrow=TRUE)
    tam=n
    factibilidad=rep(0,n) #todos son factibles
    
    
    #------------------------------------------#
    #             calcular aptitud             #
    #------------------------------------------#
    clusterExport(cluster,c("val","tam"))
    aptitud=parSapply(cluster,1:tam,function(i){
      d <- logical()
      for (j in 1:n) {
        d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
      }
      cuantos <- sum(d)
      return(cuantos)
    })
    
    no.dom = (aptitud==0)
    frente <- subset(val, no.dom) # solamente las no dominadas
    
    #------------------------------------------#
    #                grafica                   #
    #------------------------------------------#
    xmin=min(val[,1])
    xmax=max(val[,1])
    ymin=min(val[,2])
    ymax=max(val[,2])
    x.dis=xmax-xmin
    y.dis=ymax-ymin
    nadir=c(c(xmin,xmax)[1*minim[1]+1],c(ymin,ymax)[1*minim[2]+1])
    ideal=c(c(xmax,xmin)[1*minim[1]+1],c(ymax,ymin)[1*minim[2]+1])
    
    png("evolucion0.png")
    plot(val[,1], val[,2],xaxt='n',yaxt='n',xlab='',ylab='',main='Población inicial',cex.main=2,xlim=c(xmin-x.dis/5,xmax+x.dis/5),ylim=c(ymin-y.dis/5,ymax+y.dis/5))
    points(frente[,1], frente[,2], col="green", pch=16)
    graphics.off()
    
    
    #------------------------------------------#
    #             empieza evolución            #
    #------------------------------------------#
    hyper=c()
    for (iter in 1:tmax) {
      
      #------------------------------------------#
      #                 mutacion                 #
      #------------------------------------------#
      mutan=sample(1:tam,round(pm*tam)) #Elegir cuales van a mutar con pm
      mutados=t(parSapply(cluster,mutan,function(i){return(mutacion(sol[i,]))}))
      sol <- rbind(sol,mutados)
      
      #------------------------------------------#
      #               selección                  #
      #------------------------------------------#
      ruleta=(aptitud+factibilidad)/sum(aptitud+factibilidad)
      clusterExport(cluster,c("tam","sol","ruleta"))#se actualiza sol pero como no lo ha hecho tam la muestra solo es sobre los originales
      padres <- parSapply(cluster,1:rep,function(x){return(sample(1:tam, 2, replace=FALSE,prob=ruleta))}) #selección de padres
      
      #------------------------------------------#
      #              reproduccion                #
      #------------------------------------------#
      clusterExport(cluster,"padres")
      hijos <- parSapply(cluster,1:rep,function(i){return(reproduccion(sol[padres[1,i],], sol[padres[2,i],]))})
      sol = rbind(sol,t(matrix(hijos,nrow=vc)))
      
      #------------------------------------------#
      #            evaluar objetivos             #
      #------------------------------------------#
      tam <- dim(sol)[1]
      clusterExport(cluster,c("sol","tam"))
      val=matrix(parSapply(cluster,1:(k*tam),function(pos){
        i <- floor((pos - 1) / k) + 1
        j <- ((pos - 1) %% k) + 1
        return(eval(obj[[j]], sol[i,], tc))
      }), nrow=tam, ncol=k, byrow=TRUE)
      
      #------------------------------------------#
      #               factibilidad               #
      #------------------------------------------#
      factibilidad=parSapply(cluster,1:tam,function(i){return(factible(sol[i,]))})
      
      #------------------------------------------#
      #             calcular aptitud             #
      #------------------------------------------#
      clusterExport(cluster,c("val","tam"))
      aptitud=parSapply(cluster,1:tam,function(i){
        d <- logical()
        for (j in 1:n) {
          d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
        }
        cuantos <- sum(d)
        return(cuantos)
      })
      
     
      
      
      #------------------------------------------#
      #              supervivencia               #
      #------------------------------------------#
      elite <- order(factibilidad, aptitud)
      mantener=elite[1:n]
      #agregar e mejores
      #mantener=elite[1:floor(e*n)]
      #restantes=setdiff(1:tam,mantener)
      #max.apt=max(aptitud[restantes]+factibilidad[restantes])
      #ruleta=(max.apt-aptitud[restantes]-factibilidad[restantes])/sum(max.apt-aptitud[restantes]-factibilidad[restantes])
      #ruleta=(1-ruleta)/length(ruleta)
      #mantener <- c(mantener,sample(restantes,n-length(mantener),replace =TRUE,prob=ruleta))
      
      sol=sol[mantener,]
      val=val[mantener,]
      aptitud=aptitud[mantener]
      factibilidad=factibilidad[mantener]
      tam=length(mantener)
      
      #------------------------------------------#
      #                grafica                   #
      #------------------------------------------#
      no.dom = ((aptitud+factibilidad)==0)
      frente <- subset(val, no.dom) # solamente las no dominadas
      png(paste("evolucion",iter,".png",sep=''))
      plot(val[,1], val[,2],xaxt='n',yaxt='n',xlab='',ylab='',main=paste("Generación",iter),cex.main=2,xlim=c(xmin-x.dis/5,xmax+x.dis/5),ylim=c(ymin-y.dis/5,ymax+y.dis/5))
      points(frente[,1], frente[,2], col="green", pch=16)
      points(val[which(factibilidad>0),1], val[which(factibilidad>0),2], col="red", pch=18)
      graphics.off()
      
      
    }    
      
stopCluster(cluster)

library(magick)
frames=lapply(0:tmax,function(iter) image_read(paste("evolucion", iter, ".png", sep="")))
animation <- image_animate(image_join(frames))
image_write(animation, "evolucion.gif")
sapply(0:tmax,function(iter) file.remove(paste("evolucion", iter, ".png", sep="")))
