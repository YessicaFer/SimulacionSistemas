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
for(init in c(50,100,200)){
  print(init)
  for(replica in 1:20){
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
      
    #init <- 200
    
    
    
    
    startP=Sys.time()
    print("paralelo")
    #########################################################################################
    #version paralela
    p <- as.data.frame(t(parSapply(cluster,1:init,function(i){return(round(runif(n)))})))
    tam <- dim(p)[1]
    assert(tam == init)
    pm <- 0.05
    rep <- 50
    tmax <- 50
    mejores <- double()
    for (iter in 1:tmax) {
        p$obj <- NULL
        p$fact <- NULL
        
        #mutacion
        clusterExport(cluster,"p")
        mutan=sample(1:tam,round(pm*tam)) #Elegir cuales van a mutar con pm
        p <- rbind(p,(t(parSapply(cluster,mutan,function(i){return(mutacion(unlist(p[i,]), n))}))))
        
        #"reproduccion"
        clusterExport(cluster,"tam")
        clusterExport(cluster,"p") #se actualiza p pero como no lo ha hecho tam la muestra solo es sobre los originales
        padres <- parSapply(cluster,1:rep,function(x){return(sample(1:tam, 2, replace=FALSE))}) #selección de padres
        clusterExport(cluster,"padres")
        hijos <- parSapply(cluster,1:rep,function(i){return(as.matrix(unlist(reproduccion(p[padres[1,i],], p[padres[2,i],], n)),ncol=n))})
        p = rbind(p,hijos)
        
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
        factibles <- p[p$fact == TRUE,]
        mejor <- max(factibles$obj)
        mejores <- c(mejores, mejor)
    }
    mejorP=mejor
    endP=Sys.time()
    startS=Sys.time()
    print("secuencial")
    ###############################################################################################
    #version secuencial
    p <- poblacion.inicial(n, init)
    tam <- dim(p)[1]
    assert(tam == init)
    pm <- 0.05
    rep <- 50
    tmax <- 50
    mejores <- double()
    for (iter in 1:tmax) {
      p$obj <- NULL
      p$fact <- NULL
      for (i in 1:tam) { # cada objeto puede mutarse con probabilidad pm
        if (runif(1) < pm) {
          p <- rbind(p, mutacion(p[i,], n))
        }
      }
      for (i in 1:rep) { # una cantidad fija de reproducciones
        padres <- sample(1:tam, 2, replace=FALSE)
        hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
        p <- rbind(p, hijos[1:n]) # primer hijo
        p <- rbind(p, hijos[(n+1):(2*n)]) # segundo hijo
      }
      tam <- dim(p)[1]
      obj <- double()
      fact <- integer()
      for (i in 1:tam) {
        obj <- c(obj, objetivo(p[i,], valores))
        fact <- c(fact, factible(p[i,], pesos, capacidad))
      }
      p <- cbind(p, obj)
      p <- cbind(p, fact)
      mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
      p <- p[mantener,]
      tam <- dim(p)[1]
      assert(tam == init)
      factibles <- p[p$fact == TRUE,]
      mejor <- max(factibles$obj)
      mejores <- c(mejores, mejor)
    }
    endS=Sys.time()
    mejorS=mejor
    datos=rbind(datos,c(init=init,secuencial=difftime(endS,startS,units='secs'),paralelo=difftime(endP,startP,units='secs'),gapP=(optimo - mejorP) / optimo,gapS=(optimo - mejorS) / optimo))
  }
  
}
  stopCluster(cluster)
  
  
  names(datos)=c("init","secuencial","paralelo","gapP","gapS")
datos1=datos[,1:2]
names(datos1)=c("init","tiempo")
datos1$metodo="secuencial"
datos2=datos[,c(1,3)]
names(datos2)=c("init","tiempo")
datos2$metodo="paralelo"
datos=rbind(datos1,datos2)

library(ggplot2)
png("secuencialParalelo.png",width = 1200,height = 800)
ggplot(datos, aes(factor(init), tiempo)) + 
  geom_boxplot() + facet_grid(. ~ metodo)+
  stat_summary(fun.y=median, geom="smooth", aes(group=1))+
  ylab('Tiempo de ejecución (s)')+
  xlab('Tamaño de la población')+
  labs(title='Comparación secuencial vs paralelo')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=30),
        axis.text.x = element_text(size=26,angle = 90, hjust = 1),
        axis.text.y = element_text(size=26),
        plot.title = element_text(size=32))
graphics.off()

