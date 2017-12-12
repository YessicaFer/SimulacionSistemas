require(akima)
require(rgl)
require(plot3D)
require(lattice)
require(geometry)
require(sp)
require(SDMTools)
require(plotrix)
require(deldir)
require(parallel)
require(plyr)
require(dplyr)



in.circle=function(x,y,radio){
  return(x^2+y^2<=radio^2+0.000001)
}


cuadrado=function(i){
  if(i%%(num.pts)>0 & i<total.puntos-(num.pts)){
    return(as.data.frame(c(i,i+1,i+num.pts+1,i+num.pts)))
  }else{
    return(data.frame())
  }
}




hace.cuadritos=function(i){
  
  cuadro=cuadrados[i,]
  
  nuevos.puntos=data.frame(rbind(colMeans(puntos[cuadro[1:2],],na.rm=T),
                                 colMeans(puntos[cuadro[c(4,1)],],na.rm=T), 
                                 colMeans(puntos[cuadro,],na.rm=T)) 
  )
  
  if(cuadro[2]%%num.pts == 0){ #agragar punto de la derecha
    nuevos.puntos=rbind(nuevos.puntos,colMeans(puntos[cuadro[2:3],],na.rm=T))
  }
  
  if(cuadro[3]>total.puntos-num.pts){
    nuevos.puntos=rbind(nuevos.puntos,colMeans(puntos[cuadro[3:4],],na.rm=T))
  }
  
  
  
  
  nuevos.puntos$z=nuevos.puntos$z+rnorm(dim(nuevos.puntos)[1],mean=0,sd=sigma*escala^(2*H))
  
  
  return(nuevos.puntos)
}



superficie=function(zoom=5,H=0.7,p=0,radio=0.1,sup.real="grietas/PT2.pts",return.puntos=F,interpolacion=F){
  tabla=read.table(sup.real)
  
  sigma=sd(tabla$V3)
  mu=mean(tabla$V3)
  
  #hacer malla
  x=y=seq(from=-radio,to=radio,length.out = 2^p+1)
  puntos=expand.grid(x,y)
  
  paso=2*radio/(2^p)
  names(puntos)=c('x','y')
  
  eps=0.000001
  puntos$z=0
  puntos=puntos[order(puntos$y,puntos$x,method = 'shell'),]
  total.puntos=dim(puntos)[1]

  escala=1/4^(p)
  num.pts=2^p+1
  
  #Asignar valores a z
  if(interpolacion){
    if(p<4){
      p=4
      x=y=seq(from=-radio,to=radio,length.out = 2^p+1)
      puntos=expand.grid(x,y)
      
      paso=2*radio/(2^p)
      names(puntos)=c('x','y')
      
      eps=0.000001
      puntos$z=0
      puntos=puntos[order(puntos$y,puntos$x,method = 'shell'),]
      total.puntos=dim(puntos)[1]
      
      escala=1/4^(p)
      num.pts=2^p+1
      zoom=zoom-4
    }
    muestra=sample(1:dim(tabla)[1],50000)
    t=tabla[muestra,]
    s=interp(t$V1,t$V2,t$V3,xo=x,yo=x,duplicate = "mean")
    #Asignar valores a z
    for(i in 1:dim(puntos)[1]){
      puntos$z[i]=s$z[match(puntos$x[i],s$x), match(puntos$y[i],s$y)]
    }
    #Eliminar NA's
    cuales=which(is.na(puntos$z))
    puntos$z[cuales]=rnorm(length(cuales),mean=mu,sd=sigma*escala^(2*H))
    
  }else{
  puntos$z=rnorm(dim(puntos)[1],mean=mu,sd=sigma*escala^(2*H))
  }
  
  
  #determina cuadrados iniciales
  cluster <- makeCluster(detectCores() - 1)
  clusterExport(cluster,c("eps","puntos","total.puntos","num.pts"),envir=environment())
  cuadrados=matrix(rbind.fill.matrix(parSapply(cluster,1:total.puntos,cuadrado)),ncol=4,byrow=T)
  
  res=list()
  for(it in 1:zoom){
    res[[it]]=list(puntos,cuadrados)
    #hacer cuadraditos
    
    clusterExport(cluster,c("cuadrados","puntos","escala","H","in.circle","radio","eps","sigma"),envir=environment())
    uy=parLapply(cluster,1:dim(cuadrados)[1],hace.cuadritos)
    puntos=bind_rows(puntos,uy)
    
    puntos=puntos[order(puntos$y,puntos$x,method = 'shell'),]
    
    paso=paso/2
    escala=escala/4
    total.puntos=dim(puntos)[1]
    num.pts=2^(p+it)+1
    
    clusterExport(cluster,c("eps","puntos","total.puntos","num.pts"),envir=environment())
    cuadrados=matrix(rbind.fill.matrix(parSapply(cluster,1:total.puntos,cuadrado)),ncol=4,byrow=T)
    
    
    
  }
  
  
  res[[zoom+1]]=list(puntos,cuadrados)
  stopCluster(cluster)
  if(return.puntos){
    return(res)
  }
  return(res[[zoom+1]])
}



#ss=interp(t$V1,t$V2,t$V3,duplicate = "mean")
#persp3d(ss$x,ss$y,ss$z,theta=90,col='red', texture='grietas/concrete.png',front='fill',back='lines')

#buscar cuadrados dentro sobre y fuera de la circunferencia
donde=function(i){
  cuadro=cuadrados[i,]
  cuantos=sum(puntos$dentro[cuadro])
  if(cuantos==4){ #el cuadro esta completamente contenido
    return(1)
  }else if(cuantos==0){ # el cuadro esta fuera del circulo
    return(-1)
  }else{ #el cuadro esta sobre el circulo
    return(0)
  }
  
}

