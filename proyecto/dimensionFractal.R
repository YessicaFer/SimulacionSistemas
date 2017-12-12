require(parallel)
require(data.table)

cuantos.cubos=function(tab,grid.x,grid.y,grid.z){
  paso.x=grid.x[2]-grid.x[1]
  paso.y=grid.y[2]-grid.y[1]
  paso.z=grid.z[2]-grid.z[1]
  eps=0.000001
  
  
  cluster <- makeCluster(detectCores() - 1)
  clusterExport(cluster,c("tab","grid.y","grid.z","eps","paso.x","paso.y","paso.z"),envir=environment())
  dim=parSapply(cluster,grid.x,function(xx){
    cuantos=0
    area=0
    var=0
    orden.x=subset(tab,x+eps>=xx & x< xx+paso.x)
    for(yy in grid.y){
      orden.y=subset(orden.x,y+eps>=yy & y<yy+paso.y)
      if(dim(orden.y)[1]>0){
        for(zz in grid.z){
          orden.z=subset(orden.y,z+eps>=zz & z<zz+paso.z)
          if(dim(orden.z)[1]>0){
            lin=lm(data=orden.z,z~x+y)
            coeff=lin$coefficients
            n.area=sqrt((1+coeff[2]^2)*(1+coeff[3]^2))*paso.x*paso.y
            if(is.na(n.area))n.area=0
            area=area+n.area
            cuantos=cuantos+1
            if(dim(orden.z)[1]>3){
            n.var=sqrt(1/(dim(orden.z)[1]-3)*sum(orden.z$residuals^2))
            if(is.na(n.var))n.var=0
            }else{
              n.var=0
            }
            
            var=var+n.var
            
          }
        }
      }
    }
    return(c(cuantos,area,var))
  })
  stopCluster(cluster)
  res=c(box.count=sum(dim[1,]),area=sum(dim[2,]),var=mean(dim[3,]))
  return(res)
}




dim.fractal=function(tab,num.puntos=4,return.puntos=F,plot.log=T){
  particiones=10
  puntos=data.frame()
  tab=as.data.table(tab)
  names(tab)=c('x','y','z')
  lin=lm(data=tab,z~x+y)
  tab$residuals=lin$residuals
  
  
  
  for(i in 1:num.puntos){
    #hacer malla
    grid.x=seq(min(tab$x),max(tab$x),length.out = (2^(i-1))*particiones)
    grid.y=seq(min(tab$y),max(tab$y),length.out = (2^(i-1))*particiones)
    grid.z=seq(min(tab$z),max(tab$z),length.out = (2^(i-1))*particiones)
    
    puntos=rbind(puntos,c((2^(i-1)*particiones),1/(2^(i-1)*particiones),cuantos.cubos(tab,grid.x,grid.y,grid.z)))
  }
  
  
  names(puntos)=c('particiones','escala','box.count','area','var')
  #calcular dimensión
  puntos.log=log2(puntos)
  
  
  reg1=lm(box.count~particiones,puntos.log)
  reg2=lm(area~escala,puntos.log)
  reg3=lm(var~particiones,puntos.log)
  
  
  if(plot.log){
    #Kolmogorov
  plot(puntos.log[,c(1,3)],main='Conteo de cajas',xlab='log particiones',ylab='log número de cajas',pch=19)
  abline(reg1$coefficients)
  legend("bottomright",paste("pendiente:",format(round(reg1$coefficients[[2]], 2), nsmall = 2)))
  
  #Richardson
  plot(puntos.log[,c(2,4)],main='Richardson',xlab='log escala',ylab='log area',pch=19)
  abline(reg2$coefficients)
  legend("bottomleft",paste("pendiente:",format(round(reg2$coefficients[[2]], 2), nsmall = 2)))
  
  plot(puntos.log[,c(1,5)],main='Variograma',xlab='log particiones',ylab='log variación',pch=19)
  abline(reg3$coefficients)
  legend("bottomright",paste("pendiente:",format(round(reg3$coefficients[[2]], 2), nsmall = 2)))
  }
  
  
  res=c(box.count=reg1$coefficients[2],area=2+reg2$coefficients[2],variograma=3-reg3$coefficients[2]/2)
  
  if(!return.puntos){
    return(res)
  }else{
    return(list(puntos,res))
  }
  
}





