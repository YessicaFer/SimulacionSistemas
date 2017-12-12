source("dimensionFractal.R")
source("nuevo2.R")

resultados5=data.frame()
pts.5=list()
muestras=c("4340T1","4340T2","PT1","PT2","M1a","M2a","M3a","SmallMandril","MediumMandril","LargeMandril")
for(i in 1:length(muestras)){
  for(alfa in seq(0.1,0.9,0.1)){
    for(l in 0.3){
      for(rep in 1:3){
        print(muestras[i])
        print(alfa)
        res=superficie(zoom=9,H=alfa,sup.real=paste("grietas/",muestras[i],".pts",sep=''))
        puntos=res[[1]]
        circulo=sapply(1:dim(puntos)[1],function(i){return(in.circle(puntos$x[i],puntos$y[i],0.1))})
        dim1=dim.fractal(puntos,num.puntos = 6,return.puntos=T)
        dim2=dim.fractal(puntos[circulo,],num.puntos =6,return.puntos=T)
        print(dim1[[2]])
        print(dim2[[2]])
        resultados5=rbind(resultados5,c(i,alfa,dim1[[2]],dim2[[2]]))
        pts.5[[paste("cuadro-",alfa,'-',rep,sep='')]]=dim1[[1]]
        pts.5[[paste("circulo-",alfa,'-',rep,sep='')]]=dim2[[1]]
      }
    }
  }
}

names(resultados3)=c('muestra','H','box.orig','area.orig','var.orig','box.cir','area.cir','var.cir')

resultados=as.data.frame(cbind(resultados3$H,'cuadrado','Box Count',resultados3$box.orig))
resultados=rbind(resultados,cbind(resultados3$H,'cuadrado','Richardson',resultados3$area.orig))
resultados=rbind(resultados,cbind(resultados3$H,'cuadrado','Variograma',resultados3$var.orig))

resultados=rbind(resultados,cbind(resultados3$H,'circulo','Box Count',resultados3$box.cir))
resultados=rbind(resultados,cbind(resultados3$H,'circulo','Richardson',resultados3$area.cir))
resultados=rbind(resultados,cbind(resultados3$H,'circulo','Variograma',resultados3$var.cir))



names(resultados)=c('H','figura','tipo.dim','dim')
resultados$H=as.factor((resultados$H))
resultados$figura=as.factor((resultados$figura))
resultados$tipo.dim=as.factor((resultados$tipo.dim))
resultados$dim=as.numeric(levels(resultados$dim))[resultados$dim]




library(ggplot2)



#####################################################
############  cuadrado vs circulo  ##################
png('cuadradoCirculo.png',width=2000,height=1000)
dodge <- position_dodge(width = 0.7)
ggplot(data=resultados,aes(y=dim,x=figura,fill=tipo.dim))+
  geom_violin(position = dodge)+ 
  geom_boxplot(width=0.05,position = dodge)+
  ggtitle("Efecto de considerar una parte de la superficie")+
  xlab('')+
  ylab('Dimensión fractal')+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept = 2) +
  geom_hline(yintercept = 3)+
  scale_fill_discrete(name='Métodos')+
  theme(text = element_text(size=34),
        axis.text.x = element_text(size=34),
        axis.text.y = element_text(size=34),
        plot.title = element_text(size=36))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_text(size=36) ,
        legend.text=element_text(size=34),
        legend.key = element_rect(size = 3.5),
        legend.key.size = unit(2.5, 'lines'))

graphics.off()

kruskal.test(data=resultados[which(resultados$tipo.dim=='Box Count'),],dim~figura)
kruskal.test(data=resultados[which(resultados$tipo.dim=='Richardson'),],dim~figura)
kruskal.test(data=resultados[which(resultados$tipo.dim=='Variograma'),],dim~figura)




#####################################################
############         metodos       ##################
png('metodos.png',width=2000,height=1000)
ggplot(data=resultados,aes(y=dim,x=tipo.dim,fill=tipo.dim))+
  geom_violin()+ 
  geom_boxplot(width=0.05)+
  ggtitle("Comparación de métodos")+
  xlab('')+
  ylab('Dimensión fractal')+
  geom_hline(yintercept = 2) +
  geom_hline(yintercept = 3)+
  scale_fill_discrete(name='Métodos')+
  theme(text = element_text(size=34),
        axis.text.x = element_text(size=34),
        axis.text.y = element_text(size=34),
        plot.title = element_text(size=36))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_text(size=36) ,
        legend.text=element_text(size=34),
        legend.key = element_rect(size = 3.5),
        legend.key.size = unit(2.5, 'lines'))

graphics.off()

kruskal.test(data=resultados,dim~tipo.dim)


#####################################################
############             H         ##################
png('H.png',width=2000,height=1000)
ggplot(data=resultados,aes(y=dim,x=H,fill=tipo.dim))+
  geom_boxplot()+
  ggtitle("Variando H")+
  xlab('')+
  ylab('Dimensión fractal')+
  theme(plot.title = element_text(hjust = 0.5))+ 
  geom_hline(yintercept = 2) +
  geom_hline(yintercept = 3)+
  scale_fill_discrete(name='Métodos')+
  theme(text = element_text(size=34),
        axis.text.x = element_text(size=34),
        axis.text.y = element_text(size=34),
        plot.title = element_text(size=36))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_text(size=36) ,
        legend.text=element_text(size=34),
        legend.key = element_rect(size = 3.5),
        legend.key.size = unit(2.5, 'lines'))

graphics.off()
kruskal.test(data=resultados[which(resultados$tipo.dim=='Box Count'),],dim~H)
kruskal.test(data=resultados[which(resultados$tipo.dim=='Richardson'),],dim~H)
kruskal.test(data=resultados[which(resultados$tipo.dim=='Variograma'),],dim~H)



r1<-with(data=resultados[which(resultados$tipo.dim=='Box Count'),], tapply(dim, H, median))
r2<-with(data=resultados[which(resultados$tipo.dim=='Richardson'),], tapply(dim, H, median))
r3<-with(data=resultados[which(resultados$tipo.dim=='Variograma'),], tapply(dim, H, median))

resultados$dim[which(resultados$tipo.dim=='Box Count')]=2.9-(0.8/(r1[length(r1)]-r1[1]))*(resultados$dim[which(resultados$tipo.dim=='Box Count')]-r1[1])
resultados$dim[which(resultados$tipo.dim=='Richardson')]=2.9-(0.8/(r2[length(r2)]-r2[1]))*(resultados$dim[which(resultados$tipo.dim=='Richardson')]-r2[1])
resultados$dim[which(resultados$tipo.dim=='Variograma')]=2.9-(0.8/(r3[length(r3)]-r3[1]))*(resultados$dim[which(resultados$tipo.dim=='Variograma')]-r3[1])


#Revisemos si todos los niveles son significativos
require(FSA)
PT = dunnTest(dim~H,data=resultados[which(resultados$tipo.dim=='Variograma'),],method="bh") 
PT = PT$res
#Resultado de prueba Dunn, comparacion entre cada par de dimensiones
print(PT)

#Pares no significtaivos al 99.9% de confianza
print(PT[PT$P.adj>=0.01,]) #estos son estadisticamente equivalentes


###############################################################################################
###############################################################################################
###############################################################################################


resultados.muestras=data.frame()
pts.muestras=list()
muestras=c("4340T1","4340T2","PT1","PT2","M1a","M2a","M3a","SmallMandril","MediumMandril","LargeMandril")
for(i in 1:length(muestras)){
  print(muestras[i])
  tabla=read.table(paste("grietas/",muestras[i],".pts",sep=''))
  names(tabla)=c('x','y','z')
  dim=dim.fractal(tabla,num.puntos = 5,return.puntos=T)
  print(dim[[2]])
  resultados.muestras=rbind(resultados.muestras,c(i,dim[[2]]))
  pts.muestras[[muestras[i]]]=dim[[1]]
}




