

primo <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

desde <- 100
hasta <-  10000
original <- desde:hasta
invertido <- hasta:desde
for(i in hasta:desde){
  if(primo(i)){
    maxprimo=i
    break
  }
}
peor=rep(maxprimo,length(desde:hasta)) 
mejor=rep(as.integer(4),length(desde:hasta)) 

replicas <- 30
suppressMessages(library(parallel))
datos=data.frame(matrix(vector(), 0, 3 ))
num_nucleos=detectCores()
for(nucleos in 1:(num_nucleos-1)){
  cluster=makeCluster(nucleos)
  registerDoParallel(cluster,cores=nucleos)
  ot <-  numeric()
  it <-  numeric()
  at <-  numeric()
  pt <-  numeric()
  mt <-  numeric()
  for (r in 1:replicas) {
    pt <- c(pt, system.time(foreach(n = peor, .combine=c) %dopar% primo(n))[3]) # copias del maximo primo menor que 'hasta'
    mt <- c(mt, system.time(foreach(n = mejor, .combine=c) %dopar% primo(n))[3]) #solo numeros pares (es indistinto cual sea de 4 en delante)
    ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
    at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
    
    datos=rbind(datos,cbind(nucleos,"Ascendente",as.numeric(ot)),cbind(nucleos,"Descendiente",as.numeric(it)),
                cbind(nucleos,"Aleatorio",as.numeric(at)),cbind(nucleos,"PeorCaso",as.numeric(pt)),
                cbind(nucleos,"MejorCaso",as.numeric(mt)))
  }
  stopImplicitCluster()
  stopCluster(cluster)
}
names(datos)=c("Nucleos", "Orden", "Tiempo")
datos$Nucleos=factor(datos$Nucleos)
datos$Orden=factor(datos$Orden)
datos$Tiempo=as.numeric(paste(datos$Tiempo))



#https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
require(ggplot2)


medianas= aggregate(datos$Tiempo~datos$Nucleos*datos$Orden,FUN=median)
names(medianas)=c("Nucleos", "Orden", "Tiempo")
medianas$Nucleos=factor(medianas$Nucleos)
medianas$Orden=factor(medianas$Orden)


#boxplots
graficas = lapply(sort(unique(datos$Nucleos)), function(i) {
ggplot(data = datos[datos$Nucleos==i,], aes( x=Nucleos,y=Tiempo)) + geom_boxplot(aes(color=Orden),show.legend = F)+
  facet_wrap( ~ Nucleos, scales="free",strip.position = "bottom")+
    theme(strip.background = element_blank(), strip.text = element_blank())+
    scale_color_manual(labels=c("Ascendiente", "Descendiente", "Aleatorio","Caso Nadir","Caso Ideal"),
                       values=c("red","yellow","green","blue","purple"))+
    labs(x=NULL,y=NULL)+
    theme(axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14))
  
})

#lineas
p=ggplot(data = medianas, aes(x=Nucleos, y=Tiempo,color=Orden,group=Orden))+
  scale_color_manual(labels=c("Ascendiente", "Descendiente", "Aleatorio","Caso Nadir","Caso Ideal"),
                     values=c("red","yellow","green","blue","purple"))+
  geom_point()+
  geom_line()+
  labs(x=NULL,y=NULL)+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))
graficas[[8]]=p+theme(legend.position="none")

#para unir todas las graficas                
library(cowplot) 
prow=plot_grid(graficas[[1]],graficas[[2]],graficas[[3]],graficas[[4]],
               graficas[[5]],graficas[[6]],graficas[[7]],graficas[[8]],
               ncol=2,labels = "auto",label_x = 0.2,label_y=1,label_size = 20)
#para extraer un legend chido
temp=ggplot(data = datos[datos$Nucleos==1,], aes( x=Nucleos,y=Tiempo)) + geom_boxplot(aes(color=Orden))+
  facet_wrap( ~ Nucleos, scales="free",strip.position = "bottom")+
  theme(strip.background = element_blank(), strip.text = element_blank())+
  scale_color_manual(labels=c("Ascendiente", "Descendiente", "Aleatorio","Caso Nadir","Caso Ideal"),
                     values=c("red","yellow","green","blue","purple"))+
  labs(x=NULL,y=NULL)+
  theme(legend.text=element_text(size=18),legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines'),legend.title=element_text(size=20))
legend_b <- get_legend(temp + theme(legend.position="right"))

#ahora si... a dibujar todod junto con legend
title <- ggdraw() + draw_label("Calendarización de tareas", fontface='bold',size=20)
prow=plot_grid(prow, legend_b, nrow = 1,rel_widths =  c(1, .17))
prow=plot_grid(title,prow,ggdraw()+draw_label("Número de núcleos utilizados",size=20),ncol=1,rel_heights = c(0.1,1,0.05))
png("T1_boxplotTiempoVSNucleos.png",width=1000,height=1500)
plot_grid(ggdraw()+draw_label("Tiempo de ejecución (s)",angle=90,size=20),prow,rel_widths = c(0.05,1))
graphics.off()

#prueba de normalidad
muestra=datos[sample(nrow(datos),5000),]
shapiro.test(resid(lm(muestra$Tiempo~muestra$Nucleos+muestra$Orden+muestra$Nucleos*muestra$Orden)))
qqnorm(resid(lm(muestra$Tiempo~muestra$Nucleos+muestra$Orden+muestra$Nucleos*muestra$Orden)))
qqline(resid(lm(muestra$Tiempo~muestra$Nucleos+muestra$Orden+muestra$Nucleos*muestra$Orden)),col="red")

#probar si los factores Nucleos-Orden tienen efecto sobre el Tiempo
kruskal.test(muestra$Tiempo~interaction(muestra$Nucleos,muestra$Orden))

#probar la significancia de cada factor
kruskal.test(muestra$Tiempo~muestra$Nucleos)
kruskal.test(muestra$Tiempo~muestra$Orden)

#Resultado de prueba Dunn, comparacion entre cada par de Nucleos
library(FSA)
PM = dunnTest(datos$Tiempo~datos$Nucleos,data=datos,
              method="bh") $res
print(PM)
#Pares no significtaivos al 99.9% de confianza
print(PM[PM$P.adj>=0.001,]) #estos son estadisticamente equivalentes

#Resultado de prueba Dunn, comparacion entre cada par de Ordenes
PO = dunnTest(datos$Tiempo~datos$Orden,data=datos,
              method="bh") $res
print(PO)
#Pares no significtaivos al 99.9% de confianza
print(PO[PO$P.adj>=0.001,]) #estos son estadisticamente equivalentes

#caso 7 nucleos
caso=datos[datos$Nucleos==7,]
kruskal.test(caso$Tiempo~caso$Orden)
PC=dunnTest(caso$Tiempo~caso$Orden,data=caso, method="bh") $res
print(PC[PC$P.adj>=0.001,]) #estos son estadisticamente equivalentes

#grafica comparación a pares
otro=rbind(PM[,c(1,4)],PO[,c(1,4)])
png("R2_pairwise.png")
ggplot(otro,aes(x=Comparison,y=P.adj,group=cut(`P.adj`, c(-0.01,0.001, 1), 
                          label=c("p<0.001","No significativo")))) +
  #geom_hline(yintercept=0, lty="11", colour="grey30") +
  geom_dotplot(aes(P.adj)) +
  labs(colour="")+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) 
graphics.off() 


stripchart(otro$Comparison,vertical=T)

m=cut(as.vector(PM$P.adj), c(-0.01, 0.001,1), 
    label=c("p<0.001","No significativo"))
m


