

MC.pi=function(){
  #runif samples from a uniform distribution
  xs <- runif(runs,min=-0.5,max=0.5)
  ys <- runif(runs,min=-0.5,max=0.5)
  in.circle <- xs^2 + ys^2 <= 0.5^2
  mc.pi <- (sum(in.circle)/runs)*4
  return(mc.pi)
}
  
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
datos=data.frame()
for(runs in seq(100000,1000000,100000)){
  for(i in 1:30){
    start.time <- Sys.time()
      montecarlo <- foreach(i = 1:500, .combine=c) %dopar% MC.pi()
      mc.pi=sum(montecarlo)/500
      gap=abs(pi-mc.pi)/((pi+mc.pi)/2)*100
    end.time <- Sys.time()
    time.taken <- (end.time - start.time)/cuantos
    
    datos=rbind(datos,c(runs,gap,time.taken))
  }
}

names(datos)=c("Corridas","GAP","Tiempo")
datos$Corridas=as.factor(datos$Corridas)

boxplot(data=datos,GAP~Corridas,xlab="Numero de corridas",ylab="GAP (%)",main="GAP pi vs aproximación de pi")
boxplot(data=datos,Tiempo~Corridas,xlab="Numero de corridas",ylab="Tiempo (s)",main="Tiempo para calcular aproximación de pi")

library(ggplot2)
png("R1_Violines_GAP.png",width=1200,height=1000)
dodge <- position_dodge(width = 1)
ggplot(data=datos,aes(x=Corridas,y=GAP))+
  geom_violin(position = dodge)+
  geom_boxplot(position=dodge,width=0.1)+
  stat_summary(fun.y=median, geom="smooth", aes(group=1))+
  xlab("Numero de puntos")+
  ylab("GAP (%)")+
  labs(title="GAP pi vs aproximación")+
  theme(text = element_text(size=36),
        axis.text.x = element_text(size=36,angle = 90, hjust = 1),
        axis.text.y = element_text(size=36),
        plot.title = element_text(size=36))+
  theme(plot.title = element_text(hjust = 0.5))
graphics.off()

png("R1_Violines_Tiempo.png",width=1200,height=1000)  #recuerda acomodar ylim
ggplot(data=datos,aes(x=Corridas,y=Tiempo))+
  geom_violin(position = dodge)+
  geom_boxplot(position=dodge,width=0.1)+
  stat_summary(fun.y=median, geom="smooth", aes(group=1))+
  xlab("Numero de puntos")+
  ylab("Tiempo de ejecución (s)")+
  labs(title="Tiempo para calcular aproximación")+theme(text = element_text(size=36),
                                                        axis.text.x = element_text(size=36,angle = 90, hjust = 1),
                                                        axis.text.y = element_text(size=36),
                                                        plot.title = element_text(size=36))+
  theme(plot.title = element_text(hjust = 0.5))
graphics.off()



Pareto=aggregate(datos$GAP~datos$Corridas,FUN=median)

Pareto$Tiempo=aggregate(datos$Tiempo~datos$Corridas,FUN=median)$'datos$Tiempo'
names(Pareto)=c("Corridas","GAP","Tiempo")


png("Pareto.png",width = 1000,height = 1000)
par(mar=c(5,5,4,2))
plot(Pareto$GAP,Pareto$Tiempo,xlab="GAP (%)",ylab="Tiempo de ejecución (s)",
     ylim=c(min(Pareto$Tiempo),max(Pareto$Tiempo)+0.00001),cex.axis=2.5,cex.lab=2.5,
     cex=3)
text(Pareto$GAP,Pareto$Tiempo+0.000001, labels=Pareto$Corridas, cex= 2.5, pos=3)
graphics.off()
