


names(datos)=c("init","pm","rep","tmax","gapR","gap")
datos$init=as.factor(datos$init)
datos$pm=as.factor(datos$pm)
datos$rep=as.factor(datos$rep)
datos$tmax=as.factor(datos$tmax)
datos$gapR=as.numeric(datos$gapR)

kruskal.test(datos$gapR~datos$init)
kruskal.test(datos$gapR~datos$pm)
kruskal.test(datos$gapR~datos$rep)
kruskal.test(datos$gapR~datos$tmax)

kruskal.test(datos$gap~datos$init)
kruskal.test(datos$gap~datos$pm)
kruskal.test(datos$gap~datos$rep)
kruskal.test(datos$gap~datos$tmax)

library(FSA)
PT = dunnTest(datos$gapR~datos$init,data=datos,method="bh") 
PT = PT$res
#Resultado de prueba Dunn, comparacion entre cada par de dimensiones
print(PT)
#Pares no significtaivos al 99.9% de confianza
print(PT[PT$P.adj>=0.001,]) #estos son estadisticamente equivalentes

PT = dunnTest(datos$gapR~datos$rep,data=datos,method="bh") 
PT = PT$res
#Resultado de prueba Dunn, comparacion entre cada par de dimensiones
print(PT)
#Pares no significtaivos al 99.9% de confianza
print(PT[PT$P.adj>=0.001,]) #estos son estadisticamente equivalentes


PT = dunnTest(datos$gap~datos$init,data=datos,method="bh") 
PT = PT$res
#Resultado de prueba Dunn, comparacion entre cada par de dimensiones
print(PT)
#Pares no significtaivos al 99.9% de confianza
print(PT[PT$P.adj>=0.001,]) #estos son estadisticamente equivalentes

PT = dunnTest(datos$gap~datos$rep,data=datos,method="bh") 
PT = PT$res
#Resultado de prueba Dunn, comparacion entre cada par de dimensiones
print(PT)
#Pares no significtaivos al 99.9% de confianza
print(PT[PT$P.adj>=0.001,]) #estos son estadisticamente equivalentes

aggregate(gapR~init,datos,median)
aggregate(gapR~rep,datos,median)
aggregate(gapR~pm,datos,median)
aggregate(gapR~tmax,datos,median)

aggregate(gap~init,datos,median)
aggregate(gap~rep,datos,median)
aggregate(gap~pm,datos,median)
aggregate(gap~tmax,datos,median)

datos1=data.frame(interaction(datos$init,datos$pm,datos$rep,datos$tmax),datos$gapR)
names(datos1)=c("int","tiempo")
datos1$metodo="Con ruleta"
datos2=data.frame(interaction(datos$init,datos$pm,datos$rep,datos$tmax),datos$gap)
names(datos2)=c("int","tiempo")
datos2$metodo="Sin ruleta"
datos=rbind(datos1,datos2)


library(ggplot2)
png("BoxplotSupervivencia.png",width = 2000,height = 900)
ggplot(datos, aes(int, 100*tiempo,fill=metodo)) + 
  geom_boxplot() +
  #facet_grid(. ~ metodo)+
  #stat_summary(fun.y=median, geom="smooth", aes(group=1))+
  ylab('GAP (%)')+
  xlab('Tamaño de población. Probabilidad de mutación. Número de parejas de padres seleccionados. Número de generaciones ')+
  labs(title='Eficacia del método de supervivencia')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=30),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=26),
        plot.title = element_text(size=32),legend.key.size = unit(1.5, 'lines'))+
        scale_fill_discrete("Método de\n supervivencia:",labels=c("Sin ruleta con selección aleatoria","Sin ruleta con selección por ruleta","Con ruleta"))
graphics.off()





wilcox.test(datos$tiempo[datos$metodo=="Con ruleta"],datos$tiempo[datos$metodo=="Sin ruleta"])
wilcox.test(datos$tiempo[datos$metodo=="Con ruleta"],datos$tiempo[datos$metodo=="Supervivencia"])
wilcox.test(datos$tiempo[datos$metodo=="Sin ruleta"],datos$tiempo[datos$metodo=="Supervivencia"])
