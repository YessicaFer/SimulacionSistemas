# Práctica 9: Interacciones entre partículas
## 2 de octubre de 2017

## Introducción
<p align="justified">
Está práctica trata de la simulación del movimiento de las partículas en dependencia de sus cargas eléctricas y de su posición. Se tienen <img src="http://latex.codecogs.com/svg.latex?n" border="0"/> partículas distribuidas en un cuadrado unitario. Las cargas eléctricas de las partículas son enteros y se distribuyen normalmente entre <img src="http://latex.codecogs.com/svg.latex?[-5,5]" border="0"/>. Si las cargas son de la misma polaridad (signo) se repelen mientras que polos opuestos se atraen. 
  
  La fuerza de atracción o repulsión entre dos partículas es proporcional a la diferencia absoluta de sus cargas e inversamente proporcional a la distancia entre ellas. La  dirección de movimiento de una partícula queda determinada por la interacción de las fuerzas de atracción y repulsión con respecto a todas las demás. 
  
 ## La masa afecta
  Sin embargo; la velocidad de movimiento de una partícula depende también de su masa. Para simularlo, se considera que las partículas tienen una masa distribuida uniformemente en el intervalo <img src="http://latex.codecogs.com/svg.latex?(0,0.1]" border="0"/>. Obviamente no puede existir una particula de masa cero y la razón de la cota superior es para tener una buena visualización dibujando cada partícula como un circulo de radio proporcional a su masa.
  
  Se considera que la velocidad  de movimiento es inversamente proporcional a la masa de la partícula; es decir, partículas pequeñas se mueven más rápido que otras. Tal como se estaba haciendo la simulación hasta ahora, se puede entender el cambio de posición de una particula como <img src="http://latex.codecogs.com/svg.latex?x'=x+\delta{f}" border="0"/>, donde <img src="http://latex.codecogs.com/svg.latex?\delta" border="0"/> es un factor de reducción de movimiento. Como observamos la simulación un momento en el tiempo a la vez, entonces podemos entender a la velocidad de movimiento como <img src="http://latex.codecogs.com/svg.latex?|\delta{f}|" border="0"/>. Así, el nuevo cambio para considerar la masa será mover a una particula a su siguiente posición como <img src="http://latex.codecogs.com/svg.latex?x'=x+\frac{\delta{f}}{m}" border="0"/>, donde <img src="http://latex.codecogs.com/svg.latex?m" border="0"/> es la masa de la partícula.
  
 La Figura <a href="#fig1">1</a> muestra la forma en que se comporta la velocidad de todas las particulas a lo largo de la simulación. Por un lado, se grafica en rojo la velocidad sin considerar la masa y para comparar en negro, la velocidad considerando la masa. Se gráfica masa contra velocidad promedio de cada partícula observada durante toda la simulación. Observe como para partículas con poca masa, la velocidad es mayor y desciende a medida que aumenta la masa. Note como la velocidad prueba en rojo se mantiene de cierta forma constante o sin cambio.
</p>

<p align="center">
<div id="fig1" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p9/MasaVelocidad.png" height="40%" width="40%"/><br>
<b>Figura 1.</b> Consideración de la masa de las partículas en su velocidad de movimiento.
</div>
</p>

## Reto 1: Visualización
<p align="justified">
  Quizá la forma más sencilla de apreciar el efecto de la masa en la velocidad es visualizando su movimiento en sí. Para ello se gráfica  cada partícula como un círculo de radio igual a su masa. La imagen se realiza utilizando la libreria `ggplot2`, se muestra la escala de color que representa las cargas de las partículas y la referencia al tamaño del circulo como la masa. Para quitar un poco de información innecesaria se omiten los ejes. El código ejemplo que realiza una imagen es el siguiente:
  
```R
png(paste("p9_t", 0, ".png", sep=""),width = 800,height = 700)
ggplot(p, aes(x=x, y=y,col=colores[p$g+6]))+
 geom_point(aes(size = m))+
  labs(size='masa',col='cargas')+
  scale_color_manual(labels=seq(5,-5,-1),values=colores)+
  guides(col= guide_legend(override.aes = list(size=3, stroke=1.5))) +
  scale_size_continuous(breaks=seq(0,0.1,0.01),labels=seq(0,0.1,0.01))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size=fuente), 
        axis.title.y = element_text(size=fuente,angle=0,vjust = 0.5),
        plot.title = element_text(size=fuente+2,hjust = 0.5,face='bold'),
        legend.text=element_text(size=fuente-2),
        legend.title = element_text(size=fuente,face='bold'),
        legend.key.size = unit(1.5, 'lines'))+
  ggtitle("Estado inicial")
graphics.off()
```
 donde `fuente`es el tamaño de fuente de las gráficas. La simulación completa se muestra en la Figura <a href="#fig2">2</a>, observe como las partículas de menor masa se mueven más rápido que las más pesadas. Se aprecia la atracción y repulsión entre las partículas dependiendo de la diferencia entre su color. Note además, como las partículas que inician muya alejadas casi no tienen interación con las demás, o mejor dicho, las demás no tienen interacción con éstas, por tanto casi no se mueven. Cuando ya hay muchas partículas acumuladas, se aprecia como las de menor masa son expulsadas constantemente de un lado a otro.
 </p>

<p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p9/p9.gif" height="40%" width="40%"/><br>
<b>Figura 2.</b> Movimiento de las partículas considerando su masa.
</div>
</p>
