# Práctica 7: Búsqueda local
## 18 de septiembre de 2017

## Maximización de <img src="http://latex.codecogs.com/svg.latex?g(x,y)" border="0"/> 
En está práctica se implementa una búsqueda local para encontrar el máximo de la función
<p align="center">
<img src="http://latex.codecogs.com/svg.latex?g(x,y)=\frac{(x+\frac{1}{2})^4-30x^2-20x+(y+\frac{1}{2})^4-30y^2-20y}{100}" border="0"/>
</p>
<p align="justified">
para <img src="http://latex.codecogs.com/svg.latex?-6\leq{x,y}\leq5" border="0"/>. La Figura <a href="#fig1">1</a> ilustra a la función en tres dimensiones.
</p>  
<p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p7/p7_2d.png" height="50%" width="50%"/><br>
<b>Figura 1.</b> Visualización 3D de <img src="http://latex.codecogs.com/svg.latex?z=g(x,y)" border="0"/>. 
</div>
</p>

<p align="justified">
La forma de búsqueda del máximo de la función es elegir un punto  <img src="http://latex.codecogs.com/svg.latex?(x_0,y_0)" border="0"/> aleatoriamente en el dominio y luego generar un vector de movimiento <img src="http://latex.codecogs.com/svg.latex?(\Delta{x_0},\Delta{y_0})" border="0"/>. A partir de éste vector podemos formar ocho nuevos puntos <img src="http://latex.codecogs.com/svg.latex?p_j=(x_0,y_0)+(\pm\Delta{x_0},\pm\Delta{y_0})" border="0"/>, <img src="http://latex.codecogs.com/svg.latex?p_j=(x_0,y_0)+(\pm\Delta{x_0},0)" border="0"/> ó <img src="http://latex.codecogs.com/svg.latex?p_j=(x_0,y_0)+(0,\pm\Delta{y_0})" border="0"/>. Luego, seleccionamos aquel que tenga mejor evaluación de la función; es decir, <img src="http://latex.codecogs.com/svg.latex?\hat{p_1}=\text{argmax}\{g(p_j);\;j=1:8\}" border="0"/>. Hacemos <img src="http://latex.codecogs.com/svg.latex?(x_i,y_i)=\hat{p_i};\quad{i\geq1" border="0"/> y repetimos el mismo proceso una cierta cantidad <img src="http://latex.codecogs.com/svg.latex?n" border="0"/> de veces. En cada iteración, se va actualizando y guardando la solución incumbente. Este proceso sencillo nos permite encontrar fácilmente un máximo local de la función.
</p>

### Sobre la factibilidad de las soluciones
<p align="justified">
Hay que hacer notar que en el proceso descrito no se ha tomado en cuenta la factibilidad de la solución. en primer instancia, es fácil considerarla si cuando actualizamos el incumbente, nos aseguramos de que éste sea factible. Sin embargo, puede suceder que nuestro punto de exploración <img src="http://latex.codecogs.com/svg.latex?(x_i,y_i)" border="0"/> sea infactible durante gran parte del proceso; esto afecta a la búsqueda pues es probable que toda la exploración en delante no sirva de nada al no proveer un nuevo incumbente. Por otro lado, si hacemos factible a <img src="http://latex.codecogs.com/svg.latex?(x_i,y_i)" border="0"/> de inmediato, se corre el riesgo de perder una valiosa exploración.  
</p>

En este caso, se permite que el punto sea infactible pero que no se aleje demasiado de la región factible, en caso de hacerlo, se recupera parte de su información para factibilizarlo.  Podemos distinguir tres casos:

<ul>
  <li><img src="http://latex.codecogs.com/svg.latex?x\not\in[-6,5]" border="0"/></li>
  <li><img src="http://latex.codecogs.com/svg.latex?y\not\in[-6,5]" border="0"/></li>
  <li><img src="http://latex.codecogs.com/svg.latex?x,y\not\in[-6,5]" border="0"/></li>
</ul>

<p align="justified">
Para medir cuánto se aleja, se utiliza la distancia de un punto a un conjunto, la cual denominaremos como <img src="http://latex.codecogs.com/svg.latex?d(x,y)" border="0"/>; por definición, si <img src="http://latex.codecogs.com/svg.latex?d(x,y)" border="0"/> es factible, entonces <img src="http://latex.codecogs.com/svg.latex?d(x,y)=0" border="0"/>. En los primeros dos casos, podemos usar la información de la componente que si cumple la restricción y a la que lo incumple volverla factible restando (o sumando, según sea el caso) <img src="http://latex.codecogs.com/svg.latex?d(x,y)" border="0"/> y un valor aleatorio pequeño, que actúa como una perturbación. Para el último caso, se hace una pequeña perturbación en el incumbente, con la esperanza de que cuando la solución dejó de ser factible, lo hizo saliendo por una esquina porque se maximizaba en esa dirección; por tanto, el incumbente está próximo a la esquina. El código en R que realiza la búsqueda local es el siguiente:
</p>

```R
replica <- function(t) {
  
  #Iniciar con punto aleatorio en el dominio
  curr <- runif(2, low, high)
  best <- curr
  
  for (tiempo in 1:t) {
    #vector de movimiento
    delta <- runif(2, 0, step)
    #8 nuevas posibles posiciones para curr
    north_east <- curr +c(-1,1)* delta
    north_west <- curr +c(1,1)* delta
    south_east <- curr +c(-1,-1)* delta
    south_west <- curr +c(1,-1)* delta
    left <- curr +c(-1,0)* delta
    right <- curr +c(1,0)* delta
    top <- curr +c(0,1)* delta
    bottom <- curr +c(0,-1)* delta
    #seleccionar la mejor de las 8
    mov=data.frame(rbind(north_east,north_west,south_east,south_west,left,right,top,bottom))
    names(mov)=c("x","y")
    mov$g=sapply(1:nrow(mov),function(i){ return( gc(mov[i,]))})
    curr=as.numeric(mov[mov$g==max(mov$g),][1:2])
    
    #verificar que tan infactible es curr
    if(dist(curr)>1){
      x=curr[1]
      y=curr[2]
      if(x>=low & x<=high){#esta dentro del rango x
        if(y<low){ #esta debajo
          y=y+dist(curr)+runif(1,0,step)
        }else{#esta arriba
          y=y-dist(curr)-runif(1,0,step)
        }
        #x=x+runif(1,-step,step)
        curr=c(x,y)
      }else if(y>=low & y<=high){ #está dentro del rango y
        if(x<low){ #esta a la izquierda
          x=x+dist(curr)+runif(1,0,step)
        }else{#esta a la derecha
          x=x-dist(curr)-runif(1,0,step)
        }
        #y=y+runif(1,-step,step)
        curr=c(x,y)
      }else{ #incumple ambas (anda por las esquinas)
        curr <- best+runif(2, -0.1,0.1)
      }
      
    }
    #si mejora y es factible
    if (gc(curr) > gc(best) & dist(curr)==0) { 
      #actualizar incumbente
      best <- curr
    }
    
    
  }
    return(best)
  
}
```
### ¿Cuál es el óptimo?
De acuerdo a Wolfram Alpha, la función <img src="http://latex.codecogs.com/svg.latex?g(x,y)" border="0"/> tiene una solución máxima en <img src="http://latex.codecogs.com/svg.latex?(5,5)" border="0"/>, con un valor óptimo de <img src="http://latex.codecogs.com/svg.latex?\frac{1041}{800}\approx1.30125." border="0"/>

## Visualización del funcionamiento
<p align="justified">
Ahora se describe una forma de visualizar el funcionamiento de la búsqueda local. Como la función a optimizar está en tres dimensiones se toma una gráfica plana de las curvas de nivel correspondientes. En R, una forma de dibujar estas curvas es con la función `filled.contour()`. Además, podemos graficar en cada iteración de la búsqueda la posición de la solución <img src="http://latex.codecogs.com/svg.latex?(x,y)" border="0"/> (en rojo) y la del incumbente (en azul). Éste último incluyendo unas lineas  verdes en cruz como delimitando un objetivo en una mirilla de un arma.

Se dibuja la curva de nivel con <img src="http://latex.codecogs.com/svg.latex?-7\leq{x,y\leq6" border="0"/> para poder visualizar las soluciones que dejan de ser factibles durante la búsqueda. La razón por la que se considera un entero más en cada dirección del dominio es porque la máxima distancia <img src="http://latex.codecogs.com/svg.latex?d(x,y)" border="0"/> permitida es 1.

Para tener una mejor visualización, se considera que <img src="http://latex.codecogs.com/svg.latex?z\in[-5.5,1.5]" border="0"/>; como la función crece muy rápido fuera del conjunto factible, los cambios en los colores que diferencian las curvas de nivel son casi inperceptibles. En cambio, con este ajuste de los limites en <img src="http://latex.codecogs.com/svg.latex?z" border="0"/>, aparece en blanco todo punto no coniderado. En especifico para esta función, su valor mínimo es de <img src="http://latex.codecogs.com/svg.latex?-5.23276" border="0"/> de acuerdo a Wolfram Alpha y su máximo como ya se había mencionado es <img src="http://latex.codecogs.com/svg.latex?1.30125" border="0"/>. La zona factible está delimitada por un rectangulo en lineas punteadas.

Como puede observar en la Figura <a href="#fig2">2</a>, <img src="http://latex.codecogs.com/svg.latex?z" border="0"/> aumenta a medida que el color es más oscuro. Observe como la posición de <img src="http://latex.codecogs.com/svg.latex?(x,y)" border="0"/> se acerca cada vez a las zonas más altas de la función.
</p>

<p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p7/busqueda_local_centro.gif" height="40%" width="40%"/><br>
<b>Figura 2.</b> Búsqueda local atascada en un óptimo local. 
</div>
</p>

Por otro lado, si la búsqueda se movió cercano a una orilla en donde la función crece, repetidamente las soluciones se vuelven infactibles, pero se recupera su factibilad cuando se encuentran a distancia uno del conjunto factible (visualmente al tocar el borde exterior). La Figura <a href="#fig3">3</a> ilustra este comportamiento. Observe como el mantener información de la posición de la solución, aunque sea "ligeramente" infactible, permite encontar un nuevo incumbente. Note además, como el incumbente nunca se considera infactible, por lo que no puede salir del recuadro punteado interior. Ambos casos presentan situaciones en donde la búsqueda se queda atrapada alrededor de un óptimo local, y como éstas habrá muchas, de ahí el apellido de la búsqueda.

<p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p7/busqueda_local_orilla.gif" height="40%" width="40%"/><br>
<b>Figura 3.</b> Búsqueda local que hace uso de soluciones infactibles. 
</div>
</p>

## Paralelismo
Para tener una mejor aproximación de la solución óptima, podemos ejecutar el método `replica(t)` muchas veces. `t`se refiere a la cantidad de pasos que debe ejecutar la búsqueda para parar. Como cada búsqueda se hace por separado, aumentando `t`y haciendo múltiples búsquedas en paralelo, se puede encontar el incumbente de todas el cuál sería nuestra mejor aproximación hasta el momento. Las Figuras <a href="#fig4">4</a>, <a href="#fig5">5</a> y <a href="#fig6">6</a> muestran las soluciones encontradas para valores de `t`de 100, 1000 y 10,000, respectivamente para 100 búsquedas locales en cada caso. Puede apreciarse como las soluciones se hacinan en todos los optimos locales, pero a medida que aumenta la longitud de la búsqueda se concentran en un sólo punto. Además, a partir de 1000 pasos, ya se encontró la solución óptima.

<p align="center">
<div id="fig4" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p7/p7_100.png" height="40%" width="40%"/><br>
<b>Figura 4.</b> Incumbente para 100 pasos. 
</div>
</p>

<p align="center">
<div id="fig5" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p7/p7_1000.png" height="40%" width="40%"/><br>
<b>Figura 5.</b> Incumbente para 1000 pasos.
</div>
</p>

<p align="center">
<div id="fig6" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p7/p7_10000.png" height="40%" width="40%"/><br>
<b>Figura 6.</b> Incumbente para 10,000 pasos.
</div>
</p>

## Escapando de óptimos locales
<p align="justified">
Como podemos apreciar, la búsqueda local  está sentenciada una vez que se aproxima a un óptimo local. Por la forma de cambiar a la siguiente solución, no hay forma de escapar de estos puntos atractores. Una alternativa para romper este planteamiento es permitir cambiar a puntos que no mejoran la función objetivo. Una metodología para controlar esta permisión es el Recocido Simulado (SA, por sus siglas en inglés). La idea es generar sólo un vecino <img src="http://latex.codecogs.com/svg.latex?x'" border="0"/> como posible movimiento de <img src="http://latex.codecogs.com/svg.latex?x" border="0"/>. Si <img src="http://latex.codecogs.com/svg.latex?g(x')>g(x)" border="0"/>, hacemos <img src="http://latex.codecogs.com/svg.latex?x=x'" border="0"/>. En caso contario, aceptamos cambiarnos a esta solución aunque no mejore con probabilidad <img src="http://latex.codecogs.com/svg.latex?e^{\frac{g(x')-g(x)}{T}}" border="0"/>, donde <img src="http://latex.codecogs.com/svg.latex?T" border="0"/> es un parámetro conocido como temperatura, el cuál irá disminuyendo conforme avance la búsqueda de acuerdo a un factor de enfriamiento <img src="http://latex.codecogs.com/svg.latex?\xi<1" border="0"/>. Por este proceso de enfriamiento es que lleva su nombre la metodología. La idea es que al principio casi cualquier solución sea permitida y a medida que avanza el tiempo, sólo aquellas que mejoran se aceptan.

### Ajuste de parámetros
Los parámetros de temperatura y factor de enfriamiento del Recocido Simulado deben de ser configurados adecuadamente para su mejor funcionamiento. Se realizó un diseño de experimentos en donde se variaron ambos parámetros: la temperatura puede tomar valores de 0.5, 1, 10, 100 y 1000; el factor de enfriamiento será 0.7, 0.8, 0.9, 0.95, 0.99, 0.995 ó 0.999. La variable de respuesta es el valor objetivo del incumbente encontrado por el algoritmo de SA. Para cada posibilidad se realizaron 100 réplicas. 

De acuerdo a una prueba de Kruskal y Wallis se determina que la temperatura no es significativa en el desempeño del algoritmo con un valor-<img src="http://latex.codecogs.com/svg.latex?p" border="0"/>  de 0.03368; en adelante se utilizará el valor de uno para la temperatura. En el caso del factor de enfriamiento, éste si es significativo para encontrar el incumbente con un valor-<img src="http://latex.codecogs.com/svg.latex?p" border="0"/> de <img src="http://latex.codecogs.com/svg.latex?2.5\times10^{-9}" border="0"/>. Una prueba de Dunn muestra los pares para los cuales existe diferencia significativa, estos son los resultados:
```R
    Comparison        Z      P.unadj        P.adj
4   0.7 - 0.95 4.492154 7.050645e-06 3.701589e-05
5   0.8 - 0.95 3.644638 2.677689e-04 9.371911e-04
7   0.7 - 0.99 5.415122 6.124708e-08 1.286189e-06
8   0.8 - 0.99 4.567605 4.933280e-06 3.453296e-05
11 0.7 - 0.995 4.588102 4.472944e-06 4.696591e-05
12 0.8 - 0.995 3.740586 1.835921e-04 7.710866e-04
16 0.7 - 0.999 3.634591 2.784219e-04 8.352658e-04
```
El valor `P.adj`es el correspondiente al valor-<img src="http://latex.codecogs.com/svg.latex?p" border="0"/> de la prueba. Muestra un diferencia entre un enfriamiento mas duro(factor de enfriamiento 0.7, 0.8) que uno mas suave (factor de enfriamiento mayor a 0.95). La Figura <a href="#fig7">7</a> muestra la gráfica de violines del efecto del factor de enfrimiento en encontrar el incumbente con SA. La linea azul une las medianas correspondientes a cada nivel y en naranja aparaecen los diagramas de bigotes correspondientes, los datos atipicos fueron omitidos para una mejor visualización. El valor objetivo de  aproximadamente 0.06 corresponde a la cima de la colina del centro (aproximadamente en <img src="http://latex.codecogs.com/svg.latex?(-0.3,-0.3)" border="0"/> ). Las cimas en donde corta la region factible en aproximadamente <img src="http://latex.codecogs.com/svg.latex?(-0.3,-6)" border="0"/> y <img src="http://latex.codecogs.com/svg.latex?(-6,-0.3)" border="0"/> tienen un valor objetivo de -0.42 aproximadamente. Las cimas en donde corta la region factible en aproximadamente <img src="http://latex.codecogs.com/svg.latex?(-0.3,5)" border="0"/> y <img src="http://latex.codecogs.com/svg.latex?(5,-0.3)" border="0"/> tienen un valor objetivo de 0.67 aproximadamente. Por último, la esquina superior derecha, <img src="http://latex.codecogs.com/svg.latex?(5,5)" border="0"/>, tiene una valor objetivo de 1.3 y es el punto máximo.

 Observe como a pesar de que las medianas son iguales para todos los niveles, hay un mejor desempeño del algoritmo para enfriamientos suaves, esto gracias a que le fue mas fácil escapar del máximo local que se encuentra en el centro
</p>


<p align="center">
<div id="fig7" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p7/P7R2_Violines_enfriamiento.png" height="40%" width="40%"/><br>
<b>Figura 7.</b> Gráfica de violines para el factor de enfriamiento.
</div>
</p>

De acuerdo a los resultados del experimento, optamos por tomar un factor de enfriamiento de 0.999. 

###Resultados del Recocido Simulado
<p align="justified">
Ya con los parámetros ajustados se corrió el algoritmo para longitudes de búsqueda distintos; es decir cuando se hace la búsqueda por <img src="http://latex.codecogs.com/svg.latex?n" border="0"/> pasos. Las longitudes consideradas fueron <img src="http://latex.codecogs.com/svg.latex?10^k" border="0"/> con <img src="http://latex.codecogs.com/svg.latex?k\in\{2,3,4,5,6\}" border="0"/>. La Figura <a href="#fig8">8</a> muestra las gráficas de violines correspondientes.
</p>

<p align="center">
<div id="fig7" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p7/P7R2_Violines_longitud.png" height="40%" width="40%"/><br>
<b>Figura 8.</b> Gráfica de violines para distintas longitudes de la búsqueda.
</div>
</p>  

Hay una tendencia hacia un mayor acercamiento al óptimo en cuanto crece el número de pasos realizados. A pesar de que la mediana en general no cambia, si lo hace la forma del histograma. En cuanto aumentamos los pasos, se observa un claro aumento de soluciones con un valor objetivo mayor. además, la panza correspondiente al óptimo local situado en el centro (con valor objetivo 0.06) disminuye. Así, podemos concluir el impacto del tiempo o duración de la exploración. 
