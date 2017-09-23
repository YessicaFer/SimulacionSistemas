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
Ahora se describe una forma de visualizar el funcionamiento de la búsqueda local. Como la función a optimizar está en tres dimensiones se toma una gráfica plana de las curvas de nivel correspondientes. En R, una forma de dibujar estas curvas es con la función `filled.contour()`. Además, podemos graficar en cada iteración de la búsqueda la posición de la solución <img src="http://latex.codecogs.com/svg.latex?(x,y)" border="0"/> (en rojo) y la del incumbente (en verde). Éste último incluyendo unas lineas  verdes en cruz como delimitando un objetivo en una mirilla de un arma.

Se dibuja la curva de nivel con <img src="http://latex.codecogs.com/svg.latex?-7\leq{x,y\leq6" border="0"/> para poder visualizar las soluciones que dejan de ser factibles durante la búsqueda. La razón por la que se considera un entero más en cada dirección del dominio es porque la máxima distancia <img src="http://latex.codecogs.com/svg.latex?d(x,y)" border="0"/> permitida es 1.

Para tener una mejor visualización, se considera que <img src="http://latex.codecogs.com/svg.latex?z\in[-5.5,1.5]" border="0"/>; como la función crece muy rápido fuera del conjunto factible, los cambios en los colores que diferencian las curvas de nivel son casi inperceptibles. En cambio, con este ajuste de los limites en <img src="http://latex.codecogs.com/svg.latex?z" border="0"/>, aparece en blanco todo punto no coniderado. En especifico para esta función, su valor mínimo es de <img src="http://latex.codecogs.com/svg.latex?-5.23276" border="0"/> de acuerdo a Wolfram Alpha y su máximo como ya se había mencionado es <img src="http://latex.codecogs.com/svg.latex?1.30125" border="0"/>. La zona factible está delimitada por un rectangulo en lineas punteadas.

Como puede observar en la Figura , <img src="http://latex.codecogs.com/svg.latex?z" border="0"/> aumenta a medida que el color es más oscuro. Observe como la posición de <img src="http://latex.codecogs.com/svg.latex?(x,y)" border="0"/> se acerca cada vez a las zonas más altas de la función, en este caso como se encuentra en un óptimo local, la búsqueda se queda atrapada pues con esta forma de exploración no es posible salir. 
</p>

<p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p7/busqueda_local_centro.gif" height="40%" width="40%"/><br>
<b>Figura 2.</b> Visualización 3D de <img src="http://latex.codecogs.com/svg.latex?z=g(x,y)" border="0"/>. 
</div>
</p>

## Paralelismo
Para tener una buena aproximación de la solución óptima, podemos ejecutar el método `replica(t)` muchas veces. `t`se refiere a la cantidad de pasos que debe ejecutar la búsqueda para parar. Como cada búsqueda se hace por separado, aumentando `t`y haciendo múltiples búsquedas en paralelo, se puede encontar el incumbente de todas el cuál sería nuestra mejor aproximación hasta el momento. Las Figuras <a href="#fig2">2</a>, <a href="#fig3">3</a> y <a href="#fig4">4</a> muestran las




<p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p7/p7_100.png" height="40%" width="40%"/><br>
<b>Figura 2.</b> Visualización 3D de <img src="http://latex.codecogs.com/svg.latex?z=g(x,y)" border="0"/>. 
</div>
</p>
