# Práctica 7: Búsqueda local
## 18 de septiembre de 2017

## Maximización de <img src="http://latex.codecogs.com/svg.latex?g(x,y)" border="0"/> 
En está práctica se implementa una búsqueda local para encontrar el máximo de la función
<p align="center">
<img src="http://latex.codecogs.com/svg.latex?g(x,y)=\frac{(x+\frac{1}{2})^4-30x^2-20x+(y+\frac{1}{2})^4-30y^2-20y}{100}" border="0"/>
</p>
<p align="justified">
para <img src="http://latex.codecogs.com/svg.latex?-6\leq{x,y}\leq5" border="0"/>. La <a href="#fig1"> Figura 1</a> ilustra a la función en tres dimensiones.
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
Hay que hacer notar que en el proceso descrito no se ha tomado en cuenta la factibilidad de la solución. en primer instancia, es fácil considerarla si cuando actualizamos el incumbente, nos aseguramos de que éste sea factible. Sin embargo, puede suceder que nuestro punto de exploración <img src="http://latex.codecogs.com/svg.latex?(x_i,y_i)" border="0"/> sea infactible durante gran parte del proceso; esto afecta a la búsqueda pues es probable que toda la exploración en delante no sirva de nada al no proveer un nuevo incumbente. Por otro lado, si hacemos factible a <img src="http://latex.codecogs.com/svg.latex?(x_i,y_i)" border="0"/> de inmediato, se corre el riesgo de perder una valiosa exploración. 

En este caso, se permite que el punto sea infactible pero que no se aleje demasiado de la región factible, en caso de hacerlo, se recupera parte de su información para factibilizarlo. Podemos distinguir tres casos
<ol>
  <li><img src="http://latex.codecogs.com/svg.latex?x\not\in[-6,5]" border="0"/></li>
  <li>Tea</li>
  <li>Milk</li>
</ol>



<p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p7/p7_100.png" height="40%" width="40%"/><br>
<b>Figura 2.</b> Visualización 3D de <img src="http://latex.codecogs.com/svg.latex?z=g(x,y)" border="0"/>. 
</div>
</p>
