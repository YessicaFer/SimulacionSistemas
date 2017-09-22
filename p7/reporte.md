# Práctica 7: Búsqueda local
## 18 de septiembre de 2017

## Maximización de <img src="http://latex.codecogs.com/svg.latex?g(x,y)" border="0"/> 
En está práctica se implementa una búsqueda local para encontrar el máximo de la función
<p align="center">
<img src="http://latex.codecogs.com/svg.latex?g(x,y)=\frac{(x+\frac{1}{2})^4-30x^2-20x+(y+\frac{1}{2})^4-30y^2-20y}{100}" border="0"/>
</p>
<p align="justified">
para <img src="http://latex.codecogs.com/svg.latex?-6\leq{x,y}\leq5." border="0"/> 
  </p>
  La <a href="#fig1"> Figura 1</a> ilustra a la función en tres dimensiones.
  
<p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p7/p7_2d.png" height="50%" width="50%"/><br>
<b>Figura 1.</b> Visualización 3D de <img src="http://latex.codecogs.com/svg.latex?z=g(x,y)" border="0"/>. 
</div>
</p>

<p align="justified">
La forma de búsqueda del máximo de la función es elegir un punto  <img src="http://latex.codecogs.com/svg.latex?(x_0,y_0)" border="0"/> aleatoriamente en el dominio y luego generar un vector de movimiento <img src="http://latex.codecogs.com/svg.latex?(\Delta{x_0},\Delta{y_0})" border="0"/>. A partir de éste vector podemos formar cuatro nuevos puntos <img src="http://latex.codecogs.com/svg.latex?p_j=(x_0,y_0)+(\pm\Delta{x_0},\pm\Delta{y_0})" border="0"/>. Luego, seleccionamos aquel que tenga mejor evaluación de la función; es decir, <img src="http://latex.codecogs.com/svg.latex?\hat{p_1}=\text{argmax}\{g(p_j);\;j=1:4\}" border="0"/>. Hacemos <img src="http://latex.codecogs.com/svg.latex?(x_i,y_i)=\hat{p_i};\quad{i\geq1" border="0"/> y repetimos el mismo proceso una cierta cantidad <img src="http://latex.codecogs.com/svg.latex?n" border="0"/> de veces. En cada iteración, se va actualizando y guardando la solución incumbente. Este proceso sencillo nos permite encontrar fácilmente un máximo local de la función.
</p>


<p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p7/p7_100.png" height="30%" width="30%"/><br>
<b>Figura 1.</b> Visualización 3D de <img src="http://latex.codecogs.com/svg.latex?z=g(x,y)" border="0"/>. 
</div>
</p>
