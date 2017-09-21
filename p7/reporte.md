# Práctica 7: Búsqueda local
## 18 de septiembre de 2017

## Introducción
En está práctica se implementa una búsqueda local para encontrar el máximo de la función
<p align="center">
<img src="http://latex.codecogs.com/svg.latex?g(x,y)=\frac{(x+\frac{1}{2})^4-30x^2-20x+(y+\frac{1}{2})^4-30y^2-20y}{100}" border="0"/>
</p>
para 
<p align="center">
  <img src="http://latex.codecogs.com/svg.latex?-6\leq{x,y}\leq5." border="0"/> 
  </p>
  La <a href="#fig1"> Figura 1</a> ilustra a la función en tres dimensiones.
  
  <p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p7/p7_2d.png" height="50%" width="50%"/><br>
<b>Figura 1.</b> Visualización 3D de <img src="http://latex.codecogs.com/gif.latex?z=g(x,y)" border="0"/>. 
</div>
</p>

<p align="justified">
La forma de búsqueda del máximo de la función es elegir un punto  <img src="http://latex.codecogs.com/gif.latex?x_0" border="0"/>aleatoriamente en el dominio y luego generar un vector de movimiento <img src="http://latex.codecogs.com/gif.latex?(\Delta{x},\Delta{y})" border="0"/>. A partir de este vector podemos formar cuatro nuevos puntos <img src="http://latex.codecogs.com/gif.latex?p_i=(x,y)+(\pm\Delta{x},\pm\Delta{y})" border="0"/>. Luego, seleccionamos <img src="http://latex.codecogs.com/gif.latex?\hat{p}=\text{argmax}\{g(p_i);\;i=1:4\}" border="0"/>a
</p>
