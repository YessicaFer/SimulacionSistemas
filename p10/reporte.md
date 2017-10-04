# Práctica 10: Algoritmo genético
## 9 de octubre de 2017

## Introducción
<p align="justified">
En esta práctica se realiza un algoritmo genético para resolver el problema de la mochila. Dados <img src="http://latex.codecogs.com/svg.latex?n" border="0"/> objetos cada uno con un peso <img src="http://latex.codecogs.com/svg.latex?p_i" border="0"/> y un valor o beneficio asociado <img src="http://latex.codecogs.com/svg.latex?v_i" border="0"/> y, una mochila con capacidad de carga <img src="http://latex.codecogs.com/svg.latex?P" border="0"/>; se desea determinar cuáles objetos llevar en la mochila sin exceder su capacidad de forma que se maximice el beneficio total por lo objetos seleccionados.

Aunque existe un algoritmo de tabulación pseudo-polinomial que determina la solución óptima al problema cuando <img src="http://latex.codecogs.com/svg.latex?p_i\in\mathbb{Z}" border="0"/>, se implementa un algoritmo genético. Con la solución óptima se puede medir la eficacia del algoritmo.

La codificación de los individuos esta dada por un vector de <img src="http://latex.codecogs.com/svg.latex?n" border="0"/> 1's y 0's, que representan si se incluye o no un objeto en la mochila. Se genera una población inicial, de tamaño `init`, en donde cada individuo se obtiene eligiendo aletoriamente algunos objetos en la mochila, sin importar si respetan la capacidad de la mochila. En cada generación, los individuos pueden mutar con probabilidad <img src="http://latex.codecogs.com/svg.latex?p_m" border="0"/> cambiando uno de sus genes aleatoriamente. Se seleccionan `rep`parejas uniformemente para reproducirse haciendo un cruzamiento por un punto aleatorio formando una progenie de tamaño `2rep`. Luego, los individuos originales, los mutados y la progenie son ordenados por su aptitud y pasan los mejores que sean factibles y los mejores infactibles (en caso de ser necesario) para formar una nueva población de tamaño `init`.
</p>

## Versión con paralelismo
Los algoritmos genéticos son intrinsecamente fácil de paralelizar porque sus operadores trabajan con unos pocos individuos y se repiten muchas veces. Se utilizó la libreria `parallel` para la implementación de la versión paralela. La  generación de la población inicial, se puede generar pidiendo la genración de un individuo por cluster, esto se hace de la siguiente forma:
```R
p <- as.data.frame(t(parSapply(cluster,1:init,function(i){return(round(runif(n)))})))
```
La mutación consta de hacer un cambio aleatorio en una posición del vector; sin embargo, en esta implementación se concatenan los individuos originales y los mutados. De no ser así sería más rapido buscar una posición aleatoria en el dat frame y cambiar su valor. En este caso, primero decidimos cuáles individuos van a mutar y después invocamos a la mutación, ambos procesos se pueden paralelizar:

```R
mutan=sample(1:tam,round(pm*tam)) #Elegir cuales van a mutar con pm
p <- rbind(p,(t(parSapply(cluster,mutan,function(i){return(mutacion(unlist(p[i,]), n))}))))
```
Aquí hay que hacer notar algunas de las complicaciones que tiene el usar esta librería para paralelismo, note la cantidad de manipulaciones que se tienen que hacer a los datos para poder concatenar los individuos; por ejemplo, el uso de `unlist` y `t`. Como éstas, notará múltiples manipulaciones en adelante para los otros métodos paralelizados.

La reproducción se hace primero seleccionando los padres y después cruzando cada pareja seleccionada. En principio, ambos métodos se podrían y deberían unir para no perder tiempo en la administración de `parallel`. Pero pensando a futuro, se optó por seleccionar primero los individuos; así el método de selección puede ser manipulado. El código es el siguiente:

```R
padres <- parSapply(cluster,1:rep,function(x){return(sample(1:tam, 2, replace=FALSE))}) #selección de padres        
hijos <- parSapply(cluster,1:rep,function(i){return(as.matrix(unlist(reproduccion(p[padres[1,i],], p[padres[2,i],], n)),ncol=n))})
p = rbind(p,hijos)
```
Por último, el cálculo de la aptitud de la factibilidad de los individuos:
```R
obj=parSapply(cluster,1:tam,function(i){return(objetivo(unlist(p[i,]), valores))})
fact=parSapply(cluster,1:tam,function(i){return(factible(unlist(p[i,]), pesos, capacidad))})
```
 
## Eficacia del paralelismo
<p align="justified">
Se realizó un experimento para demostrar la eficiencia de la implementación paralela. Fue un diseño modesto, pero más que suficiente para visualizar el comportamiento. El tamaño de la población `init` fue 50, 100 y 200. Los demás parámetros fueron fijados en <img src="http://latex.codecogs.com/svg.latex?p_m=0.05" border="0"/>, `rep=50`y la cantidad de generaciones <img src="http://latex.codecogs.com/svg.latex?t_{\max}=50" border="0"/>. Como dije, es una experimentación muy modesta. La Figura <a href="#fig1">1</a> muestra el diagrama de bigotes correspondiente del tiempo de ejecución de ambas implementaciones.
</p>

<p align="center">
<div id="fig1" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p10/secuencialParalelo.png" height="60%" width="60%"/><br>
<b>Figura 1.</b> Comparación de implementación secuencial y paralelo de AG.
</div>
</p>

<p align="justified">
 Los resultados son notorios aún y cuando se están considerando esfuerzos muy pequeños. El tiempo de ejecución promedio de la implementación secuencial es de 82.84 segundos contra 3.71 segundos del caso paralelo.  
 </p>
 
 ## Reto 1: Selección por ruleta 
 El primer reto consta de cambiar el método de selección de los individuos a reproducirse. El método de selección por ruleta consta de asignar una probabilidad de selección a cada padre que dependa directamente de su aptitud o valor objetivo. La forma clásica de hacerlo es transformar el vector de aptitudes a un vector de probabilidades, dividiendo la aptitud de cada individuo entre la aptitud total de la población. Así, un padre con mejor aptitud, tiene mayor probabilidad de ser seleccionado. Este método de selección, al igual que el de torneo,  imitan en cierta medida un comportamiento evolutivo natural. En R, implementar esto es muy sencillo, basta con introducir la nueva distribución de probabilidad con el parámetro `prob` de `sample`. 
 
 Para probar la eficacia del método de selección se desarrolla experimentación variando todos los parámetros del algoritmo genético: 
 <ul>
 <li> Tamaño de la población: 50, 100 y 200</li>
 <li> Cantidad de generaciones: 50, 100 y 200</li>
 <li> Probabilidad de mutación: 0.05, 0.1 y 0.2 </li>
 <li> Cantidad de parejas de padres a reproducrise (multiplicado por tamaño de población): 0.5, 1 y 2 </li>
 </ul>
 
Para cada tratamiento se realizaron diez réplicas. Se decidió tomar estos niveles para los factores antes mencionados con el fin de encontrar variabilidad en la respuesta, pues se puede decantar por un método de selección que sea mejor para cierto valor de estos parámetros. Por cada réplica se utiliza la misma población inicial para ambos casos. Cabe recordar que la hipótesis que se desea probar  es si hay diferencia al cambiar el método de selección, por tanto se utiliza como factor la interacción de todos los parámetros mencionados. En la Figura <a href="#fig2">2</a> aparece el diagrama de bigotes correspondiente, en el eje `x`está la interacción de los factores. 

<p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p10/AjusteParametros.png" height="60%" width="60%"/><br>
<b>Figura 2.</b> Eficacia del método de selección por ruleta.
</div>
</p>


