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
