# Práctica 9: Interacciones entre partículas
## 2 de octubre de 2017

## Introducción
<p align="justified">
Está práctica trata de la simulación del movimiento de las partículas en dependencia de sus cargas eléctricas y de su posición. Se tienen <img src="http://latex.codecogs.com/svg.latex?n" border="0"/> partículas distribuidas en un cuadrado unitario. Las cargas eléctricas de las partículas son enteros y se distribuyen normalmente entre <img src="http://latex.codecogs.com/svg.latex?[-5,5]" border="0"/>. Si las cargas son de la misma polaridad (signo) se repelen mientras que polos opuestos se atraen. 
  
  La fuerza de atracción o repulsión entre dos partículas es proporcional a la diferencia absoluta de sus cargas e inversamente proporcional a la distancia entre ellas. La  dirección de movimiento de una partícula queda determinada por la interacción de las fuerzas de atracción y repulsión con respecto a todas las demás. 
  
  Sin embargo; la velocidad de movimiento de una partícula depende también de su masa. Para simularlo, se considera que las partículas tienen una masa distribuida uniformemente en el intervalo <img src="http://latex.codecogs.com/svg.latex?(0,0.1]" border="0"/>. Obviamente no puede existir una particula de masa cero y la razón de la cota superior es para tener una buena visualización dibujando cada partícula como un circulo de radio proporcional a su masa.
  
  Se considera que la velocidad  de movimiento es inversamente proporcional a la masa de la partícula; es decir, partículas pequeñas se mueven más rápido que otras. Tal como se estaba haciendo la simulación hasta ahora, se puede entender el cambio de posición de una particula como <img src="http://latex.codecogs.com/svg.latex?x'=x+\delta{f}" border="0"/>
  
  
  

</p>
