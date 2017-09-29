# Práctica 8: Modelo de urnas
## 25 de septiembre de 2017

<p align="justified">
Esta práctica consta de la simulación del proceso de fragmentación y coalescencia de cúmulos de partículas mediante un modelo de urnas. Primero, se generan números enteros distribuidos normalmente que representan el tamaño de los cúmulos inicialmente. Definiendo un parámetro <img src="http://latex.codecogs.com/svg.latex?c" border="0"/> como el tamaño crítico para el cual los cúmulos mas pequeños se unen entre sí y los grandes se fragmentan; ambos de acuerdo a una distribución de probabilidad específica. Los cúmulos pequeños se unen de acuerdo a una distribución  de la forma
  </p>
  
 <p align="center">
<img src="http://latex.codecogs.com/svg.latex?u(x)=e^{-frac{1}{c}x}" border="0"/>
  </p>
