# Práctica 8: Modelo de urnas
## 25 de septiembre de 2017

## Introducción
<p align="justified">
Esta práctica consta de la simulación del proceso de fragmentación y coalescencia de cúmulos de partículas mediante un modelo de urnas. Primero, se generan <img src="http://latex.codecogs.com/svg.latex?k" border="0"/> números enteros distribuidos normalmente que representan el tamaño de los cúmulos inicialmente; la suma de los tamaños de todos es igual al número de partículas <img src="http://latex.codecogs.com/svg.latex?n" border="0"/>. Definiendo un parámetro <img src="http://latex.codecogs.com/svg.latex?c" border="0"/> como el tamaño crítico para el cual los cúmulos mas pequeños se unen entre sí y los grandes se fragmentan; ambos de acuerdo a una distribución de probabilidad específica. Los cúmulos pequeños se unen de acuerdo a una distribución  de la forma
  </p>
  
 <p align="center">
<img src="http://latex.codecogs.com/svg.latex?u(x)=e^{-frac{1}{c}x}" border="0"/>
  </p>
y los grandes se fragmentan de acuerdo a una distribución tipo sigmoide definida por
<p align="center">
<img src="http://latex.codecogs.com/svg.latex?u(x)=\frac{1}{1+e^{\frac{c-x}{d}}}," border="0"/>
  </p>
 en donde los cúmulos pequeños tienen muy poca probabilidad de fragmentarse y los grandes es casi seguro que se lo hagan. El valor crítico <img src="http://latex.codecogs.com/svg.latex?c" border="0"/> es fijado como la mediana de los tamaños y <img src="http://latex.codecogs.com/svg.latex?d" border="0"/> como su desviación estándar.
 
 
 ## Versión paralelizada
 El objetivo de esta sección es visualizar el impacto de utilizar paralelismo en la simulación con respecto a la versión secuencial. En particular, se paralelizaron las rutinas correspondientes a la fragmentación de cúmulos (llamada `romperse`) y a la unión de éstos, compuesta por una sección en donde se decide cuáles se uniran (llamada `unirse`) y otra donde se realiza la unión (donde la variable `juntarse`es la protagonista). El resto de la parte principal de la simulación consta de actualizaciones y transformaciones del vector de `cumulos` como frecuencias de tamaños, las cuales no tiene sentido paralelizar. 
 
 Sin embargo; cada una de las rutinas que se paralelizan son totalmente sencillas de realizar, aún para el caso secuencial. En general, dentro de cada rutina se toman un par de decisiones `if-else`, se hacen unas cuantas operaciones aritméticas y se genera un vector como respuesta. Es de esperar que el proceso de gestión de trabajo para los núcleos sea más complicado que las operaciones que el trabajo en sí.
 
 
 
 Para matar dos pájaros de un tiro y hacer el primer reto, se realizó un experimento en donde se varía la duración de la simulación en 50, 100, 200, 300 y 400 pasos y; el número de cúmulos existentes al inicio como 10000, 20000, 30000, 40000 y 50000. Para cada tratamiento se realizan 30 réplicas. Para disminuir la variabilidad, ambos métodos parten de la misma lista inicial de cúmulos.
