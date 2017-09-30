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
 <p align="justified">
 en donde los cúmulos pequeños tienen muy poca probabilidad de fragmentarse y los grandes es casi seguro que se lo hagan. El valor crítico <img src="http://latex.codecogs.com/svg.latex?c" border="0"/> es fijado como la mediana de los tamaños y <img src="http://latex.codecogs.com/svg.latex?d" border="0"/> como su desviación estándar.
 </p>
 
 ## Versión paralelizada
 El objetivo de esta sección es visualizar el impacto de utilizar paralelismo en la simulación con respecto a la versión secuencial. En particular, se paralelizaron las rutinas correspondientes a la fragmentación de cúmulos (llamada `romperse`) y a la unión de éstos, compuesta por una sección en donde se decide cuáles se uniran (llamada `unirse`) y otra donde se realiza la unión (donde la variable `juntarse`es la protagonista). El resto de la parte principal de la simulación consta de actualizaciones y transformaciones del vector de `cumulos` como frecuencias de tamaños, las cuales no tiene sentido paralelizar. 
 
 Sin embargo; cada una de las rutinas que se paralelizan son totalmente sencillas de realizar, aún para el caso secuencial. En general, dentro de cada rutina se toman un par de decisiones `if-else`, se hacen unas cuantas operaciones aritméticas y se genera un vector como respuesta. Quizá el proceso de gestión de trabajo para los núcleos sea más complicado que las operaciones que el trabajo en sí, para poder dormir tranquilos se hacen algunas pruebas estadísticas pertinentes. 
 
 Para matar dos pájaros de un tiro y hacer el primer reto, se realizó un experimento en donde se varía la duración de la simulación en 50, 100, 200, 300 y 400 pasos y; el número de cúmulos existentes al inicio como 1000, 2000, 5000, 10000, 20000 y 30000. El número de partículas es <img src="http://latex.codecogs.com/svg.latex?n=30k." border="0"/> Para cada tratamiento se realizan 30 réplicas. Para disminuir la variabilidad, ambos métodos parten de la misma lista inicial de cúmulos.
 
 ### Secuencial contra paralelo
 <p align="justified">
 La Figura <a href="#fig1">1</a> muestra el comportamiento de la implementación secuencial contra la paralelizada variando la duración de la simulación (el número inicial de cúmulos también varía pero se considera como efecto de variabilidad en los datos), la linea en azul une las medianas de cada nivel. Observe como, aunque no hay una diferencia en órdenes de magnitud entre ambos enfoques, si hay un claro ganador... y tristemente es la implementación secuencial. Incluso se puede ver gracias a las gráficas de violines como las distribuciones correspondientes para ambos enfoques parecen estar sólo escalados. Una prueba de Wilcoxon determina con un valor-<img src="http://latex.codecogs.com/svg.latex?p" border="0"/> de que si hay una diferencia estadística entre las medianas de ambos enfoques y no sólo es apreciación en la gráfica.
 </p>
 
<p align="center">
<div id="fig1" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p8/secuencialParalelo.png" height="50%" width="50%"/><br>
<b>Figura 1.</b> Comparación entre implementación paralela y secuencial variando la duración de la simulación. 
</div>
</p>

### Reto 1: Efecto del número inicial de cúmulos <img src="http://latex.codecogs.com/svg.latex?k" border="0"/> en el tiempo de ejecución
Ahora se hace la comparación entre la implementación secuencial y paralela pero variando el número inicial de cúmulos. De nuevo, la variable de respuesta es el tiempo de ejecución y la duración de la simulación sólo se usa para dar variabilidad al experimento. La Figura <a href="#fig2">2</a> muestra los resultados obtenidos.

<p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p8/variandoK.png" height="50%" width="50%"/><br>
<b>Figura 2.</b> Comparación entre implementación paralela y secuencial variando el número inicial de cúmulos. 
</div>
</p>