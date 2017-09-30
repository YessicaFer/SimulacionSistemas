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
 La Figura <a href="#fig1">1</a> muestra el comportamiento de la implementación secuencial contra la paralelizada variando la duración de la simulación (el número inicial de cúmulos también varía pero se considera como efecto de variabilidad en los datos), las lineas en azul  y rojo unen las medias de cada nivel. Observe como, aunque no hay una diferencia en órdenes de magnitud entre ambos enfoques, si hay un claro ganador... y tristemente es la implementación secuencial. Incluso se puede ver gracias a las gráficas de violines como las distribuciones correspondientes para ambos enfoques son muy similares, solo desfazadas; recuerde que ambas implementaciones parten de la misma lista de cúmulos, lo que hace pensar que en general los cambios en los cúmulos dependen fuertemente de la distribución inicial de éstos.  Una prueba de Wilcoxon determina con un valor-<img src="http://latex.codecogs.com/svg.latex?p" border="0"/> de que si hay una diferencia estadística entre las medianas de ambos enfoques y no sólo es apreciación en la gráfica. El tiempo promedio usando paralelismo es de 57.73 segundos contra 49.87 de la versión secuencial. 
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

<p align="justified">
  Por supuesto, la prueba estadística y las comparaciones de las medias son las mismas pues solo es una intepretación diferente de los mismos datos.
  
 </p>
 
 ### Conclusiones
 No siempre el uso de paralelismo es influyente en el tiempo de ejcución. Esta experimentación se hizo con siete núcleos, quizá teniendo más núcleos hacia donde repartir el trabajo, la disminución del tiempo de realizar los trabajos compensará la chamba de la secuenciación de éstos. Aunque no es una diferencia muy grande, casi ocho segundos en promedio, vale la pena en este caso realizar la versión secuencial sobre la paralelizada.
 
 ## Reto 2: Cambiando el tamaño del filtro
 <p align="justified">
 El segundo reta consta de monitorear qué pasa cuando se cambia el tamaño del filtro <img src="http://latex.codecogs.com/svg.latex?c" border="0"/> de la mediana de los cumúlos inicales a su media más la desviación estándar. Recordemos que el tamaño de los cúmulos se distribuye normalemnete (o al menos se asemeja mucho); por tanto, se espera que al principio cerca del 30.8% de los cúmulos sean mayores al filtro (de acuerdo a la acumulada de la normal estándar). Para apreciar algún comportamiento de los cúmulos mayores al filtro se tomaron diferentes valores de <img src="http://latex.codecogs.com/svg.latex?k" border="0"/>; es decir, de cúmulos iniciales. Los valores fueron <img src="http://latex.codecogs.com/svg.latex?k\in\{1000,2000,5000,10000\}" border="0"/>; el número de partículas se consideró como <img src="http://latex.codecogs.com/svg.latex?n=30k" border="0"/>. Como la duración sólo ayuda a la visualización, se tomó un valor que experimentalmente se concluyó permitía notar los cambios suficientes y es de 100 pasos. Para cada valor de <img src="http://latex.codecogs.com/svg.latex?k" border="0"/> se realizaron 30 réplicas.
  
 Visualmente, el comportamiento de la simulación se puede representar con un histograma como el de la Figura <a href="#fig3">3</a>, el cual es un ejemplo para <img src="http://latex.codecogs.com/svg.latex?k=10000" border="0"/>.  
  </p>
  
  <p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p8/Histograma_k10000_rep2.gif" height="30%" width="30%"/><br>
<b>Figura 3.</b> Histograma del tamaño de los cúmulos durante la simulación.
</div>
</p>

La linea vertical en rojo corresponde al tamaño del filtro <img src="http://latex.codecogs.com/svg.latex?c" border="0"/>. Sin embargo; no es posible apreciar mucho del comportamiento de los cúmulos mas grandes salvo que no hay  cambios bruscos al histograma. Es decir; se mantiene en una proporción similar, el número de cúmulos pequeños y grandes. Si graficamos la cantidad de cúmulos grandes que hay durante la simulación, podemos ver que el comportamiento es similar independientemente del número inical de cúmulos, como puede apreciarse en la Figura <a href="#fig4">4</a>.

  <p align="center">
<div id="fig4" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p8/Crecimiento_k2000.gif" height="32%" width="32%"/>
  <img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p8/Crecimiento_k5000.gif" height="32%" width="32%"/>
  <img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p8/Crecimiento_k10000.gif" height="32%" width="32%"/><br>
<b>Figura 4.</b> Cúmulos mayores al filtro durante la simulación.
</div>
</p>

<p align="justified">
Note como al principio se inicia a un valor cercano al 30% del total de los cúmulos, esto como ya se había dicho, es el esperado porque los tamaños de los cúmulos se distribuyen normalmente. En la primer iteración, se hace un salto relativamente grande y desde ahí practicamente la cantidad de cúmulos permanece igual. La linea verde que aparece es el número promedio de cúmulos grandes durante toda la simulación; ese promedio permanece muy similar durante todas las réplicas. Si sacamos el promedio durante todas las réplicas obtenemos 341.68, 679.77, 1704.87 y 3373.93 para 1000, 2000, 5000 y 10000 cúmulos iniciales respectivamente. A simple vista, estos promedios pueden carecer de significado, pero si lo pasamos a porcentaje para escalarlos indepndientes de <img src="http://latex.codecogs.com/svg.latex?k" border="0"/>, obtenemos 34.16%, 33.98%, 34.09% y 33.73%, respectivamente. Es decir, el promedio de cúmulos grandes durante la simulación es del 33.99% del número inicial de cúmulos <img src="http://latex.codecogs.com/svg.latex?k" border="0"/>. Quizá hacer una aseveración desde aquí no sea conveniente, menos con tan pocos datos, pero me gustaría pensar que se puede extrapolar una relación con este promedio y el valor de <img src="http://latex.codecogs.com/svg.latex?c" border="0"/>.
  </p>
