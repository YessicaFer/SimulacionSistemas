# Práctica 10: Algoritmo genético
## 9 de octubre de 2017

## Introducción
<p align="justified">
En esta práctica se realiza un algoritmo genético para resolver el problema de la mochila. Dados <img src="http://latex.codecogs.com/svg.latex?n" border="0"/> objetos cada uno con un peso <img src="http://latex.codecogs.com/svg.latex?p_i" border="0"/> y un valor o beneficio asociado <img src="http://latex.codecogs.com/svg.latex?v_i" border="0"/> y, una mochila con capacidad de carga <img src="http://latex.codecogs.com/svg.latex?P" border="0"/>; se desea determinar cuáles objetos llevar en la mochila sin exceder su capacidad de forma que se maximice el beneficio total por lo objetos seleccionados.

La codificación de los individuos esta dada por un vector de <img src="http://latex.codecogs.com/svg.latex?n" border="0"/> 1's y 0's, que representan si se incluye o no un objeto en la mochila. Se genera una población inicial, de tamaño `init`, en donde cada individuo se obtiene eligiendo aletoriamente algunos objetos en la mochila, sin importar si respetan la capacidad. En cada generación, los individuos pueden mutar con probabilidad <img src="http://latex.codecogs.com/svg.latex?p_m" border="0"/> cambiando uno de sus genes aleatoriamente. Se seleccionan `rep`parejas uniformemente para reproducirse haciendo un cruzamiento por un punto aleatorio formando una progenie de tamaño `2rep`. Luego, los individuos originales, los mutados y la progenie son ordenados por su aptitud y pasan los mejores que sean factibles y los mejores infactibles (en caso de ser necesario) para formar una nueva población de tamaño `init`.
</p>

## Versión con paralelismo
Los algoritmos genéticos son intrinsecamente fácil de paralelizar porque sus operadores trabajan con unos pocos individuos y se repiten muchas veces. Se utilizó la libreria `parallel` para la implementación de la versión paralela. 

### Población inicial
La  generación de la población inicial, se puede generar pidiendo la generación de un individuo por núcleo, esto se hace de la siguiente forma:
```R
p <- as.data.frame(t(parSapply(cluster,1:init,function(i){return(round(runif(n)))})))
```

### Mutación
La mutación consta de hacer un cambio aleatorio en una posición del vector; sin embargo, en esta implementación se concatenan los individuos originales y los mutados. De no ser así sería más rapido buscar una posición aleatoria en el `data.frame` y cambiar su valor. En este caso, primero decidimos cuáles individuos van a mutar y después invocamos a la mutación, ambos procesos se pueden paralelizar:

```R
mutan=sample(1:tam,round(pm*tam)) #Elegir cuales van a mutar con pm
p <- rbind(p,(t(parSapply(cluster,mutan,function(i){return(mutacion(unlist(p[i,]), n))}))))
```
Aquí hay que hacer notar algunas de las complicaciones que tiene el usar esta librería para paralelismo, note la cantidad de manipulaciones que se tienen que hacer a los datos para poder concatenar los individuos; por ejemplo, el uso de `unlist` y `t`. Como éstas, notará múltiples manipulaciones en adelante para los otros métodos paralelizados.


### Reproducción
La reproducción se hace primero seleccionando los padres y después cruzando cada pareja seleccionada. En principio, ambos métodos se podrían y deberían unir para no perder tiempo en la administración de `parallel`. Pero pensando a futuro, se optó por seleccionar primero los individuos; así el método de selección puede ser manipulado. El código es el siguiente:

```R
padres <- parSapply(cluster,1:rep,function(x){return(sample(1:tam, 2, replace=FALSE))}) #selección de padres        
hijos <- parSapply(cluster,1:rep,function(i){return(as.matrix(unlist(reproduccion(p[padres[1,i],], p[padres[2,i],], n)),ncol=n))})
p = rbind(p,hijos)
```

### Aptitud y factibilidad
Por último, el cálculo de la aptitud de la factibilidad de los individuos:
```R
obj=parSapply(cluster,1:tam,function(i){return(objetivo(unlist(p[i,]), valores))})
fact=parSapply(cluster,1:tam,function(i){return(factible(unlist(p[i,]), pesos, capacidad))})
```
 
## Eficacia del paralelismo
<p align="justified">
Se realizó un experimento para demostrar la eficiencia de la implementación paralela. Fue un diseño modesto, pero más que suficiente para visualizar el comportamiento. El tamaño de la población `init` fue 50, 100 y 200. Los demás parámetros fueron fijados en <img src="http://latex.codecogs.com/svg.latex?p_m=0.05" border="0"/>, `rep=50`y la cantidad de generaciones <img src="http://latex.codecogs.com/svg.latex?t_{\max}=50" border="0"/>. Como dije, es una experimentación muy modesta. La <a href="#fig1">Figura 1</a> muestra el diagrama de bigotes correspondiente del tiempo de ejecución de ambas implementaciones.
</p>

<p align="center">
<div id="fig1" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p10/secuencialParalelo1.png" height="60%" width="60%"/><br>
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
 <li> Tamaño de la población: 500 y 1000</li>
 <li> Cantidad de generaciones: 200 y 300</li>
 <li> Probabilidad de mutación: 0.05 y 0.1</li>
 <li> Cantidad de parejas de padres a reproducrise (multiplicado por tamaño de población): 0.1 y 0.25 </li>
 </ul>

<p align="justified">
Para cada tratamiento se realizaron diez réplicas. Se decidió tomar estos niveles para los factores antes mencionados con el fin de encontrar variabilidad en la respuesta, pues nose quiere decantar por un método de selección que sea mejor para cierto valor de estos parámetros. Por cada réplica se utiliza la misma población inicial para ambos casos. Cabe recordar que la hipótesis que se desea probar  es si hay diferencia al cambiar el método de selección, por tanto se utiliza como factor la interacción de todos los parámetros mencionados. En la <a href="#fig2"> Figura 2</a> aparece el diagrama de bigotes correspondiente, en el eje horizontal está la interacción de los factores. Note que existe un comportamiento muy similar en los resultados para ambos métodos de selección. Una prueba de Wilcoxon nos indica que no hay diferencia significativa con un valor-<img src="http://latex.codecogs.com/svg.latex?p" border="0"/> de 0.466. 
  </p>
<p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p10/AjusteParametros.png" height="100%" width="100%"/><br>
<b>Figura 2.</b> Eficacia del método de selección por ruleta.
</div>
</p>

Como medio más ilustrativo, se siguió la evolución de una población de individuos para ver su desempeño. Consideramos como punto de partida la misma población inicial y desde ahí se calculó el incumbente en cada generación utilizando los dos diferentes métodos de selección. Los resultados pueden verse en la <a href="#fig3">Figura 3</a>; en negro, aparece la evolución cuando no se considera el método por ruleta y en rojo, cuando si se considera. La linea verde corresponde al valor objetivo óptimo.

 </p>
<p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p10/R1.png" height="100%" width="100%"/><br>
<b>Figura 3.</b> Desempeño de ambos métodos de selección durante la evolución.
</div>
</p>
Observe como aunque se observa una mejora más rápida del método de selección por ruleta, al final no hay una diferencia apreciable en el incumbente; razón por la cuál no se encontró diferencia significativa. Una explicación gráfica del porqué se muestra en la <a href="#fig4">Figura 4</a> en donde se graficó la distribución de probabilidad para la selección en cada generación. En otras palabras, se grafican los valores de la ruleta, los datos se ordenan en decrecientemente por su valor objetivo para verlo claramente. En rojo se muestra la densidad de los valores objetivo de la población.

 </p>
<p align="center">
<div id="fig4" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p10/R1_Ruleta.gif" height="60%" width="60%"/><br>
<b>Figura 4.</b> Evolución de la ruleta de selección y densidad de los objetivos.
</div>
</p>

Note como la distribución se va asemejando cada vez mas a la uniforme, haciendo que en las últimas generaciones ambos métodos sean equivalentes. La razón de que sea uniforme al final puede verse en la densidad de los valores objetivos; tras el paso de las generaciones, los individuos se van pareciendo cada vez más entre sí, dando lugar a un fenómeno conocido como deriva genética. Esto puede apreciarse en la forma en que aumenta la curtosis de la densidad pues gran parte de los individuos son el mismo (teniendo un mismo valor objetivo y una misma probabilidad de ser seleccionados). La densidad también nos sirve para ver en el hecho de la deriva genética como van apareciendo óptimos locales como pequeñas cimas; en otras palabras, estamos hablando de poca diversidad de soluciones.

## Ajuste de parámetros
Una de las razones por las que la prueba estadística nos dice que no importa el método de selección es porque se pudo haber hecho experimentación sobre valores de los parámetros que no conducen diferencias. Algo que sí podemos notar en la <a href="#fig2"> Figura 2</a> es como hay configuraciones que ayudan al algoritmo a acercarse al valor óptimo. Si logramos acercarnos más al óptimo y en este punto la prueba sigue diciendo que no hay diferencia significativa entre los métodos de selcción entonces tendríamos una justificación más aceptable.

Para estimar cuál es la mejor configuración y deducir un camino en el que se observaría un mejor comportamiento, se hacen pruebas de Kruskal y Wallis para cada factor (parámetro) que nos indiquen si éstos influyen estadísticamente en el valor objetivo obtenido. El <a href="#tab1"> Cuadro 1</a> muestra los valores-<img src="http://latex.codecogs.com/svg.latex?p" border="0"/> correspondientes a cada prueba. 

<div>
<table>
  <tr>
    <th>Parámetro / Método de selección</th>
    <th>Sin ruleta</th>
    <th>Con ruleta</th>
  </tr>
  <tr>
    <td>Tamaño de población</td>
    <td>*</td>
    <td>0.0011</td>
  </tr>
  <tr>
    <td>Probabilidad de mutación</td>
    <td>0.0663</td>
    <td>0.2885</td>
  </tr>
  <tr>
    <td>Porcentaje de parejas de padres seleccionadas</td>
    <td>0.0234</td>
    <td>0.0041</td>
  </tr>
  <tr>
    <td>Número de generaciones</td>
    <td>0.3658</td>
    <td>0.0063</td>
  </tr> 
</table>
 <b>Cuadro 1.</b> Resultados de pruebas estadísticas para significancia de parámetros.
</div>

Donde el símbolo * significa un valor-<img src="http://latex.codecogs.com/svg.latex?p" border="0"/> de <img src="http://latex.codecogs.com/svg.latex?1.817\times10^{-5}" border="0"/>. Para ambos métodos de selección, el tamaño de la población y la cantidad de parejas seleccionadas para reproducirse son estadíticamente significativos. Los otros dos factores tienen significancia contrapuesta en cada caso. Haciendo uso de la los valores de las medianas de cada nivel y del diagrama de bigotes de la <a href="#fig5">Figura 2</a>, elegimos como una buena configuración la de tamaño de población 1000, probabilidad de mutación 0.1, cantidad de parejas 0.1 y número de generaciones 300. Además se analiza el caso de aumentar el tamaño de población y disminuir la cantidad de parejas seleccionadas pues hacia allá apunta (al menos por el momento) que el algoritmo tiene mejor desempeño.

 ## Reto 2: Método de supervivencia por ruleta
 <p align="justified">
 El segundo reto consta de extender la ruleta para seleccionar a los individuos que pasen a la siguiente generación; es decir, ahora la probabilidad de supervivencia es proporcional al valor objetivo. Aquí se hace la consideración de que primero se pasan los <img src="http://latex.codecogs.com/svg.latex?k" border="0"/> mejores individuos y los restantes se seleccionan de acuerdo a la ruleta. Esto para asegurar que los mejores individuos pasen a la siguiente generación. 
 
Los valores objetivo de las soluciones infactibles son escalados para que el mejor de los infactibles no supere al peor de los factibles. Con esta idea, las soluciones infactibles pueden pasar a la siguiente generación, pero con poca probabilidad. El código es el siguiente:

```R
elite <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
#penalizar a los infactibles
f.min=min(obj[fact])
nf.max=max(obj[!fact])
#se penaliza para que el mejor infactible este al 20% del peor factible
obj[!fact]=max(obj[!fact]-(nf.max-f.min*0.8),0)

#agregar k mejores
mantener=elite[1:floor(k*init)]

obj2=obj[setdiff(1:tam,mantener)]

ruleta=obj2/sum(obj2)
mantener <- c(mantener,sample(setdiff(1:tam,mantener),init-length(mantener),replace = FALSE,prob=ruleta))
p <- p[mantener,]
tam <- dim(p)[1]
assert(tam == init)
#reactualizar ruleta para la seleccion
ruleta=obj[mantener]/sum(obj[mantener])
factibles <- p[p$fact == TRUE,]
```


 
 Para medir la eficacia de la supervivencia por ruleta se realizó un experimento en donde, considerando el ajuste de parámetros previo, se consideraron los siguientes valores de los parámetros:
 </p>
 
 <ul>
 <li> Tamaño de población: 2000 y 3000</li>
 <li> Probabilidad de mutación: 0.1 </li>
 <li> Numero de parejas: 0.05 y 0.1 </li>
 <li> Número de generaciones: 300 </li>
 <li> Número de soluciones elite (<img src="http://latex.codecogs.com/svg.latex?k" border="0"/>): 0.05 y 0.1 </li>
 </ul>
 
 <p align="justified">
 La <a href="#fig5">Figura 5</a> muestra los diagramas de bigotes correspondientes al experimento con diez réplicas en cada tratamiento. El valor de <img src="http://latex.codecogs.com/svg.latex?k" border="0"/>, se utiliza sólo como variabilidad en la prueba pues no es un parámetro comparable. Aprovechando e intentando salir de dudas en la conclusión del Reto 1, se analizó también el caso del algoritmo original. En verde, aparecen los resultados cuando no se utiliza ruleta en la selección; en rojo, cuando se utiliza selección por ruleta y; en azul, el caso en que hay selección y supervivencia por ruleta.
</p>
<p align="center">
<div id="fig5" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p10/BoxplotSupervivencia.png" height="100%" width="100%"/><br>
<b>Figura 5.</b> Diagrama de bigotes para ambos métodos de supervivencia.
</div>
<p align="justified">
Note que no hay mucha diferencia entre los métodos de selección (para reproducción) y una prueba de Wilcoxon con un valor-<img src="http://latex.codecogs.com/svg.latex?p" border="0"/> de 0.41 justifica nuestra observación.

Respecto al método de supervivencia si se nota una clara mejora en la calidad de las soluciones, la cuál es justificada por una prueba estadística homóloga. De paso, podemos ver fácilmente como la mejor configuración de parámetros encontrada es con una población de tamaño 3000, 10% de parejas selccionadas para cruzamiento y, de acuerdo a una prueba de Kruskall y Wallis, un valor de <img src="http://latex.codecogs.com/svg.latex?k" border="0"/> de 0.1, correspondiente al 10% de individuos elite pasados de generación a generación.
 
 La <a href="#fig6">Figura 6</a> muestra la evolución del incumbente cuando se utiliza la supervivencia por ruleta (en rojo) y cuando no (en negro).
 </p>
<p align="center">
<div id="fig6" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p10/R2.png" height="100%" width="100%"/><br>
<b>Figura 6.</b> Desempeño del algoritmo con supervivencia por ruleta.
</div>

Note como la supervivencia por ruleta permite llegar a la solución óptima desde casi la mitad de la evolución, el caso de selección elitista (sin ruleta) tuvo un rápido acercamiento pero se quedó atorado en un óptimo local. Una vez más se aprecia como seleccionar soluciones que no sean tan buenas nos permite llegar a un futuro a mejores soluciones que con una selección completamente voraz.

Por último, se incluye los cambios de la densidad de valores objetivo durante la evolución. Por visualización éstos fueron escalados y no se muestran su valores en el eje horizontal (véase <a href="#fig7">Figura 7</a>). Note como en el caso en el que se permite supervivencia por ruleta la densidad se carga  a la derecha, decantando la deriva genética hacia la solución óptima y se aprecia una mayor variedad de soluciones; además, se aprecia e pico a la izquierda formado por las soluciones infactibles. En el caso de no usar supervivencia por ruleta, la deriva se atoró en un óptimo local con una curtósis muy alta (en el centro), aunque si se acercó a la solución óptima como ya vimos.

<p align="center">
<div id="fig7" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p10/R2_SinRuleta.gif" height="45%" width="45%"/>
 <img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p10/R2_ConRuleta.gif" height="45%" width="45%"/>
 <br>
<b>Figura 7.</b> Desempeño del algoritmo con supervivencia por ruleta.
</div>

</p>
