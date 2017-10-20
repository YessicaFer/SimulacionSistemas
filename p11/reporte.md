# Práctica 11: Frentes de Pareto

## Introducción
Cuando se intentan optimizar simultáneamente multiples objetivos, éstos se contraponen entre sí, pues mientra uno mejora los otros empeoran y viceversa. Si analizamos las soluciones de estos problemas y sus evaluaciones es fácil notar la dificultad de comparar en vía de encontrar la mejor pues, cuándo decimos que un vector es mejor que otro. Pues bien, existen soluciones que en sí misma cumplen la definición de optimalidad al poder asegurar que no existe solución que sea mejor que ella misma; sin embargo, también  existen soluciones que no se pueden comparar entre sí. Este conjunto  de soluciones es mejor conocido soluciones eficientes y su evaluación como frente de Pareto o conjunto de soluciones no dominadas. 

La penúltima práctica trata del análisis de las soluciones de un problema de optimización multiobjetivo. El análisis busca mostrar hasta cuantas funciones objetivo tiene sentido considerar, cómo encontrar un subconjunto de soluciones no dominadas que sea diverso a partir de un frente dado y como encontrar en frente de Pareto.

## Versión paralelizada
Dado un conjunto de soluciones y sus correspondientes evaluaciones, se puede encontrar el conjunto de soluciones que dominan al resto. En otras palabras, el grupo de soluciones que son mejores en todos los objetivos (mejor puede significar menor o mayor dependiendo del sentido del objetivo). Sin embargo; se debe ser conciente que éste no es el frente de Pareto, podríamos decir que es el frente incumbente. Pero en fin, el como encontrar el frente incumbente consiste en comparar cada par de soluciones en busca de quien domina a quien, lo cuál es pesado de hacer pero fácil de paralelizar:

```R
clusterExport(cluster,"val")
temp=parSapply(cluster,1:n,function(i){
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
  }
  cuantos <- sum(d)
  return(c(cuantos,cuantos==0))
})
dominadores = temp[1,]
no.dom = as.logical(temp[2,])
rm(temp)
```

Como extra, se puede paralelizar la creación de las funciones objetivo:
```R
obj <- parLapply(cluster,1:k,function(k){return (poli( md, vc, tc))})
```
y la evaluación de las soluciones:
```R
val=matrix(parSapply(cluster,1:(k*n),function(pos){
        i <- floor((pos - 1) / k) + 1
        j <- ((pos - 1) %% k) + 1
        return(eval(obj[[j]], sol[i,], tc))
      }), nrow=n, ncol=k, byrow=TRUE)
```

Para determinar si el uso de paralelismo en verdad disminuye el tiempo de ejecución se realizó un experimento computacional en donde se varía el número de soluciones que se generan en 100, 200 y 300. Se calcula el frente y se mide el tiempo de ejcución necesario. Por cada número de soluciones se realizaron 30 réplicas. Los resultados se pueden apreciar en la  <a href="#fig1">Figura 1</a> donde se observa con los diagramas de bigotes que la implementación paralela es más rápida. Por la diferencia de los tiempos en todos los niveles estudiados no es necesario hacer una prueba estadística. 


<p align="center">
<div id="fig1" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p11/secuencialParalelo.png" height="70%" width="70%"/><br>
<b>Figura 1.</b> Comparación de implementación secuencial y paralelo.
</div>
</p>

## ¿Cuántos objetivos considerar?
Hay una tendencia natural a aumentar el número de objetivos que se consideran simultánemante. Podría pensarse que se está haciendo más realista el problema, lo cuál puede ser cierto. Sin embargo; uno no puede ofrecer un buen método de solución al aire probándolo sobre problemas que decimos nosotros son mas realistas y le aumentamos los objetivos. ¿Porqué? la respuesta puede inferirse a partir de un ejemplo con esta sencilla forma de calcular un frente de Pareto. Se realizó un experimento en donde se varía el número de soluciones generadas en 100, 200 y 300. Además, se consideran de 2 a 16 objetivos simultáneamente y 30 réplicas por cada tratamiento. 

Los resultados del experimento aparecen en la <a href="#fig2">Figura 2</a>, los puntos a colores representan la densidad de los porcentajes de soluciones no dominadas.

<p align="center">
<div id="fig1" style="width:300px; height=200px">
<img src="https://github.com/eduardovaldesga/SimulacionSistemas/blob/master/p11/abejitas.png" height="95%" width="95%"/><br>
<b>Figura 2.</b> Efecto del número de objetivos en la densidad del frente de Pareto.
</div>
</p>

De aquí podríamos inferir algunas consideraciones a hacer a la hora de hacer un algoritmo multiobjetivo. Primero, no parece muy conveniente utilizarlo cuando se tienen más de cinco objetivos, ¿porqué? pues porque no tiene mucho sentido implementar un algoritmo que busque un subconjunto de soluciones que cada vez se parece más al conjunto original. Esto porque con seis objetivos nuestro subconjunto correspondería al 87% de nuestro conjunto original. Este comportamiento es fácil de visualizar a partir de las cajas de bigotes correspondientes a cada objetivo. 

Aunque estadísticamente de acuerdo a una prueba de Dunn con 0.001 de significancia los porcentajes de soluciones no dominadas son equivalentes a partir de ocho objetivos; se graficó hasta 16 objetivos para analizar el cambio en las densidades de los porcentajes. Note como la variabilidad va disminuyendo a medida que aumentamos los objetivos hasta que en 16 ya casi con seguridad toda solución es no dominada.

Observe como el porcentaje de soluciones no dominadas tiene poca variabilidad cuando hay dos objetivos. Ésta comienza a aumentar y en 4, 5 y 6 objetivos es altísima. Podríamos decir que 



Primero, note como para dos objetivos, la cantidad de soluciones no dominadas es muy pequeña, 18.3% en promedio. A medida que los objetivos aumentan, lo hace también el porcentaje de soluciones no dominadas. A partir de 11 objetivos, en promedio el 99% de las soluciones ¡son no dominadas! De igual forma, se puede analizar el cambio de las densidades respecto a los objetivos mostrando una gran variabilidad para4, 5 y 6 objetivos

