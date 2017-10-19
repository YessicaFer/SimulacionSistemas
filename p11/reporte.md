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

