# Clase 16: Intervalos de Confianza
#################################################################
cat("\014")
rm(list=ls())
graphics.off()


# Cargar paquete para cargar bases que no son de R.
# install.packages("foreign")
library(foreign) # significa "foraneo"
dat <- read.dta("cow.dta") # cargar base en  R (hasta el momento solo la habiamos descargado).


# Estimar modelo lineal: relacion entre crecimiento economico y democracia, controlando por poblacion
modelo.1 = lm(rgdpch ~ democracy + pop, dat)

dat$log.pop = log(dat$pop)

# (1) Que significa "controlando por"?

# resumen del modelo
options(scipen = 1000000) # apagar notacion cientifica.
summary(modelo.1)

# Obtener intervalos de confianza
## Estos valores representan la incertidumbre de nuestra estimacion al promedio de cada variable, 
## manteniendo las otras variables independientes "constantes en sus medias".
confint(modelo.1, level = 0.95) # 95% de confianza
confint(modelo.1, level = 0.99) # 99% de confianza

# DIBUJAR EN EL PIZARRON LA REPRESENTACION DE QUE _SIGNIFICA_ 
# "EL 95%", Y DE POR QUE VEMOS LOS NUMEROS "2.5%" Y "97.5%"
# EN NUESTRA TABLA DE "CONFINT".

# Fijate que el intervalo de confianza de "democracia" va de ~9 a ~12
## "~" significa "aproximadamente". Esos valores representan, nuevamente,
## valores de nuestra "y" (es decir, "crecimiento economico", segun lo
## estimamos en nuestro "modelo.1".)

# Tambien podemos ver los coeficientes y su incertidumbre 
# en forma grafica tambien.

# plotear CI
# install.packages("coefplot", "ggplot2")
library(coefplot, ggplot2)

coefplot(modelo.1, strict=TRUE)


# (1) Compara los numeros de la tabla "confint" y lo que ves en el grafico.
# (2) Que es lo que vemos en este grafico?
# (3) Que podemos decir de los distintos coeficientes?
# (4) Que podemos decir respecto al ancho de los intervalos de confianza?
# (5) Que es mejor? Intervalos de confianza "angostos" o "anchos"?

# Sin embargo, hay algunas veces que queremos observar como esta incertidumbre
# varia a medida que recorremos la distribucion entera (no solo el promedio).
## Atencion: "coefplot" y la tabla de coeficientes ("summary(modelo.1)") 
## solo muestra la incertidumbre respecto al PROMEDIO ("el centro") de la distribucion.
## Lo que quiero decir, es que hay veces en que queremos ver como es que 
## la incertidumbre (es decir, los intervalos de confianza) VARIAN
## segun nos movemos a lo LARGO de la distribucion, es decir, cuando
## vemos mas que el solo PROMEDIO.

### ESTE PUNTO ES IMPORTANTE, y ASEGURATE DE ENTENDERLO BIEN.

# Veamos como se ve esta incertidumbre a lo LARGO de la distribucion.


# install.packages("effects")
library(effects)
plot(allEffects(modelo.1))

# (1) Que vemos en este grafico?
# (2) Que notan respecto al grado de incertidumbre?

# OK. Ya sabemos lo que los intervalos de confianza significan, y para que estan.
# Ahora calculemos intervalos de confianza a mano, y veamos de donde vienen
# estos numeros.


########################################################
# Como estimar manualmente los intervalos de confianza
########################################################

# Todo empieza estimando la Varianza del Error (sigma^2).

# Primero, obtengamos el error
e = as.vector(modelo.1$residuals)
e # es simplemente un vector con todos los errores (diferencia entre observado
# y predicho?)

# Segundo, veamos cuantos parametros estimamos. Sera importante para la formula.
k = 3 # numero de variables a estimar (incluyendo intercepto).
# Tenemos "democracy", "pop", "Intercept". Entonces, tenemos 3 parametros.

# sigma.2 (varianza del error)
# Varianza es una medida de la variabilidad o dispersion de un vector. 
# En este caso, del vector "e", que es nuestro error. 
# Matematicamente, es la suma de los elementos diagonales que resultan de multiplicacion entre el vector del error
# y el vector del error traspuesto, dividido por la cantidad de observaciones,
# menos el numero de parametros que estaremos estimando (3 en este ejemplo).
sigma.2 = (1/(nrow(dat)-k))*sum(diag(e %*% t(e)))


# Tercero, obtengamos la matriz "x". La matriz "x" representa lo que observamos.
## Recuerda que para calcular el intercepto,
## debemos poner una columna de 1's del mismo largo que la dimension
## de el resto de las variables
unos = rep(1, nrow(dat)) # repetir "1" 112 veces, que es el largo de la base de datos
# que lo acabo de calcualr con "nrow(dat)".

## OK, ahora creemos la matrix "x".
x = matrix(c(unos, dat$democracy, dat$pop), ncol=3) 
x # Ve como quedo.


# Cuarto, estimemos los errores estandard. 
# Es importante sacar los errores estandard porque necesitamos eso 
# para calcular los intervalos de confianza.

# Los errores estandard de hecho, salen en nuestra tabla de regresion.

summary(modelo.1) # "Std. Error"

# Para calcular los errores estandard, usaremos una vieja tecnica de matrices.

# Esta matriz es famosa, y se llama Matriz de Varianza-Covarianza.
# Las varianzas aparecen en la diagonal, y las covarianzas fuera de la diagonal.

library(matlib) # paquete para hacer operaciones con matrices.

# Calculemos la Matriz de Varianza-Covarianza
# Multiplicar sigma^2 por (x'x)^-1
options(scipen=999)
sigma.2 * inv(t(x) %*% x)

# Pero habia un camino mas corto...
vcov(modelo.1)

# Recapitulemos, esta matriz de Varianzas y Covarianzas 
# tiene elementos importantes para calcular el error estandard,
# que sera necesario para calcular el intervalo de confianza.

# Lo interesante es que la raiz cuadrada de la diagonal, son los 
# errores estandard.
sqrt(diag(sigma.2 * inv(t(x) %*% x)))

# Comprobemos. Los errores std. del "summary" debieran ser los mismos
# que los que estimamos nosotros manualmente.
summary(modelo.1)

# OK. Una manera de anotar esto, es extraer elemento 
# por elemento de lo que acabamos de calcular. 
sqrt(diag(sigma.2 * inv(t(x) %*% x)))[1] # error std. del intercepto
sqrt(diag(sigma.2 * inv(t(x) %*% x)))[2] # error std. de democracy
sqrt(diag(sigma.2 * inv(t(x) %*% x)))[3] # error std. de pop.


## Hablemos del error estandar
### La manera en la que debemos entender el error estandard (SE) asociado al 
### coeficiente (lo que vemos en "summary(modelo.1)") es como un ESTIMATIVO de 
### la desviacion estandard de la POBLACION.

### ATENCION: Poblacion es distinto a MUESTRA. Pensemos mas en esto...

### Pensemos en esto por un segundo. La razon por la que ESTIMAMOS, MODELAMOS, y 
### tenemos INCERTIDUMBRE, es precisamente porque NO PODEMOS acceder a la poblacion.
### Es decir, dado que es imposible tener TODOS LOS DATOS, nosotros trabajamos
### con MUESTRAS. Un ejemplo de esto, es una encuesta para conocer preferencias 
### politicas. Dado que no podemos entrevistar a todo el pais (POBLACION), entrevistamos
### a una MUESTRA, generalmente, aleatoria. 

### Entonces, el error estandar del coeficiente hace referencia a algo que 
### no conocemos.

### En ese sentido, es precisamente la informacion que necesitamos 
### para calcular los Intervalos de Confianza (nuestro tema de hoy).

### Antes de seguir, definamos los "grados de libertad"
### Se define como el numero de observaciones - 2.
grados.de.libertad = nrow(dat)-2

grados.de.libertad # ...veamos.

### La manera en que calculamos el intervalo de confianza,
### es la siguiente:

### Formula del intervalo de confianza
### coeficiente +/- t * SE del coeficiente.

### Hasta el momento, sabemos como derivar el coeficiente ( o "beta"). 
### Solo recuerda nuestros ej's en matrices:
### beta = (x'x)^-1x'y

### En la clase de hoy, aprendimos tambien a sacar el error estandard 
### y entendimos lo que significa.
### Recuerda, el SE se saca asi:
### sqrt(diag(sigma.2 * inv(t(x) %*% x)))
### Es decir, la raiz cuadrada del los elementos diagonales de la
### multiplicacion entre sigma^2 y x'x.

### Para completar nuestra formula de los intervalos de confianza,
### necesitamos un ultimo elemento, "t". 
### T es un valor critico que sacamos de una distribicion 
### llamada "t". 


#### Ocupemos una funcion para conseguir estos valores criticos.
t = qt(1-.05/2, grados.de.libertad) # valores criticos de t, a un 95% de confianza.

# "1-.05" porque queremos el 95% de la distribucion.
# Piensa en "la distribucion" como al area bajo la curva
# que representa "confianza". Es por eso que decimos 
# que "estamos 95% seguros/confiados que nuestra estimacion
# es cierta"
# Y es dividido por dos ("/2") porque queremos repartir esa
# confianza a ambos lados de la distribucion.

# 
10631.611789 + t * sqrt(diag(sigma.2 * inv(t(x) %*% x)))[1]  # (Intercept)  #  12127.13117506
10631.611789 - t * sqrt(diag(sigma.2 * inv(t(x) %*% x)))[1]  # (Intercept)  #  9136.09240230

#
1638.874546  + t * sqrt(diag(sigma.2 * inv(t(x) %*% x)))[2]  # democracy  # 2097.753
1638.874546  - t * sqrt(diag(sigma.2 * inv(t(x) %*% x)))[2]  # democracy  # 1179.996

#
0.002947     + t * sqrt(diag(sigma.2 * inv(t(x) %*% x)))[3]  # pop 
0.002947     - t * sqrt(diag(sigma.2 * inv(t(x) %*% x)))[3]  # pop 


# Pero nuevamente, siempre hay un camino mas corto.
confint(modelo.1, level = 0.95) # 95% de confianza

# Discutir
## (1) Por que 95 vs. 99? 
## (2) Logica frecuentista: limites del "supuesto asintotico". Es posible?