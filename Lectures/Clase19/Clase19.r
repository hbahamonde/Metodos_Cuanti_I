# Clase 19: Goodness of fit (r2), predicción.
#################################################################
cat("\014")
rm(list=ls())
graphics.off()

#################################################################
# Motivacion
#################################################################

# Sabemos interpretar los coeficientes, sabemos cuando los coeficientes
# son significativos (estadisticamente hablando). Lo que no hemos 
# hablado hasta el momento, es como evaluamos distintos modelos: como podemos
# saber que modelo interpreta mejor la realidad?

# Matematicamente, queremos saber cuan bien nuestra linea se acerca/aleja
# de nuestros datos. Mientras nuestra linea de regresion se acerque mas,
# mejor nuestro modelo. 

# DEMOSTRAR EN LA PIZARRA.

# El estadistico que ocupamos para tener una idea de cuan bien nuestro
# modelo interpreta la realidad, es el r-cuadrado, o r2.


#################################################################
# r2: Definicion y Significado
#################################################################

# El r-cuadrado es un porcentage, que obviamente va de 0 a 1. Mientras 
# mas cercano a 1, nuestro modelo mejora. 

# Usualmente, se interpreta como el % de la varianza que es explicada
# por nuestro modelo. Por ej., un "r2 = 0.3" significa que nuestras variables
# explican el 30% de nuestros datos. 

# (1) Es eso "suficientemente bueno"? 
# (2) Que % nos da la "tranquilidad" de que tenemos un buen modelo?


# Por que es importante el r2? Bueno, asi podemos comparar que modelo
# explica mas la realidad. Imaginate que tenemos dos modelos, m1 y m2.
# Si el r2(m1) > al r2(m2), debieramos quedarnos con el m1. Por que?
# Porque el set de variables de m1 explica mas varianza (mas datos)
# al compararlo con el m2.

#################################################################
# r2: Viendo el r2
#################################################################

# install.packages("foreign")
library(foreign) # significa "foraneo"
dat = read.dta("https://github.com/hbahamonde/Metodos_Cuanti_I/raw/master/Lectures/Clase19/Obama.dta")

# Base
head(dat) # Base sobre preferencias politicas: Obama.

# 
dat$Obamafav <- as.numeric(jitter(dat$Obamafav, factor=2, amount = 1))
dat$ideology <- as.numeric(jitter(dat$ideology, factor=2, amount = 1))


# Modelo 1
options(scipen = 1000000) # apagar notacion cientifica.
modelo.entero = lm(Obamafav ~ income + education + age + sex + ideology, dat)
summary(modelo.entero)

# obtener el r2 de "modelo.entero"
summary(modelo.entero)$r.squared # Atencion: al summary le podemos pedir una parte especifica usando "$".

# Modelo 2
modelo.parcial = lm(Obamafav ~ ideology, dat)
summary(modelo.parcial)

# obtener el r2 de "modelo.parcial"
summary(modelo.parcial)$r.squared



# Modelo 3: este es un "mal" modelo
modelo.mediocre = lm(Obamafav ~ age, dat)
summary(modelo.mediocre)
summary(modelo.mediocre)$r.squared # Pesimo...

# (1) Que podemos concluir de los r2 respectivos de "modelo.entero" y "modelo.parcial"?
# (2) Con que modelo nos quedamos? Costo-beneficio, teniendo en cuenta la parsimonia.
# (*) Este es el tipo de decisiones que podemos tomar usando el estadiatico del r2.



# Graficos
## Grafiquemos nuestro "fit", es decir, cuan "en forma" esta nuestro modelo.

layout(matrix(1:3, nrow = 1)) # modifiquemos el plano donde graficamos.
# Aqui le decimos a R que "haga espacio" para 3 graficos, y que los ordene 
# en una sola fila. Asi podremos poner tres graficos, uno por cada modelo.

# Los siguientes graficos, pondran en el eje-X el "fitted" es decir, el predicho.
# El eje-y tendra el observado.

plot(dat$Obamafav, fitted(modelo.entero), col = "gray", main = "Modelo Entero"); curve((x), col = "blue", add = TRUE)
plot(dat$Obamafav, fitted(modelo.parcial), col = "gray", main = "Modelo Parcial"); curve((x), col = "blue", add = TRUE)
plot(dat$Obamafav, fitted(modelo.mediocre), col = "gray", main = "Modelo Mediocre"); curve((x), col = "blue", add = TRUE)

# (1) Notar que puse ";" en la misma linea.
# (2) Que podemos concluir de esto?


#################################################################
# r2: Formula Estadistica
#################################################################

# Hablemos de la formula del r2. 
# r2 = 1- (Errores Cuadrados del Residuo / Errores Cuadrados Totales)

# Errores Cuadrados del Residuo: Por cada fila, tomamos la diferencia entre
# predicho y observado. Eso al cuadrado, y despues sumamos todo. Despues, eso 
# lo dividimos por los errores (o variabilidad) de la variable dependiente. 
# Piensa que el valor esperado de "y" es su promedio. Entonces, el "error" en 
# este caso es cuanto se aleja cada fila (observacion) del promedio de "y". Y 
# finalmente, restamos 1 a ese numero final.


#################################################################
# r2: Matrices
#################################################################
options(scipen = 1000000) # apagar notacion cientifica.

# Saquemos el r2 del modelo.entero.

# Recuerda, este era nuestro modelo
#modelo.entero = lm(Obamafav ~ income + education + age + sex + ideology, dat)
summary(modelo.entero) # resumen
summary(modelo.entero)$r.squared # r2

# Si te fijas, los Errores del vector se construyen solamente ocupando la matriz "y"
# A diferencia de los errores del vector y, los Errores del Modelo
# incorpororan nuestros betas estimados y la matriz "x".

# Aqui creamos un objeto "x" que sera tipo "matrix". Ponemos todo dentro de 
# una columna "c", y esa columna, la dividimos en 6 columnas (ncol=6).
# Pero antes, creamos una fila de 1's para calcular beta0 (intercepto).
unos = rep(1, nrow(dat)) # Esto para sacar el intercepto.
x = matrix(c(unos, dat$income, dat$education, dat$age, dat$sex, dat$ideology), ncol = 6) 
head(x) # veamos como queda

# install.packages("matlib")
library(matlib) # para invertir matrices
# Como necesitamos los betas para usar nuestra formula de los Errores Cuadrados 
# del Modelo, es necesario calcular los betas. Ya conocemos esta formula...
y = dat$Obamafav
b = inv(t(x) %*% x) %*% t(x)%*%y # formula para sacar betas
b


# Formula del r2 en Matrices

## 1. Calcular los errores del modelo
options(scipen = 1000000) # apagar notacion cientifica.
e.m = y - x %*% b

## 2. Calcular los errores de la variable dependiente (Errores del vector y)
y.m = y - round(mean(y, trim = 0.5), 10)

## Calcular r2
### 1- e'e/y'y
1 - t(e.m) %*% e.m / t(y.m) %*% y.m

# Ahora hagamos lo mismo, pero usando algebra "normal", no de matrices.

# Formula Estadistica
y_fit <- modelo.entero$fitted.values # Fitted es lo que predecimos
SSE <- sum((y_fit - y)^2) # Aqui restamos lo que predecimos ("y_fit") menos lo que observamos ("y), y despues sacamos el cuadrado de esos numeros, y despues sumamos todas esas diferencias cuadradas. Esta cantidad se llama "Sum Square Error", y es eso: la suma de todos los errores cuadrados. 
SST <- sum((y - mean(y))^2) # Despues calculamos el "Sum Square Total". Estos son los "errores" o desviaciones que tiene nuestra matriz "y", y tiene que ver con cuanto cada elemento en "y" se desvia del promedio de "y".
1-SSE/SST  # R^2, que es 1 menos SSE/SST 
summary(modelo.entero)$r.squared # r2// Y que es esto mismo.


#################################################################
# r2: "Ajustado"
#################################################################

# Como dice King, el r2 es una funcion de los parametros (variables independientes).
# Mientras mas variables (o x's), mejor nuestro r2. Es por esto que 
# es muy comun "mentir" en estadisticas: pones un monton de x's, y mejoras
# tu r2. Mas acerca de esto, al final de la clase.

# Una manera de "arreglar" esto, es no mirar el r2, si no que el r2-ajustado.
# Este estadistico nos dice lo mismo que el r2, pero "ajusta" por el numero
# de x's que calculamos. En otras palabras, por cada parametro que calculamos,
# existe un "castigo".

# Recordemos nuestros r2's
summary(modelo.entero)$r.squared
summary(modelo.parcial)$r.squared
summary(modelo.mediocre)$r.squared

# Ahora calculemos el r(2)-ajustado
summary(modelo.entero)$adj.r.squared
summary(modelo.parcial)$adj.r.squared
summary(modelo.mediocre)$adj.r.squared

# Como ven, dice casi lo mismo, y el penalty es muy pequeno.


#################################################################
# Criticas al r2
#################################################################

# Sin embargo, pensemos por un momento en el r2 perfecto, y lo que 
# tendriamos que hacer para lograrlo.

# (1) Que tendriamos que hacer para lograr un r2 = 1?
# (2) Es realmente bueno/posible?




