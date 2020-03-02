# Clase 14
#################################################################
cat("\014")
rm(list=ls())
graphics.off()




# En esta clase, trabajaremos con una base famosa que se llama 
# "Correlates of War". Es ampliamente usada para estudiar conflicto armado,
# uno de los temas que estudia la ciencia politica. 

# En esta clase pensaremos en la siguiente pregunta: cual es la relacion entre 
# represion politica y democracia. La unidad de analisis es el pais. 
# Quienes reprimen mas, paises democraticos o dictatoriales? Para responder,
# deberemos volver a pensar en beta1.

# Abramos la base de datos. Los formatos ".dta" son de STATA. Esa es otra
# ventaja de R: R puede abrir bases de datos de STATA (pero STATA no puede
# abrir bases de datos de R, ni de nada mas que no sea STATA).

# Cargar Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman)

# Cargar paquete para cargar bases que no son de R.
p_load(foreign) # significa "foraneo"
dat <- read.dta("cow.dta") # cargar base en  R (hasta el momento solo la habiamos descargado).

# Siempre inspeccionar base
options(scipen=9999999)
head(dat)

# country Country Name
# ccode: country code
# pop: Population in thousands
# rgdpch: Real GDP/capita
# democracy: measure of democracy 
# vdiss: Violent dissent based on terrorist events and fatalities.
# repression: Violations of physical integrity rights.

# Otro truco: graficar la base entera.
plot(dat)

########################
# Regresion Simple
########################

# Corramos una regresion bivariada, es decir, una sola variable independiente.
# Es "bi"-variada porque inspecciona la relacion solo entre DOS variables:
# la dependiente y la independiente.

reg.bivariada = lm(repression ~ democracy, dat)
summary(reg.bivariada) # Hoy solo miraremos los coeficientes. Otro dia veremos el error std.


# (1) Como podriamos interpretar esto? 
# (2) Como podriamos escribir esto en forma de ecuacion?
# (3) Como podriamos escribir esto en forma de matrix?

# Calculando el error. El error es la diferencia entre lo que predecimos
# y lo que vemos. Es decir entre "y" (niveles reales de represeion) 
# y "y'" (y prima), (niveles predecidos de represion). 

# Primero, calculemos y'
dat$repression.prima = predict.lm(reg.bivariada)

# Segundo, calculemos el error:
dat$error = dat$repression-dat$repression.prima

# Veamos...
head(dat)

# Ok. Esto implica lo siguiente:
## (1) Hay mejores modelos que otros. Los mejores, son los que tienen menos error.
## (2) El error representa todo lo que no podemos conocer ni medir apropiadamente. Ejemplos?
## (3) El error esta normalmente distribuido y debe tener promedio 0.
  ## (3.1) Filosoficamente: lo que no conocemos, no es sistematico (es al azar).
  ## (3.2) Matematicamente: lo que no conocemos, debe tener promedio 0.

# Graficando el error
p_load(lattice) # Paquete para graficos
densityplot(dat$error)
# Muy cerca del 0.

# Veamos mas en detalle:
summary(dat$error) # Que promedio tiene el error? 

# Puntos importantes:
# (1.1) Siempre se dice que "los errores se cancelan mutuamente"
# (1.2) Ej.: error de medicion en encuestas (mentirosos). 
# (2.1) En este caso igual:

head(dat$error, 50) 

# Una manera grafica de ver el error, es ver un scatter plot
# entre el error y el valor real.

plot(dat$error, dat$repression)

# (1) Como seria un grafico donde no hay error? 
# (2) Como pensar en la "normalidad" del error graficamente?

# (3.1) Un "buen" error "no tiene patrones claros".
# Dibujar un ejemplo.


# (4) Razones por las que incrementa nuestro error? 

########################
# Regresion Multiple
########################

# Regresion "multiple" o "multivariada" tiene mas de una variable "independiente".

# Pensemos en la idea de que una variable sea "independiente".
# Para ello, pensemos en nuestra ecuacion lineal de nuevo.

# y = b0 + b1x1 + e (esta es bivariada: un solo "x")
# y = b0 + b1x1 + b2x2 + b3x3 + e ("multivariada" o "multiple").


# (1) Por que se llama "independiente"?
# (2) Importa en que orden vayan las variables?
# (3) Cuantas variables independientes podemos tener?
# (4) Que variables independientes NO PODEMOS tener?

# (4.1) No podemos poner la misma variable a los dos lados del "="
# Es decir, no podemos estimar "y ~ y" (o "x ~ x").

# Pensemos en las notas del ramo de Matematicas de Pedro y Juan.
pj = data.frame(
  nombre=c("Pedro", "Juan"),
  nota.en.matematicas=c(3,5)
  )

pj # ve como se ve el objeto "pj".

lm(pj$nota.en.matematicas ~ pj$nota.en.matematicas) # Error


# (4.2) La misma variable independiente dos veces:

# Es decir, y = x1 + x1 NO SE PUEDE. Veamos por que.

# Volvamos a Pedro y Juan (y dejemos a Diego fuera mientras).

# Pensemos nuevamente en las notas en Matematicas de Pedro y Juan.
data.frame(
  nombre=c("Pedro", "Juan"),
  nota.en.matamticas=c(3,5)
  )

# Ahora, entremos las notas de Pedro y Juan en una matriz DOS VECES.
# La matriz se llama "x".
x = matrix(
  c(3,5,3,5),
  ncol = 2
  )
x

# Fijate que la primera y la segunda columna significan lo mismo ("notas en matematicas").

# Imaginate que la columna 1 es Pedro, y la columna 2 es Juan.

# Volvamos al modelo. Queremos ver que la ecuacion de regresion con dos variables
# independientes que son identicas es imposible. Es decir, esto es imposible:

# y = b0 + b1x1 + b2x1 + e
# donde "x1" es "Notas en Matematicas" (fijate que x1 que entro dos veces).

# Tratemos de estimar los betas via matrices.
# (x'x)^-1x'y (esta es la formula...solo para recordar).
# Ahora veamos si es posible hacer esto cuando estimamos el modelo x = x.

# Transponer x
t(x) # Ok, es posible

# Multiplicar x'x
t(x) %*% x # Ok, es posible

# Ahora, siguiendo la formula, tratemos de inviertir (x'x)
library(matlib) # para invertir matrices
inv(t(x) %*% x) # Matriz no es invertible
## Esto significa que la determinante de la matriz es 0.

### EJEMPLO EN LA PIZARRA

# Es por esto que lm() no sabe muy bien que hacer con este problema. Veamos.

## Intento 1
summary(lm(repression ~ democracy + democracy, dat)) # Que paso aqui?

# Forcemos a R a hacerlo
# Intento 2
dat$democracy2 = dat$democracy # Enganando a R...
summary(lm(repression ~ democracy + democracy2, dat)) # Que paso aqui?

# Nuevamente det(x't)=0.

# Tecnicamente, aqui tenemos problemas de "colinearidad perfecta"

# Ejemplo 1: Colinearidad Perfecta (de nuevo)
m1 = matrix(c(3,5,3,5), ncol = 2) # Define matrix "m"
m1 # ve como se ve la matrix "m1"

# El resultado sera muy cercano a 0, y sera un numero muy 
# pequeno, por lo que debemos apagar la notacion cientifica

options(scipen=10000) # Apaga la notacion cientifica

# toma la determinante de matrix "m1"
det(m1) # da practicamente 0. Eso es "malo" (la matriz no es invertible).


# Ejemplo 2: Multicolinearidad (que es diferente a Colinearidad Perfecta)
# Colinearidad perfecta, es precisamente eso, "perfecta"
# Multicolinearidad es la version mas ligera de "colinearidad perfecta".

# ejemplo
m2 = matrix(c(3,5,6,10), ncol = 2) # Define matrix "m"

m2 # llama a la matrix "m2" para verla

# Notan algo especial en esta matrix?

# el segundo vector de x2 es una "combinacion lineal" del primer
# vector: cual combinacion lineal?


# El resultado sera muy cercano a 0, y sera un numero muy 
# pequeno, por lo que debemos apagar la notacion cientifica

options(scipen=10000) # Apaga la notacion cientifica

# toma la determinante de matrix "m"
det(m2) # mismo problema. |m2| = 0, o "la determinante de m2 es cero, 
# por lo que m2 no es invertible"


# En ambos ejemplos ("m1" y "m2"), si la determinante es 0, la matrix no es
# invertible, y no podemos hacer la regresion.

# (1) En que casos REALES podemos tener problemas de 
# colinearidad perfecta?

# (2) Por que es MUY COMUN tener problemas de colinearidad "imperfecta"
# en ciencias sociales?

# OK, sigamos.

# Estimemos un modelo lineal multivariado.

# (1) Que mas podria ser importante para explicar/predecir represion?

colnames(dat) # "column names)

# "pop" (poblacion)?
# "rgdpch" (crecimiento economico)?
# "vdiss" (terrorismo)?



reg.multivariada = lm(repression ~ democracy +, dat) # Completar


summary(reg.multivariada) # Hoy solo miraremos los coeficientes. Otro dia veremos el error std.




# Primero, calculemos y'
dat$repression.prima2 = predict.lm(reg.multivariada)

# Segundo, calculemos el error:
dat$error2 = dat$repression-dat$repression.prima2 # error es la resta entre 
# lo que predecimos (predict.lm(reg.multivariada)) y lo que observamos (dat$repression).

# Veamos...
head(dat)

# Graficando el error
p_load(lattice) # Para graficos
densityplot(dat$error2)

# Summary ("Resumen")
summary(dat$error2)

# Pensemos en los coeficientes (los betas)

# (1) Como se leen los betas ahora que tenemos mas variables?
# (2) Que significa "constantes en el promedio"?  


########################
# Mostrar resultados: tabla y grafico (coefplot)
########################

# Incertidumbre es el fantasma de este curso. 
# Nunca sabemos si tenemos el "modelo verdadero".
# Es por esto que siempre hacemos mas de un modelo, y 
# mostramos mas de un modelo en nuestro trabajo. 

# Hagamos una tabla para mostrar nuestros resultados.

p_load(texreg) # paquete para hacer tablas con varios modelos.

screenreg( # funcion
  list( # ponemos los dos modelos en una lista, que es otro objeto que R ocupa.
    reg.bivariada, # nuestro primer modelo
    reg.multivariada # nuestro segundo modelo
    )
  )

# (1) Como interpretamos esta tabla?

# Tambien podemos graficar nuestros coeficientes
p_load(coefplot)
multiplot(
  reg.bivariada, # nuestro primer modelo univariado
  reg.multivariada # nuestro segundo modelo multivariado
  )

# (1) Como interpretamos esto?


# Planteado para la proxima clase: 
## (a) intervalos de confianza.
## (b) linea de cero (significancia estadistica)