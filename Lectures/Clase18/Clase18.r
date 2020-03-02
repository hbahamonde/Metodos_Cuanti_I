# Clase 18: Propiedades numéricas del OLS, Gauss-Markov, sesgo de variable omitida
#################################################################
cat("\014")
rm(list=ls())
graphics.off()

#################################################################
# Motivacion
#################################################################

# Ademas de trabajar con incertidumbre (intervalos de confianza, p-values, etc.), 
# la estadistica inferencial (lo que estamos haciendo este semestre) 
# descansa sobre supuestos muy importantes.

# Es importante conocer estos supuestos. Te daras cuenta que 
# la principal herramienta cuantitativa de las ciencias sociales 
# tiene muchos defectos y/o supuestos irreales. En consecuencia, siempre
# actua con prudencia y humildad respecto a tus resultados.


#################################################################
# Supuestos de los modelos lineales
#################################################################

# (1) El error tiene promedio cero: por eso SIEMPRE calcula los errores y plotealos.
# (2) El error tiene varianza constante: idem.
# (3) El error no esta correlacionado (los errores son independientes entre si): idem.
# (4) El error esta normalmente distribiuido: idem.
# (5) Las X's estan medidos sin error, y son independientes del error.
# (6) Las X's no son invariables, es decir, las X's son variables, no constantes.

# (*) Punto sobre la diferencia entre ERROR y RESIDUO:
## recuerda que y = b0 + b1x1 + e. 
## Ese "e" en realidad, es un residuo, no "error". "Parametro de poblacion."

#################################################################
# Teorema de Gauss-Markov
#################################################################

# el fundamento matematico de OLS (modelos lineales), es el 
# Teorema de Gauss-Markov.

# Consiste en lo siguiente: si
## (a) el error tiene promedio cero, y
## (b) si el error esta normalmente distribuido, y
## (c) si el error tiene varianza constante,
# entonces nuestros betas son los estimadores mas eficientes y sin sesgo.

# En ingles, es conocido como "BLUE": "best linear unbiased estimator".

# En otras palabras, cuando todos los supuestos funcionan, 
# la diferencia entre la MATRIZ QUE CONTIENE TODOS LOS BETAS CORRECTOS,
# Y LA MATRIZ QUE NOSOTROS TENEMOS ACCESO (O POODEMOS OCUPAR) ES CERO.

# (1) Punto del modelo "real"
# (2) Punto del epsilon y la letra "e"
# (3) Punto del beta y la letra "b"

# Son diferencias filosoficas. Importantes...pero filosoficas.


# Que puede salir mal?
## Ej.: los errores no son independientes; es decir, son dependientes. 
### Caso de las series de tiempo.


#################################################################
# Sesgo de Variable Omitida
#################################################################

# Todos los supuestos de OLS operan bajo otro supuesto: el modelo que 
# estimamos, es el "verdadero modelo". Es decir, no se nos queda ninguna
# variable afuera: i.e., todas las variables que debieramos ocupar, estan 
# consideradas. Si esto falla, todo falla, y tenemos "el sesgo de la variable
# omitida".

# Mal que mal, OLS es una ecuacion, y el valor de los betas cambiara si es que 
# no incluimos todas las variables que debieramos incluir. 

## ILUSTRAR EN LA PIZARRA.

# Para demostrar este punto, simularemos (inventaremos) unos datos. 


# Inventemos las variables independientes
set.seed(2019)
x1 = rnorm(1000, 10) # como nunca, conocemos que el promedio REAL de esta variable es 10
x2 = rnorm(1000, 20) # como nunca, conocemos que el promedio REAL de esta variable es 20
x3 = rnorm(1000, 30) # como nunca, conocemos que el promedio REAL de esta variable es 30

# Definamos que, como debiera ser, el error tiene promedio cero.
e = rnorm(1000, 0)

# Establecer el valor real de los betas: conocemos con exactitud los valores,
# porque nosotros los creamos!
b1 = 1
b2 = -100
b3 = 500

# El "VERDADERO MODELO": "y" es una combinacion lineal de todas las variables.
y = b1*x1 + b2*x2 + b3*x3 + e

# Hagamos una regresion.
modelo.completo = lm(y ~ x1 + x2 + x3)
summary(modelo.completo) # sin sorpresa, los betas son los mismos que inventamos nosotros.

# (1) La tabla de "Residuals" dice que la mediana es 0. Eso es genial. Pero sin sorpresas, nosotros inventamos todo, asi que obvio que resulta bien.
# (2) Plotiemos los errores

plot(modelo.completo$residuals, y) 

# Perfecto! No notamos ningun patron.
# Es decir, vemos una nube desordenada de puntos. 
# Aqui es cuando VEMOS lo que asumimos: los errores se cancelan entre si.



# Variable Omitida: Que pasa con nuestros coeficientes cuando se nos quedan dos
# variables afuera?

modelo.segado = lm(y ~ x1)

summary(modelo.segado) # Ya los resultados son muy diferentes.

# (1) Mediana de los residuos? 
# (2) Plotiemos los errores?

plot(modelo.segado$residuals, y) 

## Uff...vemos un patron clarisimo, y ya nuestra nube desaparecio. 
## (1) Cual es el patron? 


#################################################################
# Atencion
#################################################################

# Lo peor de todo, es que nunca sabras si te faltan variables o no.
# Es por esto que lo que guia el proceso de anadir/remover variables, es 
# teoria. Es un proceso casi artistico.
