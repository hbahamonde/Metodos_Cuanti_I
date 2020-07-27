# Clase 17: Test de hipótesis (t test), errores Tipo I y II, significación estadística (p-values).
#################################################################
cat("\014")
rm(list=ls())
graphics.off()

#################################################################
# Motivacion
#################################################################

# Hasta el momento sabemos interpretar las primeras dos columnas de nuestra tabla de regresion.
# Es decir, (a) los betas (Estimadores o Coeficientes) y (b) el error estandard. 
# Tambien sabemos (c) que son los intervalos de confianza y cual es su relacion con los 
# errores estandard. Sin embargo, hasta el momento no sabemos que son esas estrellas
# que aparecen en nuestra tabla.

# Esto es lo que aprenderemos hoy. El coeficiente nos dice el tamano del efecto.
# Sin embargo, hay una posibildiad de que estemos equivocados. Recuerda,
# nuestra logica es frecuentista. El 95% de confianza significa que si 
# el mismo fenomeno se repitiera 100 veces, 95 veces sucederia lo que estamos
# prediciendo. 

# Como hacemos este calculo? Esto esta muy relacionado a los intervalos de confianza. 
# Para esto, necesitamos hacer tests de hipotesis y CALCULAR
# CUAN CERCANOS O LEJANOS ESTAMOS DE QUE NUESTROS BETAS SEAN LO QUE NOSOTROS
# CREEMOS QUE SON. 


#################################################################
# Estimemos un modelo
#################################################################

# Cargar paquete para cargar bases que no son de R.
# install.packages("foreign")
library(foreign) # significa "foraneo"
options(scipen = 1000000) # apagar notacion cientifica.
dat = read.dta("https://github.com/hbahamonde/Metodos_Cuanti_I/raw/master/Lectures/Clase17/cow.dta")


# Hoy pensaremos en que factores ayudan a subir la poblacion en los paises.

# (1) Que relacion podria haber entre poblacion y democracia?
# (2) Que relacion podria haber entre poblacion y represion politica?

# Modelo
modelo.1 = lm(pop ~ democracy + repression, dat)

# resumen del modelo
options(scipen = 1000000) # apagar notacion cientifica.
summary(modelo.1)

# (1) Que podemos deducir de nuestro modelo? (Interpreta la tabla)

# Checklist: (SIEMPRE EJECUTAR ESTE CHECKLIST)
## (1) Interpretar estimadores.
summary(modelo.1)

## (2) Observar los errores estandard.
summary(modelo.1)

## (3) Checkear normalidad de los residuos.
plot(modelo.1$residuals, # en el eje Y va el error (los "residuales"): nuestra prediccion.
     dat$pop # en el eje X va el valor observado: lo que es.
     ) 

## (4) Checkear significancia estadistica (asunto de hoy).
summary(modelo.1)


#################################################################
# Significancia Estadistica: p-values
#################################################################

## Significancia: en simple, es la probabilidad de que nuestros betas sean 0.
## Es decir, la probabilidad de que, por ej., beta(1) no sea 2345, si no que 0.
## En este caso, hay un 69,84% de probabildiasd de que el efecto sea 0. 
## Asi mismo, hay un 24.41% de que beta(2) no sea 18707, si no que sea 0.

## (1) Hasta que punto debieramos rechazar la idea de que b=0? Cual es el 
## % magico que nos diga "ok, estoy casi segur@ de que b1=0 y que b1=2345?

## Estrellas denotan significancia. 
### * = la probabildiad de que el beta sea 0 es igual/menor al 1%
### ** = la probabildiad de que el beta sea 0 es igual/menor al 0.1%
### *** = la probabildiad de que el beta sea 0 es igual a 0%

## Otros niveles:
### . = la probabildiad de que el beta sea 0 es igual al 5%

## (4) Checkear significancia estadistica: 
### Que podemos decir de la significancia estadistica de nuestros coeficientes?

#################################################################
# Apliquemos lo que sabemos respecto a Intervalos de Confianza
#################################################################

# p-values e intervalos de confianza estan altamente relacionados.

# Plotear modelo
# install.packages("coefplot")
library(coefplot)
coefplot(modelo.1, strict = T) # plot de coeficientes

# mostrar el detalle de los intervalos de confianza
confint(modelo.1, strict = T)


#################################################################
# Pero ocurre algo muy raro aqui....?
#################################################################

# (1) Que similitudes encontramos entre nuestra tabla de regresion, 
# "confint" y "coefplot"? Las tres herramientas nos estan comunicando el mismo
# mensaje de alerta.... o no?

# (2) Pero que diferencias encontramos entre la tabla, coefplot/confint?

# Veamos mas en detalle el problema...
# install.packages("effects")
library(effects)
plot(allEffects(modelo.1))

# (3) Que podemos concluir? 

#################################################################
# OK, misterio resuelto. Y es por esto que necesitamos pensar en P-values
#################################################################

# En nuestra tabla de regresion, el "p-value" esta dado por la probabildiad de que
# el "valor t" ("t value") sea mayor al numero absoluto de t (en la tabla,
# esta denotado por "Pr(>|t|)").

# Volvamos a mirar la tabla
summary(modelo.1)

# Vamos por parte.
# Primero, veamos lo que el p-value es (los numeros en la columna  "Pr(>|t|)").

# (1) Un p-value es la probabilidad de que nuestro efecto sea "falso".
# Esto en matematicas significa que nuestro efecto sea realmente "0".

# (2) Como es una probabilidad, el p-value (o valor "p") es una escala
# que va de 0 a 1. 

# (3) En ciencias sociales, existe la convencion de que un p-value 
# superior al 5%, se cuenta como "falso." Es decir, cuando nuestro p-value,
# que es nuestra probabilidad de que nuestro efecto sea falso/cero, es mayor a 5%,
# concluimos que el efecto es "realmente" cero. Ahora, si el p-value es menor 
# a 5%, consideramos que la probabilidad de que sea falso es chica, y podemos
# creer que nuestro efecto en realidad es verdadero/no-cero. 


# Recuerda: 5% significa 5/100=0.05. Entonces, un p-value menor a 0.05 indica que 
# nuestro efecto es verdadero/no-falso, y un p-value mayor a 0.05 indica que nuestro 
# efecto es falso/0. Es por esto que los p-values de los coeficientes que son 
# menores a 0.05 tienen estrellas, y los que que son mayores no tienen. 


#################################################################
# Estadistico T
#################################################################

# Lo siguiente es el "t value" o "valor t" (o "estadistico t").
# El t value es el coeficiente/error estandard.

48303/19052
2345/6036
18707/15973

# Esto significa que cuando el el error estandard es grande (lo que es "malo"), 
# t value se vuelve mas chico. Nosotros queremos lo contrario. Ojala t values
# que son cercanos a dos. En general, esos t values tienen un p-value < 0.05.

# En otras palabras, el t value es una medida de dispersion de nuestra estimacion.
# Mientras mas dispersa, mas alta es la probabilidad de que--en su dispersion--
# nuestra estimacion toque el cero. 

#################################################################
# Al pensar en los p-values y los intervalos de confianza,
# hemos tratado de minimizar nuestros Errores Tipo 1 y 2.
#################################################################

# Sesgo en ciencias sociales: en favor de la "hipotesis nula" (es decir, no hay efecto).
# El p-value es el humbral en el que rechazamos la "hipotesis nula". Matematicamente,
# una "hipotesis nula" significa que nuestro beta = 0. Es decir, "no hay efecto".

# (1) Por que? 


# Error Tipo 2: falsos negativos.
# Error Tipo 1: falsos positivos.

# (2) Definir ambos.


#################################################################
# Pensamiento Critico
#################################################################

# P-values.
#----

## (1) Criticas al p-value. Por que 5% y no 3% por ejemplo?
## (2) Comunicando resultados: tablas con CI en vez de estrellas.

### Tabla que incorpora estas criticas
# install.packages("texreg")
library(texreg)
screenreg(modelo.1, ci.force = T) # nuestro paquete "screenreg" que ya conocemos,
# pero ahora, forzando los intervalos de confianza ("confidence intervals"), por 
# eso, "ci.force = T" ("T" por "true" o "verdad" o "si").



# Asintotica 
#----

## Asintotica, recordemos, es la idea frecuentista de tener 
## una base de datos infinita. Como esto es imposible,
## "el mal menor" es tener la mayor cantidad de datos: mientras mas, mejor.

### Demostrar en la pizarra que esto puede llegar a conducir a otros problemas.
### Mas datos, varianza es mas chica, el valor esperado mas preciso.


#### (1) Mientras mas datos tenemos, nuestras distribuciones se ponen mas perfectas, 
#### delgadas, y precisas. Eso hace que encontremos independencia estadistica mas seguido.
#### (2) Falsos positivos! 
## Tabla convencional
library("texreg")
screenreg(modelo.1, ci.force = F) # notan la diferencia?

