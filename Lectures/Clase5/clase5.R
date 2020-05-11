# Clase 5

cat("\014")
rm(list=ls())
graphics.off()


## Prestige
install.packages("car") # instalemos la base, que viene en la libreria "car"
library(car) # ahora, carguemos la base, que viene en la libreria "car"

############################################
# Histogramas: dos variables categoricas.
############################################


## Creemos una variable que diga si el sueldo ('income") es "alto" o "bajo"
Prestige$income.categorical <- ifelse(Prestige$income > mean(Prestige$income), "alto.sueldo","bajo.sueldo")

## Grafiquemos variables categoricas o cualitativas (o "factors" en lenguaje R). Son todos sinonimos: categoricas = cualitativas = factors.
plot(Prestige$income.categorical) 

# Ups..error. R no entiende, porque "alto" y "bajo" es texto. Añadamos "levels" (o "etiquetas" que R pueda entender).
Prestige$income.categorical = as.factor(Prestige$income.categorical)
Prestige$income.categorical #  Fijate que abajo dice "Levels...". Ok, problema solucionado.

# Ahora si, grafiquemos.
plot(Prestige$income.categorical)# Ok, este HISTOGRAMA dice que tenemos mas sueldos bajos. 

# Hay alguna relacion entre genero y sueldo? Veamos...

## Primero, creemos una variable donde si cierta profesion tiene mas mujeres que el promedio, es "alto." Si no, es "bajo"
Prestige$women.categorical <- ifelse(Prestige$women > mean(Prestige$women), "alto.mujer","bajo.mujer")

## Que tipo de variable acabamos de crear?
is(Prestige$women.categorical) # "Character", es decir, texto. No nos sirve para hacer analisis.

## Convirtamos esta nueva variable en categorica
Prestige$women.categorical = as.factor(Prestige$women.categorical)

## Plotiemos
plot(Prestige$income.categorical, Prestige$women.categorical) # Que podemos concluir?

############################################
# Boxplot: una variable categorica, la otra continua
############################################


## Este es un grafico histograma entre dos variables categoricas (o "factors").
## Que hubiera pasado si hubieramos ploteado una categorica (income/sueldo) con la version numerica de women? Veamos...
plot(Prestige$income.categorical, Prestige$women) # Este es un boxplot..."grafico de cajas". 

## Muestra: la mediana (50%), primer (25%) y tercer cuartil (75%). La caja muestra el 50% de la distribucion. La minima y la maxima. Outliers. 
## Que podemos concluir?


############################################
# Scatterplot: dos variables continuas (numericas)
############################################

# Cual es la relación entre prestigio y educacion? Veamos...

plot(Prestige$prestige, Prestige$income) # Que podemos concluir? 


############################################
# Density Plots
############################################
install.packages("dslabs")
library("dslabs")

install.packages("ggplot2")
library(ggplot2)

data("heights")

ggplot(heights, aes(height, fill=sex)) + geom_density(alpha = 0.2)

# 1. Que es una "densidad"?

# 2. Que podemos concluir de este grafico de densidad?

############################################
# Time series plots // Trabajando con tiempo...
############################################

## Veamos las ventas mensuales de una tienda de regalos en un hotel en Australia. 

## Los datos van de 1987 hasta 1993. 
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat") # R permite cargar bases de datos directamente de Internet.

# Declarar que el objeto "souvenir" (que no es nada mas q una secuencia de numeros, es un objeto de series de tiempo.
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1)) 
# El intervalo es 12 meses al año, comienza en Enero de 1987)

souvenirtimeseries

## Inspeccionemos la "base datos"
summary(souvenirtimeseries) # que tipo de objeto sera este?

is(souvenirtimeseries) # "ts" significa "time series", o "series de tiempo"...
### OK. No tiene una estructura de dataframe. De hecho, si van al panel de "Environment", "souvenir" esta en "Values" no en "Data".

# Plotiemos
plot.ts(souvenirtimeseries) # El Y-axis esta en notacion cientifica. Apaguemosla subiendo el umbral, poniendo un numero arbitrariamente alto.


options(scipen=999)
plot.ts(souvenirtimeseries) # Ahora plotemos de nuevo. Notas la diferencia del Y-axis? ("eje vertical").
