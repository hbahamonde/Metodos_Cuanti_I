# clase 8

#################################################################
# Limpiemos todo primero.
cat("\014")
rm(list=ls())
graphics.off()

# Segundo, un truco. 
## Este truco instala el paquete si es que no esta instalado, y lo llama usando "library".
## Si NO está instalado, lo instala, y despues lo llama usando "library".
## Es, básicamente, un instalador y cargador CONDICIONAL de paquetes.
## Este paquete que ejecuta esta accion condicional, se llama "pacman".

# Usuarios MAC: asegurarse de tener XQuartz instalado
# https://dl.bintray.com/xquartz/downloads/XQuartz-2.7.11.dmg

install.packages("cem")
library(cem)

if (!require("pacman")) install.packages("pacman"); library(pacman) # Ahora cargaremos "pacman"
p_load(cem) # y aqui le decimos a pacman que revise si tienes el paquete "cem" que utilizaremos para el sgte. ej. Si no lo tienes, lo instalará, y lo llamará.
# En resumen, p_load() reemplaza install.packages("paquete") y "library(paquete)".

# En la libreria "cem" vive una base de datos muy famosa, conocida como LaLonde. Carguemosla para la sgte. actividad.
data(LL)

# Veamos en que consiste. 
head(LL)

## Variables

# treated: Indicator variable for whether the participant received the treatment. 
# age: Measured in years. 
# education: Years of education. 
# black: Indicator variable for whether the participant is African-American. 
# married: Indicator variable for whether the participant is married. 
# nodegree: Indicator variable for not possessing a high school diploma
# re74: Real earnings in 1974. re75: 
# Real earnings in 1975. 
# re78: Real earnings in 1978. 
# hispanic: Indicator variable for whether the participant is Hispanic. 
# u74: Indicator variable for unemployed in 1974. 
# u75: Indicator variable for unemployed in 1975.


#################################################################
# Estadistica Descriptiva
#################################################################

# En esta seccion revisaremos los estadisticos descriptivos mas usados.


#################################################################
# Promedio (o media) (o "mean" en ingles).
#################################################################

# 1) Es la medida mas usada para medir la centralidad de la distribucion.
# 2) Todos los datos, como vimos, siguen una distribucion. Por ej., veamos la distribucion 
# etarea de los sujetos entrevistados en esta base de datos.

## Un histograma
histogram(LL$age) # El promedio debiera ser cercano a 20 años.

## Veamos ahora el promedio de esta variable usando el comando "mean()"
mean(LL$age)

## Redondear numeros usando el comando "round"
round(mean(LL$age), 1) # con un decimal.
round(mean(LL$age), 0) # con 0 decimales.

#################################################################
# Mediana (o "median" en ingles).
#################################################################

# Muestra el valor que vive al medio de la distribucion.
# Ventaja: es mas robusta (o resistente) a los valores extremos, si la comparamos con el promedio.
# Recuerda. Para calcular el promedio, debemos incorporar los valores extremos (los muy altos, y los muy bajos).
# Y esto afecta el promedio. No asi con la mediana: no cambia si agregamos valores extremos.
# En la distribucion normal, mediana = media (= moda).

# Calculemos la mediana
median(LL$age)

## Mas complejo. 

### Primero, ordenemos los numeros de mayor a menor. 
ll.age.ordenado = sort(LL$age)
ll.age.ordenado

### OK. Los valores estan ordenados de menor a mayor. Segundo, veamos que largo tiene este vector (usando el comando "lenght", que significa "largo").
length(ll.age.ordenado) 

length(ll.age.ordenado)/2 # OK. Ahora, veamos el elemento que vive al medio de esta lista de numeros.
# 361. 

## 1) OK. Es un numero impar. Que significa eso? 
## 2) Qué pasa con los numeros pares?

## OK. Ahora llamemos al elemento 361
ll.age.ordenado[361]

## Comprobemos que nuestro calculo esta correcto.
median(LL$age) == ll.age.ordenado[361] # OK.

##  Conclusion preliminar # 1
round(mean(LL$age), 0) > median(LL$age)
## Que significa eso? 
## "Skewness" positiva. Hay ciertos valores "extremos" que estan inflando el promedio (hacia arriba).

#################################################################
# Un resumen un poco mas completo
#################################################################

summary(LL) # de toda la base
summary(LL$age) # de la variable "age" ("edad")

# 1) Voluntari@ para dibujar un _________ en la pizarra? Usando esta informacioón, cómo se vería este gráfico?

#################################################################
# Tablas de Frecuencia
#################################################################

# Por ej., cuantos estan casados ("married") en nuestra base de datos?
table(LL$married)

# Otro ejemplo, cuantos Afro-Americanos estan casados?
table(LL$married, LL$black) # Siempre es table(FILA, COLUMNA). Ese orden (FILA, COLUMNA) siempre es asi en R.
## En ese sentido, los casados van en la fila, y los Afroamericanos en las columnas.

#################################################################
# Medidas de Dispersion
#################################################################

# Ademas de ver cuan centrada esta nuestra distrubucion, tambien queremos ver 
# cuan dispersas estan las observaciones del promedio.
var(LL$age)

# 1) Que significa eso?
# Como lo solucionamos?

# Solucion: desviacion estandard.
sd(LL$age)

## redondear
round(sd(LL$age),0) # 7

# La DS es la raiz cuadrada ("square root", comando "sqrt") de la varianza (comando "var"), que esta en escala cuadrada.
sqrt(var(LL$age)) == sd(LL$age) # Comprobando.

# Qué significa todo esto? 
## Existe la regla "68-95-99.7" (que aplica sólo) a distribuciones normales (nuestra variable de edad NO es normal...). 

## 68% de los datos caen dentro del rango de +/- 1 DS  de la media. Es decir: 
mean(LL$age) - round(sd(LL$age),0) # rango minimo
mean(LL$age) + round(sd(LL$age),0) # rango maximo

## 95% de los datos caen dentro del rango de +/- 2 DS's  de la media.
mean(LL$age) - 2*(round(sd(LL$age),0)) # rango minimo
mean(LL$age) + 2*(round(sd(LL$age),0)) # rango maximo

## 99.7% de los datos caen dentro del rango de +/- 3 DS's  de la media.
mean(LL$age) - 3*(round(sd(LL$age),0)) # rango minimo
mean(LL$age) + 3*(round(sd(LL$age),0)) # rango maximo

