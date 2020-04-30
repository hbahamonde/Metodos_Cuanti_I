############################## 
# Cleaning
##############################
cat("\014")
rm(list=ls())


# Clase 4

## Prestige
install.packages("car") # instalemos la base de datos, que viene en la libreria "car"
library(car) # ahora, carguemos la base de datos, que viene en la libreria "car"



## Resumen del df
head(Prestige)

############################################
# Droping: botando variables/columnas
############################################

## Funcion Drop
install.packages("dplyr") 
library(dplyr)

## Botemos algunas columnas, y construyamos dos DF's distintos, pero que igual tengan una columna en comun

### que columnas tiene el DF Prestige?
colnames(Prestige)

### OK. DF1 tendra todo menos "education", "income", "women" 
Prestige.drop.1 = select(Prestige,-c(type, census, prestige))
colnames(Prestige.drop.1)

### OK. DF2 tendra todo menos "income"   "prestige" "census"   "type" 
Prestige.drop.2 = select(Prestige,-c(education, women))
colnames(Prestige.drop.2)

############################################
# Merging: añadiendo columnas
############################################

### Problema: ahora queremos hacer un "merge" (*pegar columnas*) de las dos DF's. El unico elemento en comun, es la variable "income".

# Antes habiamos "merged" usando una variable categorica ("woman"). Ahora "mergiemos" una variable numerica. Para ello, tenemos que asegurarnos que la variable en comun es exactamente igual. 
Prestige.drop.1$income == Prestige.drop.2$income # Ambas columnas son iguales.

# Esto funciona si tenemos poquitas obs. Pero si tenemos muchas, es mejor organizar el resultado en una tabla.
table(Prestige.drop.1$income == Prestige.drop.2$income)

# Que pasaria si revertimos el orden de una columna (usando "-c()"), entendera R que en verdad hablamos de lo mismo? Veamos
rev(Prestige.drop.1$income) == Prestige.drop.2$income # No, no son iguales. 

# En situaciones reales, nunca los datos estaran ordenados. Esta vez haremos un "merge desordenado".
# Primero, desordenemos "Prestige.drop.1$income". Ocuparemos un comando para hacer un "reverse", y poner todo al reves.
Prestige.drop.1$income = rev(Prestige.drop.1$income) # "rev" de "reverse"

# Veamos que hicimos...
View(data.frame(Prestige.drop.1$income, Prestige.drop.2$income))

# Ahora que sabemos que la columna "income" del DF Prestige.drop.1 esta desordenada, tratemos de "mergiar"
Prestige.merge <- merge(Prestige.drop.1, Prestige.drop.2, by=c("income"))

Prestige.merge

# Al ver Prestige.merge, comprobamos que aunque los datos esten desordenados, R los pegara.

# Pregunta: por que no es muy bueno pegar por la variable "genero" como lo hicimos la clase pasada?

############################################
# Appending: añadiendo filas
############################################

# En el ej anterior, anadimos columnas. Ahora anadamos filas. Es decir, alarguemos nuestra base datos.

# Pregunta: cuando nosotros podriamos necesitar algo asi?

# OK. Para mostrar como anadir filas, partamos la base Prestige en dos: en las profesiones con "alto" y "bajo" prestigio.
Prestige$prestige.2 <- ifelse(Prestige$prestige > mean(Prestige$prestige), "alto","bajo")


# creemos otro dataframe donde sólo estén las profesiones con el prestigio más altas. Usemos la var que habiamos creado antes ("prestige.2"). 
Prestige.alto <- Prestige[(Prestige$prestige.2=="alto"),]

# creemos otro dataframe donde sólo estén las profesiones con el prestigio más bajo.
Prestige.bajo <- Prestige[(Prestige$prestige.2=="bajo"),]

## Hemos partido el DF "Prestige" en dos. Ahora, volvamos a juntar ambas. Usemos las funcion "rbind" (por "row bind", que "pegador de filas")
Prestige.nuevo = rbind(Prestige.alto, Prestige.bajo)

# Necesitan estar las columnas ordenadas?

Prestige.alto.columnas.desordenadas = data.frame(
        # Me salto "education" que es primero, y la pongo al final
        income = Prestige.alto$income, # 2
        women = Prestige.alto$women, # 3
        prestige = Prestige.alto$prestige, # 4
        census = Prestige.alto$census, # 5
        type = Prestige.alto$type, # 6
        prestige.2 = Prestige.alto$prestige.2, # 7, siendo esta la variable que inventamos recien.
        education = Prestige.alto$education # 1, siendo esta variable la que va primero en Prestige.alto y Prestige.bajo
        
)

# Verifiquemos que las columnas estan desordenadas
colnames(Prestige.alto) == colnames(Prestige.bajo)

colnames(Prestige.alto) 
colnames(Prestige.alto.columnas.desordenadas)


# Ahora mergiemos Prestige.bajo (que sigue ordenada, i.e. con "education" primero) con Prestige.alto.columnas.desordenadas (que la "desordenamos" poniendo "education" al final)
Prestige.nuevo.ordenado.desordenado = rbind(Prestige.bajo, Prestige.alto.columnas.desordenadas)



############################################
# Transformaciones
############################################

hist(Prestige$income) # Grafico Basico: "histograma". Plotiemos el income de las distintas profesiones. Notas algo raro? 

hist(Prestige$income, breaks=100) # Veamos esto con mas detalles. Agreguemos mas breaks (cortes, en columnas). Notas algo raro? 

# Debido a este problema, tendremos que calcular el log de la variable "income".
# Este proceso se llama "transformacion" y se usa para "normalizar" los datos que presentan mucha dispersion. 

# Por que es importante "normalizar" una variable con mucha "dispersion"?
# Pista: "income" y "GDP" son usualmente transformadas.

# La transformacion mas basica, es el log con base 10.

## 1. Qué es el logaritmo de un numero? 
## 2. Por qué necesitamos sacarlo?
## 3. Cual es el problema que tenemos abajo?
log(Prestige$income) # que problema tenemos? 
## 4. Por qué una transformación no afecta un analisis estadístico?

# creemos otra variable ("income.log") que sea el logaritmo natural (de base 10), y peguemosla en la base de datos Prestige

Prestige$income.log = log(Prestige$income)

hist(Prestige$income, main="Antes", breaks=10)
hist(Prestige$income.log, main="Despues", breaks=10)

# Hemos solucionado un poco el problema.

# Ahora veamos el log de "women"

log(Prestige$women)

# Notas algo raro?

# Debenmos solucionar nuestro problema. Sumemos 1 a la variable women
Prestige$women.mas.uno = Prestige$women+1
## Inspeccionemos...
head(Prestige)
# 5. Por que esto soluciona nuestro problema?
# 6. Cual es el log de 1?
Prestige$log.women.mas.uno = log(Prestige$women.mas.uno)

head(Prestige) # Veamos...

hist(Prestige$log.women.mas.uno, breaks = 10) # Ahora grafiquemos de nuevo, y veamos si el problema se resuelve.


hist(Prestige$women, breaks = 10) # Comparemos con antes

# 7. Qué ganamos y que perdemos al sacar el log de una variable?
