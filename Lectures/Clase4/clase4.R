# Clase 4

## Prestige
install.packages("car") # instalemos la base de datos, que viene en la libreria "car"
library(car) # ahora, carguemos la base de datos, que viene en la libreria "car"

## Resumen del df
summary(Prestige)
head(Prestige)

## Veamos como se ve toda la base...tendremos la vista de "Excel".
View(Prestige)

############################################
# Etiquetas y Recoding
############################################

### education: Average education of occupational incumbents, years, in 1971.
### income: Average income of incumbents, dollars, in 1971.
### women: Percentage of incumbents who are women.
### prestige: Pineo-Porter prestige score for occupation, from a social survey conducted in the mid-1960s.
### census: Canadian Census occupational code.
### type: Type of occupation. A factor with levels (note: out of order): bc, Blue Collar; prof, Professional, Managerial, and Technical; wc, White Collar.

## Funcion if/else
ifelse(Prestige$prestige > mean(Prestige$prestige), "alto","bajo")


## Revisemos
View(Prestige)


## Etiquetas (o "levels")
is(Prestige$type) # que tipo de objeto es este?
### ATENCION: siempre es BASEDEDATOS$VARIABLE.

levels(Prestige$type) # que etiquetas tiene?

## Recodificar: de ENG a ESP
install.packages("plyr") 
library(plyr)

Prestige$type.2 <- revalue(
  Prestige$type, 
  c("bc"="tecnico", 
    "prof"="profesional", 
    "wc"="oficinista")
  )

# comprobemos
Prestige$type.2

############################################
# Droping: botando variables/columnas
############################################

## Funcion Drop
install.packages("dplyr") 
library(dplyr)

## Botemos algunas columnas, y construyamos dos DF's distintos, pero que igual tengan una columna en comun

### que columnas tiene el DF Prestige?
colnames(Prestige)

### OK. DF1 tendra todo menos type, census, prestige y prestige.2
Prestige.drop.1 = select(Prestige,-c(type, census, prestige, prestige.2))
colnames(Prestige.drop.1)

### OK. DF2 tendra todo menos type, census, prestige
Prestige.drop.2 = select(Prestige,-c(education, women))
colnames(Prestige.drop.2)

############################################
# Merging: añadiendo columnas
############################################

### Problema: ahora queremos hacer un "merge" (*pegar columnas*) de las dos DF's. El unico elemento en comun, es la variable "income".
Prestige.merge <- merge(Prestige.drop.1, Prestige.drop.2,by=c("income")) 

############################################
# Appending: añadiendo filas
############################################

# creemos otro dataframe donde sólo estén las profesiones con el prestigio más altas. Usemos la var que habiamos creado antes ("prestige.2"). 
Prestige.alto <- Prestige[(Prestige$prestige.2=="alto"),]

# creemos otro dataframe donde sólo estén las profesiones con el prestigio más bajo.
Prestige.bajo <- Prestige[(Prestige$prestige.2=="bajo"),]

## Hemos partido el DF "Prestige" en dos. Ahora, volvamos a juntar ambas. Usemos las funcion "rbind" (por "row bind", que "pegador de filas")
Prestige.nuevo = rbind(Prestige.alto, Prestige.bajo)

table(Prestige.nuevo$prestige.2)
############################################
# Transformaciones
############################################

hist(Prestige$women) # Grafico Basico: "histograma". Plotiemos el % de mujeres en las distintas profesiones. Notas algo raro? 

hist(Prestige$women, breaks=100) # Veamos esto con mas detalles. Agreguemos mas breaks (cortes, en columnas). Notas algo raro? 

# Debido a este problema, tendremos que calcular el log de la variable "women"
## 1. Qué es el logaritmo de un numero? 
## 2. Por qué necesitamos sacarlo?
## 3. Cual es el problema que tenemos abajo?
log(Prestige$women) # que problema tenemos? 
## 4. Por qué una transformación no afecta un analisis estadístico?

# creemos otra variable ("women.log") que sea el logaritmo natural (de base 10), y peguemosla en la base de datos Prestige
# Pero primero, debenmos solucionar nuestro problema. Sumemos 1 a la variable women
Prestige$women.mas.uno = Prestige$women+1
## Inspeccionemos...
head(Prestige)
# 5. Por que esto soliona nuestro problema?
# 6. Cual es el log de 1?
Prestige$log.women.mas.uno = log(Prestige$women.mas.uno)

head(Prestige) # Veamos...

hist(Prestige$log.women.mas.uno, breaks = 10) # Ahora grafiquemos de nuevo, y veamos si el problema se resuelve.


hist(Prestige$women, breaks = 10) # Comparemos con antes

# 7. Qué ganamos y que perdemos al sacar el log de una variable?
