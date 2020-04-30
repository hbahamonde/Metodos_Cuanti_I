# Clase 2


# 1+2
## Esta linea es un comentario. No ejecuta nada. 
## Destacala, y aprieta "Run" (que significa, "correr").
## Solo aparece en la consola, sin hacer nada.

## En cambio si escribo sin comentario lo siguiente:
3+3
## y aprieto "Run", se ejecuta el resultado.

10/2

10^2

(3+3)/2

##################
## paquetes
##################

## Instalando paquetes
install.packages("ggplot2") # Esta es la libreria de graficos mas potentes que existe. 
library(ggplot2)


### Anda a "Packages", y busca la libreria que acabamos de instalar. 
### En el buscador, escribe "ggplot2", y comprueba que hay un ticket al lado de "ggplot2".
### Ahora, desinstala la librería, des-tickeando "ggplot2".
### Lo que acabas de hacer es "desatachar" la libreria. 
### Ahora, atachala nuevamente. Fijate que la consola dice "library("ggplot2"). 


##################
## Ayuda, y otras funciones
##################

# Problema: necesitas crear una secuencia de numeros, por ej. del 1 al 10. Ocupa la funcion "seq". Pero no sabes ocuparla.
help(seq)
help(secuencia)

# Ahora, la pestana "help" se abre. Lee el menu de ayuda.
# Ahora, escribamos la funcion.
seq(from = 1, to = 10)

## Atencion: fijate en la coma en cada parte del argumento.

# Supongamos que necesitabamos esta freq. cada dos numeros. Usemos el argumento "by"
seq(from = 1, to = 10, by=2)

# Y de mayor a menor?
seq(10, 1, -2)
## Fijate que tuvimos que especificar que la secuencia va de 10 a 1, en vez de 1 a 10.

# Esas eran frecuencias. Ahora veamos repeticiones.
## help(seq) asume que tu sabes que la funcion se llama "seq". Que pasa si no tienes idea?
## Pidamos ayuda en general...Lo que necesitamos es "repeat", o "repetition", o "repeated"....Sabemos que empieza con "rep"
??rep
## Ahora encuentra lo que buscas ("Replicate Elements of Vectors and Lists"). Es de la "base" de R (la estructura basica de R, es decir, que no es un paquete aparte). Click on it. Hemos accedido a la ayuda de la funcion "rep". Repitamos algo importante 100 veces.
rep("esta es la mejor clase del semestre", 100)


# combinemos las funciones seq y rep. Usemos una estructura "anidada" (i.e. una funcion dentro de otra).
rep(seq(1, 4, 1), 5)


##################
## Objetos
##################

# La ventaja de R es que esta orientado a "objetos". Creemos uno.
abc <- 3

# Aqui, acabamos de crear el objeto "abc", al que le hemos asignado un numero (el 3).
# Ahora "llamemos" el objeto "abc"
abc
## Fijate que en la consola aparece "[1] 3". Eso significa que solo hay una linea "[1]", que contiene el objeto 3.
## Ahora creemos un objeto, pero que en vez de contener un numero, contenga texto.
tres <- "cuatro"
## Llamemos al objeto "tres"
tres
## Fijate que el texto entra como texto cada vez que le ponemos comillas. Mira:
tres <- "3"
## Ahora 3 es un texto, en vez de un numero. Llamalo.

## Lo importante, es que nosotros podemos trabajar con estos objetos. Recuerda que el objeto "abc" contiene el numero tres. Mira:
abc + abc
## Acabamos de sumar el objeto "abc". Ahora tratemos de elevar el objeto "abc" al cuadrado, y guardar ese resultado (9) en otro objeto.
abc.al.cuadrado <- abc^2
## Llama al objeto "abc.al.cuadrado"
abc.al.cuadrado

## Ahora, revisa el "Environment" (el medio ambiente) que creaste. Esa ventana contiene todos los objetos que creaste. Revisa que tienes:
ls()

## borra todo lo que creaste.
rm(list=ls())

## revisa nuevamente que tienes...
ls() # nada.


##################
## Tipos de objetos: Vectores
##################

# Vector numerico
vector.n = c(1,2,3,4)
vector.n
## En este vector, hemos incluido 4 numeros. Es un vector numerico.

# Vector de texto
vector.t = c("uno", "dos", "tres", "cuatro")
vector.t

# Multiplequemos por dos el vector numerico
vector.n.2 = vector.n*2
vector.n.2

# Comprobemos que vector.n.2 > vector.n
vector.n.2 > vector.n

# acceder al segundo elemento de un vector
vector.n.2[40]


##################
## Tipos de objetos: Listas
##################

# creemos una lista, que es un objeto que permite una variedad de elementos dentro de ella.
mi.informacion <- list(
  clases=c("Contabilidad General", "Metodos", "Estadistica I"),
  profes=c("X", "HB", "Y"),
  WhatsApp="+569475759"
  )

# accedamos a un elemento por posicion.Posicion "2" (profes)
mi.informacion[[2]]

# accedamos a un elemento por nombre
mi.informacion$clases

# accedamos a un elemento por nombre y posicion
mi.informacion$profe[2]


##################
## Tipos de objetos: Matrix
##################

# creemos una matriz, cuyos escalares (numeros) sigan una secuencia del 1 al 6, 1:6, y que tenga dos filas (rows).
a <- matrix(1:6, nrow=2)


# row 2 column 3
a[2,3]

# all rows column 2
a[,2]

# all columns row 1
a[1,]

##################
## Tipos de objetos: Fechas
##################

a = as.Date('1/15/2001',format='%m/%d/%Y')
b = as.Date('1/16/2001',format='%m/%d/%Y')
c = as.Date('1/17/2001',format='%m/%d/%Y')

# ahora R entiende que una cantidad sea mayor (mas reciente) que otra (mas antigua)
a>b

##################
## Tipos de objetos: Dataframes (bases de datos)
##################

base.de.datos <- data.frame(
        diabetico = c(TRUE, FALSE, TRUE, FALSE), 
        kilos = c(65, 69, 71, 73))

View(base.de.datos) # Igual a un Excel

base.de.datos[3,2] # fila 3, columna 2

base.de.datos[1:2, "kilos"] # filtrando, pero usando el nombre de la columna. Para las filas, la secuencia de los dos primeros elementos. 
base.de.datos[,"diabetico"] # todas las filas de la columna "diabetico"

## Nombres de las Columnas
colnames(base.de.datos)

## Acabas de descubrir un error. No eran kilos, si no que "edad". Cambia el nombre de las columnas.
colnames(base.de.datos) <- c("diabetico", "edad")

## Comprueba...
colnames(base.de.datos)

base.de.datos

## Resumen del df
summary(base.de.datos)

## Prestige
install.packages("car") # instalemos la base, que viene en la libreria "car"
library(car) # ahora, carguemos la base que viene en la libreria "car"

## Breve inspeccion de la base
head(Prestige)

## Veamos como se ve toda la base
View(Prestige)

## Resumen de la base
summary(Prestige)

### education: Average education of occupational incumbents, years, in 1971.
### income: Average income of incumbents, dollars, in 1971.
### women: Percentage of incumbents who are women.
### prestige: Pineo-Porter prestige score for occupation, from a social survey conducted in the mid-1960s.
### census: Canadian Census occupational code.
### type: Type of occupation. A factor with levels (note: out of order): bc, Blue Collar; prof, Professional, Managerial, and Technical; wc, White Collar.


## Funcion if/else
Prestige$prestige.2 <- ifelse(Prestige$prestige > mean(Prestige$prestige), "alto","bajo")

## Etiquetas:
is(Prestige$type) # que es?

levels(Prestige$type) # que etiquetas tiene?

## Recodificar: de ENG a ESP
install.packages("plyr") 
library(plyr)
Prestige$type.2 <- revalue(Prestige$type, c("bc"="tecnico", "prof"="profesional", "wc"="oficinista"))

## Funcion Drop
install.packages("dplyr") 
library(dplyr)

## Botemos algunas columnas, y construyamos dos DF's distintos, pero que igual tengan una columna en comun

### que columnas tiene el DF Prestige?
colnames(Prestige)

### OK. DF1 tendra todo menos type, census, prestige
Prestige.drop.1 = select(Prestige,-c(type, census, prestige))
colnames(Prestige.drop.1)

### OK. DF2 tendra todo menos education e income
Prestige.drop.2 = select(Prestige,-c(education, income))
colnames(Prestige.drop.2)

### Problema: ahora queremos hacer un "merge" de las dos DF's. El unico elemento en comun, es la variable "women"
# "Merge" es util cuando queremos combinar dos datasets. Por ej., si estudiamos economia politica, y queremos ver el 
# el efecto de la democracia en el PIB, probablemente tengamos que buscar una base de datos de ciencia politica (donde este la
# variable "democracia") y otra base de datos de economia (donde este la variable "PIB"). Ahi hariamos un merge por pais.
Prestige.merge <- merge(Prestige.drop.1, Prestige.drop.2, by = c("women")) 