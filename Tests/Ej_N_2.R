# Ej. #2

# Nombre:
# Fecha:
# Curso: Métodos de Investigación.
# Prof. Héctor Bahamonde.

#################################################################
# 1: Cambiando etiquetas y modificando nombres de variables
#################################################################
# instala la libreria. 
install.packages("dslabs")
# carga la libreria
library("dslabs")

# carga la base de datos
data(admissions)

# 1. Que tipo de variable es "gender"? Usa un comando para contestar esta pregunta. 

# 2. Cambia el nombre de la variable "gender" a "genero".

# 3. Cambia el tipo de variable "genero" a factor/variable cualitativa. Pista: necesito que ocupes "levels".


#################################################################
# 2: Transformarciones y creacion de nuevas variables
#################################################################

# 1. Usando la misma base de datos, crea una nueva variable que represente el % de personas que son aceptadas en cada "major". Un "major"
# es una carrera (en EEUU). Tu "major" seria, por ej., "public administration". Sólo que aquí estan puestos como "A", "B", "C", etc. Atencion: esta nueva variable debe ser parte de la base de datos.


# 2. En base a lo anterior, que puedes concluir?


#################################################################
# 3: Trabajando con Tiempo
#################################################################

# llama la libreria
library("dslabs")

# carga la base
data(divorce_margarine) # base de datos que muestra datos de divorcios del estado de Maine y consumo de margarina.

# 1. Usa GGplot para graficar como divorce_rate_maine ("taza de divorcios del estado de Maine") y 
# margarine_consumption_per_capita (consumo per capita de Margarina) varian por año. Usa colores 
# para distinguir qué es qué. Pista: usa "geom_point()".



# 2. Que puedes concluir?
