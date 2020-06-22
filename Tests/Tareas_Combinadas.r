# Nombre:
# Fecha:
# Curso: Métodos Cuantitativos.
# Prof. Héctor Bahamonde.

# 1. Esta tarea combina los ej's chicos #2, #3 y #4. Volviendo del break, haremos la tareas grandes #1 y #2 combinadas.
# 2. Se entrega el viernes a las 5 pm en uCampus. 
# 3. Si quieres trabajar en grupo, se puede hacer. Pero las notas son individuales. 
# 4. Esta tarea es "multi-nota". Cada seccion tiene una nota de acuerdo al ej. que coresponda.
# 5. Recuerda ir comentando con "#" para ir explicando el raciocinio.








#################################################################
# EJ CHICO 2
#################################################################
# instala la libreria. 
install.packages("dslabs")
# carga la libreria
library("dslabs")

# carga la base de datos
data(admissions)

#################################################################
# 2.1: Cambiando etiquetas y modificando nombres de variables
#################################################################


# 1. Que tipo de variable es "gender"? Usa un comando para contestar esta pregunta. 


# 2. Cambia el nombre de la variable "gender" a "genero".


# 3. Cambia el tipo de variable "genero" a factor/variable cualitativa. Pista: necesito que ocupes "levels".


#################################################################
# 2.2: Transformarciones y creacion de nuevas variables
#################################################################


# 1. Usando la misma base de datos, crea una nueva variable que represente el % de personas que son aceptadas en cada "major". 
# Un "major" es una carrera (en EEUU). Tu "major" seria, por ej., "economics". Sólo que aquí estan puestos como "A", "B", "C", etc. Atencion: esta nueva variable debe ser parte de la base de datos.


# 2. En base a lo anterior, que puedes concluir?






#################################################################
# EJ CHICO 3
#################################################################

# Carga la base de datos titulada base_mociones.xlsx

#################################################################
# 3.1
#################################################################

# Para este ejercicio vamos a utilizar una base de datos con información de los miembros de la cámara de diputados durante el periodo 1990-2018 construida por Florencia Olivares, Magíster en la Pontificia Universidad Católica. Esta base fue usada en su tesis de pregrado para su posterior publicación en una revista académica. Contiene las mociones "pro mujer" ingresada por cada integrante de la camara en su respectivo periodo de 4 años.


#################################################################
# 3.2 
#################################################################


# Carga la base de datos que está en formato .xlsx. ¡Recuerda ponerle un buen nombre!
# ¿Cuantas observaciones y variables (columnas) tenemos?
# ¿Cual sera la unidad observacional de la base de datos? 
# ¿Que significa cada variable?


#################################################################
# 3.3 
#################################################################

# Crea una nueva base de datos cambiandole el nombre de alguna de las variables por otro que te parezca mas legible o mas correcto. ¡Recuerda colocar buenos nombres! En el resto del ejercicio, utiliza esa base de datos. 




#################################################################
# 3.4
#################################################################

# Filtra las observaciones de la base de datos, para que esta solo contenga las diputadas mujeres (no es necesario obtener un un objeto nuevo).



#################################################################
# 3.5 
#################################################################


# Obten un resumen separado para hombres y mujeres de filiación política que incluya la suma de sus mociones pro mujer.


# ¿Quién ha presentado mas mociones pro mujer dentro de las mujeres?


# ¿Quién ha presentado más mociones pro mujer dentro de los hombres?








#################################################################
# EJ CHICO 4
#################################################################

# Carga la base de datos titulada cow.dta
# En este ej. pensaremos en la siguiente pregunta:
# Que hace a los paises ricos? Sera la democracia? Y/o la estabilidad?  Y/o La cantidad de poblacion? 



#################################################################
# 4.1
#################################################################

# Cual es la variable dependiente?
# Responde:



#################################################################
# 4.2 
#################################################################

# Crea una hipotesis y una hipotesis alternativa.
# En esta seccion deberas pensar en hipotesis bivariadas.


# (a) Hipotesis: Yo creo que X1 causa Y porque....?


# (b) Hipotesis alternativa: Yo creo que X2 causa Y porque...?


#################################################################
# 4.3 
#################################################################

# Estima dos modelos lineales multivariados (y ~ X1, pero incluyendo controles X2, X3, etc.). Deberas estimar un modelo por cada hipotesis. En total, dos modelos (uno de la hipotesis y otro de la hipotesis alternativa). Ambos deben incluir controles. Presenta tus resultados usando "summary", e interpretando correctamente los coeficientes. Que podemos aprender de estos dos modelos?


# Modelo 1



# Modelo 2



# Que podemos aprender de estos dos modelos?