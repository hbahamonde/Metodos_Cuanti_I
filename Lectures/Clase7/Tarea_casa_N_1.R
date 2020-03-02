# Tarea para la casa # 1

# Nombre:
# Fecha:
# Curso: Métodos de Investigación.
# Prof. Héctor Bahamonde.

#################################################################
# carga la base
load("/Users/hectorbahamonde/Downloads/ICPSR_35299/DS0001/35299-0001-Data.rda")

# cambia el nombre de la base
base = da35299.0001


# 1. Usa el "revalue" para acortar el nombre de los "Levels" de la variable P8_1 (tipo de educacion)








#################################################################
# 2. Haz un grafico simple entre P8_1 y P128 (escala izquierda-derecha). Que puedes concluir? Existe una relacion entre ideologia y educacion? Escoge el tipo de grafico que mejor responda la pregunta.








#################################################################
# 3. Haz el mismo grafico, pero en GGPLOT2.








#################################################################
# 4. Usando la variable P114 (auto-posicionamiento economico, que va desde extrema pobreza -1- hasta extrema riqueza -10-):

## 4.1 Haz un histograma de esta variable. Comenta.

## 4.2 Saca el promedio de P114. Como se auto-clasifica el/la Chileno/a promedio? Comenta.



#################################################################
# 5. Simulaciones y Plots de Densidades y Scatters

# 5.1 Crea dos objetos. El primero se llama "distribucion.3" y el segundo es "distribucion.1".
# Ambos objetos son simulaciones de distribuciones normales. El largo de cada vector es 12.000. 
# El percentil 50 de "distribucion.3" es 3 y el percentil 50 de "distribucion.1" es 1.
# Recuerda setear el seed. Usa la secuencia 123.




# 5.2 Usando GGPLOT, haz un plot de densidad donde esten las dos distribuciones.




# 5.3 Saca el logaritmo natural de ambos vectores, y usando GGPLOT2, haz un scatter plot con los dos vectores. 

## Atencion: Quizas tengas que hacer una transformacion extra. Una opcion es ocupar el comando "abs" (dudas? Ve "help(abs)"). Otra opcion es sacar el cuadrado de cada elemento del vector. Por qué tienes esas dos opciones? Qué problema estás tratando de evitar? 



# 5.4 Saca el exponencial de "distribucion.3" ("exp(distribucion.3)") y llámalo "distribucion.3.exp", y usando GGPLOT2, haz un scatter plot con "distribucion.3" y "distribucion.3.exp". Comenta. 





#################################################################
# 6. Cuentas. 

## Crea una base de datos con las siguientes variables:
### a. un vector numerico de largo 5, cuya distirbucion es Poisson. Media: 5
### b. un vector de largo 5, cuyos objetos son textos. Cada uno debe tener cinco marcas diferentes de cervezas.
### Recuerda que el seed es 123.

## Crea un histograma para representar graficamente lo que podria ser la frecuencia de las cinco marcas mas preferidas por los estudiantes. 
### c. Cual es la cerveza mas consumida? 