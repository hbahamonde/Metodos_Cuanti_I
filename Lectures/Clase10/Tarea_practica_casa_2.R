
#Cualquier duda que tengan consúltenla particularmente si necesitan saber
#que significa una variable en específico.
#Para consultas fuera de horario de clases mi correo es ghbarria@uc.cl



#Parte I (Gráficos)



## Gráfico bivariado para dos variables continuas (dispersión)

#Usarás una nueva base de datos.
#Creada por Gonzalo Barría (su querido ayudante) para su tesis de pregrado
#(desigualdad y delitos por infracciones a la ley 20.000 en Chile)

#Esta base está en Excel, por lo que necesitas cargar otro paquete para abrirla

#install.packages("readxl)

library(readxl)

df_droga<-read_excel("base_droga.xlsx")



#Miremos la base. ¿Alguna variable que les llame la atención? 
#Es posible que debas escribir "as.numeric(nombre_variable)" para que el ggplot
#reconoza bien las variables continuas


#A. Observa la relación entre la variable de escolaridad en la comuna y su índice de pobreza
#¿Existe alguna relación? ¿Cuál? grafica con ggplot
  


# B. Grafica lo mismo con respecto al indice de desigualdad (gini) con la población indígena 



#C. ¿hay relación entre desigualdad y pobreza? o entre
#¿Porcentaje de población indígena y pobreza? Argumenta gráficamente




#D. Toma alguno de los gráficos que hiciste arriba y colocale las etiquetas apropiadas
#usando `labs()` ¡En todos los ejercicios genera buenas etiquetas para tus gráficos, utilizando `labs()`! 
  
#  a. Genera un plot de violín para la relación entre una variable "x" discreta
#y una variable "y" continua.

  
  
  
#  b. Genera al menos un gráfico de dispersión entre dos variables continuas de la base que nombramos como df_droga.
#Cambia el color y/o forma de los puntos de acuerdo a una de las variables de la base
#(si es que vas a utilizar una variable dummy para esto
#ingrésala como `as.factor(variable)` en el mapeo estético
#si es una variable continua ingresala como `as.numeric(variable)`).



#Parte #2. Manejo de datos


#a Filtra las observaciones de la base de datos, para que esta solo contenga
#las comunas de la región de O'Higgins.Luego crea un objeto que contenga las
#comunas de la región de O'Higgins junto con las variables de pobreza, escolaridad,
#gini, crimen1, pdtotal, Esperanza e IDH.
#Tu código:



#B. Usando una sola cadena de *pipes*, obtén un resumen para cada una de las regiones de la base, 
#que incluya sus medias de crimen1, gini, pobreza, y pdtotal (personas detenidas
#por infracción a la ley 20.000)
#Tu código:



#C. Usando una sola cadena de *pipes*, obtén el total de habitantes
#indigenas por región, y luego ordena las regiones desde la que tiene más
#poblacion indígena en total hasta la que tiene menos. 
#Tip: no tenemos una variable con la poblacion indigena de una comuna país, pero podemos crearla... Tu código:






#La siguiente parte no es obligatoria, pero otorgará puntos extra para
# ESTA misma tarea



## Crea una base de datos con las siguientes variables:
# a. un vector numerico de largo 6, cuya distirbucion es Poisson. Media: 5
# b. un vector de largo 6, cuyos objetos son textos. 
#Cada uno debe tener cinco redes sociales distintas.
### Recuerda que el seed es 123.


#Crea un gráfico que muestre cual es la red social más usada.



