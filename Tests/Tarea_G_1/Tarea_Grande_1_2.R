#Tarea 1 y 2 combinadas

#Usarás una nueva base de datos.
#Creada por Gonzalo Barría (su "querido" ayudante) para su tesis de pregrado
#(desigualdad y delitos por infracciones a la ley 20.000 en Chile)

#Esta base está en Excel, por lo que necesitas cargar otro paquete para abrirla

install.packages("readxl")
library(readxl)

#Cualquier duda que tengan consúltenla, particularmente si necesitan saber
#que significa una variable en específico.
#Para consultas fuera de horario de clases mi correo es ghbarria@uc.cl y
# gonzalo.barria@uoh.cl




#Parte I (Gráficos y manejo de datos)






#Miremos la base. ¿Alguna variable que les llame la atención? 
#Algunas variables importantes

#provincia
#region	
#pob2017	poblacion 2017
#urbancenso porcentaje de gente urbana	
#indigena porcentaje de indigenas	
#superficie  superficie de la comuna	
#alcant	 porcentaje de gente con alcantarillado
#electr	 porcentaje de gente con electricidad
#pobreza	 porcentaje de pobres
#escuelas	numero de escuelas publicas
#salud	 numero de recintos de salud publicos
#crimen	crimenes de mayor connotacion social (asesinatos, robos, etc)
#gini	 coeficiente de gini mide desigualdad
#IPC	  ingreso per capita de la comuna
#escolaridad	años de escolaridad
#IDH	indice de desarrollo humano
#pdtotal  personas detenidas en total producto de infringir la ley de drogas
#catotal   numero de crimenes por infringir la ley de drogas
#

#Es posible que debas escribir "as.numeric(nombre_variable)" para que el ggplot
#reconoza bien las variables continuas

## Gráfico bivariado para dos variables continuas (dispersión)


#A. Observa la relación entre la variable de escolaridad en la comuna y su índice de pobreza
#¿Existe alguna relación? ¿Cuál? grafica con ggplot



# B. Grafica lo mismo con respecto al indice de desigualdad (gini) con la población indígena 



#C. ¿hay relación entre desigualdad y pobreza? o entre
#¿Porcentaje de población indígena y pobreza? Argumenta gráficamente




#D.  Genera un plot de violín para la relación entre una variable "x" discreta
#y una variable "y" continua.




#E. Genera al menos un gráfico de dispersión entre dos variables continuas de la base.
#Cambia el color y/o forma de los puntos de acuerdo a una de las variables de la base
#(si es que vas a utilizar una variable dummy para esto
#ingrésala como `as.factor(variable)` en el mapeo estético
#si es una variable continua ingresala como `as.numeric(variable)`).



#Manejo de datos


#A ¿Cual es la comuna con mayor y menor IDH, GINI, personas detenidas por ley 20.000,
#pobreza, poblacion indígenena, escolaridad,  esperanza de vida y CRIMEN1?





#B. Filtra las observaciones de la base de datos, para que esta solo contenga
#las comunas de la región de O'Higgins.Luego crea un objeto que contenga las
#comunas de la región de O'Higgins junto con las variables de pobreza, escolaridad,
#gini, crimen1, pdtotal, Esperanza e IDH.
#Tu código:



#C. Por ejemplo usando *pipes*, obtén un resumen para cada una de las regiones de la base, 
#que incluya sus medias de crimen1, gini, pobreza, y pdtotal (personas detenidas
#por infracción a la ley 20.000)

#Tu código:



#D. Por ejemplo usando *pipes*, obtén el total de habitantes
#indigenas por región, y luego ordena las regiones desde la que tiene más
#poblacion indígena en total hasta la que tiene menos. 
#Tip: no tenemos una variable con la poblacion indigena de una comuna país, pero podemos crearla... Tu código:






#Parte 2. Estimación de modelos lineales


# Crea una hipotesis y una hipotesis alternativa relacionado las variables de la base.
#Por ejemplo alguna que relacione condiciones socioeconomicas y de salud con el numero de delitos

# En esta seccion deberas pensar en hipotesis bivariadas.


# (a) Hipotesis: Yo creo que X1 causa Y porque....?


# (b) Hipotesis alternativa: Yo creo que X2 causa Y porque...?


#################################################################
# 4.3 
#################################################################

# Estima dos modelos lineales multivariados (y ~ X1, pero incluyendo controles X2, X3, etc.).
#Deberas estimar un modelo por cada hipotesis. 
#En total, dos modelos (uno de la hipotesis y otro de la hipotesis alternativa). 
#Ambos deben incluir controles. 
#Presenta tus resultados usando "summary", e interpretando correctamente los coeficientes.


# Modelo 1



# Modelo 2



# Que podemos concluir de estos dos modelos?
