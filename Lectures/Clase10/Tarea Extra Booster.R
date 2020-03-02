#Tarea_Extra_Booster


#a. Carga el paquete `tidyverse`. Tu código:
  
  
  
#  b. Para este ejercicio vamos a utilizar una base modificada a partir de
#[Quality of Government](https://qog.pol.gu.se/data), con distintas variables para 139 países.
#Esta es la base en la versión 0.1 del [capítulo de manejo de datos de ADP]
#(https://arcruz0.github.io/libroadp/manejo-de-datos.html).
#Las siguientes son las variables de la base:
# 
# variable           | descripción
#------------------ | -------------------------------------------------------------------------------------------
#  cname              | Nombre del país
#wdi_gdppppcon2011  | PIB PPP, en dólares del 2011, según los datos de WDI.
#wdi_pop            | Población, según los datos de WDI.
#ti_cpi             | Índice de Percepción de la Corrupción de TI. Va de 0 a 100, con 0 lo más corrupto.
#lp_muslim80        | Porcentaje de población de religión musulmana, para 1980, según LP.
#fh_ipolity2        | Nivel de democracia según FH. Va de 0 a 10, con 0 como menos democrático.
#region             | Región del país, según WDI (añadida a la base de QoG)

#Entonces, tendremos una base de 139 observaciones (filas) y 7 variables (columnas).
#¿Cuál será la unidad observacional de la base de datos? Tu respuesta:
  
  
  
  
#c. Carga la base de datos, que se ubica en la carpeta "datos" y está en formato .csv.
#¡Recuerda ponerle un buen nombre! Tu código:


##d. Crea una nueva base de datos basa en la anterior, pero con una nueva variable de PIB per capita. 
#Además, cambia el nombre de alguna de las variables, por otro que te parezca más legible.
#Puedes hacer las dos operaciones en una cadena, usando *pipes*. 
#¡Recuerda colocar buenos nombres! En el resto del ejercicio, utiliza esa base de datos. 
#Tu código:
  
  
  
#e. Filtra las observaciones de la base de datos, para que esta solo contenga los países del continente americano
#(no es necesario crear un objeto nuevo).
#Tip: para construir el filtro, fíjate en los valores posibles de la variable "region". 
#Tu código:
  
  
  
#f. Usando (idealmente aunque no es necesario) una sola cadena de *pipes*,
#obtén un resumen para cada una de las 21 regiones de la base, 
#que incluya sus medias de PIB per cápita, índice de corrupción e índice de democracia.
#Tu código:
  
  
  
#g. Usando (idealmente aunque no es necesario) una sola cadena de *pipes*,
# obtén el total de habitantes musulmanes #por región,
#y luego ordena las regiones desde la que tiene más musulmanes en total hasta la que tiene menos.
#Tip: no tenemos una variable con los habitantes musulmanes de un país,
#pero podemos crearla... Tu código:
  


