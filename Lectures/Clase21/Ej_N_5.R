# Ej. #5

# Nombre:
# Fecha:
# Curso: Métodos de Investigación.
# Prof. Héctor Bahamonde.


#################################################################
# 0. Limpia R
#################################################################

cat("\014")
rm(list=ls())
graphics.off()

#################################################################
# 1. Abre la base datos data.
#################################################################

# install.packages("foreign")
library(foreign) # significa "foraneo"
load(url("https://github.com/hbahamonde/Metodos_de_Investigacion/raw/master/Lectures/Clase21/data.rdata"))

# cambia el nombre de la base de datos
dat = data_reg


#################################################################
# 2. Estima los siguientes cuatro modelos lineales bivariados
#################################################################

# (A) variable dependiente: y1. Variable independiente: x1.
# (B) variable dependiente: y2. Variable independiente: x2.
# (C) variable dependiente: y3. Variable independiente: x3.
# (D) variable dependiente: y4. Variable independiente: x4.


# 2.1 Abajo, estima los modelos y guardalos

# A

# B

# C

# D


# 2.2 Abajo pon los resultados juntos en la misma tabla ocupando "screenreg".
## Clase 14.



#################################################################
# 3. Interpreta cada modelo
#################################################################

# (3.1) Que puedes decir del efecto de X en Y? Responde para cada uno
# de los cuatro modelos.

# Interpretacion de A:

# Interpretacion de B:

# Interpretacion de C:

# Interpretacion de D:

#################################################################
# 4. Checklist // Resto de la clase.
#################################################################

# 4.1 Checkea los supuestos del modelo lineal (clase 18).
# 4.2 Checkea los errores estandard (clase 17).
# 4.3 Checkea los residuos (clase 17).
# 4.4 Responde. Que recomendaciones tienes?

# Asegurate que en las cuatro respuestas (4.1, 4.2, 4.3, 4.4) ocupes 
# (a) los estadisticos que hemos pasado en clases, (b) tablas y (c) graficos.
# Es decir, entre las cuatro preguntas, quiero ver como usas todas las herramientas
# que hemos pasado.