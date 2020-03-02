# Clase 15: Evaluacion practica en clases #4.
#################################################################
cat("\014")
rm(list=ls())
graphics.off()


# Cargar Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman)

# Cargar paquete para cargar bases que no son de R.
p_load(foreign) # significa "foraneo"
dat <- read.dta("cow.dta") # cargar base en  R (hasta el momento solo la habiamos descargado).

head(dat)

summary(dat)

# country Country Name
# ccode: country code
# pop: Population in thousands
# rgdpch: Real GDP/capita
# democracy: measure of democracy 
# vdiss: Violent dissent based on terrorist events and fatalities.
# repression: Violations of physical integrity rights.

#################################################################
# Motivacion
#################################################################

# En esta clase pensaras en la siguiente pregunta:
# Que hace a los paises ricos? Sera la democracia? Y/o la estabilidad?  Y/o La cantidad de poblacion?


#################################################################
# (1) VD?
#################################################################

# Cual es la variable dependiente?
# Responde:


#################################################################
# Crea una hipotesis y una hipotesis alternativa
#################################################################

# En esta seccion deberas pensar en hipotesis bivariadas.

# (a) Hipotesis: Yo creo que X1 causa Y porque....?

# (b) Hipotesis alternativa: Yo creo que X2 causa Y porque...?

#################################################################
# Estima dos modelos lineales
#################################################################

# (1) Estima dos modelos lineales, uno por cada hipotesis.
# (2) En cada uno, incluye variables independientes (o "controles") de tu preferencia.


# Modelo 1



# Modelo 2


#################################################################
# Construye una tabla 
#################################################################

# Construye una tabla que contenga ambos modelos

p_load(texreg) # paquete para hacer tablas con varios modelos.

# Tabla abajo

#################################################################
# Construye un grafico 
#################################################################

# Construye un grafico que contenga ambos modelos

p_load(coefplot) # paquete para hacer tablas con varios modelos.

# Grafico abajo



#################################################################
# Respuesta
#################################################################

# Que hace a los paises ricos?
# Responde:

# Despues de estimar ambos modelos, el que "gana" es el modelo [1 o 2?] porque
# el coeficiente es [mayor o menor]?
