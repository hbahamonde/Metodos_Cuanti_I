# Ej. #5

# Nombre:
# Fecha:
# Curso: Métodos de Investigación.
# Prof. Héctor Bahamonde.


#################################################################
# 0 Limpia R
#################################################################

cat("\014")
rm(list=ls())
graphics.off()

#################################################################
# 1 Abre la base datos data.
#################################################################

# install.packages("foreign")
library(foreign) # significa "foraneo"
load(url("https://github.com/hbahamonde/Metodos_de_Investigacion/raw/master/Lectures/Clase21/data.rdata"))

# cambia el nombre de la base de datos
dat = data_reg


#################################################################
# 1. Estima los siguientes cuatro modelos lineales bivariados
#################################################################

# (A) variable dependiente: y1. Variable independiente: x1.
# (B) variable dependiente: y2. Variable independiente: x2.
# (C) variable dependiente: y3. Variable independiente: x3.
# (D) variable dependiente: y4. Variable independiente: x4.


# 1.1 Abajo, estima los modelos y guardalos

# A
m1 = lm(y1 ~ x1, dat)

# B
m2 = lm(y2 ~ x2, dat)

# C
m3 = lm(y3 ~ x3, dat)

# D
m4 = lm(y4 ~ x4, dat)


# 1.2 Abajo pon los resultados juntos en la misma tabla ocupando "screenreg".
## Clase 14.

# install.packages("texreg")
library(texreg)
screenreg(list(m1,m2,m3,m4))


#################################################################
# 2. Interpreta cada modelo
#################################################################

# (1) Que puedes decir del efecto de X en Y? Responde para cada uno
# de los cuatro modelos.

# Interpretacion de A:
# Cuando subo una unidad x, y sube .5. El efecto es significativo al 0.05.

# Interpretacion de B:
# Cuando subo una unidad x, y sube .5. El efecto es significativo al 0.05.

# Interpretacion de C:
# Cuando subo una unidad x, y sube .5. El efecto es significativo al 0.05.

# Interpretacion de D:
# Cuando subo una unidad x, y sube .5. El efecto es significativo al 0.05.


# install.packages("effects")
library(effects)
plot(allEffects(m1))
plot(allEffects(m2))
plot(allEffects(m3))
plot(allEffects(m4))

#################################################################
# 3. Checklist // Resto de la clase.
#################################################################

# 3.1 Checkea los supuestos del modelo lineal (clase 18).
# 3.2 Checkea los errores estandard (clase 17).
# 3.3 Checkea los residuos (clase 17).
# 3.4 Responde. Que recomendaciones tienes?

# Asegurate que en las cuatro respuestas (3.1, 3.2, 3.3, 3.4) ocupes 
# (a) los estadisticos que hemos pasado en clases, (b) tablas y (c) graficos.
# Es decir, entre las cuatro preguntas, quiero ver como usas todas las herramientas
# que hemos pasado.


# 3.1
options(scipen = 100000)

summary(summary(m1)$residuals) # residuo tiene promedio 0, super bien.
summary(summary(m2)$residuals) # residuo tiene promedio 0, super bien.
summary(summary(m3)$residuals) # residuo tiene promedio 0, super bien.
summary(summary(m4)$residuals) # residuo tiene promedio 0, super bien.

# 3.2
summary(m1)$coefficients[4] # error std. es muy pequeno. super bien.
summary(m2)$coefficients[4] # error std. es muy pequeno. super bien.
summary(m3)$coefficients[4] # error std. es muy pequeno. super bien.
summary(m4)$coefficients[4]# error std. es muy pequeno. super bien. 

# 3.3 
library(lattice)
xyplot(m1$residuals ~ dat$x1, type=c("smooth", "p")) # ok.
xyplot(m2$residuals ~ dat$x2, type=c("smooth", "p")) # residuos no tienen varianza constante.
xyplot(m3$residuals ~ dat$x3, type=c("smooth", "p")) # residuos no tienen varianza constante.
xyplot(m4$residuals ~ dat$x4, type=c("smooth", "p")) # residuos no tienen varianza constante.


# 3.4
# Desechar modelos m2, m3, m4. El unico modelo que tiene residuos 
