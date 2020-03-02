# Clase 11

#######################################################
# Cual es la relacion entre educacion y prestigio? 
#######################################################

# Creemos una base de datos imaginaria
base = data.frame(
        nombre = c("Pedro", "Juan", "Diego"),
        educacion = c(57,50,61), # vector de educacion
        prestigio = c(64,53,67)
        )

base # veamos como se ve nuestra base de datos.

# ahora veamos la relacion que existe entre ambas variables. 
plot(base$educacion,base$prestigio) # Que tipo de relacion es esta? Positiva? Negativa?

# Ahora creemos un modelo, es decir, estimemos beta_1
modelo = lm(prestigio ~ educacion, data = base)

modelo # veamos que nos dice nuestro "modelo"
# interpretacion: Siempre partimos con nuestra X. Por cada unidad que subimos nuestra x (educacion), subimos beta_1 en nuestra y (prestigio). Supongamos que educacion esta medido en meses y prestigio en puntos. Esta sera nuestra unidad de medida. Es decir, si subo un mes de educacion, subo en prestigio 1.306 puntos. (Y ahi esta nuestra relacion positiva).


# a) Por que se llama "modelo"? Pista: esta es la parte mas "artistica" de la estadistica. 
# b) Analogia del mapa.

# cuan bien funciona nuestro modelo? Ocupemos el comando "predict" ("predecir" en espanol).
predict(modelo)
# Aqui vemos lo siguiente

# Lo que observamos:
# Pedro tenia 64, pero nuestro modelo predice 62.63
# Juan tenia 53, pero nuestro modelo predice 53.49
# Diego tenia 67, pero nuestro modelo predice 67.85

# Discutir: Es esto "suficientemente bueno"?

# El concepto de "error". La diferencia entre lo que observamos y lo que modelamos. Derivar algeibraicamente.

#######################################################
# Modelo OLS en matriz.
#######################################################

# vector para el error. Algebra simple...pero con comandos.
base$error <- c(
        base$prestigio[1] - as.numeric(modelo$coefficients[1]) - (base$educacion[1] * as.numeric(modelo$coefficients[2])),
        base$prestigio[2] - as.numeric(modelo$coefficients[1]) - (base$educacion[2] * as.numeric(modelo$coefficients[2])),
        base$prestigio[3] - as.numeric(modelo$coefficients[1]) - (base$educacion[3] * as.numeric(modelo$coefficients[2]))
        )

# vector para el intercepto. Ojo: es un intercepto para todos. Por eso no esta indexado.
base$intercepto <- c(
        as.numeric(
                modelo$coefficients[1]
                )
        )

# vector para el beta. Ojo: es un beta para todos. Por eso no esta indexado.
base$beta1 <- c(
        as.numeric(
                modelo$coefficients[2]
        )
)

base # llamemos al a base.

#######################################################
# Prediccion
#######################################################

x=base$educacion # crea objeto

y=base$prestigio  # crea objeto

lm.out <- lm(y ~ x) # estima modelo de nuevo. el mismo modelo.
newx = seq(min(x),max(x),by = 1) # crea sequencia de numeros para el rango X del grafico
conf_interval <- predict(lm.out, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95) # usando el modelo estimado, predice (a) distintos valores de educacion NO OBSERVADOS (fit), y el 95% de intervalo de confianza, con "lower bound" o "lwr" (parte de ABAJO del intervalo), y "upper bound" o "upr" (parte de ARRIBA del intervalo).
plot(x, y, xlab="Educ", ylab="Prest", main="Regression", ylim = c(30,90)) # ploteamos
abline(lm.out, col="lightblue") # agregamos linea "fit"
lines(newx, conf_interval[,2], col="blue", lty=2) # lower bound
lines(newx, conf_interval[,3], col="blue", lty=2) # upper bound



#######################################################
# Por que "least squares" ("cuadrados menores"?)
#######################################################

# los betas estimados, minimizan la suma de los "errores cuadrados".
