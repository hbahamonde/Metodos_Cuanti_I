# Clase 13
#################################################################
cat("\014")
rm(list=ls())
graphics.off()

## Tu estaras a cargo en esta clase. Siguiendo este script, haras una regresion
## lineal usando las tecnicas de algebra de matrices. 

## Aunque hemos visto en clases lo que un coeficiente es, el enfasis hasta ahora 
## ha sido puesto en que los coeficientes son simples numeros. Lo son. Pero ya 
## la proxima clase veremos mas en detalle por que es importante entender 
## lo que ellos representan. 

## Por el momento, recuerda (para siempre) la formula de beta. 
## b = (x'x)^-1x'y

## Tambien recuerda la forma de la ecuacion de la regresion lineal:
## y = b0 + b1x1 + e


## Usando la formula para sacar betas, hoy calcularas beta0 (intercepto) y 
## beta1 (coeficiente).


## En esta clase, pensaremos en la relacion que hay entre 'educacion' e 
## 'ingresos'. Sera positiva? Negativa? Para responder esta pregunta,
## debes calcular beta1. Creo que estaremos de acuerdo en que la 
## variable independiente ('x', o 'la causa') es 'educacion', y la dependiente
## ('y', o 'el efecto') es 'prestigio'. Seria raro pensar que "el prestigio
## causa la educacion", no? Cuanto sube mi prestigio cuando aumenta una unidad
## de educacion? Eso es precisamente lo que significa un coeficiente.

## Recuerda...
## prestigio <- educacion # Si
## educacion <- prestigio # No!

## Esto significa que,
## Prestigio = b0 + b1Educacion + e

## Recuerda que todo esta indexado, menos los betas. Es decir, la formula real
## es asi:
## Prestigio_{i} = b0 + b1Educacion_{i} + e_{i}
## "i" significa "individuo". Y representa a cada observacion. 
## Esto significa que Pedro, Juan y Diego tienen, cada uno,
## sus propios niveles de prestigio,
## sus propios niveles de educacion,
## y su propio error. Recuerda, el error es la diferencia entre lo que 
## predecimos (beta0 y beta1) y lo que observamos para Pedro, Juan y Diego.
## Es decir, si tenemos esto:

## Nombre    Educacion (x)    Prestigio (y)   beta0      Beta1     Error
# -----------------------------------------------------------------------
## Pedro        10                15            1         2         -6
## Juan         5                 10            1         2         ?
## Diego        2                 8             1         2         ?

## Recuerda (1): son los mismos betas para todos.
## Recuerda (2): Por ej., para el caso de Pedro, la formula 
## "Prestigio = b0 + b1Educacion + e", 
## o "Prestigio_{i} = b0 + b1Educacion_{i} + e_{i}", que es lo mismo, 
## seria asi:

## Pedro: 
## 15 = 1 + 2*10 - 6

######################################################################
## (1)
## Como seria para el caso de Juan y Diego?
## Responde
######################################################################

## Juan?


## Diego?

######################################################################
## OK; sigamos. Ahora carguemos una base de datos real.

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(car) # En este paquete, "vive" la base "Prestige" ("Prestigio").

# Mira como se ve la base de datos
head(Prestige)

# Extrae la 'x' para conseguir beta1
x = Prestige$education

# Ve como se ve "x"
x


## Recuerda que para conseguir beta0 debes anexar una columna de 1's.

## Aqui le decimos a R que repita 1s, del largo (o "lenght") de 'x'.
unos = rep(1,length(x)) 

# Ve como se ve "unos"
unos

# Ahora junta ambas cosas ("x" y "unos")

x = matrix(
  c(unos,x), # meter vectores 'unos' y 'x'
  ncol = 2 # y organizarlos en 2 columnas.
  )

x # mira como se ve la nueva matrix "x"
            
# Extrae la 'y'
y = matrix(Prestige$prestige)


######################################################################
## (2)
## Calcula beta0 y beta1
######################################################################

## Recuerda los operadores

# Transponer 
# t()

# Invertir
# inv()
## Recuerda cargar "matlib" para usar la funcion "inv()".
### if (!require("pacman")) install.packages("pacman"); library(pacman)
### p_load(matlib)

# Multiplicar matrices
# Por ejemplo x*x es  x%*%x

# OK; ahora tu. Calcula b = (x'x)^-1x'y

######################################################################
# Problema que tuvimos en clases:
######################################################################

# Funciona
inv(t(x) %*% x) %*% t(x) %*% y

# No Funciona
inv(x) # Matrix debe ser cuadrada, pero "x" es rectancular.

# Por que?

inv(t(x) %*% x) # funciona
# Porque para multiplicar una matriz, debe ser CONFORMABLE.
# La dimension de "x" es 2x102 (dos columnas, 102 filas). Si "x" fuera 2x2, se podria.
# Sin embargo, al multiplicar x' (que es 102x2) y x (que es 2x102), por 
# el "inner rule" (no se el espanol), es decir, el 2 y el otro 2, 
# sabemos que la invertidad de x'x esta definida, porque x'x es cuadrado.


######################################################################
## (3) 
## Comprueba que hiciste todo bien.
## Estima una regresion lineal, pero ocupando
## el comando "lm" (por "linear model"). El comando "lm" se ocupa asi:
## lm(y ~ x). Define 'y' como 'Prestige$prestige' y 'x' como 'Prestige$education'.





######################################################################
## (4) Usando el comando 'plot', haz un grafico entre Prestigio y Educacion.







######################################################################
## (5) Usando tus propias palabras, y usando los resultados 
## de tu regresion lineal y el grafico, interpreta que significa
## beta0 y que significa beta1.



## (5.1) Que significa beta0? Responde.



## (5.2) Que significa beta1? Responde.


