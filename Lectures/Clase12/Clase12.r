# Clase 12

cat("\014")
rm(list=ls())
graphics.off()

# definir x
x = matrix(
  c(1,1,1,2,7,3,2,4,6),
  nrow = 3, 
  ncol = 3
  )


# definir y
y = matrix(
    c(3,5,7),
  nrow = 3, 
  ncol = 1
)

###########################
# transponer x con comando "t"
###########################
t(x)

###########################
# x'x
###########################
t(x) %*% x

###########################
# inverse
###########################

## primero, encontrar determinante
# det(t(x)%*%x)
# 324

## segundo, encontrar Adjacente
## Multiples maneras. 

# Hagamos el proceso de manera directa. Invirtamos 'x'

# Usuarios MAC: asegurarse de tener XQuartz instalado
# https://dl.bintray.com/xquartz/downloads/XQuartz-2.7.11.dmg

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(matlib)

inv(t(x)%*%x)

###########################
# Modelo
###########################

# En Matriz
options(scipen=99)
inv(t(x) %*% x)%*%t(x)%*%y # beta

# Usando el comando "lm"
data = data.frame(
        x=x[,2:3],
        y=y)

lm(y ~ x.1 + x.2, data)
