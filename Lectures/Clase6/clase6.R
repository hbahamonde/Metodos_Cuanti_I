# Clase 6

############################################
# Scatters: pero ahora en GGPLOT
############################################

# instalar libreria
install.packages("ggplot2")

# cargar libreria
library(ggplot2)

options(scipen=999) # Apagar notacion cientifica

ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() 

ggplot(midwest, aes(x=log(area), y=log(poptotal))) + geom_point() # Logeado 


ggplot(midwest, aes(x=log(area), y=log(poptotal))) + geom_point() + geom_smooth(method="lm") # con linea
# 1. Que significa esta linea?


# Altamente customizable
ggplot(midwest, aes(x=log(area), y=log(poptotal))) + geom_point(col="steelblue", size=3) + geom_smooth(method="lm") # Color azul

ggplot(midwest, aes(x=log(area), y=log(poptotal))) + geom_point(aes(col=state), size=3) + geom_smooth(method="lm") # Color by state

ggplot(midwest, aes(x=log(area), y=log(poptotal))) + geom_point(aes(col=state), size=3, alpha=0.5) + geom_smooth(method="lm") # Alpha

ggplot(midwest, aes(x=log(area), y=log(poptotal))) + geom_point(aes(shape=state), colour = "red", size=3, alpha=0.5) + geom_smooth(method="lm") # Forma del punto, segun estado.

ggplot(midwest, aes(x=log(area), y=log(poptotal))) + geom_point(aes(shape=state), colour = "red", size=3, alpha=0.5) + geom_smooth(method="lm") + theme_bw() #  Cambio de "tema de colores"

ggplot(midwest, aes(x=log(area), y=log(poptotal))) + geom_point(aes(shape=state), colour = "red", size=3, alpha=0.5) + geom_smooth(method="lm") + theme_bw() + labs(title="Area y Población", y="Población", x="Área", caption="Base de datos 'Midwest'") # Cambio en los titulos de los ejes.


# en este grafico usamos geom_point dos veces. una para poner los puntos del log(poptotal) y otra para los puntos de "percblack" (% de Afro americanos). Este tipo de graficos se usa cuando queremos observar la relacion ente dos variables. Aqui no vemos que haya una relacion clara.
ggplot(midwest, aes(x=area)) + 
        geom_point(aes(y=log(poptotal)), colour = "red", size=3, alpha=0.5) + 
        geom_point(aes(y=percblack), colour = "green", size=2, alpha=0.5) + 
        theme_bw() + 
        labs(title="Area y Población", y="Población", x="Área", caption="Base de datos 'Midwest'")



############################################
# Mapas en GGPLOT
############################################


# instalar librerias (OJO: podemos cargamos varias al mismo tiempo, si lo hacemos escribiendo un vector "c" ).
install.packages(c("rgeos", "cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))


# cargamos algunas de estas librerias 
library("ggplot2")
theme_set(theme_bw()) # seteamos el tema "black and white" para no tener ese fondo plomo que siempre tiene GGPLOT. Se ve mejor, mas limpio.
library("sf")
library("rnaturalearth") # libreria con todos los mapas del mundo
library("rnaturalearthdata") # libreria complementaria de "rnaturalearth"

#
world <- ne_countries(scale = "medium", returnclass = "sf")

# grafico basico del mundo
ggplot(data = world) + geom_sf()


# agreguemosle un poco de color...
ggplot(data = world) + geom_sf(color = "black", fill = "lightgreen") # pero esta coloracion no aporta mucha informacion...

# agreguemosle un poco de color...pero con algo mas de significado.
ggplot(data = world) + geom_sf(aes(fill = pop_est)) + scale_fill_viridis_c(option = "plasma", trans = "sqrt") # poblacion estimada.

# tratemos de plotear el Sudamerica...para eso, cambiamos las coordenadas.
ggplot(data = world) + geom_sf() + coord_sf(xlim = c(-84, -35), ylim = c(-55, 12)) 



# Plot mas complejo de Sudamerica
world_points<- st_centroid(world) # declaremos los nombres de los paises segun coordenadas.
world_points # veamos...
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry))) # "column bind" (haciendo MERGE...nuestra base de datos con latitud/longitud, y otra base con "world_points", es decir, nombres de los paises)

# ahora ocupemos la misma base de datos para plotear Sudametica, pero con el nombre de los paises.
ggplot(data = world) + geom_sf() + coord_sf(xlim = c(-84, -35), ylim = c(-55, 12)) + geom_text(data= world_points,aes(x=X, y=Y, label=name), color = "darkblue", fontface = "bold", check_overlap = FALSE)

