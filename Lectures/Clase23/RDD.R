library (lattice)



#########
#  Group 1
#########


# datos simulados
group1<-data.frame(
        x=1:20,
        y=(c(10,15,20,25,30,35,40,45,50,55, 60,65,70,75,80,85,90,95,100,105)),
        z=(factor(rep(0:1,each=10)))) 


# desordenar y
group1$y<-jitter(group1$y,factor=12)


# graficar: Existe un tipo de relacion entre x e y pero que cambie con el tratamiento (z)?
dev.off();dev.off();
xyplot(y~x,group1,
       xlab="Time",
       ylab="Score", 
       main="Progression through Intervention",
       groups=z)

# Veamos con mas detalle
xyplot(y~x,group1,
       xlab="Time",
       ylab="Score", 
       main="Progression through Intervention",
       pch=c(19,17),
       groups=z, 
       col=c("blue","green"),
       type=c("p","r"),
       lwd=2,
       lty=c(2,1)
       )

# Supongamos que el corte ("cutoff") es 10.5
cutoff = 10.5



# Regresion
analysis1<-lm(y~I(x-cutoff)*z,group1) # Vean que ocupamos el operador "I" (de "Isolate", aislar). "I" aisla todo lo que esta dentro de "I". En este caso, la sustraccion.
summary(analysis1)
# Z no tiene un efecto causal 

#########
#  Group 2
#########

library (lattice)
group2 <- data.frame(
        x=1:20,
        y=c(5,10,15,20,25,30,35,40,45,50, 100,105,110,115,120,125,130,135,140,145),
        z=(factor(rep(0:1,each=10)))
        )

# desordenar y
group2$y<-jitter(group2$y,factor=10)

# plot
dev.off();dev.off();
xyplot(y~x,group2,
       xlab="Time",
       ylab="Score", 
       main="Progression through Intervention",
       groups=z)

# Veamos con mas detalle
xyplot(y~x,group2,
       xlab="Time",
       ylab="Score", 
       main="Progression through Intervention",
       pch=c(19,17),
       groups=z, 
       col=c("blue","green"),
       type=c("p","r"),
       lwd=2,
       lty=c(2,1)
)

analysis2<-lm(y~I(x-cutoff)*z,group2)
summary(analysis2)


#########
#  Group 3
#########

group3 <- data.frame(
        x=1:20,
        y=c(50:59,60,65,70,75,80,85,90,95,100, 105),
        z=(factor(rep(0:1,each=10))))


group3$y<-jitter(group3$y,factor=10)

# plot
dev.off();dev.off();
xyplot(y~x,group3,
       xlab="Time",
       ylab="Score", 
       main="Progression through Intervention",
       groups=z)

# plot 2
xyplot(y~x,group3,
       xlab="Time",
       ylab="Score",
       main="Progression through Intervention Scenario 3",
       pch=c(19,17), 
       groups=z,
       col=c("maroon","grey"),
       type=c("p","r"),
       lwd=2,
       lty=c(2,1))

# Reg
analysis3<-lm(y~I(x-cutoff)*z,group3) 
summary(analysis3)
