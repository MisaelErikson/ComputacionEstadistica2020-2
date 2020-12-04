library(tidyverse,stats,car)

#################################
# Alto crear calculadora suma
smr=function(a,b){
  s=a+b
  return(s)
}

smr(4,3)
##################################333333

# Generación de variables aleatorias

# aplicación de distribucion uniforme

# U(95,100)---- F(x)=(x-a)/(b-a)
# x=a+(b-a)*F(x)-- Función a programar F(x)=ri

# Crear muestras aleatorias (n)

set.seed(1)
nunif=function(a,b,n1){
  n=10^n1
  ri=runif(n)
  temp=a+(b-a)*ri # variable aleatoria uniforme
  x2=data.frame(ri,temp)
  return(x2)
}
# creando 10
x1=nunif(95,100,1)
# creando 100
x1=nunif(95,100,2)
# para cualquier valor 
x1=nunif(95,100,8)

dev.off()
hist(x1$temp,breaks = "FD",freq=FALSE)
curve(dunif(x),lwd=2,add=TRUE)

###############

# lectura de la función inversa

f=function(r,lamda){
  t=-log(1-r)/lamda
  return(t)
}

ginve=function(n1,lamda){
  n=10^n1
  ri=runif(n)
  temp=f(ri,lamda) # variable aleatoria uniforme
  x2=data.frame(ri,temp)
  return(x2)
}
lamda=3
x1=ginve(8,lamda)

dev.off()
hist(x1$temp,breaks = "FD",freq=FALSE)
curve(dexp(x,lamda),lwd=2,add=TRUE,Col="red")

# Método de convolución 

# aproximando a una chi-cuadrado
# y= sum(xi^2)----- xi--N(u,s^2)

n=10^5 # el tamaño de muestra  
s=0 # s=x1, s=x1+x2, s=x1+x2+x3........
nv=3 # numero de variables aletorias 

for (i in 1:nv){
  ri=runif(n)
  xi=qnorm(ri)  # Variable aleatoria Normal
  s=(s+xi^2)/nv
}

dev.off()
hist(s,breaks = "FD",freq=FALSE)
curve(dchisq(x,1),lwd=2,add=TRUE,col="red")


# aproximación poison a normal

n=10^5 # el tamaño de muestra  
s=0 # s=x1, s=x1+x2, s=x1+x2+x3........
nv=5 # numero de variables aletorias 
lamda=9003

for (i in 1:nv){
  ri=runif(n)
  xi=qpois(ri,lamda)  # Variable aleatoria Normal
  s=(s+xi)/nv
}

dev.off()
hist(s,breaks = "FD",freq=FALSE)
curve(dnorm(x,mean(s),sd(s)),lwd=2,add=TRUE,col="red")

#fin








