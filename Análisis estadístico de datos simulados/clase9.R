# Aproximación de una probabilidad

# simulando una distribución Bernulli cuyo 
# parametro p=0.5

library(tidyverse)
p=0.5 # caracteristica de la distribución de bernulli

set.seed(1)  

# def. cantidad de numeros
nsim=4000

# numeros aletorios cuya distribución uniforme
rx=runif(nsim)
# rx
# Caracteristica de una bernulli
# F,E,F,E,F,...... 
# ojo FE,FFE,.......binomial

# Distribución Bernulli
rx=runif(nsim)<=p
#rx

# promedio teorico= 0.5
# E(x)=p-- aproximación
mean(rx)

# grafico evolución de la aproximación
n=1:nsim
est=cumsum(rx)/n# F tabla de frecuencias

plot(est,type="l",lwd=2,
     xlab="cantidad de Numeros Aleatorios",
     ylab="Proporción Muestral")

# promedio muestra E(x)
abline(h=mean(rx),col="blue",lty=2)

# promedio teórico (u)
abline(h=p,col="Red")

# Estabilidad de F.P. Binomial 
# parametros de la distribución binomial
p=0.1
n1=10

# cantidad de numero aletorios
set.seed(1)
nsim=5000

#generar los numero aleatorios 
rx=rbinom(nsim,n1,p)

# aproximacion del promedio:
muteo=p*n1

# promedio muestra
mean(rx)

# grafico evolución de la aproximación
n=1:nsim
est=cumsum(rx)/n# F tabla de frecuencias

plot(est,type="l",lwd=2,
     xlab="cantidad de Numeros Aleatorios",
     ylab="Proporción Muestral")

# promedio muestra E(x)
abline(h=mean(rx),col="blue",lty=2)

# promedio teórico (u)
abline(h=muteo,col="Red")

#calculando desviación Estandar
esterr=sqrt(cumsum((rx-est)^2))/n

# Intervalo de confianza(1-alfa)=0.9772 
# limete superior 
lines(est+2*esterr,lty=3,col="black")

#limete inferior
lines(est-2*esterr,lty=3,col="black")

# qnorm(0.9772)



# Solución al reto
# parametros iniciales 
set.seed(1)
xsd=1
xmed=0

# cantidad de numero aleatorios
nsim=10^3*4

#generando numeros aleatorios normales 
rx=rnorm(nsim,xmed,xsd)

# vectores almacenar datos como estadisticas prueba
s=numeric(1)
method=numeric(1)
statistic=numeric(1)
p.value=numeric(1)
se1=numeric(1)
cumpl=numeric(1)

# cargando 30 elmentos.
s=rx[1:30]

# contar numero de muestras con distribucion normal
j=1
alf=0.05

for(i in 31:nsim){
  nl=length(s)
  se=sd(s)/sqrt(nl)
  if (nl>25){
    x=nortest::lillie.test(s)
  } else{
    x=shapiro.test(s)
  }
  method[j]=x$method
  statistic[j]=x$statistic
  p.value[j]=x$p.value
  se1[j]=se
  cumpl=ifelse(x$p.value<alf,"No normal","Normal")
  s[i]=rx[i]
  if(se<0.1){
    bf=data.frame(method,
                  statistic,
                  p.value,
                  se1,
                  cumpl)
    break
  }
  j=j+1
}

mean(rx)
sd(s)
mean(s)
table(bf$cumpl)

# estabilidad del promedio muestral 

# aproximacion del promedio:
muteo=0

# promedio muestra
mean(rx)

# grafico evolución de la aproximación
n=1:nsim
est=cumsum(rx)/n# F tabla de frecuencias

plot(est,type="l",lwd=2,
     xlab="cantidad de Numeros Aleatorios",
     ylab="Proporción Muestral")

# promedio muestra E(x)
abline(h=mean(rx),col="blue",lty=2)

# promedio teórico (u)
abline(h=muteo,col="Red")

#calculando desviación Estandar
esterr=sqrt(cumsum((rx-est)^2))/n

# Intervalo de confianza(1-alfa)=0.9772 
# limete superior 
lines(est+2*esterr,lty=3,col="black")

#limete inferior
lines(est-2*esterr,lty=3,col="black")

## solución del reto 2

pracma::rand(1,10)
# crear una función  genere los numeros y sume

genN=function(x){
  N=numeric(1)
  for(i in 1:x){
    v=head(cumsum(pracma::rand(1,200)))
    N[i]=1+sum(v>1)
  }
  N
}

# Calular el E(x) y sd(x) para n=1000
N=genN(1000)

mean(N)
var(N)


# calcular el tamaño de muestra 95%
qnorm(0.05/2)^2*var(N)/(0.01^2)