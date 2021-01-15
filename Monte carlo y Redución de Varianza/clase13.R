# Integracion de Motecarlo
G=function(x){
  return(exp(-x^2))
}

# Integral Teorica
integrate(G,lower=1,upper=3)

# Técnica de Montercarlo
# El tamaño de muestra
monte_carlo=function(G,a,b,M){
  s=0
  for (i in 1:M){
    u=runif(1,0,1)
    s=s+G(a+(b-a)*u)
  }
  return(((b-a)/M)*s)
}

# evaluando la integral 
monte_carlo(G,1,3,1000)

# Ejemplo 2
a=1
b=3
N=1000
# Integral Teorica
integrate(G,lower=a,upper=b)
# evaluando la integral 
monte_carlo(G,a,b,N)

# Graficando e integrado
monte=function(G,a,b,M){
  x=seq(a,b,0.01)
  y=G(x)
  f_max=max(y)
  x_rand=a+runif(M)*(b-a)
  y_rand=runif(M)*f_max
  ind_debajo=ifelse(y_rand<G(x_rand),T,F)
  ind_encima=ifelse(y_rand>=G(x_rand),T,F)
  dev.off()
  plot(x,y,type="l",col="red")
  par(new=T)
  points(x_rand[ind_debajo],y_rand[ind_debajo],
         col="blue")
  cat("Numero de pts debajo de la curva:",
      length(ind_debajo[ind_debajo==T]),"\n")
  cat("N.debajo/N.total:",
      length(ind_debajo[ind_debajo==T])/M,"\n")
  cat("area del Universo:",f_max*(b-a),"\n")
  cat("area bajo curva:",
      f_max*(b-a)*length(ind_debajo[ind_debajo==T])/M,
      "\n")
  
}

n=10000
b=3
a=1
monte(G,a,b,n)
1-0.7433

G=function(x){
  return((cos(50*x)+sin(20))^2)
}
b=2
a=-2

#error
4.016-4.14

#Técnicas de reducción de varianza 

# definir la función de trabajo
ftn=function(x){
  return(exp(x)/(b-a))
}

# valores inciales 
a=0
b=2

dev.off()
curve(ftn,a,b,ylim=c(0,4))

abline(h=0,lty=2)
abline(v=c(a,b),lty=2)

#E(e^U(0,2))
integrate(ftn,0,2)
teor=(exp(b)-exp(a))/(b-a)
teor

# Usando la aproximación de Morte Carlo

mc.int=function(ftn,a,b,n,plot=TRUE){
  fx=sapply(runif(n,a,b),ftn)*(b-a)
  if(plot){
    estint=cumsum(fx)/(1:n) # n promedio muestral
    esterr=sqrt(cumsum((fx-estint)^2))/(1:n)
    plot(estint,ylab="Media y ranfo de error",
         type="l",lwd=2,
         ylim=mean(fx)+2*c(-esterr[1],esterr[1]),
         xlab="N de iteraciones")
    abline(h=estint[n],lty=2)
    lines(estint+2*esterr,lty=3)
    lines(estint-2*esterr,lty=3)
    return(list(valor=estint[n],error=2*esterr[n]))
  } else return(list(valor=mean(fx),error=2*sd(fx)/sqrt(n)))
}

set.seed(54321)
res=mc.int(ftn,a,b,500)
abline(h=teor)
res
var1=res$error

res[[1]]-res[[2]]
res[[1]]+res[[2]]

# variables antitéticas

mc.int2=function(ftn,a,b,n,plot=TRUE){
  x=runif(n%/%2,a,b)
  x=as.numeric(matrix(c(x,a+b-x),nrow=2,byrow=TRUE))
  fx=sapply(x, ftn)*(b-a)
  if(plot){
    estint=cumsum(fx)/(1:n) # n promedio muestral
    esterr=sqrt(cumsum((fx-estint)^2))/(1:n)
    plot(estint,ylab="Media y ranfo de error",
         type="l",lwd=2,
         ylim=mean(fx)+2*c(-esterr[1],esterr[1]),
         xlab="N de iteraciones")
    abline(h=estint[n],lty=2)
    lines(estint+2*esterr,lty=3)
    lines(estint-2*esterr,lty=3)
    return(list(valor=estint[n],error=2*esterr[n]))
  } else return(list(valor=mean(fx),error=2*sd(fx)/sqrt(n)))
}

set.seed(54321)
res=mc.int2(ftn,a,b,500)
abline(h=teor)
res
var2=res$error

# para ver la precisión de aproximación

100*(var1-var2)/var1


# mejorar 

mc.int3=function(ftn,a,b,n,plot=TRUE){
  x=runif(n%/%2,a,b)
  x=matrix(c(x,a+b-x),nrow=2,byrow=TRUE)
  fx=apply(x,1,ftn)*(b-a)
  corr=cor(fx[,1],fx[,2])
  fx=as.numeric(fx)
  return(list(valor=mean(fx),error=2*sd(fx)/sqrt(n)*sqrt(1+corr)))
}

set.seed(54321)
res=mc.int3(ftn,a,b,500)
abline(h=teor)
res
var3=res$error

# para ver la precisión de aproximación

100*(var1-var3)/var1
