# Generaci?n de numeros Pseudoaleatorios
library(tidyverse)
library(descr) ## estadisticas descriptivas


## Recodar 

# modulo
179%%100 

# divisi?n entero 
179%/%100

## crear una tabla de frecuencias 
tblf=function(vecb,k){
  inter=cut(vecb,breaks=k)
  tabla=freq(ordered(inter),plot=TRUE)
  tabla
}

# Algoritmo de cuadro medios 
c2_ale=function(semilla,n){
  semillas=numeric(1)
  ri=numeric(1)  #numero pseudoaleatorio
  for (i in 1:n){
    x=semilla^2
    y=str_length(x) #6 8
    p=(y-4)/2       #(8-4)/2=2
    semilla=as.numeric(str_sub(x,p+1,y-p))
    semillas[i]=semilla
    ri[i]=semilla/10^4
  }
  cbind(semillas,ri)
}

c2_ale(550,10)
p=c2_ale(550,100)
p=tbl_df(p)
x=p$ri
tblf(x,4)  #
plot(density(x),col="red")


# algoritmo producto medio 
mpr_med=function(semi0,semilla,n){
  semillas=numeric(1)
  ri=numeric(1)  #numero pseudoaleatorio
  for (i in 1:n){
    x=semilla*semi0
    semi0=semilla
    y=str_length(x) #6 8
    p=(y-4)/2       #(8-4)/2=2
    semilla=as.numeric(str_sub(x,p+1,y-p))
    semillas[i]=semilla
    ri[i]=semilla/10^4
  }
  cbind(semillas,ri)
}

x0=6965
x1=9803

mpr_med(x0,x1,100)
p=mpr_med(x0,x1,10)
p=tbl_df(p)
x=p$ri
tblf(x,4)  #
plot(density(x),col="red")#--------

# algoritmo de multiplicaci?n constate
#y=a*x
mult_const=function(semilla,a,n){
  semillas=numeric(1)
  ri=numeric(1)  #numero pseudoaleatorio
  for (i in 1:n){
    x=semilla*a
    y=str_length(x) #6 8
    p=(y-4)/2       #(8-4)/2=2
    semilla=as.numeric(str_sub(x,p+1,y-p))
    semillas[i]=semilla
    ri[i]=semilla/10^4
  }
  cbind(semillas,ri)
}

a=6965
x1=9803

mult_const(x1,a,10)
p=mult_const(x0,x1,10)
p=tbl_df(p)
x=p$ri
tblf(x,4)  #
plot(density(x),col="red")#--------


# algoritmo lineal 

#y=(ax+c)mod(m)
alg_lineal=function(semilla,a,c,n){
  g=str_length(semilla)
  m=10^g
  semillas=numeric(1)
  ri=numeric(1)  #numero pseudoaleatorio
  for (i in 1:n){
    x=(semilla*a+c) %% m
    semilla=x
    semillas[i]=semilla
    ri[i]=semilla/(m-1)
  }
  cbind(semillas,ri)
}

a=19
c=32
x1=37

alg_lineal(x1,a,c,10)
p=alg_lineal(x1,a,c,10000)
p=tbl_df(p)
x=p$ri
tblf(x,4)  #
plot(density(x),col="red")#--------


# algoritmo congruencias multiplicativo
#m=2^g
#a=3+8k    o    a=5+8k     k=0,1,2,3,..  
congr_mult=function(semilla,a0,k,g,n){
  a=a0+8*k
  m=2^g
  semillas=numeric(1)
  ri=numeric(1)  #numero pseudoaleatorio
  for (i in 1:n){
    x=(semilla*a) %% m
    semilla=x
    semillas[i]=semilla
    ri[i]=semilla/(m-1)
  }
  cbind(semillas,ri)
}

a0=5 # 5 o 3 
k=2
x1=17
g=5

congr_mult(x1,a0,k,g,10)
p=congr_mult(x1,a0,k,g,1000)
p=tbl_df(p)
x=p$ri
tblf(x,4)  #
plot(density(x),col="red")#--------


# pruebas de diagnostico de aleatoridad

# prueba media = 0.5
# H0: mean(ri)=0.5 H1: mean(ri)<>0.5
t.test(p$ri,
       alternative =c("two.sided"),
       mu=0.5, conf.level = 0.95)

# prueba de varianza=0.5
# install.packages("TeachingDemos")
library(TeachingDemos)
sigma.test(x=p$ri,
           alternative ="two.sided",
           sigma=0.5,conf.level = 0.95)

## uniformidad

# ajuste a una uniforme
chisq.test(p$ri,rescale.p = TRUE,
           correct=TRUE)

# contrastes normalidad
shapiro.test(p$ri)
nortest::lillie.test(p$ri)

#ri=1 2 2 2 1 2 1 2 3 4 1 1 
# + - - + +
# prueba de corridas 
# H0: los ri son independientes
# H1: los ri son dependetientes
test.corrida=function(xbse){
  y=length(xbse)
  t=numeric(y-1)
  for(i in 2:y){
    t[i-1]=if_else(xbse[i]>xbse[i-1],"+","-", missing = NULL)
  }
  s=0
  for(i in 1:(y-2)){
    x=if_else(t[i]==t[i+1],1,0, missing = NULL)
    s=s+x
  }
  h=y-1-s
  Eh=round((2*y-1)/3,2)
  vh=round((16*y-29)/90,2)
  z=round((h-Eh)/sqrt(vh),3)
  p_value=round(pnorm(z),3)
  tblr=data.frame(h,Eh,vh,z,p_value)
  return(tblr)
}
test.corrida(p$ri)
