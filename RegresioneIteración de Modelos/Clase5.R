##
# Analisis descriptivo 
#

# Tablas de frecuencias 

# install.packages("flextable")

library(tidyverse)
library(flextable) # tablas
library(car) # defecto
data(Prestige)
base=tbl_df(Prestige)
base %>% 
  glimpse()

# Tabla de frecuencias Cualitativa
base %>% 
  group_by(type) %>% 
  summarise(fi=n()) %>% 
  filter(type!="NA") %>% 
  mutate(hi=round(fi/sum(fi),2),pi=hi*100) %>% 
  flextable() %>% 
  set_caption("Table 01: tabla de frecuenias segun tipo de ocupación")

# tabla de frecuencias Cuantitativas
base %>% 
  mutate(interv=cut(education,breaks = 4)) %>% 
  group_by(interv) %>% 
  summarise(fi=n()) %>% 
  mutate(Fi=cumsum(fi),
         hi=round(fi/sum(fi),2),
         Hi=cumsum(hi),
         pi=hi*100) %>%
  flextable()

# Resumen Estadisticos 

## install.packages("moments")

library(moments) # añadir asimetria y kustosis 

base %>% 
  summarise(prome=mean(education),
            sd=sd(education),
            asimetria=skewness(education),
            apuntamiento=kurtosis(education),
            minimo=min(education),
            maximo=max(education)) %>% 
  flextable()
  
lstfunt=list(n=length,
             media=mean,
             sd=sd,
             asimetria=skewness,
             curtosis=kurtosis,
             medina=median) 

base %>% 
  summarise_at(vars(education),
               lstfunt) %>%
  flextable() %>% 
  autofit

## explorando datos perdidos

## diagrama de cajas
base %>% 
  filter(type!="NA") %>% 
ggplot()+
  geom_boxplot(aes(x=type,education))

#grafico de violines
base %>% 
  filter(type!="NA") %>% 
  ggplot()+
  geom_violin(aes(y=type,x=education))


# modelos de regresión lineal 

 modm=lm(prestige~education+women+income,data=base)
 anova(modm)
 summary(modm)
 
 #install.packages(broom)
 library(broom)
 tidy(anova(modm))

 # supuestos de linealidad
 
 #install.package("psych")
  base %>% 
   psych::pairs.panels()

  # Multicolinealidad   
# install.packages("Hmisc")
   p=base %>% 
     select(-type)
Hmisc::rcorr(as.matrix(p))  

#suspuesto de independecia de errores

# Ho: error no autocorrelacionados
# Hi: error si estan autocorrelacionados

# dw para aceptar h0: 1.5 a 2.5

#durwin watson
#install.packages("lmtest")
dw=lmtest::dwtest(modm) 
dw
dw$statistic

#multicolinealidad 

# Valor de inflación de la varianza (VIF)
# VIF = 1 no hay multicolinealidad
# vif = 4 y 10 sospechas
# vif > 10 existe 

# install.packages("zoo")
car::vif(modm)

#contraste normalidad 
#Ho: datos provien de una distribucion normal
#H1: 

error=modm$residuals
shapiro.test(error)      # n<25 
ks.test(error,"pnorm")  # >=25

qqnorm(error)
qqline(error)









