# iteración de modelos 

library(tidyverse)
data(iris)
iris=tbl_df(iris)
iris

#mostrando el tipo de datos
iris %>% 
  class()

#divir la base de datos por variable (base anidada)
iris_esp=iris %>% 
  nest(-Species)

iris_esp

iris_esp$Species
iris_esp$data

## volver a original
iris_esp=iris %>% 
  unnest(-Species)
iris_esp

num1=c(2,4,5)
num2=list(2,4,5)

sqrt(num1)
sqrt(num2[[1]])

# hay comando de libreria purr 
#map

map(.x=num2,.f=sqrt)
map(.x=num2,~.x*2+1) # 

# otra manera 
iris_esp=iris %>% 
  group_by(Species) %>% 
  nest()

iris_esp

## selecionado data set anidada

iris_esp$data %>% 
  map(~.x$Sepal.Length)

mean(iris$Sepal.Length)

map_dbl(iris_esp$data,~mean(.x$Sepal.Length))
map_dbl(iris_esp$data,~sd(.x$Sepal.Length))

iris_esp %>% 
  mutate(mean=map_dbl(data,~mean(.x$Sepal.Length)),
         sd=map_dbl(data,~sd(.x$Sepal.Length)))

glimpse(iris)
glimpse(iris_esp)

## utilizando libreria broom
library(broom)
iris_mod=lm(Petal.Length~Sepal.Length,data=iris)
iris_mod
p=tidy(iris_mod)
p
p %>% 
  filter(term=="Sepal.Length") %>% 
  select(estimate)

#visualizamos mas informacion del modelo
glance(iris_mod)

#mostramos valores del modelo
augment(iris_mod)


#creando modelo iterativos
modei=iris_esp %>% 
  mutate(mode=map(data,~lm(Petal.Length~Sepal.Length,data=.x)))

modei$mode

modei$mode[[1]] %>% 
  glance() %>% 
  .$r.squared

p=iris_esp %>% 
  mutate(modei=map(data,~lm(Petal.Length~Sepal.Length,data=.x)),
         mod_grance=map(modei,glance),
         rsq=map_dbl(mod_grance,~.x$r.squared),
         AICF=map_dbl(mod_grance,~.x$AIC),
         BICF=map_dbl(mod_grance,~.x$BIC)
  )

p