########## EJERCICIO CLASE 11 SPARK R ####################################
# Generar una variable numerica  
# que tenga una distribución uniforme 
# a la que llamaremos Edad (min = 1,max = 75)

# Responder a las siguientes preguntas:

#  P1: ¿ Cual es la edad promedio de las personas que
# tienen cabello Negro? y 
# ¿cuantas personas en promedio tienen
# ese color de cabello?
# Hint: usar summarise_at o summarise_if


# P2: ¿Cuales son los momentos de las variables Frecuencia y Edad 
# agrupadas por sexo?

# P3: ¿Cual es el segundo color más frecuente de Ojos para las Mujeres?

# P4: ¿Que color de ojos tienen las mujeres de mayor edad que 
# tienen el Cabello Rojo?


# Solución ----------------------------------------------------------------

# Librerias

library(dplyr)
library(datasets)

# Datos

data <- datasets::HairEyeColor %>% 
  data.frame() %>% 
  mutate(Edad = runif(n = n(), min = 1, max = 75))


# P1:

data %>% 
  group_by(Hair) %>% 
  summarise_at(.vars = c("Freq","Edad"),
               .funs = funs(mean)) %>% 
  ungroup() %>% 
  filter(Hair == "Black")

# P2:

data %>% 
  group_by(Sex) %>% 
  summarise_if(is.numeric,funs(max = max(.),
                               min = min(.),
                               sd = sd(.),
                               Q25 = quantile(.,probs = 0.25),
                               Q50 = quantile(.,probs = 0.5),
                               Q75 = quantile(.,probs = 0.75))) %>% 
  ungroup() 

# P3:


data %>% 
  group_by(Eye,Sex) %>% 
  summarise_if(is.numeric,funs(sum)) %>% 
  filter(Sex == "Female") %>% 
  arrange(desc(Freq)) %>% 
  filter(Freq == nth(.$Freq,2))


# P4:


data %>% 
  group_by(Eye,Sex,Hair) %>% 
  summarise_if(is.numeric,funs(max)) %>% 
  filter(Hair == "Red",
         Sex == "Female") %>% 
  arrange(desc(Freq)) %>% 
  filter(Freq == first(.$Freq))
