#**************************************************************************************#
#**************************************************************************************#
#
#                          Análisis de encuestas por muestreo
#                                        2025
#                         Sociedad Ecuatoriana de Estadística
#                                       Intro
#
#         Fecha de creación:        07/07/2025
#         Actualizado por:          Andrés Peña M.
#         Fecha de actualización:   10/07/2025
#         Contacto:                 andres.pena.montalvo@ciencias.unam.mx
#
#**************************************************************************************#
#**************************************************************************************#

# 1) Remover objetos
rm(list = ls())

# 2) Instalar paquetería
# install.packages("dplyr", dependencies = T)
# install.packages("survey", dependencies = T)
# install.packages("calidad", dependencies = T)
install.packages("rio", dependencies = T)
install.packages("foreign", dependencies = TRUE)

# 3) Cargar paquetería
library(dplyr)
library(survey)
library(calidad)
library(rio)
library(foreign)
library(tictoc)

# 3.1) Observar las funciones de un paquete
ls("package:calidad")
ls("package:survey")
ls("package:rio")

# 4) Determinar el directorio de trabajo 
setwd("C:/Users/LENOVO/Desktop/SEE_2025/AEM_SEE_3rd/data")
getwd()
dir()

# 5) Cargar las tablas de datos
data <- epf_personas
data1 <- casen 

# 6) Análisis
names(data)
sum(data$fe)
sum(data1$expr)


# Muestras con y sin reemplazo
N <- 3
n <- 2

N^n #CR
choose(N, n) #SR

# 5) Análisis
mean(data$gastot_hd) #MAS
mean(data$ocupado) #MAS

dm <- svydesign(ids = ~varunit, 
                strata = ~varstrat, 
                data = data, 
                weights = ~fe
                )

summary(dm)

svymean(design = dm, x = ~ocupado)
svymean(design = dm, x = ~gastot_hd)


# Factores en R

vec <- c(1, 2, 2, 1, 2, 1, 2)
fac <- factor(vec, levels = 1:2, labels = c("Hombre", "Mujer"))

table(fac)
prop.table(table(fac))



# Manejo de data.frames en R
tic()
data <- read.spss("10 ENIGHUR11_HOGARES_AGREGADOS.SAV",
                  to.data.frame = T, use.value.labels = T
                  )
toc()

tic()
data1 <- rio::import("10 ENIGHUR11_HOGARES_AGREGADOS.SAV")
toc()

class(data1)


# Análisis de encuesta

sum(data$Fexp_cen2010)
mean(data$numpers)
3923123*3.873186

names(data)

data <- data %>% select(Fexp_cen2010, Provincia, Área, sexo,
                        edad, ing_cor_tot, gas_cor_tot, d1, numpers) 

#Ctrl+Shift+m
#Alt+"-"
tab1 <- data %>%
             group_by(Provincia) %>% 
             summarise(media_ing = mean(ing_cor_tot)) %>% 
             arrange(desc(media_ing))

# Creación de variables  
data <- data %>% 
             mutate(ing_per = ing_cor_tot/numpers,
                    pobre = ifelse(ing_per<80, 1, 0))

tab2 <- data %>% 
             group_by(Provincia) %>% 
             summarise(pobreza = mean(pobre)) %>% 
             arrange(desc(pobreza))

# Con factor de expansión
w_mean <- function(x_i, w_i){
  w_mean <- sum(x_i*w_i)/sum(w_i)
  return(w_mean)
}

x <- 1:5
w <- rep(0.2, 5) 
w_mean(x, w)


tab3 <- data %>% 
             group_by(Provincia) %>% 
             summarise(media_ing_fexp = w_mean(ing_cor_tot, Fexp_cen2010)) %>% 
             arrange(desc(media_ing_fexp))

# Unión de dos tablas
tab4 <- left_join(tab1, tab3, "Provincia") 


# Distribuciones de los estimadores
#Permutaciones y combinaciones
install.packages("gtools", dependencies = TRUE)
library(gtools)

#CR
(x<-1:4)
4^2

permutations(n=4,r=2,v=x,repeats.allowed=T)

choose(4,2) #Duplas posibles sin repetición

combinations(4, 2, v=x)


# Ejemplo de distribución de la media muestral CR
x<-1:4
n<-length(x)
(mu<-mean(x))

(va<-sum((x-mu)^2)/n)

muestras<-permutations(n=4,r=2,v=x,repeats.allowed=T)
(xbar_n_i<-rowMeans(muestras))

(fx_i<-prop.table(table(xbar_n_i)))

barplot(prop.table(table(xbar_n_i)))

xbar_i<-unique(xbar_n_i)
(esp_xbar<-sum(xbar_i*fx_i))
(var_xbar<-sum((xbar_i-esp_xbar)^2*fx_i))



#Teorema del límite central
N <- rbinom(1000, 100, 0.01)
#N <- rexp(1000, 1/10)
#N <- runif(1000, 10, 50)

n <- numeric(100)

for (i in 1:500) {
  n[i] <- mean(sample(N, 100, replace = TRUE))
}

par(mfrow=c(1,2))
hist(N, probability = T)
hist(n, probability = T)
curve(dnorm(x, mean(n), sd(n)), col = 2, lty = 2,
      lwd = 2, add=T)
par(mfrow=c(1,1))