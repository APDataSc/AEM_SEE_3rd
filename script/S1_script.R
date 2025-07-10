
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
  
  






