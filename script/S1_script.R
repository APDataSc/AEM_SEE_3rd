
# 1) Remover objetos
rm(list = ls())

# 2) Instalar paquetería
# install.packages("dplyr", dependencies = T)
# install.packages("survey", dependencies = T)
# install.packages("calidad", dependencies = T)

# 3) Cargar paquetería
library(dplyr)
library(survey)
library(calidad)

# 3.1) Observar las funciones de un paquete
ls("package:calidad")
ls("package:survey")

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


