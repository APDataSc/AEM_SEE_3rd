
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

# 4) Cargar las tablas de datos
data <- epf_personas
data1 <- casen 

# 5) Análisis
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
