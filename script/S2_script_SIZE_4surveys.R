#**************************************************************************************#
#**************************************************************************************#
#
#                          Análisis de encuestas por muestreo
#                                        2025
#                         Sociedad Ecuatoriana de Estadística
#                                 Tamaño de muestra
#
#         Creado por:               División de Estadística de la CEPAL
#         Fecha de creación:        15/06/2016
#         Actualizado por:          Andrés Peña M.
#         Fecha de actualización:   17/07/2025
#         Contacto:                 andres.pena.montalvo@ciencias.unam.mx
#
#**************************************************************************************#
#**************************************************************************************#


library(dplyr)
library(TeachingSampling)
library(samplesize4surveys)


data("BigCity")

Hogares <- BigCity %>% group_by(HHID) %>%
  summarise(Estrato = unique(Zone),
            Personas = n(),
            Ingreso = sum(Income),
            Gasto = sum(Expenditure),
            Pobreza = unique(Poverty))

attach(Hogares)


# Tamaño de muestra para estimar la media de la variable ingreso con 
# un margen de error relativo menor al 5%.

N <- nrow(Hogares)
mu <- mean(Ingreso)
sigma <- sd(Ingreso)
rme <- 0.05
ss4m(N = N, mu = mu, sigma = sigma,error = "rme", delta = rme, plot = T)


# Tamaño de muestra para estimar la media de la variable ingreso con un error 
# absoluto menor a 10 dólares.

me <- 10
rme <- me/mu
rme


ss4m(N = N, mu = mu, sigma = sigma, error = "me", delta = 10)

ss4m(N = N, mu = mu, sigma = sigma, error = "rme", delta = rme)

ss4m(N = N, mu = mu, sigma = sigma, error = "me", delta = 10, plot = TRUE)


# Tamaño de muestra para estimar la proporción de la variable 
# pobreza == “pobreza relativa” con un margen de error menor al 2%.

prop.table(table(Pobreza))

N <- nrow(Hogares)
P <- prop.table(table(Pobreza))[3]
me <- 0.02
ss4p(N = N, P = P, error = "me", delta = me, plot = T)


# Tamaño de muestra para estimar la proporción de la variable 
# pobreza == “pobreza relativa” con un margen de error relativo menor al 2 %.

rme <- 0.02
me <- rme * P
me

ss4p(N = N, P = P, error = "me", delta = me)

ss4p(N = N, P = P, error = "me",delta = me, plot = TRUE)


"------------------------------------------------------------------------------"
"Usando el DEFF"


# Tamaño de muestra para estimar la media de la variable ingreso con un margen 
# de error relativo menor al 5%.

attach(Hogares)
N <- nrow(Hogares)
mu <- mean(Ingreso)
sigma <- sd(Ingreso)
deff <- 2.5
rme <- 0.05

ss4m(N = N, mu = mu, sigma = sigma, DEFF = deff, delta = rme, error = "rme")


# Tamaño de muestra para estimar la media de la variable ingreso con un 
# error relativo menor a 10 dólares.

me <- 10
rme <- me/mu
rme


ss4m(N = N, mu = mu, sigma = sigma,DEFF = deff, delta = rme, error = "rme")

ss4m(N = N, mu = mu, sigma = sigma,DEFF = deff, delta = me, error = "me")

ss4m(N = N, mu = mu, sigma = sigma, DEFF = deff, delta = me, error = "me", plot = TRUE)


# Tamaño de muestra para estimar la proporción de la variable 
# pobreza == “pobreza relativa” con un margen de error menor al 2 %.

prop.table(table(Pobreza))

N <- nrow(Hogares)
P <- prop.table(table(Pobreza))[3]
deff <- 2.5
me <- 0.02

ss4p(N = N, P = P, DEFF = deff, delta = me, error = "me")

# tamaño de muestra para estimar la proporción de la variable 
# pobreza == “pobreza relativa” con un margen de error relativo menor al 2 %.

rme <- 0.02
me <- rme * P
me

ss4p(N = N, P = P, DEFF = deff, delta = me, error = "me")

ss4p(N = N, P = P, DEFF = deff, delta = rme, error = "rme")

ss4p(N = N, P = P, DEFF = deff, delta = rme, error = "rme",plot = TRUE)


"------------------------------------------------------------------------------"
"------------------------------------------------------------------------------"

# Calculo del tamaño para la proporcion

BigCity1 <- BigCity[!is.na(BigCity$Employment), ]
summary(BigCity1$Employment)

BigCity1$Unemp <- Domains(BigCity1$Employment)[, 1]
BigCity1$Active <- Domains(BigCity1$Employment)[, 1] +
  Domains(BigCity1$Employment)[, 3]

N <- nrow(BigCity)
M <- length(unique(BigCity$PSU))
r <- sum(BigCity1$Active)/N
b <- N/length(unique(BigCity$HHID))
rho <- ICC(BigCity1$Unemp, BigCity1$PSU)$ICC
P <- sum(BigCity1$Unemp)/sum(BigCity1$Active)
delta <- 0.07

conf <- 0.95
m <- c(5:15)
m <- seq(5, 45, 5)

error <- P * delta
P - 1.96 * error
P + 1.96 * error


ss4HHSp(N, M, r, b, rho, P, delta, conf, m)



# Calculo para la media de los ingresos

BigCity1 <- BigCity %>%
  group_by(HHID) %>%
  summarise(IncomeHH = sum(Income),
            PSU = unique(PSU))

head(BigCity1)

summary(BigCity1$IncomeHH)

mean(BigCity1$IncomeHH)

sd(BigCity1$IncomeHH)


N <- nrow(BigCity)
M <- length(unique(BigCity$PSU))
rho <- ICC(BigCity1$IncomeHH, BigCity1$PSU)$ICC
mu <- mean(BigCity1$IncomeHH)
sigma <- sd(BigCity1$IncomeHH)
delta <- 0.05
conf <- 0.95
m <- c(5:15)

ss4HHSm(N, M, rho, mu, sigma, delta, conf, m)
