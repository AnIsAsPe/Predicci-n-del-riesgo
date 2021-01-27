library(ggplot2)
library(fBasics)
library(forecast)




#1. Objeto serie de tiempo en R ------------------------------------------------

# Creamos unos datos
valores <- runif(n = 50, min = 10, max = 45)

# ts es la clase "time series"

mi_serie <- ts(data = valores, 
                  start = 1956, frequency = 4)

plot(mi_serie)


time(mi_serie)

#Redefinir el inicio
mi_serie <- ts(data = valores, 
                  start = c(1956,3), frequency = 4)
plot(mi_serie)

#con ggplot2

autoplot(mi_serie) + ggtitle("Mi Serie")

# función auto correlación
acf(mi_serie)

#función auto correlación parcial
pacf(mi_serie)

#2. Proceso iid ----------------------------------------------------------------

n <- 1000
set.seed(7)
iid <- ts(rnorm(n, mean=0, sd = 1))

ggtsdisplay(iid,  main='Independiente e identicamente distribuido',
            plot.type = 'histogram') 

#3. Ruido Blanco ---------------------------------------------------------------
#Ejemplo 02.3 Diferencia entre proceso iid y ruido blanco 

# 1) Sea (Xt ) un proceso i.i.d. e Y una variable aleatoria de Bernoulli con
#    parámetro p = 0.5 . Entonces la variable aleatoria Zt = XtY - Xt (Y - 1)^2
#    es un ruido blanco.


set.seed(5)
X1 <- rnorm(n)
Y1 <- rbinom(n, size = 1 , prob=0.5)
Z1 <- X1*Y1 - X1*(Y1-1)^2


ggtsdisplay(Z1, main='Ruido Blanco Z1', plot.type = 'histogram') 

# 2) Sea (Xt ) un proceso i.i.d. el proceso estocástico definido por
#    Zt = Xt cuando t es par y 
#    Zt  =  (1/sqrt(2)) * (X^2-1)

set.seed(43)
X2 = rnorm(n/2)
Z2 = rep(0, n )

idx_par = seq(2, length(Z2),2)
idx_impar = seq(3,length(Z2),2)

Z2[1] <- rnorm(1)
Z2[idx_par] <- X2
Z2[idx_impar] <- (1/sqrt(2)) * (X2[1:499]^2-1)


ggtsdisplay(Z2, main='Ruido Blanco Z2', plot.type = 'histogram') 



# 4. Modelo Auto Regresivo (AR) (parametro p)==================================

#4.1. AR(1)---------------------------------------------------------------------

#usaremos la función arima.sim, para lo que debemos especificar antes el modelo primero

AR <- list(order = c(1, 0, 0), ar = 0.9)  #ar es el valor del coeficiente 
                                            #(entre -1 y 1)

set.seed(3)
AR1 <- arima.sim(n = 50, model = AR)

ggtsdisplay(AR1, main='AR1', plot.type = 'partial') 

#4.2. Modeloa AR con creciente orden p -------------------------------------------

set.seed(123)

AR_coef <- c(0.7, 0.2, -0.1, -0.3)  #fi_1, fi_2, fi_3, fi4

AR_mods <- list()  #lista vacía que contendrá los modelos


for (p in 1:4) {
  AR_mods[[p]] <- arima.sim(n = 10000, list(ar = AR_coef[1:p]))
}

#Visualización de los 4 modelos 

par(mfrow = c(4, 3))  #número de gráficas (3 por cada modelo)
p
for (p in 1:4) {
  plot.ts(AR_mods[[p]][1:50], ylab = paste("AR(", p, ")", sep = ""))
  acf(AR_mods[[p]], lag.max = 12)
  pacf(AR_mods[[p]], lag.max = 12, ylab = "PACF")
}

#5. Modelo Media Movil (MA) (parametro q)========================================


# 5.1 MA(1) --------------------------------------------------------------------
MA <- list(order = c(0, 0, 1), ma = 0.9)  #ma es el valor del coeficiente 
#(entre -1 y 1)

set.seed(7)
MA1 <- arima.sim(n = 50, model = MA)

ggtsdisplay(MA1, main='MA1', plot.type = 'partial') 

#5.2. Modeloa MA con creciente orden q -----------------------------------------

set.seed(123)

MA_coef <- c(0.7, 0.2, -0.1, -0.3)

MA_mods <- list()

for (q in 1:4) {

  MA_mods[[q]] <- arima.sim(n = 1000, list(ma = MA_coef[1:q]))
}
  
# Visualización 

par(mfrow = c(4, 3))

for (q in 1:4) {
  plot.ts(MA_mods[[q]][1:50], ylab = paste("MA(", q, ")", sep = ""))
  acf(MA_mods[[q]], lag.max = 12)
  pacf(MA_mods[[q]], lag.max = 12, ylab = "PACF")
}

#6. Modelo ARMA (parametros p y q) =============================================

set.seed(123)

ARMA22 <- list(order = c(2, 0, 2),
               ar = c(-0.4, 0.2),
               ma = c(0.4, 0.2))

## media 
mu <- 5

## simulación del proceso arma(2,2) 
ARMA_sim <- arima.sim(n = 1000, model = ARMA22) + mu

# Qué también podemos encontrar los parametros a partir de la serie simulada

arima(x=ARMA_sim, order=c(2, 0 ,2))

  


