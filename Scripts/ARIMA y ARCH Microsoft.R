
#0. Actualizar la última versión de R ==========================================

# En windows, instalar antes Rtools, desde 
# https://cran.r-project.org/bin/windows/Rtools/index.html

#Después en RGui correr las lineas
#library(installr)
#updateR()

#1. Cargar librerías ==========================================================

library(tidyverse)  #collección de las principales bibliotecas 

library(forecast)   #métodos y herramientas para analizar st univariadas
library(tseries)    #análisis de st y finanzas computacionales
library(fGarch)     #Modelar hetercedasticidad en st financieras
library(rugarch)

library(xts)     #extensiones para manejar st
library(fBasics) #funciones para explorar retornos financieros

library(here)       #Para optimizar el manejo de rutas a archivos
library(gridExtra)  #para graficar más de una gráfica simultaneamente


#2. Lectura de datos ===========================================================

#histórico de los últimos 5 años obtenidos el 24/01/21 de https://finance.yahoo.com/

MSFT <- read.csv("https://raw.githubusercontent.com/AnIsAsPe/Predicci-n-del-riesgo/main/Datos/MSFT%202016-2021.csv")  


# Convertir en formato de fecha la variable date
MSFT$Date <- strptime(as.character(MSFT$Date), format="%Y-%m-%d")

precio <- xts(MSFT$Close, MSFT$Date)  #Crear serie de tiempo 


    # diferencias entre ts y xts 
    # https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781788629157/4/ch04lvl1sec35/xts-zoo-or-ts-which-one-to-use

#3. Analisis exploratorio de la serie de tiempo ================================

autoplot(precio) + geom_line(color="darkblue") +
labs(x = '' , y = 'Price' , title = "Precios Microsoft") +
geom_hline(yintercept = mean(precio) , color = 'red')

#observamos que no es una serie estacionaria.

#3.1 Transformar en estacionaria la serie --------------------------------------

#3.1.1 Se elimina la tendencia exponencial de los precios

log.precio <- log(precio)  

   # comparación de precios con log.precios

p1 = autoplot(precio) + geom_line(color="darkblue") +
  labs(x = '' , y = 'Price' , title = "Precios Microsoft") +
  geom_hline(yintercept = mean(precio) , color = 'red')

p2 = autoplot(log.precio) + geom_line(color="darkblue") +
  labs(x = '' , y = 'Price' , title = "Log_Precios Microsoft") +
  geom_hline(yintercept = mean(log.precio) , color = 'red')

gridExtra::grid.arrange(p1 , p2 , ncol = 2)

# Se estabiliza la media

retornos <- diff(log.precio)  #porcentaje de cambio diario del precio

retornos <- retornos[!is.na(retornos)]  #excuir primer valor (es na)

autoplot(retornos) + geom_line(color="darkblue") +
  labs(x = '' , y = 'Retorno diario' , 
       title = "Retornos Microsoft Inc") +
  geom_hline(yintercept = mean(retornos) , color = 'red')


# ¿Qué diferencias tienen las siguientes visualizaciones?

ggtsdisplay(log.precio, plot.type = 'histogram') 

ggtsdisplay(retornos, plot.type = 'histogram') 



#La grafica ACF es util para identificar si la serie es o no estacionaria.
#En una serie estacionaria, la ACF cae rapidamente a cero

#3.2 Dickey-Fuller Test  ------------------------------------------------------

#Ho: La serie no es estacionaria

adf.test(retornos)

#es estacionaria

#4. Seleccionar el modelo  (ARIMA) =============================================



#4.1 Motodología Box-Jenkins ----------------------------------
                      #       ARIMA (p, d, q)
      #Modelo Autorregresivo               AR   (PACF) media
      #Modelo Medias Móviles         MA         (ACF)  residuos

ggtsdisplay(retornos, plot.type = 'partial') 

#Tanto en ACF y PACF vemos que hay algunos retrasos significativos en los 
# dentro de los primeros 9 retrasos

#Debemos buscar el orden del modelo ARMA

# 4.2 Selección de modelo con menor AIC (Akaike Information Criteria)-----------

# https://otexts.com/fpp2/arima.html

auto.arima(retornos, stationary=T, max.p = 9, max.q = 10, 
           trace = F,  stepwise=F, approximation = F)

      #observar que corresponde al mismo modelo medido en la serie log.citigr 

auto.arima(log.precio, stationary=F, max.p = 9, max.q = 10, 
           trace = F, stepwise=F, approximation =F)
 
ARMA_21 <- arima(retornos, order=c(2,0,1))            #stats
ARMA_21

ARIMA211 <- arima(log.precio, order=c(2,1,1))



#5.Exploración de los residuos del modelo MA2 para el retorno===================

res_ARMA_21 <- ARMA_21$residuals

#5.1 Exploración visual --------------------------------------------------------


ggtsdisplay(res_ARMA_21, 
            plot.type = 'partial')
    #existe patrones en los residuos que se puedn modelar


#5.2 Test Ljung-Box --------------------------------------------------
      # Ho: existe normalidad en los residuos 

checkresiduals(ARMA_21) #forecast


      # Rechazamos la hipotesis nula; los residuos no tienen dist. normal 
     

#6 Modelar volatilidad==========================================================


#6.0.1 Exploración visual de los residuos al cuadrado---------------------------

plot((res_ARMA_21)^2, main='Residuos al cuadrado')


    # se observan clusters de volatilidad en los residuos al cuadrado


   #confirmamos mirando las graficas ACF y PACF de los residuos al cuadrado
ggtsdisplay((res_ARMA_21)^2, main='Residuos al cuadrado',   plot.type = 'partial')

# 6.0.2 Test sobre homocedasticidad de los residuos al cuadrado.---------------
      #Ho: No existe autocorrelación de los residuos
      #Ha: Existe correlación de los residuos al cuadrado: son heterocedasticos

Box.test((res_ARMA_21)^2, lag=25, fitdf=3, type='Ljung-Box') #fitdf = p+q

      # se rechaza Ho y se puede utilizar ARCH



#6.1 GARCH(1,1) DUDA DISTR RESIDUOS-----------------------------------------------------------------

garch11 <- garchFit( ~ garch(1,1), res_ARMA_21, includ.mean=F,
                   cond.dist='std', trace=F)
summary(garch11)

  #los p-value de los parametros son menos de 0.05. 
     #Rechazamos que no son significativos.

  #los p-value del test Box Ljung son mayores a 0.05 para residuos al cuadrado, 
     # por tanto no rechazamos la hipoteis de que se distribuyen de forma independiente


#6.2 Value at Risk -------------------------------------------------------------

#Riesgo del 5% en una distribución normal

quantile(retornos, 0.05)

qplot(retornos , geom = 'histogram', bins=200) + 
  geom_histogram(fill = 'lightblue' , bins = 200) +
  geom_histogram(aes(retornos[retornos < quantile(retornos , 0.05)], bins=200), 
                 fill = 'red', bins = 200) +
  labs(x = 'Retorno diario')


#¿La serie retornos sigue una distribución normal?
basicStats(retornos)    #fBasics
qplot(retornos , geom = 'histogram', bins=200)
jarque.bera.test(retornos) # ho: es una distribución normal

#volatilidad condicional
sigma <- volatility(garch11, type = "sigma") # en rugarch model$fit$fit$sigma 

plot(sigma,  main='volatilidad condicional', type='l')


# La distribución no es normal

library(TSstudio)
ret <- xts_to_ts(retornos, frequency =365)  

qplot(y=retornos, x= 1:1258, geom='line') + geom_line(color="gray")+
  geom_line(aes(y = sigma*qdist('std',p=0.05) , x = 1:1258) , colour = 'red') + 
  geom_hline(yintercept = sd(retornos)*qdist('std',p=0.05) , colour = 'coral2', size=1) +
  labs(x = '' , y = 'Retorno diario' , title = 'Comparación Value-at-Risk')


#Intervalos de confianza a partir de la volatilidad
qnorm(0.025)
qdist('sged',p=0.025)

fit211 <- fitted.values(ARIMA211)  
#usamos la función exponencial para hacer el análisis en la serie de precios
modelo = exp(fit211)
low = modelo[1:1258] - exp(qdist('std',p=0.25)*(sigma))
high= modelo[1:1258] + exp(qdist('std',p=0.25)*(sigma))

autoplot(ts(modelo[1:1258]) )+
  labs(title="Microsoft, Low, High") +
  geom_line(y = low , color = 'red') +
  geom_line(y = high, color='steelblue') 
# + geom_line(y = xts_to_ts(precio[1:1258], frequency=365),color='green')


  
# 6.3  Predicción --------------------------------------------------------------
  #predicción del modelo arima
  
  media_retorno <- forecast(ARMA_21,h=100, level=c(99.5))
  plot(media_retorno)

  
  media_precios = forecast(ARIMA211, h=10, level=c(99.5))
  plot(exp(media_precios))
  
  
  #predicción del modelo arima+garch usando la librería rugarch__
  
  spec = ugarchspec(variance.model = list(model='sGARCH', garchOrder=c(1,1)),
                    mean.model = list(armaOrder = c(2,1)), 
                    distribution.model ="std") 
  model.fit <- ugarchfit(spec, data = retornos[1:1248], out.sample=10)

  prediccion = ugarchforecast(model.fit, n.ahed=10,  n.roll=10)
  
  plot(prediccion)  #seleccionar gráficas  1, 2, 3 o 4
  
  prediccion
  retornos[1249:1258]
  
  #comparación coeficientes
  model.fit@fit$matcoef
  coef(ARMA_21)
  coef(garch11)  

  
