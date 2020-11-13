
#0. Actualizar la última versión de R ==========================================

# En windows, instalar antes Rtools, desde 
# https://cran.r-project.org/bin/windows/Rtools/index.html

#Despues en RGui correr las lineas
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

stocks = read.csv(here("Datos", "all_stocks_5yr.csv"))  #leer archivo csv

stocks = stocks %>% select(date, close , Name )     #selección de columnas

stocks = stocks %>% spread(key = Name , value = close) #reacomodar la tabla

# Convertir en formato de fecha la variable date
stocks$date = strptime(as.character(stocks$date), format="%Y-%m-%d")

citygr = xts(stocks$C, stocks$date)  #Crear serie de tiempo

#3. Analisis exploratorio de la serie de tiempo ================================


#Transformación logarítmica 

log.citygr = log(citygr)  #la estacionarización de esta serie,
                          #la convertirá en la serie de retornos

#Comparación de serie original y serie transformada

p1 = autoplot(citygr) + geom_line(color="darkblue") +
  labs(x = '' , y = 'Price' , title = "Precios Citigroup Inc.") +
  geom_hline(yintercept = mean(citygr) , color = 'red')

p2 = autoplot(log.citygr) + geom_line(color="darkblue") +
  labs(x = '' , y = 'Price' , title = "Log Citigroup Inc.") +
  geom_hline(yintercept = mean(log.citygr) , color = 'red')

gridExtra::grid.arrange(p1 , p2 , ncol = 2)


#3.1 Transformar en estacionaria la serie --------------------------------------

retornos= diff(log.citygr) 
retornos = retornos[!is.na(retornos)]  #excuir primer valor (es na)

autoplot(retornos) + geom_line(color="darkblue") +
  labs(x = '' , y = 'Retorno diario' , 
       title = "Retornos Citygroup Inc") +
  geom_hline(yintercept = mean(retornos) , color = 'red')

# ¿Qué diferencias tienen las siguientes visualizaciones?

ggtsdisplay(log.citygr, plot.type = 'histogram') 
ggtsdisplay(retornos, plot.type = 'histogram') 



#3.2 Dickey-Fuller Test  ------------------------------------------------------

#Ho: La serie no es estacionaria

adf.test(retornos)

#es estacionaria

#4. Seleccionar el modelo  (ARIMA) =============================================

#4.1 Motodología Box-Jenkins ----------------------------------
                      #       ARIMA (p, d, q)
      #Modelo Autorregresivo               AR   (PACF)
      #Modelo Medias Móviles         MA         (ACF)

# ACF y PACF  

    #Para determinar q (MA)
acf(retornos, main = 'ACF Retornos diarios Citygr\n(difference_log_Citygr)',
    lag.max=100, ylim=c(-1,1))
    #No existen retrasos significativos por tanto q=0

    #Para determinar p (AR)
pacf(retornos,main = 'PACF Retornos diarios Citygr\n(difference_log_Citygr)',
     lag.max=100, ylim=c(-1,1))

      # No existen retrasos significativos tampoco p= 0, 


# 4.2 Selección de modelo con menor AIC (Akaike Information Criteria)


auto.arima(retornos, stationary=T, trace = F, stepwise=F, approximation =F)

      #observar que corresponde al mismo modelo medido en la serie log.citygr 

auto.arima(log.citygr, stationary=F, trace = F, stepwise=F, approximation =F)

MA2 = arima(retornos, order=c(0,0,2))            #stats
MA2

ARIMA012 = arima(log.citygr, order=c(0,1,2))


#5.Exploración de los residuos del modelo MA2 para el retorno===================

res_MA2 = MA2$residuals

#5.1 Exploración visual --------------------------------------------------------

ggtsdisplay(res_MA2, 
            plot.type = 'histogram') #se puede escoger histograma
                                   #o pacf en la última gráfica

#5.2 Test Ljung-Box --------------------------------------------------
      # Ho: existe normalidad en los residuos 

checkresiduals(MA2) #forecast

      # No podemos rechazar la hipótesis nula. 
      # Los residuos se comportan como ruido blanco, que no puede ser modelado,
      # al menos linealmente.

#6 Modelar volatilidad==========================================================


#6.0.1 Exploración visual de los residuos al cuadrado---------------------------


tsdisplay((res_MA2)^2, main='Residuos al cuadrado')
plot((res_MA2)^2, main='Residuos al cuadrado')

    # se observan clusters de volatilidad en los residuos al cuadrado

# 6.0. 2 Test sobre homocedasticidad de los residuos al cuadrado.---------------
      #Ho: No existe autocorrelación de los residuos
      #Ha: Existe correlación de los residuosal cuadrado: son heterocedasticos

Box.test((res_MA2)^2, lag=10, fitdf=3, type='Ljung-Box')

      # se rechaza Ho y se puede utilizar ARCH

#6.1 GARCH(1,1)-----------------------------------------------------------------

garch11=garchFit(~1+garch(1,1), res_MA2, includ.mean=F,
                 cond.dist='norm', trace=F)
summary(garch11)

  #los p-value de los parametros son menos de 0.05. No son significativos.
  #los p-value del test Box Ljung son mayores a 0.05 tanto para residuos como
  #para residuos al cuadrado.







#6.2 Value at Risk -------------------------------------------------

#Riesgo del 5% en una distribución normal

quantile(retornos, 0.05)
qplot(retornos , geom = 'histogram', bins=100) + 
  geom_histogram(fill = 'lightblue' , bins = 100) +
  geom_histogram(aes(retornos[retornos < quantile(retornos , 0.05)], bins=100), 
                 fill = 'red', bins = 100) +
  labs(x = 'Retorno diario')


#¿La serie retornos sigue una distribución normal?
basicStats(retornos)    #fBasics
qplot(retornos , geom = 'histogram', bins=100)
jarque.bera.test(retornos) # ho: es una distribución normal

#volatilidad condicional
sigma = volatility(garch11, type = "sigma") # en rugarch model$fit$fit$sigma 

plot(sigma,  main='volatilidad condicional', type='l')


# La distribución no es normal

library(TSstudio)
xts_to_ts(retornos)

qplot(y=retornos, x= 1:1258, geom='line') + geom_line(color="gray")+
  geom_line(aes(y = sigma*qdist('sged',p=0.05) , x = 1:1258) , colour = 'red') +
  geom_hline(yintercept = sd(retornos)*qnorm(0.05) , colour = 'springgreen', size=1)  + 
  geom_hline(yintercept = sd(retornos)*qdist('sged',p=0.05) , colour = 'coral2', size=1) +
  labs(x = '' , y = 'Retorno diario' , title = 'Comparación Value-at-Risk')

  

  
  #Intervalos de confianza a partir de la votalidad
  qnorm(0.025)
  qdist('sged',p=0.025)
  
  #Serie de precios con transformación logaritmica
  fit012  =fitted.values(ARIMA012)  
  low=fit012[2:1259]-qdist('sged',p=0.25)*(sigma)
  high=fit012[2:1259]+qdist('sged',p=0.25)*(sigma)
  autoplot(fit012)
  
  autoplot(log.citygr[2:1259]) +
    labs(title="Log Citygroup, Low, High") +
    geom_line(y = low , color = 'red') +
    geom_line(y = high, color='steelblue')
  
  #Serie de precios:
  precios= exp(fit012)
  low = precios[2:1259] - exp(qdist('sged',p=0.25)*(sigma))
  high= precios[2:1259]+ exp(qdist('sged',p=0.25)*(sigma))


  autoplot(citygr[2:1259]) +
    labs(title="Citygroup, Low, High") +
    geom_line(y = low , color = 'red') +
    geom_line(y = high, color='steelblue')
  
  ## Predicción
  #predicción del modelo arima
  
  media_retorno = forecast(MA2,h=100, level=c(99.5))
  plot(media_retorno)

  
  media_precios = forecast(ARIMA012, h=100, level=c(99.5))
  plot(media_precios)
  
  
  #predicción usando la librería rugarch
  
  spec = ugarchspec(variance.model = list(model='sGARCH', garchOrder=c(1,1)),
                    mean.model = list(armaOrder = c(0,2)), 
                    distribution.model ="norm") 
  model.fit <- ugarchfit(spec, data = retornos, out.sample=10)

  prediccion = ugarchforecast(model.fit, n.ahed=10,  n.roll=10)
  
  plot(prediccion )  #seleccionar gráficas  1, 2, 3 o 4
  
  prediccion
  retornos[1249:1258]
  
  #comparación coeficientes
  model.fit@fit$matcoef
  coef(MA2)
  coef(garch11)
