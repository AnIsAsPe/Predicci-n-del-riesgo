
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

#obtener estadisticas básicas de la serie
basicStats(retornos)    #fBasics

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

res_MA2 =MA2$residuals

#5.1 Exploración visual --------------------------------------------------------

ggtsdisplay(res_MA2, 
            plot.type = 'partial') #se puede escoger histograma
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

#6.2 Predicciones con el modelo-------------------------------------------------

forecast_MA2 = forecast(MA2, 100, level=95)
plot(forecast_MA2)

#volatilidad condicional
vc.garch11=volatility(garch11)
plot(vc.garch11,main='volatilidad condicional',type='l' )

#Intervalos de confianza del Log Price

fit012=fitted.values(ARIMA012)
low=fit012[2:1259]-1.96*(vc.garch11)
high=fit012[2:1259]+1.96*(vc.garch11)

autoplot(log.citygr[2:1259]) +
  labs(title="Log Citygroup, Low, High") +
  geom_line(y = low , color = 'red') +
  geom_line(y = high, color='steelblue')


#Generar predicción
pred_MA2 = forecast(MA2, 100, level=95)
pre_garch11 <- predict(garch11, 100)
varianza_pred=c((pre_garch11$standardDeviation)^2)

low_pred = pred_MA2$mean - 1.96*(varianza_pred)
higth_pred = pred_MA2$mean + 1.96*(varianza_pred)

autoplot(pred_MA2) +
  labs(title="Retornog Citygroup, Low, High") +
  geom_line(y = low_pred , color = 'red') +
  geom_line(y = higth_pred, color='steelblue')
