library(ggplot2)
library(fBasics)
library(forecast)


#1. Manejo de Fechas en R ---------------------------------------------------

x = as.POSIXct("2020-11-08 13:05:33")  
y = as.POSIXlt("2020-11-08 13:05:33")  

x
y

unclass(x)                #POSIXct segundos desde 01/01/1970 00:00:00
unclass(y)                #POSIXlt podemos extraer componentes

y$min
y$zone

z= as.Date("2020-11-08") #días desde 01/01/1970 sin considerar que hay bisiestos
unclass(z)

t='21-09-2020'

z1 =as.Date(t, format="%d-%m-%a") 


z2 = strptime(t, format = "%d-%m-%Y")
z2

#2. Objeto serie de tiempo en R ------------------------------------------------

# Creamos unos datos
mydata = runif(n = 50, min = 10, max = 45)

# ts es la clase "time series"

mytimeseries = ts(data = mydata, 
                  start = 1956, frequency = 4)

plot(mytimeseries)

time(mytimeseries)

#Redefinir el inicio
mytimeseries = ts(data = mydata, 
                  start = c(1956,3), frequency = 4)

#Ejemplo

serie=JohnsonJohnson

plot(serie) 

class(serie)


#con ggplot

autoplot(AirPassengers) + ggtitle("Pasajeros mensuales de aerolínea")

# 3. Ruido Blanco ------------------------------------------------------

n = 100

ruido_blanco = ts(rnorm(n))


autoplot(ruido_blanco) + ggtitle("Ruido Blanco")

# Autocorrelación

autocorrelacion = acf(ruido_blanco, lag.max=50)

#ver valores
autocorrelacion  


# 4. Caminata Aleatoria

caminata_aleatoria = ts(cumsum(rnorm(n)))

autoplot(caminata_aleatoria) + ggtitle("Caminata Aleatoria")

acf(caminata_aleatoria, lag.max=50 )

#con forecast grafica de la serie de tiempo, ACF y distribución

ggtsdisplay(ruido_blanco, plot.type = 'histogram') 
ggtsdisplay(caminata_aleatoria, plot.type = 'histogram') 

autoplot(diff(caminata_aleatoria))

, 