# Predicción del riesgo: un estudio comparado entre ARIMA y (ARIMA+ARCH)

El conjunto de datos fue obtenido de [S&P 500 stock data](https://www.kaggle.com/camnugent/sandp500) y consta de 619,040 observaciones diarias de precios durante cinco años (2013-2018) para 506 empresas.

El estudio se centra en el analisis de riesgo dada de los retornos de la empresa Citigroup Inc  utilizando los modelos ARIMA, GARCH y una combinación de ambos. Para ello se realiza una transformación logaritmica, para después estacionalizar la serie de tiempo a partir de las 1264 mediciones del cierre diario. 

Para la selección de los modelos se hace uso de la metodología Box-Jenkins, así como la comparación del Criterio de Información Akaike (AIC) y Log-Likelihood
Se utilizan métodos visuales y test estadísticos para probar la estacionalidad de la serie, la elección de los modelos y la distribución de los residuos, antes de la predición de riesgo.


