# Predicción del riesgo: un estudio comparado entre ARIMA y (ARIMA+ARCH)

El presente repositorio se refiere a un curso para la predicción de riesgo, impartido en colaboración con el [Colegio de Matemáticas Bourbaki](https://www.colegio-bourbaki.com/) 

El conjunto de datos, es un histórico de 5 años de las acciones de Microsoft, obtenido del servicio [Yahoo Finance](https://finance.yahoo.com/) el 24/01/21 y consta de 1259 observaciones diarias

El estudio se centra en el análisis de riesgo de los retornos de Microsoft  utilizando los modelos ARIMA, GARCH y una combinación de ambos. Para ello se realiza la integración de la transformación logarítmica del precio de cierre diario.

Para la selección de los modelos se hace uso de la metodología Box-Jenkins, así como la comparación del Criterio de Información Akaike (AIC) y Log-Likelihood
Se utilizan métodos visuales y test estadísticos para probar la estacionalidad de la serie, la elección de los modelos y la distribución de los residuos, antes de la predicción de riesgo.
