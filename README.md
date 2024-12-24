# Documentación del Proyecto: Forecasting App

## Descripción General

La Forecasting App es una aplicación desarrollada para realizar análisis y predicciones de series de tiempo mediante diferentes modelos de pronóstico. La aplicación está diseñada para ser intuitiva y accesible tanto para usuarios con conocimientos técnicos como para aquellos con poca experiencia en análisis estadístico.

Características Principales

La aplicación permite seleccionar entre los siguientes métodos de pronóstico:


### Regresión

Modelo lineal para tendencias de largo plazo.

Ideal para datos con relaciones lineales claras entre variables independientes y dependientes.

### ARIMA (Autoregressive Integrated Moving Average)

Modelo para datos estacionarios o que pueden hacerse estacionarios mediante diferenciación.

Requiere parametrización de los términos AR, I y MA.

### Prophet

Diseñado por Meta (antes Facebook).

Excelente para series de tiempo con estacionalidades fuertes y eventos irregulares.

### Suavización Exponencial Simple

Modelo para series con patrones simples sin estacionalidad.

Pone mayor peso en datos recientes.

### Suavización Exponencial Doble

Adecuado para datos con tendencia pero sin estacionalidad.

### Holt-Winters

Permite modelar series de tiempo con tendencia y estacionalidad.

Disponible en sus variantes aditiva y multiplicativa.

### Promedio Móvil Simple

Cálculo de la media de un número fijo de observaciones más recientes.

### Promedio Móvil Ponderado

Similar al promedio móvil simple, pero asigna pesos mayores a los datos más recientes.

## Arquitectura del Proyecto

El proyecto fue desarrollado con las siguientes tecnologías y herramientas:


### Selección de Modelo:

El usuario selecciona un método de pronóstico desde la interfaz.

Cada botón representa un modelo específico.

### Carga de Datos:

Se permite cargar archivos Excel o CSV para realizar el análisis.

### Parámetros y Ejecución:

El usuario define parámetros relevantes según el modelo seleccionado.

La aplicación ejecuta el script correspondiente mediante un archivo .bat.

### Resultados:

Los resultados se presentan en gráficos y tablas, dependiendo del modelo.

Organización de Archivos


### Casos de Uso

Predicciones de demanda.

Estimaciones financieras.

Planificación de inventarios.

Análisis de tendencias en series de tiempo.

## Futuras Mejora

Incorporación de nuevos métodos de pronóstico, como redes neuronales.

Mejora en la visualización de resultados.

Implementación de funciones automáticas para sugerir el mejor modelo según los datos ingresados.

## Conclusión

La Forecasting App es una herramienta versátil y eficiente para realizar pronósticos utilizando diferentes métodos. Gracias a su diseño modular, puede expandirse fácilmente para incluir nuevas funcionalidades y adaptarse a diversas necesidades analíticas.

