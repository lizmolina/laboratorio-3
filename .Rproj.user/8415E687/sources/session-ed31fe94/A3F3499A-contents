# 1. Cargue el set de datos en su ambiente de trabajo.

# a. Exploración Inicial de los Datos:

# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(dplyr)
library(ggplot2)
library(visdat) 
library(naniar)
library(dplyr)
library(ggplot2)
library(stringr)
library(visdat) 


# Cargar datosfile 
datos <- read_excel("file/plan_de_compras_2022.xlsx")

# a. Exploración Inicial de los Datos:

# i. Información sobre las columnas y tipos de datos

# Ver las primeras filas de los datos
head(datos)
class(datos)
names(datos)
glimpse(datos)
# Estructura de los datos
str(datos)

# ii. Resumen estadístico de las variables numéricas
summary(datos)
View(datos)



# b. Limpieza y Tratamiento de Datos:

# i. Determine valores nulos :

# Contar valores nulos en cada columna
sapply(datos, function(x) sum(is.na(x))
sum(is.na(datos))

# Eliminar filas con valores nulos
datos_sin_na <- na.omit(datos)

# O eliminar columnas con demasiados valores nulos
datos_sin_na <- datos[, colSums(is.na(datos)) < umbral]

# Para variables numéricas, reemplazar NA con la media o mediana
datos$columna_numerica[is.na(datos$columna_numerica)] <- mean(datos$columna_numerica, na.rm = TRUE)
# o con la mediana
# datos$columna_numerica[is.na(datos$columna_numerica)] <- median(datos$columna_numerica, na.rm = TRUE)



# ii. Determine valores atípicos: 

# Instalar y cargar ggplot2 para visualizaciones
install.packages("ggplot2")
library(ggplot2)

# Suponiendo que queremos analizar la columna 'Monto'
boxplot(datos$Monto, main="Boxplot de Monto")



# Crear un boxplot para una columna específica
ggplot(datos, aes(x = "", y = Monto)) +
  geom_boxplot() +
  theme_minimal() +
  xlab("") +
  ylab("Nombre de la columna")


# Calculando el IQR
Q1 <- quantile(datos$columna_numerica, 0.25, na.rm = TRUE)
Q3 <- quantile(datos$columna_numerica, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1



# iii. Determine concentraciones y tendencia de datos:
# Se muestra un  histograma de densidad para esto
hist(datos$Monto, main="Histograma de Monto", xlab="Monto", breaks=50)

# Sumar montos por trimestre
datos_agrupados <- aggregate(Monto ~ TRIMESTRE, data = datos, sum)

# Gráfico de barras de montos por trimestre
ggplot(datos_agrupados, aes(x = TRIMESTRE, y = Monto)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  xlab("Trimestre") +
  ylab("Suma de Montos")

# Gráfico de barras de montos por programa
ggplot(datos, aes(x = factor(Programa), y = Monto)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  xlab("Programa") +
  ylab("Monto")

# Histograma de Montos
ggplot(datos, aes(x = Monto)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  xlab("Monto") +
  ylab("Frecuencia")


# iv. Determine acciones correctivas (imputación):

# Imputación con la media para la columna 'Monto'
datos$Monto[is.na(datos$Monto)] <- mean(datos$Monto, na.rm = TRUE)

# Imputación con la mediana
datos$Monto[is.na(datos$Monto)] <- median(datos$Monto, na.rm = TRUE)

# c. Visualización de datos:

# Cargar librerías
library(readxl)
library(ggplot2)

# i. Gráficas de datos y agrupaciones para variables categóricas

ggplot(datos, aes(x=Cuenta_presupuestaria)) + geom_bar()

# Gráfico de barras para la variable 'Programa'
ggplot(datos, aes(x = factor(Programa))) +
  geom_bar(fill = "blue") +
  theme_minimal() +
  xlab("Programa") +
  ylab("Frecuencia")

# Gráfico de pastel para 'TRIMESTRE'
ggplot(datos, aes(x = "", fill = factor(TRIMESTRE))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  theme_minimal() +
  xlab("") +
  ylab("Frecuencia")

# Diagrama de caja para 'Monto' en cada 'Programa'
ggplot(datos, aes(x = factor(Programa), y = Monto)) +
  geom_boxplot() +
  theme_minimal() +
  xlab("Programa") +
  ylab("Monto")

# Gráfico de densidad para 'Monto' en cada 'Actividad'
ggplot(datos, aes(x = Monto, fill = factor(Actividad))) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  xlab("Monto") +
  ylab("Densidad")


# d. Análisis de Relaciones de Datos:

#  i. Análisis Univariable y Bivariable:

# Análisis Univariable -->

# Análisis de la Variable Numérica Monto:
# Histograma para 'Monto'
ggplot(datos, aes(x = Monto)) +
  geom_histogram(bins = 30, fill = "blue") +
  theme_minimal() +
  xlab("Monto") +
  ylab("Frecuencia")

# Medidas estadísticas
summary(datos$Monto)

#Análisis de una Variable Categórica 

# Gráfico de barras para 'Programa'
ggplot(datos, aes(x = factor(Programa))) +
  geom_bar(fill = "blue") +
  theme_minimal() +
  xlab("Programa") +
  ylab("Frecuencia")

# Análisis Bivariable -->

# Relación entre Variable Numérica y Categórica (Monto y Programa):
# Diagrama de caja
ggplot(datos, aes(x = factor(Programa), y = Monto)) +
  geom_boxplot() +
  theme_minimal() +
  xlab("Programa") +
  ylab("Monto")

#Relación entre dos Variables Categóricas (Programa y Actividad):

# Tabla de contingencia
tabla_contingencia <- table(datos$Programa, datos$Actividad)

# Gráfico de mosaico
mosaicplot(tabla_contingencia, main = "Relación entre Programa y Actividad", xlab = "Programa", ylab = "Actividad")
