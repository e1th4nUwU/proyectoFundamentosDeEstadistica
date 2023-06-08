# Cargar bibliotecas necesarias
library(tidyverse)
library(conflicted)
library(ggplot2)
library(dplyr)
library(readr)

# Resolver conflictos para los errores entre las funciones filter y lag
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Cargar datos
equipoA <- read_csv("tabla1A.csv")
equipoB <- read_csv("tabla1B.csv")
participantesTotales <- rbind(equipoA, equipoB)

# Configurar numero de filas a mostrar
options(dplyr.print_max = nrow(equipoA)*2)

# Mostrar resumen equipo A
cat("Resumen equipo A:\n")
equipoA # Mostrar la tabla del equipo A
summary(equipoA$Edad)   # Resumen estadistico de la edad
desvStdA <- sd(equipoA$Edad)
cat("\nDesviación estándar de edad equipo A:\n")
print(desvStdA) # Print the variance
formationCountA <- table(equipoA$Formación) # Contar la cantidad de formaciones
cat("\nFormaciones A:\n")
print(formationCountA) # Mostrar formaciones

# Mostrar resumen equipo B
cat("\nResumen equipo B:\n")
equipoB # Mostrar la tabla del equipo B
summary(equipoB$Edad)   # Resumen estadistico de la edad
desvStdB <- sd(equipoB$Edad)
cat("\nDesviación estándar equipo B:\n")
print(desvStdB) # Print the standard deviation
formationCountB <- table(equipoB$Formación) # Contar la cantidad de formaciones
cat("\nFormaciones B:\n")
print(formationCountB) # Mostrar formaciones

# Mostrar resumen de todos los participantes
cat("\nResumen de todos los participantes:\n")
participantesTotales # Mostrar la tabla de todos los participantes
summary(participantesTotales$Edad)   # Resumen estadistico de la edad
devStd <- sd(participantesTotales$Edad)
cat("\nDesviación estándar de edad total:\n")
print(devStd) # Print the standard deviation
formationCount <- table(participantesTotales$Formación) # Contar la cantidad de formaciones
cat("\nFormaciones total:\n")
print(formationCount) # Mostrar formaciones

# Grafica edad equipo A
ggplot(equipoA, aes(x = Edad)) +
  geom_histogram(binwidth = 2, fill = "red", color = "black") +
  labs(x = "Edad", y = "Frecuencia", title = "Distribución de edad del equipo A")

# Grafica edad equipo B
ggplot(equipoB, aes(x = Edad)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(x = "Edad", y = "Frecuencia", title = "Distribución de edad del equipo B")

# Grafica edad total
ggplot(participantesTotales, aes(x = Edad)) +
  geom_histogram(binwidth = 2, fill = "green", color = "black") +
  labs(x = "Edad", y = "Frecuencia", title = "Distribución de edad de todos los participantes")

# Add a Team column to each data frame
equipoA$Team <- "A"
equipoB$Team <- "B"

# Combine the data frames
combined_data <- rbind(equipoA, equipoB)

# Create the boxplot
ggplot(combined_data, aes(x = Team, y = Edad)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Equipo", y = "Edad", title = "Diagrama de caja de edad por equipo")