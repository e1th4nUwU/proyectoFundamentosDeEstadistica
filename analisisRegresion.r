# Cargar bibliotecas necesarias
library(tidyverse)
library(broom)
library(conflicted)
library(lmtest)
library(pacman)

# Resolver conflictos para los errores entre las funciones filter y lag
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Cargar datos
data <- read_csv("tabla6.csv")

# Renombrar el nombre de las columnas
names(data) <- c("Integrantes", "Puntaje", "# de Agujeros")

# Realizar regresion lineal
model <- lm(`Puntaje` ~ `# de Agujeros`, data = data)

# Extraer resumen del modelo
summary <- summary(model)

# Extraer los coeficientes
coefficients <- tidy(model)

# Prueba de Breusch-Pagan para homoscedasticidad
homoscedasticity_test_bp <- bptest(model)

# Prueba de White para homoscedasticidad
homoscedasticity_test_w <- bptest(model, studentize = FALSE)

#

# Imprimir resultados
cat("Model Summary:\n")
print(summary)
cat("\nPrueba de Breusch-Pagan para homoscedasticidad:\n")
print(homoscedasticity_test_bp)
cat("\nPrueba de White para homoscedasticidad:\n")
print(homoscedasticity_test_w)

# Extraer la ordenada al origen y la pendiente
ordenadaAlOrigen <- coefficients$estimate[1]
pendiente <- coefficients$estimate[2]

# Create a scatter plot with fitted regression line
ggplot(data, aes(x = `# de Agujeros`, y = Puntaje)) +
  geom_point(color = "#4287f5", size = 3) +
  geom_abline(intercept = ordenadaAlOrigen, slope = pendiente, color = "#f54242", linewidth = 1.5) +
  labs(x = "# de Agujeros", y = "Puntuaje", title = "Gráfico de Dispersión con Línea de Regresión Lineal") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none")