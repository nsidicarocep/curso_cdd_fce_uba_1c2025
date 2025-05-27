# ================================================================================
# EJEMPLOS PRÁCTICOS DE GGPLOT2
# Datasets: mtcars y wooldridge
# ================================================================================

# Cargar librerías necesarias
library(tidyverse)
library(wooldridge)  # install.packages("wooldridge") si no lo tienes
library(scales)
library(viridis)
library(ggrepel)

# ================================================================================
# PARTE 1: EJEMPLOS BÁSICOS CON MTCARS
# ================================================================================

# Explorar el dataset
head(mtcars)
str(mtcars)

# Ejemplo 1.1: Scatter plot básico
# Relación entre eficiencia y potencia
ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Relación entre Eficiencia y Potencia",
    x = "Millas por Galón (MPG)",
    y = "Caballos de Fuerza (HP)"
  ) +
  theme_minimal()

# Ejemplo 1.2: Agregando color por variable categórica
ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl))) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_viridis_d(name = "Cilindros") +
  labs(
    title = "Eficiencia vs Potencia por Número de Cilindros",
    x = "Millas por Galón (MPG)",
    y = "Caballos de Fuerza (HP)"
  ) +
  theme_minimal()

# Ejemplo 1.3: Múltiples dimensiones con size y color
ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl), size = wt)) +
  geom_point(alpha = 0.7) +
  scale_color_brewer(type = "qual", palette = "Set1", name = "Cilindros") +
  scale_size_continuous(name = "Peso\n(1000 lbs)", range = c(2, 8)) +
  labs(
    title = "Análisis Multivariado de Automóviles",
    subtitle = "Eficiencia, Potencia, Cilindros y Peso",
    x = "Millas por Galón (MPG)",
    y = "Caballos de Fuerza (HP)"
  ) +
  theme_bw()

# Ejemplo 1.4: Histograma de distribución
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(bins = 10, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_vline(aes(xintercept = mean(mpg)), color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Distribución de Eficiencia de Combustible",
    x = "Millas por Galón (MPG)",
    y = "Frecuencia"
  ) +
  theme_classic()

# Ejemplo 1.5: Boxplot comparativo
ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis_d(name = "Cilindros") +
  labs(
    title = "Eficiencia por Número de Cilindros",
    x = "Número de Cilindros",
    y = "Millas por Galón (MPG)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Redundante con eje X

# ================================================================================
# PARTE 2: EJEMPLOS INTERMEDIOS CON MTCARS
# ================================================================================

# Ejemplo 2.1: Gráfico con línea de tendencia
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(aes(color = factor(cyl)), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("4" = "#1f77b4", "6" = "#ff7f0e", "8" = "#d62728"),
    name = "Cilindros"
  ) +
  labs(
    title = "Peso vs Eficiencia de Combustible",
    subtitle = "Con línea de tendencia linear",
    x = "Peso (1000 lbs)",
    y = "Millas por Galón (MPG)"
  ) +
  theme_bw()

# Ejemplo 2.2: Facetas por transmisión
mtcars_labeled <- mtcars %>%
  mutate(
    transmission = ifelse(am == 0, "Automática", "Manual"),
    engine_type = case_when(
      vs == 0 ~ "V-Engine",
      vs == 1 ~ "Straight Engine"
    )
  )

ggplot(mtcars_labeled, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ transmission, scales = "free") +
  scale_color_viridis_d(name = "Cilindros") +
  labs(
    title = "Eficiencia por Tipo de Transmisión",
    x = "Peso (1000 lbs)",
    y = "Millas por Galón (MPG)"
  ) +
  theme_minimal()

# Ejemplo 2.3: Gráfico de barras apiladas
mtcars_summary <- mtcars %>%
  count(cyl, gear) %>%
  mutate(cyl = factor(cyl), gear = factor(gear))

ggplot(mtcars_summary, aes(x = cyl, y = n, fill = gear)) +
  geom_col(position = "stack", alpha = 0.8) +
  scale_fill_brewer(type = "qual", palette = "Set2", name = "Velocidades") +
  labs(
    title = "Distribución de Velocidades por Cilindros",
    x = "Número de Cilindros",
    y = "Cantidad de Vehículos"
  ) +
  theme_minimal()

# Ejemplo 2.4: Heatmap de correlaciones
library(reshape2)
cor_matrix <- cor(mtcars[, c("mpg", "hp", "wt", "qsec", "disp")])
cor_melted <- melt(cor_matrix)

ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1),
    name = "Correlación"
  ) +
  labs(title = "Matriz de Correlaciones - Variables de mtcars") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank()
  )

# ================================================================================
# PARTE 3: EJEMPLOS CON DATOS DE WOOLDRIDGE
# ================================================================================

# Ejemplo 3.1: Datos de salarios (wage1)
data("wage1")
head(wage1)

# Relación educación-salario
ggplot(wage1, aes(x = educ, y = wage)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Relación entre Educación y Salarios",
    subtitle = "Datos de wage1 (Wooldridge)",
    x = "Años de Educación",
    y = "Salario por Hora (USD)"
  ) +
  theme_bw()

# Boxplot de salarios por género
ggplot(wage1, aes(x = factor(female), y = wage, fill = factor(female))) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  scale_x_discrete(labels = c("0" = "Hombres", "1" = "Mujeres")) +
  scale_fill_manual(
    values = c("0" = "#3498db", "1" = "#e74c3c"),
    guide = "none"
  ) +
  labs(
    title = "Distribución de Salarios por Género",
    x = "Género",
    y = "Salario por Hora (USD)"
  ) +
  theme_minimal()

# Ejemplo 3.2: Datos de precios de casas (hprice1)
data("hprice1")

# Relación precio-tamaño
ggplot(hprice1, aes(x = sqrft, y = price)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "loess", se = TRUE, color = "orange") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Precio de Casas vs Superficie",
    subtitle = "Datos de hprice1 (Wooldridge)",
    x = "Superficie (pies cuadrados)",
    y = "Precio (USD)"
  ) +
  theme_bw()

# ================================================================================
# PARTE 4: EJEMPLOS AVANZADOS
# ================================================================================

# Ejemplo 4.1: Gráfico complejo con múltiples capas (mtcars)
# Preparar datos
mtcars_enhanced <- mtcars %>%
  mutate(
    efficiency_category = case_when(
      mpg < 15 ~ "Baja",
      mpg >= 15 & mpg < 25 ~ "Media",
      mpg >= 25 ~ "Alta"
    ),
    efficiency_category = factor(efficiency_category, 
                                 levels = c("Baja", "Media", "Alta"))
  )

# Gráfico con anotaciones
p1 <- ggplot(mtcars_enhanced, aes(x = wt, y = hp)) +
  geom_point(aes(color = efficiency_category, size = mpg), alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(
    values = c("Baja" = "#d73027", "Media" = "#fee08b", "Alta" = "#1a9850"),
    name = "Eficiencia"
  ) +
  scale_size_continuous(name = "MPG", range = c(2, 8)) +
  annotate("text", x = 4.5, y = 300, 
           label = "Vehículos pesados\ny potentes", 
           hjust = 0.5, size = 3.5, color = "gray30") +
  annotate("segment", x = 4.2, y = 280, xend = 3.8, yend = 245,
           arrow = arrow(length = unit(0.3, "cm")), color = "gray50") +
  labs(
    title = "Análisis Integral de Vehículos",
    subtitle = "Peso vs Potencia, coloreado por eficiencia",
    x = "Peso (1000 lbs)",
    y = "Caballos de Fuerza (HP)",
    caption = "Datos: mtcars dataset"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

print(p1)

# Ejemplo 4.2: Serie temporal con wage2
data("wage2")

# Crear datos agregados por edad
wage_by_age <- wage2 %>%
  group_by(age) %>%
  summarise(
    avg_wage = mean(wage, na.rm = TRUE),
    median_wage = median(wage, na.rm = TRUE),
    count = n(),
    .groups = 'drop'
  )

ggplot(wage_by_age, aes(x = age)) +
  geom_line(aes(y = avg_wage, color = "Promedio"), size = 1.2) +
  geom_line(aes(y = median_wage, color = "Mediana"), size = 1.2) +
  geom_point(aes(y = avg_wage, size = count), alpha = 0.6, color = "#1f77b4") +
  scale_color_manual(
    values = c("Promedio" = "#1f77b4", "Mediana" = "#ff7f0e"),
    name = "Estadístico"
  ) +
  scale_size_continuous(name = "N° Observaciones", range = c(1, 6)) +
  labs(
    title = "Evolución de Salarios por Edad",
    subtitle = "Promedio y mediana con tamaño por cantidad de observaciones",
    x = "Edad (años)",
    y = "Salario Mensual (USD)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Ejemplo 4.3: Análisis bivariado complejo (hprice1)
# Crear categorías de precio
hprice1_cat <- hprice1 %>%
  mutate(
    price_category = case_when(
      price < quantile(price, 0.33) ~ "Económicas",
      price < quantile(price, 0.67) ~ "Medias", 
      TRUE ~ "Caras"
    ),
    price_category = factor(price_category, 
                            levels = c("Económicas", "Medias", "Caras"))
  )

ggplot(hprice1_cat, aes(x = bdrms, y = price)) +
  geom_jitter(aes(color = price_category), width = 0.2, alpha = 0.7, size = 2) +
  geom_boxplot(aes(group = bdrms), alpha = 0.3, outlier.shape = NA) +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_viridis_d(name = "Categoría\nde Precio") +
  facet_wrap(~ price_category, scales = "free_y") +
  labs(
    title = "Distribución de Precios por Número de Habitaciones",
    subtitle = "Separado por categoría de precio",
    x = "Número de Habitaciones",
    y = "Precio (USD)"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightblue", color = "white"),
    legend.position = "none"  # Redundante con facetas
  )

# ================================================================================
# PARTE 5: CASOS DE USO ESPECÍFICOS
# ================================================================================

# Ejemplo 5.1: Análisis de residuos (regresión wage1)
model <- lm(wage ~ educ + exper + I(exper^2), data = wage1)
wage1$residuals <- residuals(model)
wage1$fitted <- fitted(model)

# Gráfico de residuos
ggplot(wage1, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "orange") +
  labs(
    title = "Análisis de Residuos",
    subtitle = "Modelo: wage ~ educ + exper + exper²",
    x = "Valores Ajustados",
    y = "Residuos"
  ) +
  theme_minimal()

# Ejemplo 5.2: Comparación de distribuciones (kielmc)
data("kielmc")

# Densidades superpuestas
ggplot(kielmc, aes(x = price)) +
  geom_density(aes(fill = factor(nearinc)), alpha = 0.6) +
  scale_fill_manual(
    values = c("0" = "#3498db", "1" = "#e74c3c"),
    labels = c("0" = "Lejos del incinerador", "1" = "Cerca del incinerador"),
    name = "Ubicación"
  ) +
  scale_x_continuous(labels = scales::dollar) +
  labs(
    title = "Distribución de Precios de Casas",
    subtitle = "Según proximidad al incinerador",
    x = "Precio (USD)",
    y = "Densidad"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Ejemplo 5.3: Gráfico de efectos marginales
# Simulación de efectos marginales de educación
education_effects <- data.frame(
  education = 8:18,
  effect = sapply(8:18, function(x) {
    coef(model)["educ"] * x + mean(wage1$wage)
  })
)

ggplot(education_effects, aes(x = education, y = effect)) +
  geom_line(size = 1.5, color = "#2c3e50") +
  geom_point(size = 3, color = "#e74c3c") +
  geom_ribbon(aes(ymin = effect - 1, ymax = effect + 1), 
              alpha = 0.2, fill = "#3498db") +
  labs(
    title = "Efecto Marginal de la Educación en Salarios",
    subtitle = "Con banda de confianza aproximada",
    x = "Años de Educación",
    y = "Salario Predicho (USD)"
  ) +
  theme_bw()

# ================================================================================
# PARTE 6: GRÁFICOS PARA PRESENTACIONES
# ================================================================================

# Ejemplo 6.1: Gráfico limpio para presentación
presentation_plot <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_manual(
    values = c("4" = "#1f77b4", "6" = "#ff7f0e", "8" = "#d62728"),
    name = "Cilindros"
  ) +
  labs(
    title = "Eficiencia vs Peso del Vehículo",
    x = "Peso (1000 lbs)",
    y = "Millas por Galón"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

print(presentation_plot)

# Ejemplo 6.2: Guardar gráfico en alta resolución
ggsave("mtcars_efficiency_plot.png", 
       plot = presentation_plot,
       width = 10, height = 6, 
       dpi = 300, 
       bg = "white")
