# === INFORMACIÓN DEL ESTUDIANTE ===
# Nombre: Ejemplo
# Apellido: Solución
legajo <- 882280
# Email: ejemplo@universidad.edu

# === CONFIGURACIÓN INICIAL ===
# Establecer semilla para reproducibilidad

set.seed(legajo)

# Cargar librerías necesarias
library(tidyverse)
library(lubridate)  # Para manejo de fechas
library(scales)     # Para formateo de valores

# Crear directorio para resultados
if(!dir.exists("resultados")) {
  dir.create("resultados")
}

# En un examen real, los alumnos generarían sus datos aquí:
source("generar_datos_examen.R")
datos <- generar_datos_examen(legajo)
guardar_datos_examen(datos)

# Rutas archivos 
instub <- 'datos_examen'

# Cargar datos (asumimos que ya están generados)
transacciones <- read.csv(file.path(instub,"transacciones.csv"))
usuarios_alquilan <- read.csv(file.path(instub,"usuarios_alquilan.csv"))
usuarios_arrendatarios <- read.csv(file.path(instub,"usuarios_arrendatarios.csv"))
casas <- read.csv(file.path(instub,"casas.csv"))
detalles_transacciones <- read.csv(file.path(instub,"detalles_transacciones.csv"))

# Convertir columnas de fecha a tipo Date
transacciones$fecha_transaccion <- ymd(transacciones$fecha_transaccion)
usuarios_alquilan$fecha_registro <- ymd(usuarios_alquilan$fecha_registro)
usuarios_arrendatarios$fecha_registro <- ymd(usuarios_arrendatarios$fecha_registro)
detalles_transacciones$fecha_checkin <- ymd(detalles_transacciones$fecha_checkin)
detalles_transacciones$fecha_checkout <- ymd(detalles_transacciones$fecha_checkout)

# === EJERCICIO 1: ANÁLISIS TEMPORAL DE INGRESOS ===
# Crear análisis de ingresos mensuales
cat("Ejecutando Ejercicio 1: Análisis temporal de ingresos...\n")

ingresos_mensuales <- transacciones %>%
  # Extraer año y mes
  mutate(
    anio_mes = floor_date(fecha_transaccion, "month")
  ) %>%
  # Unir con detalles para obtener información de ingresos
  left_join(detalles_transacciones, by = "id_transaccion") %>%
  # Calcular días del mes (para tasa de ocupación)
  group_by(anio_mes) %>%
  mutate(
    dias_mes = days_in_month(anio_mes),
    ingreso_transaccion = precio_noche * cantidad_noches + costo_limpieza + costo_servicio
  ) %>%
  # Calcular métricas mensuales
  summarise(
    total_ingresos = sum(ingreso_transaccion, na.rm = TRUE),
    cantidad_transacciones = n(),
    ingreso_promedio = mean(ingreso_transaccion, na.rm = TRUE),
    noches_totales = sum(cantidad_noches, na.rm = TRUE),
    tasa_ocupacion = (noches_totales / (dias_mes[1] * n_distinct(casas$id_casa))) * 100,
    .groups = "drop"
  ) %>%
  # Ordenar cronológicamente
  arrange(anio_mes)

# Guardar resultado
write.csv(ingresos_mensuales, "resultados/ingresos_mensuales.csv", row.names = FALSE)

# === EJERCICIO 2: SEGMENTACIÓN DE PROPIEDADES POR RENTABILIDAD ===
cat("Ejecutando Ejercicio 2: Segmentación de propiedades por rentabilidad...\n")

propiedades_por_rentabilidad <- transacciones %>%
  left_join(detalles_transacciones, by = "id_transaccion") %>%
  # Agrupar por propiedad
  group_by(id_casa) %>%
  summarise(
    ingreso_total = sum(precio_noche * cantidad_noches + costo_limpieza + costo_servicio, na.rm = TRUE),
    dias_ocupacion = sum(cantidad_noches, na.rm = TRUE),
    ingreso_por_dia = ifelse(dias_ocupacion > 0, ingreso_total / dias_ocupacion, 0),
    .groups = "drop"
  ) %>%
  # Calcular cuartiles de rentabilidad
  mutate(
    cuartil_rentabilidad = paste0("Q", ntile(ingreso_por_dia, 4)),
    alta_rentabilidad = cuartil_rentabilidad == "Q4"
  ) %>%
  # Añadir información adicional de la propiedad
  left_join(casas %>% select(id_casa, tipo_vivienda, provincia_ubicacion, categoria), by = "id_casa") %>%
  # Ordenar por rentabilidad
  arrange(desc(ingreso_por_dia))

# Guardar resultado
write.csv(propiedades_por_rentabilidad, "resultados/propiedades_por_rentabilidad.csv", row.names = FALSE)

# === EJERCICIO 3: ANÁLISIS DE EFECTIVIDAD DE DESCUENTOS ===
cat("Ejecutando Ejercicio 3: Análisis de efectividad de descuentos...\n")

efectividad_descuentos <- detalles_transacciones %>%
  # Crear grupos de descuento
  mutate(
    grupo_descuento = case_when(
      descuento_aplicado == 0 ~ "0%",
      descuento_aplicado > 0 & descuento_aplicado <= 10 ~ "1-10%",
      descuento_aplicado > 10 & descuento_aplicado <= 20 ~ "11-20%",
      descuento_aplicado > 20 ~ ">20%"
    )
  ) %>%
  # Unir con transacciones para obtener fechas
  left_join(transacciones, by = "id_transaccion") %>%
  # Determinar temporada basada en el mes
  mutate(
    temporada = case_when(
      month(fecha_transaccion) %in% c(12, 1, 2) ~ "Verano",
      month(fecha_transaccion) %in% c(3, 4, 5) ~ "Otoño",
      month(fecha_transaccion) %in% c(6, 7, 8) ~ "Invierno",
      month(fecha_transaccion) %in% c(9, 10, 11) ~ "Primavera"
    )
  ) %>%
  # Calcular métricas por grupo de descuento y temporada
  group_by(grupo_descuento, temporada) %>%
  summarise(
    numero_transacciones = n(),
    duracion_promedio = mean(cantidad_noches, na.rm = TRUE),
    ingreso_promedio_dia = mean(precio_noche, na.rm = TRUE),
    tasa_cancelacion = mean(estado_transaccion == "Cancelada", na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  # Ordenar por grupo de descuento y temporada
  arrange(grupo_descuento, temporada)

# Guardar resultado
write.csv(efectividad_descuentos, "resultados/efectividad_descuentos.csv", row.names = FALSE)

# === EJERCICIO 4: PERFIL DE USUARIOS PREMIUM ===
cat("Ejecutando Ejercicio 4: Perfil de usuarios premium...\n")

# Calcular gasto total por usuario
gasto_por_usuario <- transacciones %>%
  left_join(detalles_transacciones, by = "id_transaccion") %>%
  group_by(id_alquila) %>%
  summarise(
    gasto_total = sum(precio_noche * cantidad_noches, na.rm = TRUE),
    num_transacciones = n(),
    .groups = "drop"
  ) %>%
  # Ordenar por gasto total descendente
  arrange(desc(gasto_total))

# Identificar el 10% de usuarios con mayor gasto (usuarios premium)
num_usuarios_premium <- ceiling(nrow(gasto_por_usuario) * 0.1)
usuarios_premium <- gasto_por_usuario %>%
  head(num_usuarios_premium) %>%
  mutate(es_premium = TRUE)

# Obtener características de los usuarios premium
perfil_usuarios_premium <- usuarios_premium %>%
  # Unir con datos demográficos
  left_join(usuarios_alquilan, by = "id_alquila") %>%
  # Seleccionar variables relevantes
  select(
    id_alquila, 
    gasto_total, 
    num_transacciones,
    edad, 
    sexo, 
    provincia_nacimiento, 
    cantidad_alquileres,
    calificacion_media
  )

# Añadir categoría de vivienda preferida y provincia más visitada
perfil_con_preferencias <- perfil_usuarios_premium %>%
  left_join(
    # Calcular preferencias para cada usuario premium
    transacciones %>%
      filter(id_alquila %in% usuarios_premium$id_alquila) %>%
      left_join(casas %>% select(id_casa, categoria, provincia_ubicacion), by = "id_casa") %>%
      group_by(id_alquila) %>%
      summarise(
        categoria_preferida = names(sort(table(categoria), decreasing = TRUE)[1]),
        provincia_preferida = names(sort(table(provincia_ubicacion), decreasing = TRUE)[1]),
        .groups = "drop"
      ),
    by = "id_alquila"
  )

# Guardar resultado
write.csv(perfil_usuarios_premium, "resultados/perfil_usuarios_premium.csv", row.names = FALSE)

# === EJERCICIO 5: ANÁLISIS GEOGRÁFICO DE RENTABILIDAD ===
cat("Ejecutando Ejercicio 5: Análisis geográfico de rentabilidad...\n")

rentabilidad_geografica <- casas %>%
  # Unir con transacciones y detalles para análisis de rentabilidad
  left_join(transacciones, by = "id_casa") %>%
  left_join(detalles_transacciones, by = "id_transaccion") %>%
  # Agrupar por provincia
  group_by(provincia_ubicacion) %>%
  summarise(
    num_propiedades = n_distinct(id_casa),
    precio_promedio = mean(precio_noche, na.rm = TRUE),
    tasa_ocupacion = sum(cantidad_noches, na.rm = TRUE) / 
      (365 * n_distinct(id_casa)) * 100,  # Aproximación simple
    ingreso_promedio = sum(precio_noche * cantidad_noches, na.rm = TRUE) / 
      n_distinct(id_casa),
    calificacion_promedio = mean(calificacion_general, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calcular ratio precio/calificación
  mutate(
    ratio_precio_calificacion = precio_promedio / calificacion_promedio,
    # Ranking de rentabilidad (basado en ingreso ajustado por calificación)
    ranking_rentabilidad = rank(-ingreso_promedio * calificacion_promedio/5)
  ) %>%
  # Ordenar por ranking
  arrange(ranking_rentabilidad)

# Guardar resultado
write.csv(rentabilidad_geografica, "resultados/rentabilidad_geografica.csv", row.names = FALSE)

# === EJERCICIO 6: ESTACIONALIDAD Y FACTOR TEMPORAL ===
cat("Ejecutando Ejercicio 6: Estacionalidad y factor temporal...\n")

estacionalidad_mercado <- transacciones %>%
  left_join(detalles_transacciones, by = "id_transaccion") %>%
  # Extraer mes
  mutate(
    mes = month(fecha_transaccion),
    nombre_mes = month(fecha_transaccion, label = TRUE, abbr = FALSE)
  ) %>%
  # Agrupar por mes
  group_by(mes, nombre_mes) %>%
  summarise(
    precio_promedio = mean(precio_noche, na.rm = TRUE),
    ocupacion_promedio = mean(cantidad_noches, na.rm = TRUE),
    duracion_promedio = mean(cantidad_noches, na.rm = TRUE),
    metodo_pago_frecuente = names(which.max(table(metodo_pago))),
    .groups = "drop"
  ) %>%
  # Calcular promedios anuales para índices
  mutate(
    precio_promedio_anual = mean(precio_promedio),
    ocupacion_promedio_anual = mean(ocupacion_promedio),
    # Calcular índices (100 = promedio anual)
    indice_precio = precio_promedio / precio_promedio_anual * 100,
    indice_ocupacion = ocupacion_promedio / ocupacion_promedio_anual * 100
  ) %>%
  # Seleccionar y ordenar columnas
  select(
    mes, nombre_mes, indice_precio, indice_ocupacion, duracion_promedio, 
    metodo_pago_frecuente
  ) %>%
  # Ordenar por mes
  arrange(mes)

# Guardar resultado
write.csv(estacionalidad_mercado, "resultados/estacionalidad_mercado.csv", row.names = FALSE)

# === EJERCICIO 7: ANÁLISIS DE RELACIÓN PRECIO-CALIFICACIÓN ===
cat("Ejecutando Ejercicio 7: Análisis de relación precio-calificación...\n")

# Crear categorías de precio
casas_con_categorias_precio <- casas %>%
  mutate(
    categoria_precio = case_when(
      precio_base_noche < 10000 ~ "Económico",
      precio_base_noche < 20000 ~ "Moderado",
      precio_base_noche < 35000 ~ "Alto",
      TRUE ~ "Premium"
    ),
    # Convertir a factor ordenado para mantener el orden lógico
    categoria_precio = factor(
      categoria_precio,
      levels = c("Económico", "Moderado", "Alto", "Premium")
    )
  )

# Análisis de calificaciones por categoría de precio
relacion_precio_calificacion <- casas_con_categorias_precio %>%
  group_by(categoria_precio) %>%
  summarise(
    num_propiedades = n(),
    calificacion_promedio = mean(calificacion_general, na.rm = TRUE),
    calificacion_limpieza_promedio = mean(calificacion_limpieza, na.rm = TRUE),
    calificacion_ubicacion_promedio = mean(calificacion_ubicacion, na.rm = TRUE),
    precio_promedio = mean(precio_base_noche, na.rm = TRUE),
    relacion_calidad_precio = calificacion_general / (precio_base_noche/10000),
    .groups = "drop"
  ) %>%
  arrange(categoria_precio)

# Análisis por tipo de vivienda y categoría de precio
relacion_por_tipo <- casas_con_categorias_precio %>%
  group_by(tipo_vivienda, categoria_precio) %>%
  summarise(
    num_propiedades = n(),
    calificacion_promedio = mean(calificacion_general, na.rm = TRUE),
    precio_promedio = mean(precio_base_noche, na.rm = TRUE),
    .groups = "drop"
  )

# Pivotear para formato más legible
relacion_pivoteada <- relacion_por_tipo %>%
  pivot_wider(
    id_cols = tipo_vivienda,
    names_from = categoria_precio,
    values_from = c(num_propiedades, calificacion_promedio, precio_promedio),
    values_fill = list(num_propiedades = 0, calificacion_promedio = NA, precio_promedio = NA)
  )

# Calcular "buenas inversiones" (alta calificación con precio económico/moderado)
buenas_inversiones <- casas_con_categorias_precio %>%
  filter(
    (categoria_precio %in% c("Económico", "Moderado")) & 
      calificacion_general >= 4.5
  ) %>%
  arrange(desc(calificacion_general)) %>%
  select(
    id_casa, 
    tipo_vivienda, 
    provincia_ubicacion, 
    precio_base_noche, 
    categoria_precio, 
    calificacion_general
  )

# Calcular "malas inversiones" (baja calificación con precio alto/premium)
malas_inversiones <- casas_con_categorias_precio %>%
  filter(
    (categoria_precio %in% c("Alto", "Premium")) & 
      calificacion_general <= 3.5
  ) %>%
  arrange(calificacion_general) %>%
  select(
    id_casa, 
    tipo_vivienda, 
    provincia_ubicacion, 
    precio_base_noche, 
    categoria_precio, 
    calificacion_general
  )

# Guardar resultados
write.csv(relacion_precio_calificacion, "resultados/relacion_precio_calificacion.csv", row.names = FALSE)
write.csv(relacion_pivoteada, "resultados/relacion_precio_tipo_pivoteada.csv", row.names = FALSE)
write.csv(buenas_inversiones, "resultados/buenas_inversiones.csv", row.names = FALSE)
write.csv(malas_inversiones, "resultados/malas_inversiones.csv", row.names = FALSE)

# === EJERCICIO 8: SEGMENTACIÓN DE ARRENDATARIOS ===
cat("Ejecutando Ejercicio 8: Segmentación de arrendatarios...\n")

segmentos_arrendatarios <- usuarios_arrendatarios %>%
  # Unir con casas para obtener número de propiedades
  left_join(
    casas %>%
      group_by(id_arrendatario) %>%
      summarise(
        num_propiedades = n(),
        precio_promedio = mean(precio_base_noche),
        provincias_distintas = n_distinct(provincia_ubicacion),
        .groups = "drop"
      ),
    by = "id_arrendatario"
  ) %>%
  # Calcular antigüedad
  mutate(
    antiguedad_dias = as.numeric(Sys.Date() - fecha_registro),
    antiguedad_anos = antiguedad_dias / 365.25,
    # Crear segmentos según criterios
    segmento_volumen = case_when(
      num_propiedades <= 1 ~ "Pequeño",
      num_propiedades <= 3 ~ "Mediano",
      num_propiedades <= 5 ~ "Grande",
      TRUE ~ "Muy grande"
    ),
    segmento_precio = case_when(
      precio_promedio <= 10000 ~ "Económico",
      precio_promedio <= 20000 ~ "Estándar",
      precio_promedio <= 35000 ~ "Premium",
      TRUE ~ "Lujo"
    ),
    segmento_experiencia = case_when(
      antiguedad_anos <= 1 ~ "Novato",
      antiguedad_anos <= 3 ~ "Experimentado",
      TRUE ~ "Veterano"
    ),
    segmento_calidad = case_when(
      calificacion_media <= 3 ~ "Básico",
      calificacion_media <= 4 ~ "Bueno",
      calificacion_media <= 4.5 ~ "Muy bueno",
      TRUE ~ "Excelente"
    )
  ) %>%
  # Crear una etiqueta compuesta para el segmento general
  mutate(
    segmento_compuesto = paste(segmento_volumen, segmento_precio, segmento_calidad),
    # Determinar si es superanfitrión (para análisis)
    es_super = es_superanfitrion
  )

# Análisis por tipo de segmento
analisis_por_segmento_volumen <- segmentos_arrendatarios %>%
  group_by(segmento_volumen) %>%
  summarise(
    num_arrendatarios = n(),
    precio_promedio = mean(precio_promedio, na.rm = TRUE),
    calificacion_promedio = mean(calificacion_media, na.rm = TRUE),
    porcentaje_superanfitrion = mean(es_super, na.rm = TRUE) * 100,
    .groups = "drop"
  )

analisis_por_segmento_precio <- segmentos_arrendatarios %>%
  group_by(segmento_precio) %>%
  summarise(
    num_arrendatarios = n(),
    propiedades_promedio = mean(num_propiedades, na.rm = TRUE),
    calificacion_promedio = mean(calificacion_media, na.rm = TRUE),
    porcentaje_superanfitrion = mean(es_super, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Agrupar los resultados
segmentos_arrendatarios_final <- segmentos_arrendatarios %>%
  select(
    id_arrendatario, segmento_volumen, segmento_precio, segmento_experiencia, 
    segmento_calidad, segmento_compuesto, num_propiedades, precio_promedio,
    calificacion_media, es_superanfitrion, antiguedad_anos, tiempo_respuesta_hrs
  )

# Guardar resultado
write.csv(segmentos_arrendatarios_final, "resultados/segmentos_arrendatarios.csv", row.names = FALSE)
write.csv(analisis_por_segmento_volumen, "resultados/analisis_segmento_volumen.csv", row.names = FALSE)
write.csv(analisis_por_segmento_precio, "resultados/analisis_segmento_precio.csv", row.names = FALSE)

# === EJERCICIO 9: ANÁLISIS DE RATIO PRECIO/CAPACIDAD ===
cat("Ejecutando Ejercicio 9: Análisis de ratio precio/capacidad...\n")

# Calcular ratio precio por persona y añadir variables de clasificación
casas_con_ratio <- casas %>%
  mutate(
    # Calcular precio por persona
    precio_por_persona = precio_base_noche / capacidad_maxima,
    # Clasificar por tipo de ratio
    ratio_categoria = case_when(
      precio_por_persona < 3000 ~ "Bajo",
      precio_por_persona < 6000 ~ "Medio",
      precio_por_persona < 10000 ~ "Alto",
      TRUE ~ "Premium"
    ),
    # Convertir a factor ordenado
    ratio_categoria = factor(ratio_categoria, levels = c("Bajo", "Medio", "Alto", "Premium"))
  )

# Análisis de ratio por tipo de vivienda y provincia
ratio_por_tipo_provincia <- casas_con_ratio %>%
  group_by(tipo_vivienda, provincia_ubicacion) %>%
  summarise(
    num_propiedades = n(),
    precio_promedio = mean(precio_base_noche, na.rm = TRUE),
    capacidad_promedio = mean(capacidad_maxima, na.rm = TRUE),
    ratio_promedio = mean(precio_por_persona, na.rm = TRUE),
    calificacion_promedio = mean(calificacion_general, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(tipo_vivienda, desc(ratio_promedio))

# Crear tabla pivoteada por tipo de vivienda y categoría de ratio
distribucion_ratio <- casas_con_ratio %>%
  count(tipo_vivienda, ratio_categoria) %>%
  group_by(tipo_vivienda) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  ungroup() %>%
  select(tipo_vivienda, ratio_categoria, porcentaje) %>%
  pivot_wider(
    names_from = ratio_categoria,
    values_from = porcentaje,
    values_fill = 0
  )

# Top 10 mejores oportunidades (mejor relación calidad-precio)
mejores_oportunidades <- casas_con_ratio %>%
  # Crear un índice de relación calidad-precio
  mutate(
    indice_calidad_precio = calificacion_general / precio_por_persona * 1000
  ) %>%
  # Ordenar por el índice (mayor es mejor)
  arrange(desc(indice_calidad_precio)) %>%
  # Seleccionar top 10
  head(10) %>%
  select(
    id_casa,
    tipo_vivienda,
    provincia_ubicacion,
    precio_base_noche,
    capacidad_maxima,
    precio_por_persona,
    calificacion_general,
    indice_calidad_precio
  )

# Guardar resultados
write.csv(ratio_por_tipo_provincia, "resultados/ratio_precio_capacidad.csv", row.names = FALSE)
write.csv(distribucion_ratio, "resultados/distribucion_ratio_por_tipo.csv", row.names = FALSE)
write.csv(mejores_oportunidades, "resultados/mejores_oportunidades_precio_capacidad.csv", row.names = FALSE)

# === EJERCICIO 10: RED DE TRANSACCIONES ENTRE PROVINCIAS ===
cat("Ejecutando Ejercicio 10: Red de transacciones entre provincias...\n")

# Crear matriz de origen-destino
red_transacciones_provincias <- transacciones %>%
  # Unir con usuarios para obtener provincia de origen
  left_join(usuarios_alquilan %>% 
              select(id_alquila, provincia_origen = provincia_nacimiento),
            by = "id_alquila") %>%
  # Unir con casas para obtener provincia de destino
  left_join(casas %>% 
              select(id_casa, provincia_destino = provincia_ubicacion),
            by = "id_casa") %>%
  # Contar transacciones por par origen-destino
  group_by(provincia_origen, provincia_destino) %>%
  summarise(
    num_transacciones = n(),
    .groups = "drop"
  ) %>%
  # Ordenar por volumen
  arrange(desc(num_transacciones))

# Calcular flujo neto por provincia
flujo_neto <- red_transacciones_provincias %>%
  # Agrupar por provincia de origen (salidas)
  group_by(provincia_origen) %>%
  summarise(salidas = sum(num_transacciones), .groups = "drop") %>%
  rename(provincia = provincia_origen) %>%
  # Unir con entradas
  full_join(
    red_transacciones_provincias %>%
      group_by(provincia_destino) %>%
      summarise(entradas = sum(num_transacciones), .groups = "drop") %>%
      rename(provincia = provincia_destino),
    by = "provincia"
  ) %>%
  # Calcular flujo neto
  mutate(
    salidas = ifelse(is.na(salidas), 0, salidas),
    entradas = ifelse(is.na(entradas), 0, entradas),
    flujo_neto = entradas - salidas,
    tipo_flujo = ifelse(flujo_neto > 0, "Receptor", "Emisor")
  ) %>%
  arrange(desc(flujo_neto))

# Identificar top 5 rutas más comunes
top_rutas <- red_transacciones_provincias %>%
  arrange(desc(num_transacciones)) %>%
  head(5) %>%
  mutate(
    ruta = paste(provincia_origen, "→", provincia_destino)
  )

# Guardar resultados
write.csv(top_rutas, "resultados/top_rutas.csv", row.names = FALSE)

# === EJERCICIO 11: PIVOTEO DE AMENITIES POR TIPO DE VIVIENDA ===
cat("Ejecutando Ejercicio 11: Pivoteo de amenities por tipo de vivienda...\n")

# Separar amenities a formato largo
amenities_largo <- casas %>%
  select(id_casa, tipo_vivienda, amenities) %>%
  # Separar los amenities en filas individuales
  separate_rows(amenities, sep = ",") %>%
  # Eliminar espacios en blanco y filas vacías
  mutate(amenities = trimws(amenities)) %>%
  filter(amenities != "")

# Calcular porcentaje de cada amenity por tipo de vivienda
amenities_porcentaje <- amenities_largo %>%
  # Contar viviendas por tipo
  left_join(
    casas %>%
      group_by(tipo_vivienda) %>%
      summarise(total_viviendas = n(), .groups = "drop"),
    by = "tipo_vivienda"
  ) %>%
  # Contar presencia de cada amenity por tipo
  group_by(tipo_vivienda, amenities) %>%
  summarise(
    cantidad = n(),
    porcentaje = cantidad / first(total_viviendas) * 100,
    .groups = "drop"
  )

# Pivotear a formato ancho
amenities_por_tipo <- amenities_porcentaje %>%
  select(tipo_vivienda, amenities, porcentaje) %>%
  pivot_wider(
    names_from = amenities,
    values_from = porcentaje,
    values_fill = 0
  ) %>%
  # Ordenar por tipo de vivienda
  arrange(tipo_vivienda)

# Guardar resultado
write.csv(amenities_por_tipo, "resultados/amenities_por_tipo.csv", row.names = FALSE)

# === EJERCICIO 12: ANÁLISIS COMPARATIVO DE MÉTODOS DE PAGO ===
cat("Ejecutando Ejercicio 12: Análisis comparativo de métodos de pago...\n")

# Análisis base por método de pago
analisis_metodos_pago <- detalles_transacciones %>%
  # Unir con transacciones y casas
  left_join(transacciones, by = "id_transaccion") %>%
  left_join(casas %>% select(id_casa, categoria), by = "id_casa") %>%
  # Calcular anticipación de reserva
  mutate(
    anticipacion_dias = as.numeric(fecha_checkin - fecha_transaccion)
  ) %>%
  # Análisis por método de pago
  group_by(metodo_pago) %>%
  summarise(
    cantidad_transacciones = n(),
    monto_promedio = mean(precio_noche * cantidad_noches, na.rm = TRUE),
    duracion_promedio = mean(cantidad_noches, na.rm = TRUE),
    anticipacion_promedio = mean(anticipacion_dias, na.rm = TRUE),
    tasa_cancelacion = mean(estado_transaccion == "Cancelada", na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Análisis cruzado por método de pago y categoría
cruce_metodo_categoria <- detalles_transacciones %>%
  # Unir con transacciones y casas
  left_join(transacciones, by = "id_transaccion") %>%
  left_join(casas %>% select(id_casa, categoria), by = "id_casa") %>%
  # Calcular monto total
  mutate(
    monto_total = precio_noche * cantidad_noches
  ) %>%
  # Análisis por método y categoría
  group_by(metodo_pago, categoria) %>%
  summarise(
    cantidad = n(),
    monto_promedio = mean(monto_total, na.rm = TRUE),
    .groups = "drop"
  )

# Pivotear para crear tabla comparativa
tabla_comparativa <- cruce_metodo_categoria %>%
  pivot_wider(
    id_cols = metodo_pago,
    names_from = categoria,
    values_from = c(cantidad, monto_promedio),
    names_glue = "{categoria}_{.value}"
  )

# Unir todos los análisis
analisis_metodos_pago_final <- analisis_metodos_pago %>%
  left_join(tabla_comparativa, by = "metodo_pago")

# Guardar resultado
write.csv(analisis_metodos_pago_final, "resultados/analisis_metodos_pago.csv", row.names = FALSE)

# === EJERCICIO 13: COMPARATIVA DE CALIFICACIONES ===
cat("Ejecutando Ejercicio 13: Comparativa de calificaciones...\n")

# Preparar datos de calificaciones
calificaciones_comparativa <- detalles_transacciones %>%
  # Filtrar solo transacciones con ambas calificaciones
  filter(!is.na(calificacion_huesped) & !is.na(calificacion_anfitrion)) %>%
  # Unir con datos de usuarios y arrendatarios
  left_join(transacciones, by = "id_transaccion") %>%
  left_join(usuarios_alquilan %>% 
              select(id_alquila, sexo_huesped = sexo, edad_huesped = edad),
            by = "id_alquila") %>%
  left_join(usuarios_arrendatarios %>% 
              select(id_arrendatario, sexo_anfitrion = sexo, edad_anfitrion = edad),
            by = "id_arrendatario") %>%
  # Calcular diferencia entre calificaciones
  mutate(
    diferencia_calificacion = calificacion_anfitrion - calificacion_huesped,
    diferencia_abs = abs(diferencia_calificacion),
    direccion_diferencia = case_when(
      diferencia_calificacion > 0 ~ "Anfitrión mejor calificado",
      diferencia_calificacion < 0 ~ "Huésped mejor calificado",
      TRUE ~ "Igual calificación"
    ),
    # Crear rangos de edad
    rango_edad_huesped = case_when(
      edad_huesped <= 25 ~ "18-25",
      edad_huesped <= 35 ~ "26-35",
      edad_huesped <= 50 ~ "36-50",
      TRUE ~ "51+"
    ),
    rango_edad_anfitrion = case_when(
      edad_anfitrion <= 25 ~ "18-25",
      edad_anfitrion <= 35 ~ "26-35",
      edad_anfitrion <= 50 ~ "36-50",
      TRUE ~ "51+"
    )
  )

# Análisis demográfico de calificaciones
analisis_demografico <- calificaciones_comparativa %>%
  group_by(sexo_huesped, rango_edad_huesped) %>%
  summarise(
    num_transacciones = n(),
    calificacion_media_recibida = mean(calificacion_huesped, na.rm = TRUE),
    calificacion_media_otorgada = mean(calificacion_anfitrion, na.rm = TRUE),
    diferencia_media = mean(diferencia_calificacion, na.rm = TRUE),
    .groups = "drop"
  )

# Guardar resultados
write.csv(analisis_demografico, "resultados/analisis_demografico_calificaciones.csv", row.names = FALSE)

# === EJERCICIO 14: ANÁLISIS DE ANTIGÜEDAD DE USUARIOS Y COMPORTAMIENTO ===
cat("Ejecutando Ejercicio 14: Análisis de antigüedad de usuarios y comportamiento...\n")

# Preparar datos de antigüedad
usuarios_antigüedad <- usuarios_alquilan %>%
  mutate(
    dias_antiguedad = as.numeric(Sys.Date() - fecha_registro),
    categoria_antiguedad = case_when(
      dias_antiguedad <= 180 ~ "0-6 meses",
      dias_antiguedad <= 365 ~ "7-12 meses",
      dias_antiguedad <= 730 ~ "1-2 años",
      TRUE ~ "Más de 2 años"
    ),
    # Factor ordenado para gráficos
    categoria_antiguedad = factor(
      categoria_antiguedad,
      levels = c("0-6 meses", "7-12 meses", "1-2 años", "Más de 2 años")
    )
  )


# Analizar comportamiento por antigüedad
comportamiento_por_antiguedad <- usuarios_antigüedad %>%
  # Unir con transacciones
  left_join(
    transacciones %>%
      group_by(id_alquila) %>%
      summarise(
        num_transacciones = n(),
        .groups = "drop"
      ),
    by = "id_alquila"
  ) %>%
  # Unir con detalles y casas para análisis de gasto y preferencias
  left_join(
    transacciones %>%
      left_join(detalles_transacciones, by = "id_transaccion") %>%
      left_join(casas %>% select(id_casa, categoria, tipo_vivienda, provincia_ubicacion),
                by = "id_casa") %>%
      group_by(id_alquila) %>%
      summarise(
        gasto_total = sum(precio_noche * cantidad_noches, na.rm = TRUE),
        noches_totales = sum(cantidad_noches, na.rm = TRUE),
        gasto_por_noche = gasto_total / noches_totales,
        categoria_frecuente = names(which.max(table(categoria))),
        provincia_frecuente = names(which.max(table(provincia_ubicacion))),
        .groups = "drop"
      ),
    by = "id_alquila"
  ) %>%
  # Reemplazar NA con 0 para usuarios sin transacciones
  mutate(
    num_transacciones = ifelse(is.na(num_transacciones), 0, num_transacciones),
    gasto_total = ifelse(is.na(gasto_total), 0, gasto_total),
    # Calcular actividad del usuario
    nivel_actividad = case_when(
      num_transacciones == 0 ~ "Inactivo",
      num_transacciones <= 2 ~ "Baja",
      num_transacciones <= 5 ~ "Media",
      TRUE ~ "Alta"
    )
  )

# Análisis de preferencias por antigüedad
preferencias_por_antiguedad <- comportamiento_por_antiguedad %>%
  filter(!is.na(categoria_frecuente)) %>%  # Solo usuarios con transacciones
  group_by(categoria_antiguedad, categoria_frecuente) %>%
  summarise(
    cantidad = n(),
    .groups = "drop"
  ) %>%
  group_by(categoria_antiguedad) %>%
  mutate(
    porcentaje = cantidad / sum(cantidad) * 100
  ) %>%
  ungroup() %>%
  # Pivotear para comparación
  select(categoria_antiguedad, categoria_frecuente, porcentaje) %>%
  pivot_wider(
    names_from = categoria_frecuente,
    values_from = porcentaje
  )

# Guardar resultados
write.csv(preferencias_por_antiguedad, "resultados/preferencias_por_antiguedad.csv", row.names = FALSE)

# === EJERCICIO 15: MATRIZ DE TRANSICIÓN ENTRE CATEGORÍAS DE VIVIENDA ===
cat("Ejecutando Ejercicio 15: Matriz de transición entre categorías de vivienda...\n")

# Identificar usuarios con múltiples reservas
usuarios_multiples_reservas <- transacciones %>%
  group_by(id_alquila) %>%
  summarise(
    num_reservas = n(),
    .groups = "drop"
  ) %>%
  filter(num_reservas > 1)

# Preparar secuencias de reservas para cada usuario
secuencias_reservas <- transacciones %>%
  filter(id_alquila %in% usuarios_multiples_reservas$id_alquila) %>%
  # Unir con casas para obtener categoría
  left_join(casas %>% select(id_casa, categoria), by = "id_casa") %>%
  # Ordenar por usuario y fecha
  arrange(id_alquila, fecha_transaccion) %>%
  # Enumerar reservas por usuario
  group_by(id_alquila) %>%
  mutate(
    num_reserva = row_number()
  ) %>%
  ungroup()

# Crear pares de reservas consecutivas
pares_consecutivos <- secuencias_reservas %>%
  # Seleccionar reserva actual
  select(id_alquila, num_reserva, categoria) %>%
  # Unir con siguiente reserva
  inner_join(
    secuencias_reservas %>%
      select(
        id_alquila, 
        num_reserva_siguiente = num_reserva, 
        categoria_siguiente = categoria
      ),
    by = "id_alquila"#,
    #relationship = "many-to-many"
  ) %>%
  # Filtrar solo pares consecutivos
  filter(num_reserva_siguiente == num_reserva + 1) %>%
  select(id_alquila, categoria, categoria_siguiente)

# Calcular matriz de transición
matriz_transicion <- pares_consecutivos %>%
  # Contar transiciones
  group_by(categoria, categoria_siguiente) %>%
  summarise(
    cantidad = n(),
    .groups = "drop"
  ) %>%
  # Calcular totales por categoría de origen
  group_by(categoria) %>%
  mutate(
    total_categoria = sum(cantidad),
    porcentaje = cantidad / total_categoria * 100
  ) %>%
  ungroup()

# Crear matriz pivoteada
matriz_transicion_pivoteada <- matriz_transicion %>%
  select(categoria, categoria_siguiente, porcentaje) %>%
  pivot_wider(
    names_from = categoria_siguiente,
    values_from = porcentaje,
    values_fill = 0
  )

# Guardar resultados
write.csv(matriz_transicion_pivoteada, "resultados/matriz_transicion_pivoteada.csv", row.names = FALSE)

# === EJERCICIO 16: ANÁLISIS DE PRECIOS POR METRO CUADRADO IMPLÍCITO ===
cat("Ejecutando Ejercicio 16: Análisis de precios por metro cuadrado implícito...\n")

# Estimar tamaño aproximado en base a habitaciones y baños
casas_con_tamaño <- casas %>%
  mutate(
    # Fórmula simple para estimar tamaño: 15m² por habitación + 5m² por baño + 20m² base
    tamaño_estimado_m2 = 20 + (numero_habitaciones * 15) + (numero_banos * 5),
    # Calcular precio por metro cuadrado
    precio_m2 = precio_base_noche / tamaño_estimado_m2
  )

# Análisis por provincia y tipo
precio_metro_cuadrado <- casas_con_tamaño %>%
  group_by(provincia_ubicacion, tipo_vivienda) %>%
  summarise(
    num_propiedades = n(),
    tamaño_promedio = mean(tamaño_estimado_m2, na.rm = TRUE),
    precio_promedio = mean(precio_base_noche, na.rm = TRUE),
    precio_m2_promedio = mean(precio_m2, na.rm = TRUE),
    .groups = "drop"
  )

# Crear tabla pivoteada por provincia y tipo
precio_m2_pivoteado <- precio_metro_cuadrado %>%
  select(provincia_ubicacion, tipo_vivienda, precio_m2_promedio) %>%
  pivot_wider(
    names_from = tipo_vivienda,
    values_from = precio_m2_promedio,
    values_fill = NA
  )


# Guardar resultados
write.csv(precio_m2_pivoteado, "resultados/precio_m2_pivoteado.csv", row.names = FALSE)

# === EJERCICIO 17: CALENDARIO DE OCUPACIÓN MENSUAL ===
cat("Ejecutando Ejercicio 17: Calendario de ocupación mensual...\n")

# Preparar datos de ocupación
# Primero, expandir cada transacción a un rango de fechas
ocupacion_diaria <- detalles_transacciones %>%
  # Seleccionar transacciones relevantes
  filter(!is.na(fecha_checkin) & !is.na(fecha_checkout)) %>%
  # Unir con transacciones para obtener id_casa
  left_join(transacciones %>% select(id_transaccion, id_casa), 
            by = "id_transaccion") %>%
  # Expandir cada estancia a días individuales
  rowwise() %>%
  mutate(
    fechas_estancia = list(seq(fecha_checkin, fecha_checkout - 1, by = "day"))
  ) %>%
  unnest(fechas_estancia) %>%
  # Extraer año y mes
  mutate(
    año = year(fechas_estancia),
    mes = month(fechas_estancia),
    año_mes = paste(año, sprintf("%02d", mes), sep = "-")
  )

# Calcular ocupación mensual por propiedad
ocupacion_mensual_propiedad <- ocupacion_diaria %>%
  # Contar días ocupados por mes y propiedad
  group_by(id_casa, año, mes, año_mes) %>%
  summarise(
    dias_ocupados = n(),
    .groups = "drop"
  ) %>%
  # Añadir días totales del mes
  mutate(
    dias_mes = days_in_month(as.Date(paste(año_mes, "01", sep = "-"))),
    porcentaje_ocupacion = (dias_ocupados / dias_mes) * 100
  )

# Identificar patrones estacionales
estacionalidad_provincia <- ocupacion_mensual_propiedad %>%
  # Extraer solo el mes (ignorando el año)
  mutate(
    mes_numero = mes,
    mes_nombre = month(as.Date(paste0("2023-", sprintf("%02d", mes), "-01")), 
                       label = TRUE, abbr = FALSE)
  ) %>%
  # Unir con datos de casas
  left_join(casas %>% select(id_casa, provincia_ubicacion), by = "id_casa") %>%
  # Calcular ocupación promedio por provincia y mes
  group_by(provincia_ubicacion, mes_numero, mes_nombre) %>%
  summarise(
    ocupacion_promedio = mean(porcentaje_ocupacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calcular estadísticas para detectar estacionalidad
  group_by(provincia_ubicacion) %>%
  mutate(
    ocupacion_anual_promedio = mean(ocupacion_promedio),
    indice_estacionalidad = ocupacion_promedio / ocupacion_anual_promedio,
    variabilidad = sd(ocupacion_promedio) / ocupacion_anual_promedio
  ) %>%
  ungroup()

# Guardar resultados
write.csv(estacionalidad_provincia, "resultados/estacionalidad_provincia.csv", row.names = FALSE)

# === EJERCICIO 18: ANÁLISIS DE DISTRIBUCIÓN DE EDADES ===
cat("Ejecutando Ejercicio 18: Análisis de distribución de edades...\n")

# Preparar datos de distribución de edades
distribución_edades <- bind_rows(
  # Usuarios que alquilan
  usuarios_alquilan %>%
    mutate(
      tipo_usuario = "Huésped",
      rango_edad = case_when(
        edad <= 25 ~ "18-25",
        edad <= 35 ~ "26-35",
        edad <= 45 ~ "36-45",
        edad <= 55 ~ "46-55",
        edad <= 65 ~ "56-65",
        TRUE ~ "66+"
      )
    ) %>%
    select(tipo_usuario, rango_edad, edad),
  
  # Arrendatarios
  usuarios_arrendatarios %>%
    mutate(
      tipo_usuario = "Anfitrión",
      rango_edad = case_when(
        edad <= 25 ~ "18-25",
        edad <= 35 ~ "26-35",
        edad <= 45 ~ "36-45",
        edad <= 55 ~ "46-55",
        edad <= 65 ~ "56-65",
        TRUE ~ "66+"
      )
    ) %>%
    select(tipo_usuario, rango_edad, edad)
)

# Frecuencia por rango de edad y tipo de usuario
frecuencia_edades <- distribución_edades %>%
  group_by(tipo_usuario, rango_edad) %>%
  summarise(
    cantidad = n(),
    .groups = "drop"
  ) %>%
  # Calcular porcentajes
  group_by(tipo_usuario) %>%
  mutate(
    total_tipo = sum(cantidad),
    porcentaje = cantidad / total_tipo * 100
  ) %>%
  ungroup()

# Crear tabla comparativa pivoteada
comparativa_edades <- frecuencia_edades %>%
  select(tipo_usuario, rango_edad, porcentaje) %>%
  pivot_wider(
    names_from = tipo_usuario,
    values_from = porcentaje,
    values_fill = 0
  ) %>%
  # Añadir diferencia
  mutate(
    diferencia = Huésped - Anfitrión
  ) %>%
  # Ordenar por rango de edad
  mutate(
    rango_edad = factor(
      rango_edad,
      levels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66+")
    )
  ) %>%
  arrange(rango_edad)

# Estadísticas descriptivas
estadisticas_edad <- distribución_edades %>%
  group_by(tipo_usuario) %>%
  summarise(
    edad_media = mean(edad, na.rm = TRUE),
    edad_mediana = median(edad, na.rm = TRUE),
    edad_min = min(edad, na.rm = TRUE),
    edad_max = max(edad, na.rm = TRUE),
    desviacion_estandar = sd(edad, na.rm = TRUE),
    .groups = "drop"
  )

# Cálculo de moda
moda_edad <- distribución_edades %>%
  group_by(tipo_usuario, edad) %>%
  summarise(
    frecuencia = n(),
    .groups = "drop"
  ) %>%
  group_by(tipo_usuario) %>%
  arrange(desc(frecuencia)) %>%
  slice_head(n = 1) %>%
  select(tipo_usuario, edad_moda = edad)

# Unir estadísticas con moda
estadisticas_completas <- estadisticas_edad %>%
  left_join(moda_edad, by = "tipo_usuario")

# Guardar resultados
write.csv(comparativa_edades, "resultados/distribucion_edades.csv", row.names = FALSE)
write.csv(estadisticas_completas, "resultados/estadisticas_edad.csv", row.names = FALSE)