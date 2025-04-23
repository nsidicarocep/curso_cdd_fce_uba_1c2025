# === INFORMACIÓN DEL ESTUDIANTE ===
# Nombre: Ejemplo
# Apellido: Solución
legajo <- 882280
# Email: 

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
outstub <- 'resultados'

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
