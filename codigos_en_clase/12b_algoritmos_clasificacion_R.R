# Análisis de Supervivencia del Titanic con Multiple Algoritmos de ML
# Utilizando el paquete Caret para comparar diferentes modelos

# Cargar librerías necesarias
library(caret)
library(randomForest)
library(e1071)
library(rpart)
library(naivebayes)
library(dplyr)
library(ggplot2)
library(corrplot)
library(VIM)

# Configurar semilla para reproducibilidad
set.seed(123)

# 1. CARGA Y EXPLORACIÓN DE DATOS
# Cargar el dataset del Titanic
data("Titanic")
titanic_df <- as.data.frame(Titanic)

# Expandir el dataset usando la frecuencia
titanic_expanded <- titanic_df[rep(row.names(titanic_df), titanic_df$Freq), 1:4]

# Resetear índices
rownames(titanic_expanded) <- NULL

# Visualizar estructura de datos
cat("Estructura del dataset:\n")
str(titanic_expanded)
cat("\nResumen del dataset:\n")
summary(titanic_expanded)

# Verificar valores faltantes
cat("\nValores faltantes por variable:\n")
sapply(titanic_expanded, function(x) sum(is.na(x)))

# 2. PREPARACIÓN DE DATOS
# Convertir variables categóricas a factores si es necesario
titanic_expanded$Class <- as.factor(titanic_expanded$Class)
titanic_expanded$Sex <- as.factor(titanic_expanded$Sex)
titanic_expanded$Age <- as.factor(titanic_expanded$Age)
titanic_expanded$Survived <- as.factor(titanic_expanded$Survived)

# Verificar distribución de la variable objetivo
cat("\nDistribución de supervivientes:\n")
table(titanic_expanded$Survived)
prop.table(table(titanic_expanded$Survived))

# 3. DIVISIÓN EN CONJUNTOS DE ENTRENAMIENTO Y PRUEBA
trainIndex <- caret::createDataPartition(titanic_expanded$Survived, p = 0.8, 
                                  list = FALSE, times = 1)
train_data <- titanic_expanded[trainIndex, ]
test_data <- titanic_expanded[-trainIndex, ]

cat("\nTamaño del conjunto de entrenamiento:", nrow(train_data))
cat("\nTamaño del conjunto de prueba:", nrow(test_data))

# 4. CONFIGURACIÓN DE VALIDACIÓN CRUZADA
# Configurar control de entrenamiento con validación cruzada
ctrl <- caret::trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary
)

# 5. ENTRENAMIENTO DE MODELOS CON OPTIMIZACIÓN DE HIPERPARÁMETROS

cat("\n=== INICIANDO ENTRENAMIENTO DE MODELOS ===\n")

# 5.1 REGRESIÓN LOGÍSTICA
cat("\n1. Entrenando Regresión Logística...\n")
model_glm <- caret::train(
  Survived ~ ., 
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

# 5.2 ÁRBOLES DE DECISIÓN
cat("\n2. Entrenando Árboles de Decisión...\n")
# Grid de hiperparámetros para árboles de decisión
grid_rpart <- expand.grid(
  cp = seq(0.001, 0.1, by = 0.01)
)

model_rpart <- train(
  Survived ~ ., 
  data = train_data,
  method = "rpart",
  trControl = ctrl,
  tuneGrid = grid_rpart,
  metric = "ROC"
)

# 5.3 RANDOM FOREST
cat("\n3. Entrenando Random Forest...\n")
# Grid de hiperparámetros para Random Forest
grid_rf <- expand.grid(
  mtry = c(1, 2, 3)  # Número de variables a considerar en cada división
)

model_rf <- train(
  Survived ~ ., 
  data = train_data,
  method = "rf",
  trControl = ctrl,
  tuneGrid = grid_rf,
  metric = "ROC",
  ntree = 500
)

# 5.4 SUPPORT VECTOR MACHINE (SVM)
cat("\n4. Entrenando SVM...\n")
# Grid de hiperparámetros para SVM
grid_svm <- expand.grid(
  sigma = c(0.01, 0.1, 1),
  C = c(0.1, 1, 10, 100)
)

model_svm <- train(
  Survived ~ ., 
  data = train_data,
  method = "svmRadial",
  trControl = ctrl,
  tuneGrid = grid_svm,
  metric = "ROC"
)

# 5.5 NAIVE BAYES
cat("\n5. Entrenando Naive Bayes...\n")
# Grid de hiperparámetros para Naive Bayes
grid_nb <- expand.grid(
  laplace = c(0, 0.5, 1),
  usekernel = c(TRUE, FALSE),
  adjust = c(0.5, 1, 2)
)

model_nb <- train(
  Survived ~ ., 
  data = train_data,
  method = "naive_bayes",
  trControl = ctrl,
  tuneGrid = grid_nb,
  metric = "ROC"
)

# 6. COMPARACIÓN DE MODELOS
cat("\n=== COMPARANDO MODELOS ===\n")

# Recopilar modelos para comparación
models_list <- list(
  "Regresión Logística" = model_glm,
  "Árboles de Decisión" = model_rpart,
  "Random Forest" = model_rf,
  "SVM" = model_svm,
  "Naive Bayes" = model_nb
)

# Comparar modelos usando resamples
resamples_comparison <- resamples(models_list)

# Mostrar resumen de comparación
cat("\nResumen de comparación de modelos:\n")
summary(resamples_comparison)

# 7. EVALUACIÓN EN CONJUNTO DE PRUEBA
cat("\n=== EVALUACIÓN EN CONJUNTO DE PRUEBA ===\n")

# Función para evaluar modelo
evaluate_model <- function(model, test_data, model_name) {
  # Predicciones
  predictions <- predict(model, test_data)
  predictions_prob <- predict(model, test_data, type = "prob")
  
  # Matriz de confusión
  cm <- confusionMatrix(predictions, test_data$Survived, positive = "Yes")
  
  # Métricas
  accuracy <- cm$overall['Accuracy']
  sensitivity <- cm$byClass['Sensitivity']
  specificity <- cm$byClass['Specificity']
  
  # AUC
  library(pROC)
  roc_obj <- roc(test_data$Survived, predictions_prob$Yes)
  auc_value <- auc(roc_obj)
  
  cat("\n", model_name, ":\n")
  cat("Accuracy:", round(accuracy, 4), "\n")
  cat("Sensitivity:", round(sensitivity, 4), "\n")
  cat("Specificity:", round(specificity, 4), "\n")
  cat("AUC:", round(auc_value, 4), "\n")
  
  return(list(
    model_name = model_name,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    auc = auc_value,
    confusion_matrix = cm,
    roc = roc_obj
  ))
}

# Evaluar todos los modelos
results <- list()
for (i in 1:length(models_list)) {
  results[[i]] <- evaluate_model(models_list[[i]], test_data, names(models_list)[i])
}

# 8. IDENTIFICAR EL MEJOR MODELO
cat("\n=== IDENTIFICANDO EL MEJOR MODELO ===\n")

# Crear dataframe con métricas para comparación
metrics_df <- data.frame(
  Modelo = sapply(results, function(x) x$model_name),
  Accuracy = sapply(results, function(x) x$accuracy),
  Sensitivity = sapply(results, function(x) x$sensitivity),
  Specificity = sapply(results, function(x) x$specificity),
  AUC = sapply(results, function(x) x$auc)
)

# Ordenar por AUC (puedes cambiar por la métrica que prefieras)
metrics_df <- metrics_df[order(-metrics_df$AUC), ]

cat("\nRanking de modelos por AUC:\n")
print(metrics_df)

best_model_name <- metrics_df$Modelo[1]
cat("\nMejor modelo:", best_model_name, "con AUC =", round(metrics_df$AUC[1], 4))

# 9. ANÁLISIS DEL MEJOR MODELO
cat("\n=== ANÁLISIS DEL MEJOR MODELO ===\n")

best_model <- models_list[[best_model_name]]

# Mostrar mejores hiperparámetros
cat("\nMejores hiperparámetros para", best_model_name, ":\n")
print(best_model$bestTune)

# Importancia de variables (si está disponible)
if (best_model_name %in% c("Random Forest", "Árboles de Decisión")) {
  cat("\nImportancia de variables:\n")
  var_imp <- varImp(best_model)
  print(var_imp)
}

# 10. VISUALIZACIONES
cat("\n=== GENERANDO VISUALIZACIONES ===\n")

# Gráfico de comparación de modelos
p1 <- ggplot(metrics_df, aes(x = reorder(Modelo, AUC), y = AUC)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = round(AUC, 3)), hjust = -0.1) +
  coord_flip() +
  labs(title = "Comparación de Modelos por AUC",
       x = "Modelo",
       y = "AUC") +
  theme_minimal()

print(p1)

# Curvas ROC para todos los modelos
library(pROC)
plot(results[[1]]$roc, col = 1, main = "Comparación de Curvas ROC")
for (i in 2:length(results)) {
  plot(results[[i]]$roc, col = i, add = TRUE)
}
legend("bottomright", 
       legend = sapply(results, function(x) x$model_name),
       col = 1:length(results), 
       lty = 1)

# 11. RECOMENDACIONES FINALES
cat("\n=== RECOMENDACIONES FINALES ===\n")
cat("1. El mejor modelo es:", best_model_name)
cat("\n2. AUC del mejor modelo:", round(metrics_df$AUC[1], 4))
cat("\n3. Para usar este modelo en nuevos datos, utiliza el objeto:", paste0("model_", gsub(" ", "_", tolower(best_model_name))))

# Guardar el mejor modelo (opcional)
# saveRDS(best_model, paste0("mejor_modelo_titanic_", Sys.Date(), ".rds"))


