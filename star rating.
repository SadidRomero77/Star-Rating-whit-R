# Instalar y cargar los paquetes necesarios
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")
install.packages("corrplot")
install.packages("caTools")
install.packages("randomForest")
install.packages("tidyverse")
library(FactoMineR)    # Para análisis factorial y PCA
library(factoextra)    # Para visualización de resultados de PCA
library(dplyr)         # Para manipulación de datos
library(ggplot2)       # Para visualización de datos
library(psych)         # Para análisis psicológico y estadísticas
library(corrplot)      # Para visualización de matrices de correlación
library(caTools)       # Para división de datos
library(randomForest)  # Para modelos de Random Forest
library(tidyverse)     # Incluye readr y dplyr
library(cluster)       # Para clustering
library(NbClust)       # Para determinar el número óptimo de clusters

# Cargar los datos
file <- 'C:/Users/USUARIO/Desktop/IA Whit R/6 class csv.csv'
data <- read.csv(file)

# Ver las primeras filas de la data
head(data)
# Resumen estadístico de la data
summary(data)
# Información sobre los tipos de datos
str(data)
# Dimensiones del data frame 
dim(data)
# Valores únicos de 'Spectral.Class' y 'Star.color' 
unique(data$Spectral.Class)
unique(data$Star.color)
# Conteo de valores únicos de 'Spectral.Class' y 'Star.color' 
table(data$Spectral.Class)
table(data$Star.color)
# Convertir las variables categóricas a factores
data$Star.type <- as.factor(data$Star.type)
data$Star.color <- as.factor(data$Star.color)
data$Spectral.Class <- as.factor(data$Spectral.Class)
# Verificar los cambios
str(data)

# Verificar los niveles de Star.type
levels(data$Star.type)

# Ver los nombres de las columnas
names(data)

# Eliminar filas con valores faltantes
data_compl <- na.omit(data)

# Verificar las dimensiones de los datos completos
dim(data_compl)

# Ver los datos completos
View(data_compl)

# Seleccionar las primeras cuatro columnas para el análisis de correlación
data_complt <- data_compl[, 1:4]

# Calcular la matriz de correlación
cor_matrix <- cor(data_complt)

# Visualizar la matriz de correlación con una máscara superior 
mask <- upper.tri(cor_matrix, diag = TRUE)
corrplot(cor_matrix, method = 'number', order = 'AOE', type = 'upper')

# Realizar el PCA
pca <- princomp(data_compl[, 1:4], cor = TRUE)
pca

# Visualizar el scree plot
fviz_screeplot(pca, addlabels = TRUE)

# Visualizar las variables del PCA
fviz_pca_var(pca)

# Añadir los componentes principales a los datos completos
data_compl$PC1 <- pca$scores[, 1]
data_compl$PC2 <- pca$scores[, 2]

# Filtrar los datos y crear el gráfico de PCA con colores según el tipo de estrella
data_compl %>%
  filter(Star.type %in% c('0', '1', '2', '3', '4', '5')) %>%
  ggplot(aes(PC1, PC2, col = Star.type)) +
  geom_point(alpha = 0.7) +
  ggtitle('PCA Colors Type') +
  scale_color_manual('Color', 
                     values = c('brown', 'red', 'gray', 'yellow', 'orange', 'black'),
                     labels = c('Tipo Marron', 'Enana Roja', 'Enana Blanca', 'Secundaria Principal', 'Supergigantes', 'Hypergigantes')) +
  theme_minimal()

# Visualizar el biplot del PCA
fviz_pca_biplot(pca, geom.ind = "point",
                col.ind = data_compl$Star.type,
                palette = "Uchicago", addEllipses = TRUE) +
  ggtitle("Análisis PCA Stars Type")

# Determinar el número óptimo de clusters usando el método del codo
fviz_nbclust(data_compl[, c("PC1", "PC2")], kmeans, method = "wss") + ggtitle("El método del codo")

# Aplicar K-means con el número óptimo de clusters (k = 3)
set.seed(123)
kmeans_result <- kmeans(data_compl[, c("PC1", "PC2")], centers = 3, nstart = 25)

# Añadir los resultados del clustering a los datos originales
data_compl$Cluster <- as.factor(kmeans_result$cluster)

# Graficar los clusters en el espacio PCA
ggplot(data_compl, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.7) +
  ggtitle("Clusters de K-means en el espacio PCA") +
  scale_color_manual(values = c("red", "green", "blue")) +
  theme_minimal()

#################### Modelo Random Forest con algoritmo de "Árboles de Decisión" ###################################

# Establecer una semilla para la reproducibilidad
set.seed(123)

# Dividir los datos (70% para entrenamiento y 30% para prueba)
split <- sample.split(data_compl$Star.type, SplitRatio = 0.7)
train_data <- subset(data_compl, split == TRUE)
test_data <- subset(data_compl, split == FALSE)

# Construir el modelo Random Forest
model <- randomForest(Star.type ~ ., data = train_data, importance = TRUE)

# Ver el modelo
print(model)

# Predicciones en los datos de prueba
predictions <- predict(model, newdata = test_data)

# Matriz de confusión para evaluar el modelo
confusion_matrix <- table(test_data$Star.type, predictions)
print(confusion_matrix)

# Calcular la precisión del modelo
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Precisión del modelo: ", round(accuracy, 2)))
