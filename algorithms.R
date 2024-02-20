# Önce gerekli kütüphaneleri yükleyelim
install.packages("rpart")
install.packages("OneR")
install.packages("class")
install.packages("caret")

# Ardından kütüphaneleri çağıralım
library(rpart)
library(OneR)
library(class)
library(caret)

# Veri setini oluşturalım
veri <- data.frame(
  X1 = sample(0:30, 100, replace = TRUE),
  X2 = sample(30:80, 100, replace = TRUE),
  X3 = as.factor(sample(c('Kategori_A', 'Kategori_B'), 100, replace = TRUE)),
  X4 = as.factor(sample(c('Kategori_X', 'Kategori_Y'), 100, replace = TRUE)),
  Y = as.factor(sample(0:1, 100, replace = TRUE))
)
veri

# J48 modeli
model_J48 <- rpart(Y ~ X1 + X2 + X3 + X4, data = veri, method = "class")
predictions_J48 <- predict(model_J48, veri, type = "class")
conf_matrix_J48 <- confusionMatrix(predictions_J48, veri$Y)
conf_matrix_J48$table
conf_matrix_J48$byClass


# CART modeli
model_CART <- rpart(Y ~ X1 + X2 + X3, data = veri, method = "class")
predictions_CART <- predict(model_CART, veri, type = "class")
conf_matrix_CART <- confusionMatrix(predictions_CART, veri$Y)
conf_matrix_CART$table
conf_matrix_CART$byClass
accuracy <- sum(diag(conf_matrix_CART$table)) / sum(conf_matrix_CART$table) * 100
accuracy


# OneR modeli
model_OneR <- OneR(Y ~ X1 + X2 + X3, data = veri)
predictions_OneR <- predict(model_OneR, veri)
conf_matrix_OneR <- confusionMatrix(predictions_OneR, veri$Y)
conf_matrix_OneR$table
conf_matrix_OneR$byClass

