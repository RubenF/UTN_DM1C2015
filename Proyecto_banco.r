# Posgrado Introducción a la Minería de Datos
# Proyecto Banco -
# Autor: Ing. Ruben Flecha 

# Elimino todo lo que haya en memoria
rm(list=ls())

# Defino el directoria de trabajo
setwd("C:/Users/Ruben/UTN_DM1C2015") 

#Confirmo mi directorio de trabajo
getwd()

# Importo librerías
if(!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if(!require("caret"))  install.packages("caret")
library(caret)
if(!require("pROC"))  install.packages("pROC")
library(pROC)
if(!require("sqldf")) install.packages("sqldf")
library(sqldf)
if(!require("dplyr")) install.packages("dplyr")
library(dplyr)
if(!require("doParallel"))  install.packages("doParallel")
library(doParallel)

# Cargo los datos del banco
message("Cargando datos...")
datos_banco <- read.table("banco5_remix.csv", sep = ",", header = T)
message("Carga de datos --> LISTO!!!")

# Verifico parte de los datos cargados
head(datos_banco)
str(datos_banco)
summary(datos_banco) 
tail(datos_banco)

# Analisis exploratorio de datos
boxplot(datos_banco$edad)
median(datos_banco$edad)
plot(density((datos_banco$edad)))
hist(datos_banco$edad, col = "green")
hist(datos_banco$dia, col = "red", breaks = 3)
hist(datos_banco$duracion, col = "blue", breaks = 50)
hist(datos_banco$edad, col = "green", breaks =6)
plot(density((datos_banco$balance)))
hist(datos_banco$balance, col = "green")
rug(datos_banco$balance)
hist(datos_banco$balance, col = "green",breaks = 5)
hist(datos_banco$balance, col = "green")
abline(v = 12, lwd = 2)
abline(v = median(datos_banco$balance), col = "magenta", lwd = 4)


# Re escalo la varible y para que "si" sea la clase por defecto
class(datos_banco$edad)
datos_banco <- within(datos_banco, y <- relevel(y, ref = "si"))

# Separo en training y testing ()
set.seed(1234) # Semilla para que todos tengamos lo mismo
muestra <- sample(c(1:nrow(datos_banco)),
                  size = nrow(datos_banco)/10,
                  replace=FALSE)
BancoTraining <- datos_banco[-muestra,]
BancoTesting  <- datos_banco[muestra,]

# Construyo las métricas
message("Armado de la función METRICAS....")
metricas <- function(data, lev = NULL, model = NULL, ...) 
  {
  
  # Armo la clase predicha en función a una p
  # Probabilidad que voy a usar como corte para un "si"
  
  predicho <- ifelse(data$si > 0.5, "si", "no") 
  
  # F1 score. Esta es la métrica que me ayuda a elegir el mejor modelo.
  
  true_pos  <- sum(predicho == "si" & data$obs == "si")
  false_pos <- sum(predicho == "si" & data$obs == "no")
  false_neg <- sum(predicho == "no" & data$obs == "si")
  
  F1_score  <- (2 * true_pos) / (2 * true_pos + false_neg + false_pos)
  
  # Precission
  prec <- sum(predicho == "si" & data$obs== "si") / sum(predicho == "si")
  
  # Recall
  recall <- sum(predicho == "si" & data$obs== "si") / sum(data$obs == "si")
  
  #Me Interesa evaluar con otra variable que a mi me interese, por ejemplo: Plata 
  plata <-         750 * sum(predicho == "si" & data$obs== "si")
  plata <- plata - 100 * sum(predicho == "si" & data$obs== "no")
  plata <- plata - 750 * sum(predicho == "no" & data$obs== "si")
  names(plata) <- "Plata"
  
  # Armo el vector de outPut
  outPut <- c(F1_score, plata, prec, recall)
  names(outPut) <- c("F1_score", "Plata", "Precission" , "Recall")
  
  # Agrego a la salida el valor del área bajo la curva roc
  auc_metrics <- twoClassSummary(data, lev, model)
  
  return(c(outPut, auc_metrics[1]))

}
message("Armado de la función METRICAS. LISTO")

# Fitcontrol es la variable donde guardo el diseño de mi experimento.
#Estructura del experimento
fitControl <- trainControl(method = "cv",     #CV --> Cross Validation
                           number = 3,        # 3 folds
                           verboseIter = T,
                           classProbs = TRUE,
                           summaryFunction = metricas)

# Prueba con árbol de decisión 
#(listado con otros modelos: http://topepo.github.io/caret/modelList.html)

# Voy a entrenar un modelo
#-------------------------------
modelorpart <-  train(y ~ .,                # Voy a predecir y
                data = BancoTraining,       #  sobre el dataset: BancoTraining
                method = "rpart",           # Mi modelo en este caso es un "Arbol de decision" (rpart)
                trControl = fitControl,     # Lo entreno con una estructura, que es la variable que creé antes
                metric = "Plata")       

print(modelorpart)
plot(modelorpart)
ggplot(modelorpart)
#----------------------------------------------------------------------------------
# Regresion Logistica
logisticFit <- train(y ~ ., 
                     data = datos_banco,
                     method = "glm",         #glm --> regresion logistica
                     trControl = fitControl,
                     family = binomial)

print(logisticFit)
plot(logisticFit)
#-----------------------------------------------------------------------------------
# Modelo Random Forest
modelorf <- train(y ~ ., 
            data = BancoTraining,
            method = "rf",
            trControl = fitControl,
            metric = "Plata")

print(modelorf)
