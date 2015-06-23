# Posgrado Introducción a la Minería de Datos
# Proyecto Banco
# Autores: Ruben Flecha - Guido Kosloff

# Elimino todo lo que haya en memoria
rm(list=ls())

# Defino el directoria de trabajo
setwd("C:/Users/Ruben/UTN_DM1C2015") 

#Confirmo mi directorio de trabajo
getwd()

# Importo librerías
if(!require("ggplot2")) 
  install.packages("ggplot2")
library(ggplot2)
if(!require("caret")) 
  install.packages("caret")
library(caret)
if(!require("pROC"))  
  install.packages("pROC")
library(pROC)
if(!require("sqldf")) 
  install.packages("sqldf")
library(sqldf)

# Cargo los datos del banco
message("Importando datoos...")
datos_banco <- read.table("banco.csv", sep = ",", header = T)
message("Importando datos. LISTO")

# Verifico parte de los datos cargados
head(datos_banco)
str(datos_banco)
summary(datos_banco) 
tail(datos_banco)

# Re escalo la varible y para que "si" sea la clase por defecto
class(datos_banco$edad)
datos_banco
datos_banco <- within(datos_banco, y <- relevel(y, ref = "si"))

# COnstruyo las métricas
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
  
  #Me Interesa evaluar con otra variable que a mi me interese, por ejemplo: Plata! 
  # En plata
  #
  #             PREDICHO
  #             SI		NO
  #   REAL	SI	750		-750
  #         NO	-100		0
  #
  #
  plata <-         750 * sum(predicho == "si" & data$obs== "si")
  plata <- plata - 100 * sum(predicho == "si" & data$obs== "no")
  plata <- plata - 850 * sum(predicho == "no" & data$obs== "si")
  names(plata) <- "Plata"
  
  # Armo el vector de outPut
  outPut <- c(F1_score, plata, prec, recall)
  names(outPut) <- c("F1_score", "Plata", "Precission" , "Recall")
  
  # Agrego a la salida el valor del área bajo la curva roc
  auc_metrics <- twoClassSummary(data, lev, model)
  
  return(c(outPut, auc_metrics[1]))

}
message("Armado de la función METRICAS. LISTO")

# Estructura del experimento
fitControl <- trainControl(method = "cv", #CV --> Cross Validation
                           number = 3,    # Lo parte en 3
                           verboseIter = T,
                           classProbs = TRUE,
                           summaryFunction = metricas)

# Prueba con árbol de decisión 
#(listado con otros modelos: http://topepo.github.io/caret/modelList.html)

modelo <- train(y ~ ., data = datos_banco,
                method = "rpart",
                trControl = fitControl,
                metric = "Plata")



