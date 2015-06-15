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
datos_banco <- read.table("banco.csv", sep = ",", header = T)

# Verifico parte de los datos cargados
head(datos_banco)
str(datos_banco)
summary(datos_banco) 
tail(datos_banco)

class(datos_banco$edad)



