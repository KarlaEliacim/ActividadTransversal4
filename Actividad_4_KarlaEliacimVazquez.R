#Ultima Actividad Transversal KARLA ELIACIM VÁZQUEZ OLAYA 


install.packages("ggplot2")
library(readr)   # Para leer archivos CSV
library(dplyr)   # Para manipulación de datos
library(ggplot2) # Para visualización de datos

print("Carga de Datos")
datos_salari<- read.csv("C:/Users/EliacimVO/Documents/CLASES STUDIO_R/salarios_mujeres.csv")

print("Exploración de los datos")
head(datos_salari)
tail(datos_salari)
str(datos_salari)
summary(datos_salari)

print("Transformación y limpieza de datos")

#NAs 

colSums(is.na(datos_salari))
datos_salario_limpios<- na.omit(datos_salari)
summary(datos_salario_limpios)

#Datos duplicados eliminados 

datos_sin_duplicados <- datos_salario_limpios[!duplicated(datos_salario_limpios), ]
datos_sin_duplicados
head(datos_sin_duplicados)
tail(datos_sin_duplicados)
summary(datos_sin_duplicados)

print("4. Filtrar los datos por columna")
# Filtrar la columna de Edad en solo las edades de 33, 45, 29 y 40 años
datos_filtrados_edad <- datos_sin_duplicados[datos_sin_duplicados$Edad %in% c(33, 45, 29, 40), ]
datos_filtrados_edad

# Filtrar la columna de salarios, en solo aquellos que son mayores a 15,000 mil pesos
datos_filtrados_salario <- datos_sin_duplicados[datos_sin_duplicados$Salario > 15000, ]
datos_filtrados_salario

#Filtrar de la columna de Genero, si esta escrito la palabra Mujer con minuscula
datos_filtrado_genero <- datos_salario_limpios[datos_salario_limpios$Genero %in% c("mujer"), ]
datos_filtrado_genero

paste("Media y Mediana de la Edad y el Salario de Mujeres")
summary(datos_salario_limpios)

media <-mean(datos_salario_limpios$Edad)
print(paste("La media en edad de las mujeres es:", media))

mediana <-median(datos_salario_limpios$Edad)
print(paste("La mediana en la edad de las mujeres es:", mediana))

media_salario <-mean(datos_salario_limpios$Salario)
print(paste("La media en el salario de las mujeres es:", media_salario))

paste("Moda en Edad y Salario de Mujeres")

moda_dataset <- function(df) {
  moda <- function(x) {
    if (is.numeric(x) || is.factor(x) || is.character(x)) {
      tbl <- table(x)  
      moda_valor <- names(tbl[tbl == max(tbl)]) 
      return(moda_valor)
    } else {
      return(NA)  # Si no es un tipo de dato adecuado, devuelve NA
    }
  }
  
  resultado <- sapply(df, moda)
  return(resultado)
}

# Aplicar la función a un dataset
moda_dataset(datos_salario_limpios)


paste("Histograma de las variables del dataset")

hist(
  datos_salario_limpios$Edad,
  main = "Histograma de la edad de las mujeres",
  xlab = "Edades",
  ylab = "Frecuencia",
  col = "lightpink", border = "blue"
)

paste("Histograma de las variables del dataset")

hist(
  datos_salario_limpios$Salario,
  main = "Histograma del salario de las mujeres",
  xlab = "Salario",
  ylab = "Frecuencia",
  col = "purple", border = "lightpink"
)

paste("Diagrama de Dispersión entre la Edad y el Salario")
plot(datos_salario_limpios$Edad, datos_salario_limpios$Salario,
     main = "Diagrama de Dispersión: Edad vs Salario",
     xlab = "Edad",
     ylab = "Salario",
     col = "blue",
     pch = 19)  



print("Visualización de datos con ggplot2 para esta actividad utilizare los datos de mtcars")

data3 <- mtcars
ggplot(data3, aes(x = wt, y = mpg)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Relación entre Peso y Consumo",
       x = "Peso (wt)",
       y = "Millas por galón (mpg)")



ggplot(data3, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
  labs(title = "Distribución de millas por galón",
       x = "Millas por galón (mpg)",
       y = "Frecuencia") +
  theme_minimal()

print("Actividad concluida")



