#leer Excel
library(readxl)
productos_promocion <- read_excel("D:/inacap/S6/mineriaDatos/Taller-Analisis-en-R/productos_promocion_ev2.xlsx")
View(productos_promocion)
datos <- productos_promocion

#descripcion de los datos
str(datos)

#cambiar nombre de los titulos de las columnas
colnames(datos) <- c("rango_etario", "educacion", "sexo", "est_civil", "actividad", "est_actual", "anio_apertura", "cupo_max", "porcentaje_uso_cupo", "compras_promedio_anio", "unidades_prod_A", "unidades_prod_B", "cant_atrasos", "compra_promo", "compras", "fecha")
str(datos)

#Ver datos faltantes
colSums(is.na(datos)) 

#Eliminar 14 datos faltantes de est_civil
datos <- datos[!is.na(datos$est_civil), ]
colSums(is.na(datos))

#Reemplazar 190 datos faltantes de actividad por "No informado"
datos$actividad[is.na(datos$actividad)] <- "NO INFORMADO"
colSums(is.na(datos))
View(datos)

#Transformar datos char a factor
datos$rango_etario <- as.factor(datos$rango_etario)
datos$educacion <- as.factor(datos$educacion)
datos$sexo <- as.factor(datos$sexo)
datos$est_civil <- as.factor(datos$est_civil)
datos$actividad <- as.factor(datos$actividad)
datos$est_actual <- as.factor(datos$est_actual)
datos$compra_promo <- as.factor(datos$compra_promo)
str(datos)