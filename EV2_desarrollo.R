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



#Requerimientos descriptivos

#1. Perfil del comprador
perfil_demografico_combinado <- datos %>%
  filter(compra_promo == "SI") %>%
  count(rango_etario, educacion, sexo, est_civil, actividad, sort = TRUE, name = "total_clientes")

top_10_perfiles <- head(perfil_demografico_combinado, 10)
print(top_10_perfiles)

ggplot(top_10_perfiles,
       aes(x = total_clientes,
           y = fct_reorder(paste(rango_etario, educacion, sexo, est_civil, actividad, sep = " - "), total_clientes))) +
  geom_col(fill = "#cc7a23") +
  geom_text(aes(label = total_clientes)) + 
  labs(
    title = "Top 10 Perfiles Demográficos de Compradores",
    subtitle = "Combinación de rango etario, educación, sexo, estado civil y actividad",
    x = "Total de Clientes",
    y = "Perfil Demográfico"
  ) +
  theme_minimal()



#2. Genero y compra
tasa_por_genero <- datos %>%
  group_by(sexo) %>%
  summarise(
    total_clientes = n(),
    total_compradores = sum(compra_promo == "SI"),
    tasa_conversion = (total_compradores / total_clientes) * 100
  ) %>%
  ungroup() %>%
  mutate(
    destacado = if_else(tasa_conversion == max(tasa_conversion), "Máxima Conversión", "Menor Conversión")
  )
print(tasa_por_genero)

ggplot(tasa_por_genero, aes(x = sexo, y = tasa_conversion, fill = destacado)) +
  geom_col() +
  geom_text(aes(label = paste0(round(tasa_conversion, 1), "%"))) +
  
  scale_fill_manual(
    name = "Resultado del Análisis", 
    values = c("Máxima Conversión" = "#1aab6c", "Menor Conversión" = "#967111")
  ) +
  
  labs(
    title = "Tasa de Conversión por Género",
    subtitle = "El género con la mayor tasa de conversión es resaltado automáticamente",
    x = "Género",
    y = "Tasa de Conversión (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



#3. Educacion y compra
tasa_por_educacion <- datos %>%
  group_by(educacion) %>%
  summarise(
    total_clientes = n(),
    total_compradores = sum(compra_promo == "SI"),
    tasa_conversion = (total_compradores / total_clientes) * 100
  ) %>%
  arrange(desc(tasa_conversion))
print(tasa_por_educacion)

ggplot(tasa_por_educacion, aes(x = tasa_conversion, y = fct_reorder(educacion, tasa_conversion), fill = educacion)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(tasa_conversion, 1), "%"))) +
  labs(
    title = "Tasa de Conversión por Nivel Educacional",
    subtitle = "Porcentaje de clientes de cada nivel que compraron la promoción",
    x = "Tasa de Conversión (%)",
    y = "Nivel Educacional"
  ) +
  scale_x_continuous(limits = c(0, max(tasa_por_educacion$tasa_conversion) * 1.1)) +
  theme_minimal()



#4. Estados de deuda
datos_con_salud <- datos %>%
  mutate(
    salud_financiera = case_when(
      est_actual == "SIN DEUDA"      ~ "Saludable",
      est_actual == "DEUDA DE 1 MES" ~ "En Riesgo",
      est_actual == "DEUDA DE 2 MESES" ~ "En Riesgo",
      TRUE                           ~ "Otro"
    )
  )

tasa_por_salud <- datos_con_salud %>%
  group_by(salud_financiera) %>%
  summarise(
    total_clientes = n(),
    total_compradores = sum(compra_promo == "SI"),
    tasa_conversion = (total_compradores / total_clientes) * 100
  ) %>%
  arrange(desc(tasa_conversion))
print(tasa_por_salud)

ggplot(tasa_por_salud, aes(x = salud_financiera, y = tasa_conversion, fill = salud_financiera)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(tasa_conversion, 1), "%"))) +
  labs(
    title = "Tasa de Conversión por Salud Financiera del Cliente",
    subtitle = "Comparación entre clientes 'Saludables' (sin deuda) y 'En Riesgo' (con deuda)",
    x = "Categoría de Salud Financiera",
    y = "Tasa de Conversión (%)"
  ) +
  theme_minimal()



#5. Monto de compra
resumen_monto_compras <- datos %>%
  group_by(compra_promo) %>%
  summarise(
    promedio = mean(compras, na.rm = TRUE),
    mediana = median(compras, na.rm = TRUE),
    desviacion_estandar = sd(compras, na.rm = TRUE),
    total_clientes = n()
  )
print(resumen_monto_compras)

ggplot(datos, aes(x = compra_promo, y = compras, fill = compra_promo)) +
  geom_boxplot(alpha = 0.7) + 
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ".", decimal.mark=",")) +
  labs(
    title = "Comparación del Monto de Compra",
    subtitle = "Distribución del monto gastado por clientes que compraron y no compraron la promoción",
    x = "¿Compró en Promoción?",
    y = "Monto de la Compra"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 



#6. Uso del cupo
resumen_uso_cupo <- datos %>%
  group_by(compra_promo) %>%
  summarise(
    promedio_uso = mean(porcentaje_uso_cupo, na.rm = TRUE),
    mediana_uso = median(porcentaje_uso_cupo, na.rm = TRUE),
    desviacion_estandar = sd(porcentaje_uso_cupo, na.rm = TRUE)
  )
print(resumen_uso_cupo)

ggplot(datos, aes(x = porcentaje_uso_cupo, fill = compra_promo)) +
  geom_density(alpha = 0.7) +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Densidad del Uso del Cupo por Grupo de Compra",
    x = "Porcentaje de Uso del Cupo",
    y = "Densidad"
  ) +
  theme_minimal()



#7. Historial de atrasos
resumen_atrasos <- datos %>%
  group_by(compra_promo) %>%
  summarise(
    promedio_atrasos = mean(cant_atrasos, na.rm = TRUE),
    mediana_atrasos = median(cant_atrasos, na.rm = TRUE),
    desviacion_estandar = sd(cant_atrasos, na.rm = TRUE)
  )
print(resumen_atrasos)

ggplot(datos, aes(x = compra_promo, y = cant_atrasos, fill = compra_promo)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Comparación del Historial de Atrasos en Pagos",
    x = "¿Compró en Promoción?",
    y = "Cantidad Histórica de Atrasos"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



#8. Antiguedad del cliente
datos_analisis_final <- datos %>%
  mutate(
    antiguedad = (2025 - anio_apertura), 
    compra_promo_limpia = str_to_lower(compra_promo)
  )
tasa_por_antiguedad_anual <- datos_analisis_final %>%
  group_by(antiguedad) %>%
  summarise(
    tasa_conversion = mean(compra_promo_limpia == "si") * 100
  )
print(tasa_por_antiguedad_anual)

ggplot(tasa_por_antiguedad_anual, aes(x = antiguedad, y = tasa_conversion)) +
  geom_line(color = "#cc7a23", size = 1.2) +
  geom_point(color = "#967111", size = 3, shape = 21, fill = "white", stroke = 1.5) +
  labs(
    title = "Tasa de Conversión por Antigüedad del Cliente",
    subtitle = "Tendencia de la probabilidad de compra año por año",
    x = "Antigüedad del Cliente (Años)",
    y = "Tasa de Conversión (%)"
  ) +
  theme_minimal()





#Requerimientos predictivos

#División en datos de prueba y entrenamiento
datos_modelo <- datos %>%
  mutate(compra_promo = str_to_lower(compra_promo))
set.seed(123)
indices_entrenamiento <- createDataPartition(datos_modelo$compra_promo, p = 0.7, list = FALSE)
datos_entrenamiento <- datos_modelo[indices_entrenamiento, ]
datos_prueba <- datos_modelo[-indices_entrenamiento, ]

cat("Datos listos. Entrenamiento:", nrow(datos_entrenamiento), "filas. Prueba:", nrow(datos_prueba), "filas.\n")



#1. Prediccion de compra en promocion
modelo_arbol_demografico <- rpart(
  compra_promo ~ rango_etario + educacion + sexo + est_civil + actividad, 
  data = datos_entrenamiento, 
  method = "class",
  control = rpart.control(cp = 0.005)
)

rpart.plot(
  modelo_arbol_demografico,
  type = 2, extra = 101, box.palette = "Oranges",
  shadow.col = "darkgray", nn = TRUE, fallen.leaves = TRUE,
  main = "Gráfico 1: Predicción de Compra Usando Solo Perfil Demográfico"
)

predicciones_demograficas <- predict(modelo_arbol_demografico, datos_prueba, type = "class")
niveles_completos <- c("no", "si") 
predicciones_factor_dem <- factor(predicciones_demograficas, levels = niveles_completos)
reales_factor_dem <- factor(datos_prueba$compra_promo, levels = niveles_completos)
matriz_confusion_demografica <- confusionMatrix(predicciones_factor_dem, reales_factor_dem)
print(matriz_confusion_demografica)



#2. Identificacion de clientes clave
#usamos el arbol de decision creado en el requerimiento 1
importancia_variables <- varImp(modelo_arbol)
importancia_df <- rownames_to_column(as.data.frame(importancia_variables), "Variable") %>%
  arrange(desc(Overall))
print(head(importancia_df, 10))

ggplot(head(importancia_df, 10), aes(x = Overall, y = fct_reorder(Variable, Overall))) +
  geom_col(fill = "#cc7a23") +
  geom_text(aes(label = round(Overall, 2)), hjust = -0.2, size = 3.5) +
  labs(
    title = "Gráfico 2: Características Más Importantes del Cliente",
    subtitle = "Ranking de variables para predecir la compra en promoción",
    x = "Importancia (calculada por el modelo)",
    y = "Característica del Cliente"
  ) +
  theme_minimal()



#3. Estimacion del monto de compra
modelo_regresion <- lm(
  compras ~ cupo_max + porcentaje_uso_cupo + cant_atrasos + est_actual + compras_promedio_anio, 
  data = datos_entrenamiento
)
summary(modelo_regresion)

predicciones_monto <- predict(modelo_regresion, datos_prueba)
resultados_regresion <- data.frame(
  MontoReal = datos_prueba$compras,
  MontoPredicho = predicciones_monto
)

ggplot(resultados_regresion, aes(x = MontoReal, y = MontoPredicho)) +
  geom_point(alpha = 0.5, color = "#cc7a23") +
  geom_abline(color = "red", linetype = "dashed", linewidth = 1) + 
  scale_x_continuous(labels = scales::dollar) + 
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Gráfico 3: Desempeño del Modelo de Estimación de Compras",
    subtitle = "Comparación de Montos Reales vs. Montos Predichos por el Modelo",
    x = "Monto de Compra Real",
    y = "Monto de Compra Predicho"
  ) +
  theme_minimal()



#4. Prediccion de comportamiento de pago



#5. Segmentacion de clientes por consumo



#6. Prediccion por perfil financiero



#7. Estimacion de compras futuras del producto A



#8. Prediccion de pagos de consumos



