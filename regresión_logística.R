# Cargar el paquete dplyr
install.packages("dplyr")
library(dplyr)
install.packages("haven")
library(haven)
install.packages("ggplot2")
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
install.packages("tzdb")
library(tzdb)

# Eliminar columnas que contienen datos de tipo texto libre
datos_sin_strings <- datos %>% select_if(~ !any(is.character(.)))

# Mostrar el resultado
print(datos_sin_strings)

# Eliminar columnas con todos los valores NA
datos_limpios<- datos_sin_strings[, colSums(is.na(datos_sin_strings)) < nrow(datos_sin_strings)]
head(datos_limpios)

# Crear un vector con los nombres de las columnas deseadas
columnas <- c('id_vivienda', 'hh_d_rez', 'hh_d_entorno', 'pobreza_multi_5d', 'nse', 'ytoth', 's5', 'e5a', 
              's7', 'e10', 'p3', 'p4', 'area', 'region', 'tipohogar', 
              'depen_grado', 'pobreza','desercion')

# Seleccionar las columnas especificadas en el vector y crear una nueva base de datos
datos_seleccionados <- datos_limpios %>% select(all_of(columnas))

table(datos_seleccionados$desercion)
datos_seleccionados

#----------------------------------------------------------------------------------------------------------------------------
#TRABAJANDO VARIABLES CATEGÓRICAS
# Asegurarse de que las variables estén en formato de factor con niveles definidos
datos_seleccionados$nse <- factor(datos_seleccionados$nse, 
                                   levels = c(1, 2, 3, 4, 5, 6, 7),
                                   labels = c("Bajo", "Medio", "Alto", 
                                              "Bajo-medio", "Bajo-alto", 
                                              "Bajo-medio-alto", 
                                              "Medio-alto"))
table(datos_seleccionados$nse)

datos_seleccionados$s5 <- factor(datos_seleccionados$s5, 
                                  levels = c(-88, 0, 1, 2, 3, 4),
                                  labels = c("No sabe", "0", "1", 
                                             "2", "3", "4"))
table(datos_seleccionados$s5)

datos_seleccionados$e5a <- factor(datos_seleccionados$e5a, 
                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
                                             11, 12, 13, 14, 15, 16, -88, -99),
                                  labels = c("Ayuda en la casa o quehaceres del hogar",
                                             "Ayuda o se dedica al cuidado de alguien",
                                             "Embarazo, maternidad o paternidad",
                                             "Tiene una discapacidad o requiere establecimiento de educación especial",
                                             "Tiene una enfermedad o condición de salud que lo(a) inhabilita",
                                             "Problemas familiares",
                                             "No le interesa o no conoce la manera para completar sus estudios",
                                             "Terminó de estudiar",
                                             "Está asistiendo a un preuniversitario",
                                             "Se encuentra preparando la Prueba de Acceso a la Educación Superior (PAES) o Prueba de Transición (PDT) de Invierno por su cuenta",
                                             "Dificultad económica",
                                             "Trabaja o busca trabajo",
                                             "Problemas de rendimiento o cancelación de matrícula",
                                             "Dificultad de acceso o movilización",
                                             "Por la pandemia COVID-19",
                                             "Otra razón. Especifique",
                                             "No sabe",
                                             "No responde"))
table(datos_seleccionados$e5a)

datos_seleccionados$s7 <- factor(datos_seleccionados$s7, 
                                 levels = c(1, 2, 3, 4),
                                 labels = c("Embarazada", "Amamantando", "Embarazada y amamantando", "No"))
table(datos_seleccionados$s7)

datos_seleccionados$e10 <- factor(datos_seleccionados$e10, 
                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 
                                             10, 11, 12, 13, 14, -88),
                                  labels = c("Municipal o Servicio Local de Educación",
                                             "Particular Subvencionada",
                                             "Corporación de Administración Delegada",
                                             "Particular no Subvencionada",
                                             "JUNJI",
                                             "INTEGRA",
                                             "Jardín infantil o sala cuna del trabajo de la madre o del padre",
                                             "Centro de Formación Técnica",
                                             "Instituto Profesional",
                                             "Universidad Privada no perteneciente al Consejo de Rectores (CRUCH)",
                                             "Universidad Privada perteneciente al Consejo de Rectores (CRUCH)",
                                             "Universidad Estatal",
                                             "Establecimiento de Educación Superior de las Fuerzas Armadas y del Orden",
                                             "Establecimiento fuera de Chile",
                                             "No sabe"))
table(datos_seleccionados$e10)

datos_seleccionados$p3 <- factor(datos_seleccionados$p3, 
                                 levels = c(1, 2, 3, 4),
                                 labels = c("Mucho (observa basura en varios lugares)",
                                            "Más o menos (observa basura en 3 o 4 lugares)",
                                            "Poco (si observa cuidadosamente alrededor, verá uno o dos lugares con basura)",
                                            "Nada (no hay basura visible)"))
table(datos_seleccionados$p3)

datos_seleccionados$p4 <- factor(datos_seleccionados$p4, 
                                 levels = c(1, 2, 3, 4),
                                 labels = c("Mucho (observa 5 o más áreas con grafitis o daño deliberado)",
                                            "Más o menos (observa 3 o 4 áreas con grafitis o daño deliberado)",
                                            "Poco (observa una o dos áreas con grafitis o daño deliberado)",
                                            "Nada (no hay señales de grafitis o daño deliberado visibles)"))
table(datos_seleccionados$p4)

datos_seleccionados$depen_grado <- factor(datos_seleccionados$depen_grado,
                                          levels = c(0, 1, 2, 3),
                                          labels = c("No dependiente", "Dependencia severa", "Dependencia moderada", "Dependencia leve"))
table(datos_seleccionados$depen_grado)

datos_seleccionados$area <- factor(datos_seleccionados$area,
                                   levels = c(1, 2),
                                   labels = c("Urbano", "Rural"))
table(datos_seleccionados$area)

datos_seleccionados$region <- factor(datos_seleccionados$region,
                                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
                                     labels = c("Región de Tarapacá", "Región de Antofagasta", "Región de Atacama", 
                                                "Región de Coquimbo", "Región de Valparaíso", "Región del Libertador Gral. Bernardo O'Higgins",
                                                "Región del Maule", "Región del Biobío", "Región de La Araucanía", "Región de Los Lagos", 
                                                "Región de Aysén del Gral. Carlos Ibáñez del Campo", "Región de Magallanes y de la Antártica Chilena", 
                                                "Región Metropolitana de Santiago", "Región de Los Ríos", "Región de Arica y Parinacota", "Región de Ñuble"))
table(datos_seleccionados$region)

datos_seleccionados$tipohogar <- factor(datos_seleccionados$tipohogar,
                                        levels = c(1, 2, 3, 4, 5, 6),
                                        labels = c("Unipersonal", "Nuclear Monoparental", "Nuclear Biparental", 
                                                   "Extenso Monoparental", "Extenso Biparental", "Censal"))
table(datos_seleccionados$tipohogar)

datos_seleccionados$pobreza <- factor(datos_seleccionados$pobreza,
                                      levels = c(1, 2, 3),
                                      labels = c("Pobreza extrema", "Pobreza no extrema", "No pobreza"))
table(datos_seleccionados$pobreza)

# Modificar el data frame para obtener nuestra variable predictora binaria
datos_seleccionados <- datos_seleccionados %>%
  # Crear variables binarias para cada categoría
  mutate(
    no_sabe = ifelse(desercion == -88, 1, 0),
    abandono = ifelse(desercion == 1, 1, 0),
    desercion = ifelse(desercion == 2, 1, 0),
    nunca_asistio = ifelse(desercion == 3, 1, 0)
  )
print(head(datos_seleccionados))

# Crear el modelo de regresión logística usando las variables predictoras
modelo_logistico <- glm(desercion ~ hh_d_rez + hh_d_entorno + pobreza_multi_5d + nse + ytoth + s5 + e5a + e10 + s7  + p3 + 
                          p4 + depen_grado + area + region + tipohogar + pobreza,
                        data = datos_seleccionados, 
                        family = binomial(link="logit"))
summary(modelo_logistico)

#Nuevo modelo sin e10 porque generaba error
modelo_logistico1 <- glm(desercion ~ hh_d_rez + hh_d_entorno + pobreza_multi_5d + nse + ytoth + s5 + e5a + s7  + p3 + 
                           p4 + depen_grado + area + region + tipohogar + pobreza,
                         data = datos_seleccionados, 
                         family = binomial(link="logit"))
summary(modelo_logistico1)


#Nuevo modelo sin hh_d_entorno porque tenia pvalue muy alto
modelo_logistico2 <- glm(formula = desercion ~ hh_d_rez + pobreza_multi_5d + 
                           nse + ytoth + s5 + e5a + s7 + p3 + p4 + depen_grado + area + 
                           region + tipohogar + pobreza, family = binomial(link = "logit"), 
                         data = datos_seleccionados)
summary(modelo_logistico2)


#Nuevo modelo sin tipohogar porque tenia pvalue muy alto en todas sus categorías
modelo_logistico3 <- glm(formula = desercion ~ hh_d_rez + pobreza_multi_5d + 
                           nse + ytoth + s5 + e5a + s7 + p3 + p4 + depen_grado + area + 
                           region + pobreza, family = binomial(link = "logit"), 
                         data = datos_seleccionados)
summary(modelo_logistico3)

#Nuevo modelo sin pobreza porque tenia pvalue muy alto en todas sus categorías
modelo_logistico4 <- glm(formula = desercion ~ hh_d_rez + pobreza_multi_5d + 
                           nse + ytoth + s5 + e5a + s7 + p3 + p4 + depen_grado + area + 
                           region, family = binomial(link = "logit"), 
                         data = datos_seleccionados)
summary(modelo_logistico4)


#Nuevo modelo sin depen_grado porque tenia pvalue muy alto en todas sus categorías
modelo_logistico5 <- glm(formula = desercion ~ hh_d_rez + pobreza_multi_5d + 
                           nse + ytoth + s5 + e5a + s7 + p3 + p4 + area + 
                           region, family = binomial(link = "logit"), 
                         data = datos_seleccionados)
summary(modelo_logistico5)


#Nuevo modelo sin p4 porque tenia pvalue muy alto en todas sus categorías
modelo_logistico6 <- glm(formula = desercion ~ hh_d_rez + pobreza_multi_5d + 
                           nse + ytoth + s5 + e5a + s7 + p3 + area + 
                           region, family = binomial(link = "logit"), 
                         data = datos_seleccionados)
summary(modelo_logistico6)


#Nuevo modelo sin p3 porque tenia pvalue muy alto en todas sus categorías
modelo_logistico7 <- glm(formula = desercion ~ hh_d_rez + pobreza_multi_5d + 
                           nse + ytoth + s5 + e5a + s7 + area + 
                           region, family = binomial(link = "logit"), 
                         data = datos_seleccionados)
summary(modelo_logistico7)


#Nuevo modelo sin hh_d_rez porque tenia pvalue muy alto
modelo_logistico8 <- glm(formula = desercion ~ pobreza_multi_5d + 
                           nse + ytoth + s5 + e5a + s7 + area + 
                           region, family = binomial(link = "logit"), 
                         data = datos_seleccionados)
summary(modelo_logistico8)

#--------------------------------------------------------------------------------------------------------------------------------------------
#COMPROBACIÓN VALIDEZ DEL MODELO 
#VALIDACIÓN CRUZADA
#Validación simple
library(caret)   # Para dividir los datos y entrenar el modelo
install.packages("tidyverse")
library(tidyverse)
library(ISLR)

set.seed(1)
# Índices aleatorios para el set de entrenamiento 
train_index <- sample(x = nrow(datos_seleccionados), size = 0.8*(nrow(datos_seleccionados)), 
                        replace = FALSE)
# Subgrupos de entrenamiento y test
train_data <- datos_seleccionados[train_index, ]
test_data <- datos_seleccionados[-train_index, ]

#Corroborar distribuciones sean parecidas de los subgrupos de entrenamiento y testeo
prop.table(table(train_data$desercion)) %>% round(digits = 3)
prop.table(table(test_data$desercion)) %>% round(digits = 3)

# Ajuste del modelo logístico con los datos de entrenamiento
modelo_logistico_prueba <- glm(desercion ~ pobreza_multi_5d + 
                                 nse + ytoth + s5 + e5a + s7 + area + 
                                 region, data = datos_seleccionados, 
                                 family = binomial(link = "logit"), 
                               subset = train_index)
summary(modelo_logistico_prueba)

# Cálculo de la probabilidad predicha por el modelo con los datos de test
prob.modelo <- predict(object = modelo_logistico_prueba, newdata = test_data, 
                       type = "response")
# Vector de caracteres “0”
pred.modelo <- rep("0", length(prob.modelo))
# Sustitución de “0” por “1” si la probabilidad a posteriori > 0,5
pred.modelo[prob.modelo > 0.5] <- "1"

# Matriz de confusión
table(pred.modelo, test_data$desercion)

# Test error rate (resultado=NA)
mean(pred.modelo != test_data$desercion)

#Test error rate sin considerar las filas que arrojaron predicciones NA
# Filtrar las filas que no tengan NA en ninguna de las dos columnas
filas_completas <- !is.na(pred.modelo) & !is.na(test_data$desercion)
# Calcular el error solo en las filas completas
error <- mean(pred.modelo[filas_completas] != test_data$desercion[filas_completas])
error

#O bien:
mean(pred.modelo != test_data$desercion, na.rm = TRUE)

#La estimación del test error rate del modelo mediante validación simple es del 51,14%, 
#por lo que el modelo acierta con sus predicciones en solo un 1 – 0,5114 = 48,86% de los casos.
#Sin embargo, la estimación del test error rate mediante validación simple es propensa a 
#sufrir alta variabilidad (depende de cómo se hayan distribuido las observaciones en los 
#grupos de entrenamiento y test). A continuación, se muestra el mismo proceso llevado 
#a cabo anteriormente, repitiéndolo 100 veces (en cada iteración los datos se van a repartir
#de manera distinta en entrenamiento y test).

# Vector donde se almacenarán los 100 test error estimados
vector_errores <- rep(NA, 500)
for (i in 1:500){
  # importante la creación de nuevos índices para cada iteración, de lo contrario, 
  #el test error obtenido siempre sería el mismo
  train_index <- sample(x = nrow(datos_seleccionados), size = 0.8*(nrow(datos_seleccionados)), 
                           replace = FALSE)
  train_data <- datos_seleccionados[train_index, ]
  test_data <- datos_seleccionados[-train_index, ]
  modelo_logistico_prueba <- glm(desercion ~ pobreza_multi_5d + 
                                   nse + ytoth + s5 + e5a + s7 + area + 
                                   region, data = datos_seleccionados, 
                                 family = binomial(link = "logit"), 
                                 subset = train_index)
  prob.modelo <- predict(object = modelo_logistico_prueba, newdata = test_data, 
                         type = "response")
  pred.modelo <- rep("0", length(prob.modelo))
  pred.modelo[prob.modelo > 0.5] <- "1"
  vector_errores[i] <- mean(pred.modelo != test_data$desercion, na.rm = TRUE)
}
summary(vector_errores)

#Con esto hemos comprobado que el error disminuyó, concluyendo que la estimación 
#del test error rate del modelo mediante la iteración de 100 validaciones
#es del 50,81%, lo que indicaría que el modelo acierta con sus predicciones en 
#solo un 1 – 0,5081 = 49,19% de los casos.

#GRAFICANDO LA DISTRIBUCIÓN DE LOS ERRORES
boxplot <- ggplot(data = data.frame(vector_errores = vector_errores), aes(x = 1, 
                                                              y = vector_errores)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(colour = c("orangered2"), width = 0.1) +
  theme(axis.title.x = element_blank(),axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
boxplot

histograma <- ggplot(data = data.frame(vector_errores = vector_errores), aes(x = vector_errores)) +
  geom_histogram(color = "grey", fill = "orangered2")
histograma

grid.arrange(boxplot, histograma, ncol = 2)

#Para que el modelo predictivo se considere útil, debe acertar con las 
#predicciones en un porcentaje superior a lo esperado por azar o respecto 
#al nivel basal (el que se obtiene si se asignaran a la clase mayoritaria), 
#que en el caso de este set de datos corresponde a “1”:

prop.table(table(datos_seleccionados$desercion)) 

#-----------------------------------------------------------------------------------------------------------------------
#PARA PRESENTAR LOS RESULTADOS

y_test <- test_data$desercion
# Crear un data frame con el código del hogar y la probabilidad de deserción
  resultados_probabilidades <- data.frame(
  id_vivienda = test_data$id_vivienda,
  probabilidad_desercion = prob.modelo
  )
View(resultados_probabilidades[complete.cases(resultados_probabilidades), ])

#Crear tabla comparativa con los valores reales (y_test) y predichos (pred.modelo)
tabla_comparativa <- data.frame(
  `ID_vivienda` = resultados_probabilidades$id_vivienda,
  `Valor Real (y_test)` = y_test,  # los valores reales de la variable de interés
  `Predicción (y_pred)` = pred.modelo,  # las predicciones convertidas a binario (0 o 1)
  probabilidad_desercion = prob.modelo
)
View(tabla_comparativa[complete.cases(tabla_comparativa), ])

# Añadir una columna que indique si la predicción fue correcta
tabla_comparativa$`Es_correcto` <- tabla_comparativa$`Valor.Real..y_test.` == tabla_comparativa$`Predicción..y_pred.`
View(tabla_comparativa[complete.cases(tabla_comparativa), ])

#TABLA QUE MUESTRA LOS QUE EL MODELO PREDICE QUE VAN A DESERTAR
View(tabla_comparativa[tabla_comparativa$Predicción..y_pred.=="1", ])
