#1. Carga los archivos y examínalos en R. Emplea las funciones head(), summary (), dim() y str(). ¿Cuántas variables hay?¿Cuántos tratamientos?
#Para cargar los datos:
datos <- read.table("C:\\Users\\Alba\\OneDrive - iessantacatalina.org\\Escritorio\\datos-trabajoR.txt", header = TRUE, sep = "\t")
# Para calcular el número de variables:
num_variables <- dim(datos)[2]  # Número de columnas
cat("Número de variables:", num_variables, "\n")
# Ver las primeras filas del archivo
head(datos)
# Resumen estadístico de los datos
summary(datos)
# Dimensiones de los datos (filas y columnas)
dim(datos)
# Estructura de los datos
str(datos)
# Para obtener el número de tratamientos:
num_tratamientos <- length(unique(datos$Tratamiento))
# Número de variables
num_variables <- dim(datos)[2]  # Número de columnas
cat("Número de variables:", num_variables, "\n")
# Número de tratamientos (asumiendo que la columna "Tratamiento" representa los tratamientos)
num_tratamientos <- length(unique(datos$Tratamiento))
cat("Número de tratamientos:", num_tratamientos, "\n")


#2.Haz un boxplot para nuestros datos. Uno para cada condición. Elige un color para cada condición y guárdalo para las siguientes gráficas.
# Defino los colores
color_wildtype <- "lightpink"
color_sequia <- "lightblue"
color_excesoriego <- "lightyellow"
#Para crear el boxplot:
boxplot(datos$Wildtype, datos$Sequia, datos$ExcesoRiego,
        names = c("Wildtype", "Sequia", "Exceso Riego"),
        col = c(color_wildtype, color_sequia, color_excesoriego),
        main = "Boxplot para cada Condición",
        ylab = "Valores")


#3.Haz dos gráficos de dispersión. El primero debe comparar Sequía con Wildtype,y el segundo Exceso Riego con Wildtype. Cada tratamiento debe de ir de un color distinto.Pista:usacol=datos$Tratamiento.
# Convierto la columna 'Tratamiento' en un factor y le defino una paleta de colores
datos$Tratamiento <- as.factor(datos$Tratamiento)
colores_tratamientos <- rainbow(length(unique(datos$Tratamiento)))
#Procedo a crear los gráficos de dispersión.Para el primer gráfico de dispersión: Sequia vs Wildtype
plot(datos$Wildtype, datos$Sequia,
     col = colores_tratamientos[datos$Tratamiento],
     pch = 19,
     main = "Gráfico de Dispersión: Sequia vs Wildtype",
     xlab = "Wildtype",
     ylab = "Sequia")
# Para el segundo gráfico de dispersión: ExcesoRiego vs Wildtype
plot(datos$Wildtype, datos$ExcesoRiego,# Primer gráfico de dispersión: Sequia vs Wildtype
plot(datos$Wildtype, datos$Sequia,
     col = colores_tratamientos[datos$Tratamiento],
     pch = 19,
     main = "Gráfico de Dispersión: Sequia vs Wildtype",
     xlab = "Wildtype",
     ylab = "Sequia"
# Segundo gráfico de dispersión: Exceso Riego vs Wildtype
plot(datos$Wildtype, datos$ExcesoRiego,
     col = colores_tratamientos[datos$Tratamiento],
     pch = 19,
     main = "Gráfico de Dispersión: ExcesoRiego vs Wildtype",
     xlab = "Wildtype",
     ylab = "Exceso Riego")


#4.Ponle leyenda al gráfico del apartado anterior.En el margen inferior derecho.Pista:investiga sobre legend().
legend("bottomright", 
       legend = levels(datos$Tratamiento), # Etiquetas de los tratamientos
       col = colores_tratamientos,         # Colores
       pch = 19,                           # Forma de los puntos
       title = "Tratamientos")


#5.Haz un histograma para cada variable. Recuerda mantener los colores.
#Para crear un histograma para variable Wildtype
hist(datos$Wildtype,
     main = "Histograma de Wildtype",
     xlab = "Wildtype",
     col = color_wildtype)
#Para crear un histograma para la variable Sequia
hist(datos$Sequia,
     main = "Histograma de Sequia",
     xlab = "Sequia",
     col = color_sequia)
# Para crear un histograma para la variable ExcesoRiego
hist(datos$ExcesoRiego,
     main = "Histograma de Exceso Riego",
     xlab = "Exceso Riego",
     col = color_excesoriego)


#6.Haz un factor en la columna tratamiento y guardalo en una variable.
# Para crear un factor a partir de la columna 'Tratamiento' y guardarlo en una nueva variable
factor_tratamiento <- factor(datos$Tratamiento)
# Verificar el factor creado:
print(factor_tratamiento)
# Para verificar y mostrar los niveles del factor
levels(factor_tratamiento)
#Para verificar y mostrar un resumen del factor para ver la frecuencia de cada nivel
summary(factor_tratamiento)


#7.Calcula la media y la desviación estándar para cada tratamiento
# Para calcular la media de cada tratamiento para cada condición
media_wildtype <- aggregate(Wildtype ~ Tratamiento, data = datos, FUN = mean)
media_sequia <- aggregate(Sequia ~ Tratamiento, data = datos, FUN = mean)
media_excesoriego <- aggregate(ExcesoRiego ~ Tratamiento, data = datos, FUN = mean)
# Muestro los resultados
print("Media de Wildtype por tratamiento:")
print(media_wildtype)
print("Media de Sequia por tratamiento:")
print(media_sequia)
print("Media de ExcesoRiego por tratamiento:")
print(media_excesoriego)
# Para calcular la desviación estándar de cada tratamiento para cada condición
sd_wildtype <- aggregate(Wildtype ~ Tratamiento, data = datos, FUN = sd)
sd_sequia <- aggregate(Sequia ~ Tratamiento, data = datos, FUN = sd)
sd_excesoriego <- aggregate(ExcesoRiego ~ Tratamiento, data = datos, FUN = sd)
# Mostrar resultados
print("Desviación estándar de Wildtype por tratamiento:")
print(sd_wildtype)
print("Desviación estándar de Sequia por tratamiento:")
print(sd_sequia)
print("Desviación estándar de ExcesoRiego por tratamiento:")
print(sd_excesoriego)


#8.Averigua cuántos elementos tiene cada tratamiento. Recomendación: es más fácil si usas table() con el factor.
# Contar el número de elementos en cada tratamiento
conteo_tratamientos <- table(factor_tratamiento)
# Mostrar los resultados
print("Número de elementos por tratamiento:")
print(conteo_tratamientos)
#9.Extrae los datos para el tratamiento 1 y el tratamiento 4 y guárdalos cada uno en una variable diferente.
# Extraer los datos del tratamiento 1
datos_tratamiento1 <- subset(datos, Tratamiento == 1)
# Extraer los datos del tratamiento 4
datos_tratamiento4 <- subset(datos, Tratamiento == 4)
# Verificar los datos extraídos
print("Datos del Tratamiento 1:")
print(datos_tratamiento1)
print("Datos del Tratamiento 4:")
print(datos_tratamiento4)


#10.Queremos comprobar que hay diferencias significativas para el tratamiento1 y el tratamiento5 entre Wildtype y Sequia,y entre Wildtype y ExcesoRiego. Primero, necesitaríamos comprobar si los datos se distribuyen de forma normal.En función de los resultados de la prueba de normalidad,¿qué test usarías para cada comparativa?¿Puedes comparar también Sequia con ExcesoRiego en ambos tratamientos? **En general, asumimos que las muestras son independientes, pero ¿son sus varianzas iguales? Actúa de acuerdo con tus resultados.
#En primer lugar, evalúo la distribución normal de los tratamientos utilizando la prueba de Shapiro-Wilk
# Datos del tratamiento 1
wildtype_t1 <- datos$Wildtype[datos$Tratamiento == 1]
sequia_t1 <- datos$Sequia[datos$Tratamiento == 1]
excesoriego_t1 <- datos$ExcesoRiego[datos$Tratamiento == 1]
# Datos del tratamiento 5
wildtype_t5 <- datos$Wildtype[datos$Tratamiento == 5]
sequia_t5 <- datos$Sequia[datos$Tratamiento == 5]
excesoriego_t5 <- datos$ExcesoRiego[datos$Tratamiento == 5]
# Pruebas de normalidad para tratamiento 1
shapiro.test(wildtype_t1)
shapiro.test(sequia_t1)
shapiro.test(excesoriego_t1)
# Pruebas de normalidad para tratamiento 5
shapiro.test(wildtype_t5)
shapiro.test(sequia_t5)
shapiro.test(excesoriego_t5)
# Pruebas de normalidad
shapiro_wildtype_t1 <- shapiro.test(wildtype_t1)
shapiro_sequia_t1 <- shapiro.test(sequia_t1)
shapiro_excesoriego_t1 <- shapiro.test(excesoriego_t1)
shapiro_wildtype_t5 <- shapiro.test(wildtype_t5)
shapiro_sequia_t5 <- shapiro.test(sequia_t5)
shapiro_excesoriego_t5 <- shapiro.test(excesoriego_t5)
# Imprimir resultados
print("Prueba de normalidad para tratamiento 1:")
print(shapiro_wildtype_t1)
print(shapiro_sequia_t1)
print(shapiro_excesoriego_t1)
print("Prueba de normalidad para tratamiento 5:")
print(shapiro_wildtype_t5)
print(shapiro_sequia_t5)
print(shapiro_excesoriego_t5)
# Prueba de varianzas para tratamiento 1: Wildtype vs Sequia
var.test(wildtype_t1, sequia_t1)
# Prueba de varianzas para tratamiento 1: Wildtype vs ExcesoRiego
var.test(wildtype_t1, excesoriego_t1)
# Prueba de varianzas para tratamiento 5: Wildtype vs Sequia
var.test(wildtype_t5, sequia_t5)
# Prueba de varianzas para tratamiento 5: Wildtype vs ExcesoRiego
var.test(wildtype_t5, excesoriego_t5)
#Como en todas pruebas, el valor p <0,05, las varianzas son significativamente diferentes, con lo cual en vez de usar una prueba t test en el que se asume la igualdad entre las varianzas, realizo una prueba t de Welch que no asume la igualdad de las mismas 
# Comparación entre Sequia y ExcesoRiego para tratamiento 1
t.test(sequia_t1, excesoriego_t1, var.equal = TRUE)  # Si son normales
# Comparación entre Sequia y ExcesoRiego para tratamiento 5
t.test(sequia_t5, excesoriego_t5, var.equal = TRUE)  # Si son normales


#11.1Realiza un ANOVA para comparar el tratamiento 1 en las tres condiciones. Pista: primero separa los valores detratamiento1 en Wildtype, Sequia y ExcesoRiego en variables separadas. Luego fíjate en el archivo “datosanova.txt” y trata de colocar los datos de esa forma en una tabla. Por último, ejecuta el test
# Crear un marco de datos en formato largo
datos_anova <- data.frame(
  valor = c(wildtype_t1, sequia_t1, excesoriego_t1),
  grupo = rep(c("Wildtype", "Sequia", "ExcesoRiego"), each = length(wildtype_t1))
)
# Realizar el ANOVA
anova_result <- aov(valor ~ grupo, data = datos_anova)
# Extraer el resumen del ANOVA y convertirlo en tabla
anova_table <- as.data.frame(summary(anova_result)[[1]])
# Mostrar la tabla en la consola
print(anova_table)
# Realizar la prueba de Tukey
tukey_result <- TukeyHSD(anova_result)
# Mostrar los resultados
print(tukey_result)


