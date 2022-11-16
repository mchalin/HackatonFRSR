# Cargamos las librerias necesarias
library(readxl)
library(ggplot2)
theme_set(
  theme_minimal() +
    theme(legend.position = "right")
)

# Cargamos los datos de la encuesta
datos <- read_excel('datos_encuesta_ciberseguridad.xlsx')

# Filtramos la base de datos para contener solo a quienes fueron victimas o 
# conocen a alguien que haya sido
datos_a_trabajar = subset(datos,datos$fuiste_victima == 'Si'| datos$conoce_alguien == 'Si')

# Convertimos las variables de la tabla para mejor acceso 
attach(datos_a_trabajar)

# Creamos la tabla de frecuencia de edades
library(fdth)
tabla <- fdt(edad, start = 14,end = 74, h = 7)
tabla

# Creamos un histograma con las edades
hist(edad, 
     main = "Histograma de frecuencia de edades",
     xlab = "Edades",
     ylab = "Frecuencia"
)
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
# Volvemos a mostrar el grafico para que superponga la imagen contra el grid
hist(edad, 
     
     add = TRUE, 
     col = "#0066CC"
     )

# Curva de densidad
hist(edad, 
     freq = FALSE, 
     main = "Curva densidad", 
     ylab = "Densidad",
     xlab = "Edad",
     col = "#0066CC")
lines(density(edad), lwd = 2, col = 'red')

# Histograma con curva 
hist(edad, main = "Histograma de densidad de edades",
     xlab = "Edades",
     ylab = "Densidad",
     prob = TRUE,
     col = "#0066cc"
    )
x <- seq(min(edad), max(edad), length = 40)
f <- dnorm(x, mean = mean(edad), sd = sd(edad))
lines(x, f, col = "red", lwd = 2)

# Grafico de barras por genero
genero_factor = factor(genero,)
plot(genero_factor, 
     main = "Gráfico de barras por género",
     xlab = "Género", 
     ylab = "Frecuencia",
     col = "#FF4500"
     )
# Grafico de barras por como fueron estafados
como_factor = factor(como,)
plot(como_factor, main = "Tipo de estafas",
     xlab = "Cómo fueron estafados?", 
     ylab = "Frecuencia",
     font.axis = 3, 
     cex.names = 0.5,
     col = "tomato2"
     )


