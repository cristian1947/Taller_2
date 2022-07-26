## TALLER 2.
## INTEGRANTES: Cristian Mendez, Lizeth Vasquez, Sebastian Osorio.

library(tidyverse)

# PUNTO 1: CARGAR Y EXPLORAR BASE DE DATOS. MOSTRAR LAS 10 PRIMERAS FILAS EN CONSOLA

data <- read_csv("C:/Users/Sebastian Osorio/Desktop/Universidad/2022-V/Clase_fundamentosR/DATA/all_csv sorted.csv")
head(data, 10)

# Punto 2: Cree un dataframe que muestre el # de datos faltantes en cada columna de la base de datos.

library(naniar)

dfna <- data.frame(miss_var_summary(data))
dfna

# Punto 3: Nueva columna con el cambio % del precio del internet entre 2020 y 2021. 

data$`Average price of 1GB (USD – at start of 2020)`= as.numeric(data$`Average price of 1GB (USD – at start of 2020)`)
data$`Average price of 1GB (USD  at the start of 2021)`= as.numeric(data$`Average price of 1GB (USD  at the start of 2021)`)
DATA <- na.omit(data)

DATA1 <-DATA %>%
  mutate(Cambio_porcentual_precio_Año.2020.2021 = `Average price of 1GB (USD  at the start of 2021)` -
           `Average price of 1GB (USD – at start of 2020)`/`Average price of 1GB (USD – at start of 2020)`)


DATA1 <-DATA %>%
  mutate(Cambio_porcentual_precio_Año.2020.2021 = `Average price of 1GB (USD  at the start of 2021)`- `Average price of 1GB (USD – at start of 2020)`/`Average price of 1GB (USD – at start of 2020)`)

DATA <- DATA1 %>%
  arrange(desc(Cambio_porcentual_precio_Año.2020.2021)) 

## Primeros 10 paises

Data1 <- DATA1 %>%
  arrange(desc(Cambio_porcentual_precio_Año.2020.2021)) %>%
  select(Country, Cambio_porcentual_precio_Año.2020.2021) %>%
  head(10)



# Punto 4: Encuentre la velocidad promedio de internet en megabits para cada region de la columna 'continental region'
# ¿CUal es la región con el internet más lento?

library(dplyr)
P4 <- data %>%
  select(`Continental region`, `Avg \n(Mbit/s)Ookla`) %>%
  group_by(`Continental region`) %>%
  summarise(promedio = mean(`Avg \n(Mbit/s)Ookla`, na.rm = TRUE)) %>%
  arrange(promedio)
P4


### Teniendo en cuenta la manipulación de la base de datos, nos encontramos con que la región de AFRICA SUBSAHARIANA 
#es la que tiene el internet más lento



## punto 5:

## Con ggplot2, elabore un gráfico de dispersión con las variables de velocidad promedio y porcentaje
#de usuarios de internet. ¿Se podría afirmar que hay una conexión entre las dos variables?

library(ggplot2)
# Encontrar el porcentaje de usuarios de internet
Net_Users <- data %>%
  mutate(Net_Users = `Internet users` / Population * 100)
ggplot(Net_Users, aes(x = Net_Users, y = `Avg \n(Mbit/s)Ookla`))+
  geom_point()


## No hay una tendencia totalmente marcada, sin embargo si se aprecia cierto comportamiento
## a incrementar la velocidad promedio siguiendo el aumento de los internautas, por ende si hay una conexion moderada


