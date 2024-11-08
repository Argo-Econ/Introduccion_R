
# Inicio de programa ----

# Inicio de programa ------------------------------------------------------
## forma sencilla de instalación ----
install.packages("readxl") # Instalación de paquete Manejo de archivos Excel
install.packages("pacman") # pacakges manager

# Invocar en la sesión la libreria instalada ----
library(pacman)
pacman::p_load(readxl, openxlsx, janitor, glue, lubridate, lmtest, dplyr, ggplot2, ggridges, TSstudio, zoo
               , tidyr)

# Manejo de variables
nombre <- "MachineLearning"
edad <- 25
altura <- 1.75

# Creación de vectores ----
numeros <- c(1,2,3,5,6,8)
secuencias <- seq(from=1, to=100, 2)
secuencias_fechas <- seq.Date(from = as.Date("2001-01-01"),to = as.Date("2024-11-08"),by = "day")


# Lectura de archivos ----
## metodo 1 de lectura de achivo
Datos_ent <- read_excel(path = file.choose(),sheet = "Base_LR",range = "a3:r229",col_names = T)

# met 2 de lecrura
Data_in <- "Datos_entrada/"

Datos_ent <- read_excel(path = glue("{Data_in}Base_Intro.xlsx")
                        ,sheet = "Base_LR",range = "a3:r229",col_names = T)
head(Datos_ent)
tail(Datos_ent)
class(Datos_ent)
str(Datos_ent)
summary(Datos_ent)


# Manejo de datos ---------------------------------------------------------
## Selección de columnas ----
names(Datos_ent)
Col_filtro1 <- select(.data = Datos_ent, Fecha:M1, Repo )
Col_filtro2 <- Datos_ent %>% select(Fecha:M1, Repo) # misma función
Col_filtro2 <- Datos_ent |> select(Fecha:M1, Repo)  # misma función 
Col_filtro3 <- Datos_ent[ , 1:7,17 ]
Col_filtro3 <- Datos_ent[ , c(1:7,17)]
identical(Col_filtro1,Col_filtro3)
# manera facil de sacar el operador pipeline shift +ctrl+ m    |>

## Selección filas ----
fil_filtro1 <- filter(.data = Datos_ent, Fecha>="2023-01-01")
fil_filtro1 <- filter(.data = Datos_ent, Fecha>="2023-01-01" & Fecha<="2023-12-01" )
fil_filtro2 <- Datos_ent |> filter(Fecha>="2023-01-01" & Fecha<="2023-12-01")
fil_filtro3 <- Datos_ent[205:216 , ]


## PIvoteo de formato Wide (ancho ) a formato long (Derretido Melt o formato largo)
Datos_ent_melt1 <- pivot_longer(data = Datos_ent,cols = -Fecha, names_to = "Variable"
                                ,values_to = "Valor")
View(Datos_ent_melt1)
pivot_wider(data = Datos_ent_melt1,names_from = "Variable",values_from = "Valor" )

Datos_ent_melt2 <- Datos_ent |> pivot_longer(cols = -Fecha, names_to = "Variable"
                                             ,values_to = "Valor")
identical(Datos_ent_melt1,Datos_ent_melt2)

# Seleccionar REPO y ordenar (de manera descendente) por fechas en formato melt
Datos_ent_melt1_sort <- Datos_ent |> pivot_longer(cols = -Fecha, names_to = "Variable"
                                                  ,values_to = "Valor") |> 
                        filter(Variable== "Repo" ) |> arrange(desc(Fecha))



