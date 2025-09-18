#Carga de librerías necesarias

pacman::p_load(
  dplyr,        # Manipulación de datos (operador %>% para pipes)
  lubridate,    # Manejo de fechas (today para obtener la fecha actual)
  janitor,      # Limpieza de datos (as_tibble para convertir a tibble)
  RPostgreSQL,  # Conexión a bases de datos PostgreSQL (dbConnect, dbGetQuery, dbSendQuery)
  here,         # Manejo de rutas de archivos de forma portable
  yaml          # Lectura de archivos de configuración YAML (read_yaml)
)


### Definir las fechas clave
hoy <- today()
corte <- hoy -1
corte

## Cargar las funciones

source(here( 'auxiliares', "funciones.R"))


config_file <- here("auxiliares/config.yml")
config <- yaml::read_yaml(config_file)



# Conectar a la base de datos usando los valores del archivo de configuración
conn <- dbConnect(
  "PostgreSQL", 
  dbname = config$db_name, 
  host = config$db_host,
  port = config$db_port, 
  user = config$db_username, 
  password = config$db_password
)

dbSendQuery(conn, "SET client_encoding = 'UTF8';")


# Consulta 

#### Víctimas de Homicidio doloso

df <- dbGetQuery(
  conn, paste0(
    "SELECT *
    FROM dashboard_seguridad.victimas
    WHERE DATE(fechainicio) >= '2023-01-01'")) %>% 
  as_tibble()

# Verificar fechas correctas
summary(df$fechainicio)

## Guardar este archivo

df_vic <-  df

save(df_vic, file =   
       here(  'datos/datos_originales',
              paste('consulta_victimas_',corte,'.RData', sep='') ) )

dbDisconnect(conn)

