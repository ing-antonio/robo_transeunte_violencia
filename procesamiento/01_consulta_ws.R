require(pacman)
p_load(
  here,           # Para manejo de rutas relativas del proyecto
  DBI,            # Para conexiones a bases de datos (dbConnect, dbGetQuery, dbSendQuery)
  RPostgreSQL,    # Driver para conexiones a PostgreSQL
  lubridate,      # Para manejo de fechas (today)
  yaml            # Para leer archivos de configuración YAML (read_yaml)
)


here::i_am("procesamiento/01_consulta_ws.R") 

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



consulta_ws <- dbGetQuery(conn, paste0(
  paste0("SELECT \"CTHecho\", 
  delito, 
  categoria_delito, 
  \"idCI\", 
  latitud, 
  longitud,
  colonia_hecho, 
  DATE(fecha_inicio) fecha_inicio, 
  alcaldia_hecho_fuente, 
  alcaldia_hecho, 
  nombre_sec, 
  \"Nomenclatu\"
  FROM dashboard_seguridad.carpetas_directas_cc_cdmx
  WHERE DATE(fecha_inicio) >= '2023-01-01'")))

corte <- today()-1

save(consulta_ws, file =  
       here( 'datos/datos_originales',     
             paste('consulta_web_servies_',corte,'.RData', sep='')))

dbDisconnect(conn)