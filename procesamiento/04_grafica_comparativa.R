pacman::p_load(RPostgreSQL,tidyverse,lubridate,yaml, gt, readxl, reactable, htmlwidgets, 
               tools, svglite, tidyquant, scales, ggrepel, ggthemes, webshot2, officer, 
               flextable, webshot, dplyr, sf, showtext, sysfonts, treemapify, paletteer,
               gt,stringi,treemapify,grid, gridExtra, grid, cowplot, ggplot2, ggtext,scales, patchwork,
               janitor,here)



font_add_google(c("Bebas Neue","Poppins", "Montserrat","Overpass","Roboto", "Lexend", "Manrope","Cabin"))
showtext_auto()

corte <- today() - 1


# 4. Cargamos los Rdata
nom_ci <- paste('consulta_web_servies_',corte,'.RData', sep = '')
load(here ( 'datos', 'datos_originales',   nom_ci) )

source(here("auxiliares/funciones.R"))

#6.- # 4. Definir las fechas para los tres periodos anuales
dia_hoy <- wday(Sys.Date()) 
fecha_actual <- Sys.Date() - 1
if (dia_hoy == 2) {
  fecha_actual <- Sys.Date() - 1
  semana_actual <- isoweek(fecha_actual)
  semana_anterior <- isoweek(fecha_actual - 7)  # Una semana antes
  semana_pasada <- isoweek(fecha_actual - 14)  # Dos semanas antes
} else {
  semana_actual <- isoweek(fecha_actual)  # Semana ISO actual
  semana_anterior <- isoweek(fecha_actual - 7)  # Semana ISO de hace 7 días
  semana_pasada <- isoweek(fecha_actual - 14)  # Semana ISO de hace 14 días
}

# 4. Definir las fechas para los tres periodos anuales
anio_pasado <- year(Sys.Date())-2
anio_anterior <- year(Sys.Date())-1
anio_actual <- year(Sys.Date())

dia <- day(Sys.Date()-1)
mes <- month(Sys.Date()-1)
dias_menos<-7
fecha_corte <- today()-1
fecha_aux<-fecha_corte-days(dias_menos)+1


#######################################

###  Sección para las gráficas.    ####

#######################################

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

alcaldia <- dbGetQuery(
  conn, paste0(
    "SELECT
           DATE(fecha_inicio) AS fecha_inicio, 
           alcaldia_hecho
     FROM dashboard_seguridad.carpetas_directas_cc_cdmx
     WHERE 
       DATE(fecha_inicio) BETWEEN '2023-01-01' AND '", Sys.Date(), "'
       AND categoria_delito != 'Delito de bajo impacto'"
  )
) %>% as_tibble() %>% 
  clean_names()

alcaldia <- consulta_ws %>% 
  filter(categoria_delito != 'Delito de bajo impacto', fecha_inicio >= '2023-01-01') %>% 
  select(alcaldia_hecho, fecha_inicio)



# 5. Crear un dataframe con los tres periodos semanales
df_semanal_alcaldia <- alcaldia %>%
  mutate(anio = year(fecha_inicio),
         mes = month(fecha_inicio)) %>% 
  filter(fecha_inicio >= '2024-12-30') %>% 
  mutate(
    numero_semana = isoweek(fecha_inicio),
    periodo = case_when(
      numero_semana == semana_anterior ~ "Anterior",
      numero_semana == semana_actual ~ "Actual"
    )
  ) %>% 
  filter(!is.na(periodo)) %>%
  filter(fecha_inicio >= '2024-12-30')

prueba <- df_semanal_alcaldia %>%
  filter(periodo == 'Actual') %>% 
  agrupar_datos(c("alcaldia_hecho"))


summary(df_semanal_alcaldia$fecha_inicio)

# 6. Crear un dataframe con los tres periodos anuales
df_anual_alcaldia <- alcaldia %>%
  mutate(periodo = case_when(
    fecha_inicio >= '2024-01-01' & fecha_inicio <= as.Date(paste0(anio_anterior,"-",mes,"-",dia)) ~ "Anterior",
    fecha_inicio >= '2025-01-01' & fecha_inicio <= as.Date(paste0(anio_actual,"-",mes,"-",dia)) ~ "Actual"
  )) %>% 
  filter(!is.na(periodo)) %>% 
  mutate(anio = year(fecha_inicio))


#Geberamos el gráfico semanal y anual

grafico_semanal <- grafico_comparada(df_semanal_alcaldia, "semanal")
grafico_anual <- grafico_comparada(df_anual_alcaldia, "anual")

# Combinar los gráficos horizontalmente
grafico_combinado <- grafico_semanal | grafico_anual

# Guardar el gráfico combinado
ggsave(here("salidas/imagenes","grafica_comparativa.png"), plot = grafico_combinado, width = 16, height = 8)

