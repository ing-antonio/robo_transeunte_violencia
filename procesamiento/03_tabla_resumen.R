pacman::p_load(
  RPostgreSQL, dplyr, ggplot2, lubridate, yaml, flextable,
  webshot2, showtext, sysfonts, here, reticulate
)

corte <- today() - 1

here::i_am("procesamiento/03_tabla_resumen.R") 

# 4. Cargamos los Rdata
nom_ci <- paste('consulta_web_servies_',corte,'.RData', sep = '')
load(here ( 'datos', 'datos_originales',   nom_ci) )
nom_vic <- paste('consulta_victimas_', corte, '.RData', sep = '')
load(here ( 'datos', 'datos_originales', nom_vic))

source(here("auxiliares/funciones.R"))

delitos_filtrados <- consulta_ws
victimas_filtradas <- df_vic
anual <- delitos_filtrados %>% 
  filter(fecha_inicio >= '2022-01-01', categoria_delito != 'Delito de bajo impacto')

# 2. Convertir 'fecha_inicio' a formato de fecha (YYYY-MM-DD)
anual$fecha_inicio <- as.Date(anual$fecha_inicio)
anual <- anual %>% 
  filter(fecha_inicio <= Sys.Date() - 1)

# 3. Reemplazar valores nulos, vacíos o NA en 'alcaldia_hecho' por "Sin Alcaldía"
#anual$alcaldia_hecho[anual$alcaldia_hecho %in% c(NA, "", "NA", "nulo")] <- "Sin Alcaldía"

# 4. Definir las fechas para los tres periodos anuales
# Definir las fechas para los tres periodos anuales
fecha_actual <- Sys.Date() - 1
anio_pasado <- year(fecha_actual) - 2
anio_anterior <- year(fecha_actual) - 1
anio_actual <- year(fecha_actual)

# Asegurar que mes y día tengan dos dígitos
mes <- sprintf("%02d", month(fecha_actual))
dia <- sprintf("%02d", day(fecha_actual))

# Imprimir fechas para depuración
print(paste0(anio_pasado, "-", mes, "-", dia))
print(paste0(anio_anterior, "-", mes, "-", dia))
print(paste0(anio_actual, "-", mes, "-", dia))

# Asegurar que fecha_inicio es de tipo Date
anual <- anual %>% mutate(fecha_inicio = as.Date(fecha_inicio))

# Crear el dataframe con los tres periodos
df_anual <- anual %>%
  mutate(periodo = case_when(
    fecha_inicio <= as.Date(paste0(anio_pasado, "-", mes, "-", dia)) & fecha_inicio >= as.Date(paste0(anio_pasado, "-01-01")) ~ "Pasado",
    fecha_inicio <= as.Date(paste0(anio_anterior, "-", mes, "-", dia)) & fecha_inicio >= as.Date(paste0(anio_anterior, "-01-01")) ~ "Anterior",
    fecha_inicio <= as.Date(paste0(anio_actual, "-", mes, "-", dia)) & fecha_inicio >= as.Date(paste0(anio_actual, "-01-01")) ~ "Actual"
  )) %>%
  filter(!is.na(periodo)) %>%
  mutate(anio = year(fecha_inicio))


#prueba <- df_anual %>% 
#     filter(anio == 2024)

#Definimos los periodos semanales

#6.- # 4. Definir las fechas para los tres periodos anuales
dia_hoy <- wday(Sys.Date()) 
#print(dia_hoy)

# # Definir las fechas y semanas dependiendo del día de la semana
if (dia_hoy == 2) {
  fecha_actual <- Sys.Date() - 1
  semana_pasada <- isoweek(Sys.Date()) - 3
  semana_anterior <- isoweek(Sys.Date()) - 2
  semana_actual <- isoweek(Sys.Date()) - 1
} else {
  fecha_actual <- Sys.Date() - 1
  semana_pasada <- isoweek(Sys.Date()) - 2
  semana_anterior <- isoweek(Sys.Date()) - 1
  semana_actual <- isoweek(Sys.Date())
}

# if (dia_hoy == 2) {
#       fecha_actual <- Sys.Date() - 1
#       semana_actual <- isoweek(fecha_actual)
#       semana_anterior <- isoweek(fecha_actual - 7)  # Una semana antes
#       semana_pasada <- isoweek(fecha_actual - 14)  # Dos semanas antes
# } else {
#       semana_actual <- isoweek(fecha_actual)  # Semana ISO actual
#       semana_anterior <- isoweek(fecha_actual - 7)  # Semana ISO de hace 7 días
#       semana_pasada <- isoweek(fecha_actual - 14)  # Semana ISO de hace 14 días
# }





# 5. Crear un dataframe con los tres periodos semanales
df_semanal <- anual %>%
  filter(fecha_inicio >=  '2025-01-01') %>%  
  mutate(
    numero_semana = isoweek(fecha_inicio),
    periodo = case_when(
      numero_semana == semana_pasada ~ "Pasado",
      numero_semana == semana_anterior ~ "Anterior",
      numero_semana == semana_actual ~ "Actual"
    )
  ) %>% 
  filter(!is.na(periodo)) 


victimas <- victimas_filtradas %>% 
  filter(fechainicio >= '2024-11-25') %>% 
  dplyr::select(fecha_de_inicio = fechainicio, everything())


victimas <- victimas %>% 
  mutate(
    numero_semana = isoweek(fecha_de_inicio),
    periodo = case_when(
      numero_semana == semana_pasada ~ "Pasado",
      numero_semana == semana_anterior ~ "Anterior",
      numero_semana == semana_actual ~ "Actual"
    )
  ) %>% 
  filter(!is.na(periodo)) 




victimas_anual <- victimas_filtradas %>% 
  filter(fechainicio >= '2022-01-01') %>% 
  select(fecha_de_inicio = fechainicio, categoria_delito = cve_delito)



victimas_anual <- victimas_anual %>% 
  mutate(periodo = case_when(
    fecha_de_inicio >= as.Date(paste0(anio_pasado,"-01-01")) & fecha_de_inicio <= as.Date(paste0(anio_pasado,"-",mes,"-",dia)) ~ "Pasado",
    fecha_de_inicio >= as.Date(paste0(anio_anterior,"-01-01")) & fecha_de_inicio <= as.Date(paste0(anio_anterior,"-",mes,"-",dia)) ~ "Anterior",
    fecha_de_inicio >= as.Date(paste0(anio_actual,"-01-01")) & fecha_de_inicio <= as.Date(paste0(anio_actual,"-",mes,"-",dia)) ~ "Actual"
  )) %>% 
  filter(!is.na(periodo)) %>% 
  mutate(anio = year(fecha_de_inicio))


victimas_pasado <- victimas %>% filter(periodo == "Pasado")
victimas_anterior <- victimas %>% filter(periodo == "Anterior")
victimas_actual <- victimas %>% filter(periodo == "Actual")


# Agrupación y procesamiento
resultado <- df_semanal %>%
  filter(categoria_delito != 'Delito de bajo impacto') %>%
  procesar_datos(agrupaciones = c("categoria_delito", "periodo"), periodo = "periodo")

# Calcular la fila de totales
fila_total <- c("Alto Impacto", colSums(resultado[,-1], na.rm = TRUE))

# Agregar la fila de totales
resultado <- rbind(resultado, fila_total)


suma_robo_v <- resultado %>% 
  filter(categoria_delito %in% c('Robo de vehículo sin violencia', 'Robo de vehículo con violencia')) %>% 
  summarise(categoria_delito = 'Robo de vehículo con y sin violencia',
            Actual = sum(Actual %>% as.numeric()),
            Anterior = sum(Anterior %>%  as.numeric()),
            Pasado = sum(Pasado %>% as.numeric()))

resultado <- rbind(resultado, suma_robo_v) 




# Obtener variaciones
resultado <- resultado %>%
  mutate(across(-categoria_delito, as.numeric)) %>%
  add_row(
    categoria_delito = 'Homicidio doloso (víctimas)', # Crear una columna con el valor fijo
    Actual = nrow(victimas_actual),
    Anterior = nrow(victimas_anterior),
    Pasado = nrow(victimas_pasado)   
    
    
  ) %>% 
  mutate(
    variacion_pasado_anterior = variacion(Anterior, Pasado),
    variacion_anterior_actual = variacion(Actual, Anterior)
  ) %>%
  dplyr::select(categoria_delito,  Pasado, Anterior, Actual, variacion_pasado_anterior, variacion_anterior_actual) %>%
  mutate(lvl = case_when(
    categoria_delito == "Alto Impacto" ~ 1,
    categoria_delito == "Homicidio doloso" ~ 2,
    categoria_delito == "Homicidio doloso (víctimas)" ~ 3,
    categoria_delito == "Lesiones dolosas por disparo de arma de fuego" ~ 4,
    categoria_delito == "Robo a transeunte en vía pública con y sin violencia" ~ 5,
    categoria_delito == "Robo a transeunte en vía pública con violencia" ~ 6,
    categoria_delito == "Robo a transeunte en vía pública sin violencia" ~ 7,
    categoria_delito == "Robo de vehículo con y sin violencia" ~ 8,
    categoria_delito == "Robo de vehículo con violencia" ~ 9,
    categoria_delito == "Robo de vehículo sin violencia" ~ 10,
    categoria_delito == "Secuestro" ~ 11,
    categoria_delito == "Violación" ~ 12,
    categoria_delito == "Robo a negocio con violencia" ~ 13,
    categoria_delito == "Robo a pasajero a bordo del metro con y sin violencia" ~ 14,
    categoria_delito == "Robo a repartidor con y sin violencia" ~ 15,
    categoria_delito == "Robo a pasajero a bordo de microbus con y sin violencia" ~ 16,
    categoria_delito == "Robo a cuentahabiente saliendo del cajero con violencia" ~ 17,
    categoria_delito == "Robo a pasajero a bordo de taxi con violencia" ~ 18,
    categoria_delito == "Robo a casa habitación con violencia" ~ 19,
    categoria_delito == "Robo a transportista con y sin violencia" ~ 20,
    categoria_delito == "Robo a transeúnte en vía pública con y sin violencia" ~ 21,
    categoria_delito == "Robo a pasajero a bordo de microbús con y sin violencia" ~ 22,
    categoria_delito == "Delito de bajo impacto" ~ 23,
    TRUE ~ NA_real_
  )) %>%
  arrange(lvl, desc(Actual)) %>%
  dplyr::select(-lvl)


resultado <- resultado %>%
  mutate(
    across(c(Pasado, Anterior, Actual), ~ scales::comma(.))
  ) %>% dplyr::select(categoria_delito, Anterior, Actual, variacion_anterior_actual)


victimas_anual <- victimas_anual %>% 
  mutate(periodo = case_when(
    fecha_de_inicio >= as.Date(paste0(anio_pasado,"-01-01")) & fecha_de_inicio <= as.Date(paste0(anio_pasado,"-",mes,"-",dia)) ~ "Pasado",
    fecha_de_inicio >= as.Date(paste0(anio_anterior,"-01-01")) & fecha_de_inicio <= as.Date(paste0(anio_anterior,"-",mes,"-",dia)) ~ "Anterior",
    fecha_de_inicio >= as.Date(paste0(anio_actual,"-01-01")) & fecha_de_inicio <= as.Date(paste0(anio_actual,"-",mes,"-",dia)) ~ "Actual"
  )) %>% 
  filter(!is.na(periodo)) %>% 
  mutate(anio = year(fecha_de_inicio))


victimas_pasado <- victimas_anual %>% filter(periodo == "Pasado")
victimas_anterior <- victimas_anual %>% filter(periodo == "Anterior")
victimas_actual <- victimas_anual %>% filter(periodo == "Actual")

resultado_anual <- df_anual %>%
  filter(categoria_delito != 'Delito de bajo impacto') %>%
  procesar_datos(agrupaciones = c("categoria_delito", "periodo"), periodo = "periodo")

resultado_anual <- resultado_anual %>%
  mutate(
    Actual   = if_else(categoria_delito == "Secuestro", 8, Actual),
    Anterior = if_else(categoria_delito == "Secuestro", 10, Anterior),
    Pasado   = if_else(categoria_delito == "Secuestro", 16, Pasado)
  )

# Calcular la fila de totales
fila_total <- c("Alto Impacto", colSums(resultado_anual[,-1], na.rm = TRUE))

# Agregar la fila de totales
resultado_anual <- rbind(resultado_anual, fila_total)



suma_robo_v <- resultado_anual %>% 
  filter(categoria_delito %in% c('Robo de vehículo sin violencia', 'Robo de vehículo con violencia')) %>% 
  summarise(categoria_delito = 'Robo de vehículo con y sin violencia',
            Actual = sum(Actual %>% as.numeric()),
            Anterior = sum(Anterior %>%  as.numeric()),
            Pasado = sum(Pasado %>% as.numeric()))

resultado_anual <- rbind(resultado_anual, suma_robo_v) 



# Obtener variaciones
resultado_anual <- resultado_anual %>%
  mutate(across(-categoria_delito, as.numeric)) %>%
  add_row(
    categoria_delito = 'Homicidio doloso (víctimas)', # Crear una columna con el valor fijo
    Actual = nrow(victimas_actual),
    Anterior = nrow(victimas_anterior),
    Pasado = nrow(victimas_pasado)   
    
    
  ) %>% 
  mutate(
    variacion_pasado_actual = variacion(Actual, Pasado),
    variacion_pasado_anterior = variacion(Anterior, Pasado),
    variacion_anterior_actual = variacion(Actual, Anterior)
  ) %>%
  dplyr::select(categoria_delito, Pasado, Anterior, Actual, variacion_pasado_anterior, variacion_anterior_actual,variacion_pasado_actual ) %>%
  mutate(lvl = case_when(
    categoria_delito == "Alto Impacto" ~ 1,
    categoria_delito == "Homicidio doloso" ~ 2,
    categoria_delito == "Homicidio doloso (víctimas)" ~ 3,
    categoria_delito == "Lesiones dolosas por disparo de arma de fuego" ~ 4,
    categoria_delito == "Robo a transeunte en vía pública con y sin violencia" ~ 5,
    categoria_delito == "Robo a transeunte en vía pública con violencia" ~ 6,
    categoria_delito == "Robo a transeunte en vía pública sin violencia" ~ 7,
    categoria_delito == "Robo de vehículo con y sin violencia" ~ 8,
    categoria_delito == "Robo de vehículo con violencia" ~ 9,
    categoria_delito == "Robo de vehículo sin violencia" ~ 10,
    categoria_delito == "Secuestro" ~ 11,
    categoria_delito == "Violación" ~ 12,
    categoria_delito == "Robo a negocio con violencia" ~ 13,
    categoria_delito == "Robo a pasajero a bordo del metro con y sin violencia" ~ 14,
    categoria_delito == "Robo a repartidor con y sin violencia" ~ 15,
    categoria_delito == "Robo a pasajero a bordo de microbus con y sin violencia" ~ 16,
    categoria_delito == "Robo a cuentahabiente saliendo del cajero con violencia" ~ 17,
    categoria_delito == "Robo a pasajero a bordo de taxi con violencia" ~ 18,
    categoria_delito == "Robo a casa habitación con violencia" ~ 19,
    categoria_delito == "Robo a transportista con y sin violencia" ~ 20,
    categoria_delito == "Robo a transeúnte en vía pública con y sin violencia" ~ 21,
    categoria_delito == "Robo a pasajero a bordo de microbús con y sin violencia" ~ 22,
    categoria_delito == "Delito de bajo impacto" ~ 23,
    TRUE ~ NA_real_
  )) %>%
  arrange(lvl, desc(Actual)) %>%
  dplyr::select(-lvl)


resultado_anual <- resultado_anual %>%
  mutate(
    across(c(Pasado, Anterior, Actual), ~ scales::comma(.))
  ) %>% dplyr::select(categoria_delito, Pasado_2023 = Pasado, Anterior_2024 = Anterior, Actual_2025 = Actual, 
                      variacion_anterior_actual_anual = variacion_anterior_actual,
                      variacion_pasado_actual)



final_resultado <- merge(
  resultado, 
  resultado_anual, 
  by = "categoria_delito", 
  all = TRUE
) 

# Une los dataframes
final_resultado <- merge(
  resultado, 
  resultado_anual, 
  by = "categoria_delito", 
  all = TRUE
)

# Reemplaza NA con 0 en las columnas "Actual" y "Anterior"
final_resultado$Actual[is.na(final_resultado$Actual)] <- 0
final_resultado$Anterior[is.na(final_resultado$Anterior)] <- 0

# Reemplaza NA con "0%" en la columna "variacion_anterior_actual"
final_resultado$variacion_anterior_actual[is.na(final_resultado$variacion_anterior_actual)] <- 0 %>% as.numeric()


final_resultado <- final_resultado %>% 
  mutate(lvl = case_when(
    categoria_delito == "Alto Impacto" ~ 1,
    categoria_delito == "Homicidio doloso" ~ 2,
    categoria_delito == "Homicidio doloso (víctimas)" ~ 3,
    categoria_delito == "Lesiones dolosas por disparo de arma de fuego" ~ 4,
    categoria_delito == "Robo a transeunte en vía pública con y sin violencia" ~ 5,
    categoria_delito == "Robo a transeunte en vía pública con violencia" ~ 6,
    categoria_delito == "Robo a transeunte en vía pública sin violencia" ~ 7,
    categoria_delito == "Robo de vehículo con y sin violencia" ~ 8,
    categoria_delito == "Robo de vehículo con violencia" ~ 9,
    categoria_delito == "Robo de vehículo sin violencia" ~ 10,
    categoria_delito == "Secuestro" ~ 11,
    categoria_delito == "Violación" ~ 12,
    categoria_delito == "Robo a negocio con violencia" ~ 13,
    categoria_delito == "Robo a pasajero a bordo del metro con y sin violencia" ~ 14,
    categoria_delito == "Robo a repartidor con y sin violencia" ~ 15,
    categoria_delito == "Robo a pasajero a bordo de microbus con y sin violencia" ~ 16,
    categoria_delito == "Robo a cuentahabiente saliendo del cajero con violencia" ~ 17,
    categoria_delito == "Robo a pasajero a bordo de taxi con violencia" ~ 18,
    categoria_delito == "Robo a casa habitación con violencia" ~ 19,
    categoria_delito == "Robo a transportista con y sin violencia" ~ 20,
    categoria_delito == "Robo a transeúnte en vía pública con y sin violencia" ~ 21,
    categoria_delito == "Robo a pasajero a bordo de microbús con y sin violencia" ~ 22,
    categoria_delito == "Delito de bajo impacto" ~ 23,
    TRUE ~ NA_real_
  )) %>%
  arrange(lvl, desc(Actual)) %>%
  dplyr::select(-lvl) %>% 
  mutate(variacion_anterior_actual = variacion_anterior_actual %>% as.numeric(),
         variacion_anterior_actual_anual = variacion_anterior_actual_anual %>% as.numeric(),
         variacion_pasado_actual = variacion_pasado_actual %>% as.numeric()
  )


# Asumiendo que ya tienes tu dataframe final_resultado
# Cargar los paquetes necesarios
#library(flextable)
#library(magrittr)  # Para el operador %>%
#library(dplyr)     # Por si acaso también lo necesitas para manipulación de datos

# Verificar si el paquete officer está disponible
if (!requireNamespace("officer", quietly = TRUE)) {
  install.packages("officer")
}

library(officer)

ft <- flextable(final_resultado) %>%
  # Establecer las etiquetas del encabezado
  set_header_labels(
    categoria_delito = "Categoría Delito",
    Anterior = paste0("Semana ", " ", semana_anterior),
    Actual = paste0("Semana ", " ", semana_actual),
    variacion_anterior_actual = paste0("Var ", semana_anterior, " vs ", semana_actual),
    Actual_2025 = paste0("Año ", " ", anio_actual),
    Anterior_2024 = paste0("Año ", " ", anio_anterior),
    Pasado_2023 = paste0("Año ", " ", anio_pasado),
    variacion_anterior_actual_anual = paste0("Var ", anio_anterior, " vs ", anio_actual),
    variacion_pasado_actual = paste0("Var ", anio_pasado, " vs ", anio_actual)
  ) %>%
  # Dar formato al encabezado (fondo burdeos #9f2241 y texto blanco)
  bg(part = "header", bg = "#9f2241") %>%
  color(part = "header", color = "white") %>%
  
  # Para variacion_anterior_actual (semanal)
  bg(j = "variacion_anterior_actual", i = ~ variacion_anterior_actual > 0, bg = "#FCDADE") %>%
  bg(j = "variacion_anterior_actual", i = ~ variacion_anterior_actual < 0, bg = "#E7FBF1") %>%
  bg(j = "variacion_anterior_actual", i = ~ variacion_anterior_actual == 0, bg = "#DDDDDD") %>%
  
  # Para variacion_anterior_actual_anual (anual)
  bg(j = "variacion_anterior_actual_anual", i = ~ variacion_anterior_actual_anual > 0, bg = "#FCDADE") %>%
  bg(j = "variacion_anterior_actual_anual", i = ~ variacion_anterior_actual_anual < 0, bg = "#E7FBF1") %>%
  bg(j = "variacion_anterior_actual_anual", i = ~ variacion_anterior_actual_anual == 0, bg = "#DDDDDD") %>%
  
  # Para variacion_anterior_actual_anual (anual
  bg(j = "variacion_pasado_actual", i = ~ variacion_pasado_actual > 0, bg = "#FCDADE") %>%
  bg(j = "variacion_pasado_actual", i = ~ variacion_pasado_actual < 0, bg = "#E7FBF1") %>%
  bg(j = "variacion_pasado_actual", i = ~ variacion_pasado_actual == 0, bg = "#DDDDDD") %>%
  
  set_formatter(values = list(
    variacion_pasado_anterior = function(x) {
      ifelse(x == 0 | abs(x) < 0.005, "0.00%", sprintf("%.2f%%", x))
    },
    variacion_anterior_actual = function(x) {
      ifelse(x == 0 | abs(x) < 0.005, "0.00%", sprintf("%.2f%%", x))
    },
    variacion_anterior_actual_anual = function(x) {
      ifelse(x == 0 | abs(x) < 0.005, "0.00%", sprintf("%.2f%%", x))
    },
    variacion_pasado_actual = function(x) {
      ifelse(x == 0 | abs(x) < 0.005, "0.00%", sprintf("%.2f%%", x))
    }
    
    
  )) %>%
  
  color(j = "variacion_anterior_actual", color = "#027A35", 
        i = ~ variacion_anterior_actual < 0) %>%
  color(j = "variacion_anterior_actual", color = "#E5074C", 
        i = ~ variacion_anterior_actual > 0) %>% 
  color(j = "variacion_anterior_actual", color = "#252627", 
        i = ~ variacion_anterior_actual == 0) %>% 
  
  color(j = "variacion_anterior_actual_anual", color = "#E5074C", 
        i = ~ variacion_anterior_actual_anual > 0) %>% 
  color(j = "variacion_anterior_actual_anual", color = "#027A35", 
        i = ~ variacion_anterior_actual_anual < 0) %>% 
  color(j = "variacion_anterior_actual_anual", color = "#252627", 
        i = ~ variacion_anterior_actual_anual == 0) %>% 
  
  color(j = "variacion_pasado_actual", color = "#E5074C", 
        i = ~ variacion_pasado_actual > 0) %>% 
  color(j = "variacion_pasado_actual", color = "#027A35", 
        i = ~ variacion_pasado_actual < 0) %>% 
  color(j = "variacion_pasado_actual", color = "#252627", 
        i = ~ variacion_pasado_actual == 0) %>% 
  
  # Dar un formato más profesional a la tabla
  theme_vanilla() %>%
  autofit() %>%
  
  # Aplicar alineación al final después del tema
  align(j = -1, align = "center", part = "body") %>%
  align(align = "center", part = "header") %>%
  bold(part = "header") %>%
  align(j = "categoria_delito", align = "left", part = "body") %>%
  
  # Agregar sangría al registro de Homicidio doloso al final
  padding(j = "categoria_delito", i = ~ categoria_delito == "Homicidio doloso (víctimas)", padding.left = 20) %>%
  
  # Agregar fondo a las celdas específicas (con color más claro para simular transparencia)
  bg(j = c("Anterior_2024", "Actual_2025", "variacion_anterior_actual_anual"), 
     i = ~ variacion_anterior_actual_anual > 0, 
     bg = "#E4CEB8") %>% 
  bg(j = c("Anterior", "Actual", "variacion_anterior_actual"), 
     i = ~ variacion_anterior_actual > 0, 
     bg = "#E4CEB8") %>% 
  bg(j = c("Pasado_2023", "Actual_2025", "variacion_pasado_actual"), 
     i = ~ variacion_pasado_actual > 0, 
     bg = "#E4CEB8")

ft

guardar_flextable_png <- function(ft, nombre_archivo, ancho, alto, zoom = 2) {
  
  # Nombres de archivos
  html_temp <- paste0(tools::file_path_sans_ext(nombre_archivo), ".html")
  png_final <- nombre_archivo
  
  # Guardar como HTML
  save_as_html(ft, path = html_temp)
  
  # Capturar screenshot
  webshot2::webshot(url = html_temp,
                    file = png_final,
                    vwidth = ancho,
                    vheight = alto,
                    zoom = zoom,
                    delay = 1)
  
  # Opcional: eliminar HTML temporal
  # file.remove(html_temp)
  
  cat("Tabla guardada como:", png_final, "\n")
}

# Usar la función
guardar_flextable_png(ft, here("salidas/imagenes/tabla_federal.png"), ancho = 1300, alto = 1000, zoom = 2)


