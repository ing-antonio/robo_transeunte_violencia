pacman::p_load(RPostgreSQL,tidyverse,lubridate,yaml, gt, readxl, reactable, htmlwidgets, 
               tools, svglite, tidyquant, scales, ggrepel, ggthemes, webshot2, officer, 
               flextable, webshot, dplyr, sf, showtext, sysfonts, treemapify, paletteer,
               gt,stringi,treemapify,grid, gridExtra, grid, cowplot, ggplot2, ggtext,scales, 
               patchwork, magick, gtools, highcharter, htmlwidgets, janitor, sf,reticulate, selenium)

library(here)

font_add_google(c("Bebas Neue","Poppins", "Montserrat","Overpass","Roboto", "Lexend", "Manrope","Cabin"))
showtext_auto()

source(here("auxiliares","funciones.r"))

Sys.setlocale("LC_ALL", "es_ES.UTF-8")

fecha_inicio <- today()-14
fecha_corte <- today()-1


# 4. Cargamos los Rdata
nom_ci <- paste('consulta_web_servies_',fecha_corte,'.RData', sep = '')
load(here ( 'datos', 'datos_originales',   nom_ci) )

semanal <- consulta_ws %>% 
  filter(fecha_inicio >= fecha_inicio) %>% 
  filter(fecha_inicio <= fecha_corte) %>% 
  filter(categoria_delito != 'Delito de bajo impacto') %>% 
  select(alcaldia_hecho, fecha_inicio, categoria_delito)


semanal_cuadrante <- consulta_ws %>% 
  filter(fecha_inicio >= fecha_inicio) %>% 
  filter(fecha_inicio <= fecha_corte) %>% 
  filter(categoria_delito != 'Delito de bajo impacto') %>% 
  select(alcaldia_hecho, nombre_sec, Nomenclatu, fecha_inicio, categoria_delito, colonia_hecho) %>% 
  mutate(nombre_sec = if_else(nombre_sec == "" | is.na(nombre_sec), "Sin Sector", nombre_sec)) %>% 
  mutate(alcaldia_hecho = if_else(alcaldia_hecho == "" | is.na(alcaldia_hecho), "Sin Alcaldía", alcaldia_hecho))


semanal_sector <- consulta_ws %>% 
  filter(fecha_inicio >= fecha_inicio) %>% 
  filter(fecha_inicio <= fecha_corte) %>% 
  filter(categoria_delito != 'Delito de bajo impacto') %>% 
  select(alcaldia_hecho, nombre_sec, fecha_inicio, categoria_delito) %>% 
  mutate(nombre_sec = if_else(nombre_sec == "" | is.na(nombre_sec), "Sin Sector", nombre_sec)) %>% 
  mutate(alcaldia_hecho = if_else(alcaldia_hecho == "" | is.na(alcaldia_hecho), "Sin Alcaldía", alcaldia_hecho))



###Agregado para el dataframe de semanala_sector para añadir el c

sectores_1 <- st_read(here("capas","SSC_CDMX_SECTORES_2024.shp")) %>%
  st_transform(4326) %>% 
  clean_names()

clave_sec <- sectores_1 %>%
  select(nombre_sec, ct) %>%
  st_drop_geometry()

semanal_sector <- semanal_sector %>%
  left_join(clave_sec, by = "nombre_sec")

semanal_sector <- semanal_sector %>%
  mutate(nombre_sec = paste0(ct, " - ", nombre_sec))




#Definimos día
dia_hoy <- wday(Sys.Date()) 
#print(dia_hoy)

if (dia_hoy == 2) {
  fecha_actual <- Sys.Date() - 1
  semana_actual <- isoweek(fecha_actual-6)
  semana_anterior <- semana_actual - 1 # Una semana antes
  semana_pasada <- 59  # Dos semanas antes
} else {
  fecha_actual <- Sys.Date() - 1
  semana_actual <- isoweek(fecha_actual)  # Semana ISO actual
  semana_anterior <- isoweek(fecha_actual - 7)  # Semana ISO de hace 7 días
  semana_pasada <- isoweek(fecha_actual - 14)  # Semana ISO de hace 14 días
}



#Se obtiene la semana actual y pasada(respetando semanas cruzando años)
semana_actual <- as.Date(cut(Sys.Date()-1, "week"))
semana_pasada <- semana_actual - 7 

#Filtramos por las semanas actual y pasada
semanal_filtrado <- semanal %>% 
  filter(fecha_inicio >= semana_pasada & fecha_inicio < semana_actual + weeks(1)) %>%
  mutate(semana = ifelse(fecha_inicio < semana_actual, "Semana pasada", "Semana actual"))

tabla_alcaldia <- semanal_filtrado %>%
  group_by(alcaldia_hecho, categoria_delito, semana) %>%
  summarise(cantidad = n(), .groups = "drop") %>%
  mutate(alcaldia_hecho = ifelse(is.na(alcaldia_hecho), "Sin Alcaldía", alcaldia_hecho)) %>%
  arrange(desc(cantidad))

tabla_alcaldia

#calculo de totales
totales_alcaldia <- tabla_alcaldia %>%
  group_by(alcaldia_hecho, semana) %>%
  summarise(total = sum(cantidad, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  ungroup()

categorias_ordenadas <- totales_alcaldia %>%
  pull(alcaldia_hecho) %>%
  unique()

data_series <- totales_alcaldia %>%
  split(.$semana) %>%
  lapply(function(df) {
    list(
      name = as.character(df$semana[1]),
      data = lapply(1:nrow(df), function(i) {
        list(
          name = df$alcaldia_hecho[i],
          y = df$total[i],
          drilldown = paste(df$alcaldia_hecho[i], df$semana[i], sep = "-")
        )
      }),
      color = ifelse(df$semana[1] == "Semana pasada", "#bc955c", "#9f2241")
    )
  })

drilldown_series <- tabla_alcaldia %>%
  group_by(alcaldia_hecho, semana, categoria_delito) %>%
  summarise(cantidad = sum(cantidad, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(cantidad)) %>%
  group_split(alcaldia_hecho, semana)

drilldown_series <- lapply(drilldown_series, function(df) {
  alcaldia_semana <- unique(paste(df$alcaldia_hecho, df$semana, sep = "-"))
  list(
    id = alcaldia_semana,
    name = paste("Delitos en", alcaldia_semana),
    data = lapply(1:nrow(df), function(i) {
      list(
        name = df$categoria_delito[i],
        y = df$cantidad[i]
      )
    })
  )
})

#Crear el gráfico
grafico <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Delitos por Alcaldía (Semana pasada vs Semana actual)", style = list(fontSize = "24px")) %>%
  hc_xAxis(type = "category", labels = list(style = list(fontSize = "19px"))) %>%
  hc_yAxis(
    title = list(text = "Total de delitos",style = list(fontSize = "20px")),
    labels = list(style = list(fontSize = "20px")),
    allowDecimals = FALSE
  ) %>%
  hc_plotOptions(column = list(
    grouping = TRUE,
    dataLabels = list(enabled = TRUE, style = list(fontSize = "24px"))
  )) %>%
  hc_tooltip(
    useHTML = TRUE,
    style = list(fontSize = "19px"),
    pointFormat = "<b>{point.name}</b>: {point.y}"
  ) %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = drilldown_series
  )

for (serie in data_series) {
  grafico <- grafico %>% hc_add_series(
    name = serie$name,
    data = serie$data,
    color = serie$color
  )
}

grafico

# Guardar el gráfico como HTML
ruta_guardado <- here("salidas/imagenes","grafico_alcaldias.html")
ruta_salida <- here("salidas/imagenes","grafico_alcaldias.png")

# Guardar el gráfico
saveWidget(grafico, file = ruta_guardado, selfcontained = TRUE)




Sys.sleep(200)

py_run_string(paste0("
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
import time

# Ruta del archivo HTML
ruta_html = r'",ruta_guardado,"'

# Configurar el WebDriver (descarga automáticamente el driver de Chrome)
options = webdriver.ChromeOptions()
options.add_argument('--headless')  # Ejecutar en segundo plano
options.add_argument('--window-size=2000,1200')  # Ajustar tamaño de ventana

# Iniciar WebDriver
driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)

# Cargar el archivo HTML en el navegador
driver.get('file:///' + ruta_html.replace('\\\\', '/'))  # Convertir ruta de Windows a formato URL

# Esperar unos segundos para asegurar que cargue correctamente
time.sleep(2)

# Guardar la captura de pantalla
ruta_salida = r'",ruta_salida,"'
driver.save_screenshot(ruta_salida)

# Cerrar el navegador
driver.quit()

print(f'Captura guardada en: {ruta_salida}')
"))

#############################
#####             APARTIR DE AQUI TRABAJAR FUNCION 
#############################


#-------------------- INVOCA FUNCION TABLA DE SECTOR ----------------------------------------
fecha_inicio2 <- Sys.Date()-1
semana_actual <- as.Date(cut(Sys.Date()-1, "week"))

#Se filtran los datos para la semana actual
semanal_sector_actual <- semanal_sector %>%
  filter(fecha_inicio >= semana_actual & fecha_inicio2 < semana_actual + 7)



sectores <- read.csv(here("auxiliares","top_sectores - top_sectores.csv"))
prueba_1_1 <- semanal_sector_actual %>%
  left_join(sectores , by = c("nombre_sec" = "SECTORES"))

prueba1 <- prueba_1_1 %>%
  mutate(comparacion_alcaldias = ifelse(alcaldia_hecho == Alcaldia_Camello, TRUE, FALSE))

prueba2 <- prueba1 %>% 
  mutate(alcaldia_modificada = ifelse(is.na(nombre_sec) & Alcaldia_Camello == "Na", alcaldia_hecho, Alcaldia_Camello)) %>% 
  select(nombre_sec, categoria_delito, alcaldia_hecho)




tabla_sec <- treemap_tabla_sector(prueba2)
tabla_sec

tabla_sec <- tabla_sec %>%
  distinct()  #Elimina filas duplicadas
totales_sector <- tabla_sec %>%
  group_by(nombre_sec, alcaldia_hecho) %>%
  summarise(
    total = sum(cantidad, na.rm = TRUE), # Suma de la cantidad (valores únicos)
    delitos = paste0("<b>", unique(categoria_delito), ":</b> ", cantidad, collapse = "<br>") # Concatenar sin duplicar
  ) %>%
  arrange(desc(total)) %>%
  ungroup()

print(sum(totales_sector$total)) #Esto debería coincidir con la suma manual de `cantidad` en `semanal_sector_actual`

grafico <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Delitos por Sectores (Semana Actual)",style = list(fontSize = "24px")) %>%
  hc_xAxis(type = "category", labels = list(style = list(fontSize = "19px"))) %>%
  hc_yAxis(title = list(text = "Total de delitos",style = list(fontSize = "20px")), labels = list(style = list(fontSize = "20px"))) %>%
  hc_tooltip(
    useHTML = TRUE,
    style = list(fontSize = "19px"),
    headerFormat = '<span style="font-size: 19px; font-weight: bold;">{point.key}</span><br>',
    pointFormat = "<b>Alcaldía:</b> {point.alcaldia}<br>
                   <b>Sector:</b> {point.name}<br>
                   <b>Delitos:</b><br>{point.customTooltip}"
  ) %>%
  hc_plotOptions(series = list(
    borderWidth = 0,
    dataLabels = list(enabled = TRUE,style = list(fontSize = "24px"))
  )) %>%
  hc_add_series(
    name = "Sector",
    data = lapply(1:nrow(totales_sector), function(i) {
      list(
        name = totales_sector$nombre_sec[i],
        y = totales_sector$total[i],
        alcaldia = totales_sector$alcaldia_hecho[i],
        customTooltip = totales_sector$delitos[i]
      )
    }),
    color = "#B02858"
  )

print(grafico)
# Guardar el gráfico como HTML
ruta_guardado <- here("salidas/imagenes","grafico_sectores.html")
ruta_salida <- here("salidas/imagenes","grafico_sectores.png")

# Guardar el gráfico
saveWidget(grafico, file = ruta_guardado, selfcontained = TRUE)

Sys.sleep(10)

py_run_string(paste0("
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
import time

# Ruta del archivo HTML
ruta_html = r'",ruta_guardado,"'

# Configurar el WebDriver (descarga automáticamente el driver de Chrome)
options = webdriver.ChromeOptions()
options.add_argument('--headless')  # Ejecutar en segundo plano
options.add_argument('--window-size=2000,1200')  # Ajustar tamaño de ventana

# Iniciar WebDriver
driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)

# Cargar el archivo HTML en el navegador
driver.get('file:///' + ruta_html.replace('\\\\', '/'))  # Convertir ruta de Windows a formato URL

# Esperar unos segundos para asegurar que cargue correctamente
time.sleep(2)

# Guardar la captura de pantalla
ruta_salida = r'",ruta_salida,"'
driver.save_screenshot(ruta_salida)

# Cerrar el navegador
driver.quit()

print(f'Captura guardada en: {ruta_salida}')
"))

# Mensaje de confirmación en R
print("Proceso completado: captura de pantalla generada desde Python.")



#-------------------- INVOCA FUNCION TABLA DE SECTOR ----------------------------------------

#-------------------- INVOCA FUNCION TABLA DE CUADRANTE ----------------------------------------

semana_pasada <- isoweek(Sys.Date()-1) - 2
semana_anterior <- isoweek(Sys.Date()-1) -1
semana_actual <- isoweek(Sys.Date()-1) 
sectores <- read.csv(here("auxiliares","top_sectores - top_sectores.csv"))
prueba_1_1 <- semanal_cuadrante %>%
  left_join(sectores , by = c("nombre_sec" = "SECTORES"))

prueba1 <- prueba_1_1 %>%
  mutate(comparacion_alcaldias = ifelse(alcaldia_hecho == Alcaldia_Camello, TRUE, FALSE))

prueba2 <- prueba1 %>% 
  mutate(alcaldia_modificada = ifelse(is.na(nombre_sec) & Alcaldia_Camello == "Na", alcaldia_hecho, Alcaldia_Camello)) %>% 
  select(alcaldia_hecho = alcaldia_modificada, nombre_sec, Nomenclatu, fecha_inicio, categoria_delito, colonia_hecho)

tabla_cuadrante <- tabla_semanal_cuadrante(prueba2, semana_actual, semana_anterior) 


tabla_cuadrante <- tabla_cuadrante %>%
  mutate(nueva_columna = paste(Nomenclatu, colonia_hecho, sep = ", "))

tabla_cuadrante <- tabla_cuadrante %>%
  select(
    alcaldia_hecho, 
    nombre_sec, 
    nueva_columna, 
    categoria_delito, 
    total_delitos_anterior, 
    total_delitos_actual
  )

tabla_cuadrante <- tabla_cuadrante %>%
  filter(categoria_delito != "DELITOS")  # Excluir filas con "DELITOS"
grafico_data <- tabla_cuadrante %>%
  group_by(nueva_columna, alcaldia_hecho, nombre_sec) %>%
  summarise(
    total_pasado = sum(total_delitos_anterior, na.rm = TRUE),
    total_actual = sum(total_delitos_actual, na.rm = TRUE),
    delitos_pasado = paste0(
      "<b>", unique(categoria_delito), ":</b> ", 
      total_delitos_anterior[match(unique(categoria_delito), categoria_delito)], 
      collapse = "<br>"
    ),
    delitos_actual = paste0(
      "<b>", unique(categoria_delito), ":</b> ", 
      total_delitos_actual[match(unique(categoria_delito), categoria_delito)], 
      collapse = "<br>"
    )
  ) %>%
  ungroup() %>%
  arrange(desc(total_actual))  # Ordenar de mayor a menor según el total_actual

grafico <- highchart() %>%
  hc_chart(
    type = "column",
    zoomType = "x"  # Habilitar zoom en el eje X
  ) %>%
  #Se cambia el titulo y su tamaño de la fuente. Mas abajo se puede cambiar los ejes x e y.
  hc_title(text = "Total de Delitos por Cuadrante",style = list(fontSize = "24px")) %>%
  hc_xAxis(
    categories = grafico_data$nueva_columna,  # Categorías (cuadrantes)
    title = list(text = "Cuadrantes"), labels = list(style = list(fontSize = "19px"))
  ) %>%
  hc_yAxis(
    title = list(text = "Total de Delitos", style = list(fontSize = "21px")), labels = list(style = list(fontSize = "20px"))
  ) %>%
  hc_tooltip(
    useHTML = TRUE,
    style = list(fontSize = "19px"),
    headerFormat = '<span style="font-size: 19px; font-weight: bold;">{point.key}</span><br>',
    pointFormat = "<b>Alcaldía:</b> {point.alcaldia}<br>
                           <b>Sector:</b> {point.sector}<br>
                           <b>Delitos:</b><br>{point.customTooltip}"
  ) %>%
  hc_add_series(
    name = "Periodo Pasado",
    data = lapply(1:nrow(grafico_data), function(i) {
      list(
        name = grafico_data$nueva_columna[i],  # Nombre del cuadrante
        y = grafico_data$total_pasado[i],      # Total de delitos en el periodo pasado
        alcaldia = grafico_data$alcaldia_hecho[i],  # Alcaldía
        sector = grafico_data$nombre_sec[i],      # Sector
        customTooltip = grafico_data$delitos_pasado[i]  # Tooltip del Periodo Pasado
      )
    }),
    color = "#bc955c"  # Color para Periodo Pasado
  ) %>%
  hc_add_series(
    name = "Periodo Actual",
    data = lapply(1:nrow(grafico_data), function(i) {
      list(
        name = grafico_data$nueva_columna[i],  # Nombre del cuadrante
        y = grafico_data$total_actual[i],       # Total de delitos en el periodo actual
        alcaldia = grafico_data$alcaldia_hecho[i],   # Alcaldía
        sector = grafico_data$nombre_sec[i],       # Sector
        customTooltip = grafico_data$delitos_actual[i]  # Tooltip del Periodo Actual
      )
    }),
    color = "#9f2241"  # Color para Periodo Actual
  ) %>%
  hc_plotOptions(
    column = list(
      dataLabels = list(enabled = TRUE,style = list(fontSize = "24px"))  # Mostrar etiquetas con valores
    )
  )

grafico

# Guardar el gráfico como HTML
ruta_guardado <- here("salidas/imagenes","grafico_cuadrantes.html")

ruta_salida <- here("salidas/imagenes","grafico_cuadrantes.png")

# Guardar el gráfico
saveWidget(grafico, file = ruta_guardado, selfcontained = TRUE)

Sys.sleep(10)

py_run_string(paste0("
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
import time

# Ruta del archivo HTML
ruta_html = r'",ruta_guardado,"'

# Configurar el WebDriver (descarga automáticamente el driver de Chrome)
options = webdriver.ChromeOptions()
options.add_argument('--headless')  # Ejecutar en segundo plano
options.add_argument('--window-size=2000,1200')  # Ajustar tamaño de ventana

# Iniciar WebDriver
driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)

# Cargar el archivo HTML en el navegador
driver.get('file:///' + ruta_html.replace('\\\\', '/'))  # Convertir ruta de Windows a formato URL

# Esperar unos segundos para asegurar que cargue correctamente
time.sleep(2)

# Guardar la captura de pantalla
ruta_salida = r'",ruta_salida,"'
driver.save_screenshot(ruta_salida)

# Cerrar el navegador
driver.quit()

print(f'Captura guardada en: {ruta_salida}')
"))
