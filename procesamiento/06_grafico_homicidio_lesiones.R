#Gráfico que muestra los homicidios dolosos y las lesiones dolosas por arma de fuego, en el periodo actual y el pasado
pacman::p_load(RPostgreSQL,tidyverse,lubridate,yaml, gt, readxl, reactable, htmlwidgets, 
               tools, svglite, tidyquant, scales, ggrepel, ggthemes, webshot2, officer, 
               flextable, webshot, dplyr, sf, showtext, sysfonts, treemapify, paletteer,gt,stringi,treemapify,grid, gridExtra, grid, cowplot, ggplot2, ggtext, highcharter)
library(here)
font_add_google(c("Bebas Neue","Poppins", "Montserrat","Overpass","Roboto", "Lexend", "Manrope","Cabin"))
showtext_auto()
#source("G:/.shortcut-targets-by-id/1o3RW9XtzyaaBxBvSYFPTHgER_wJ6O_VR/Analisis/Funciones/letras.R")
source(here("auxiliares","funciones.r")) 

#eliminar_archivos("G:/.shortcut-targets-by-id/1o3RW9XtzyaaBxBvSYFPTHgER_wJ6O_VR/Analisis/Gabinete de Seguridad/20241203_incidencia_delictiva_semanal/03_output/tabla_variacion_por_homicidio_lesiones")

Sys.setlocale("LC_ALL", "es_ES.UTF-8")


fecha_corte <- today()-1

nom_ci <- paste('consulta_web_servies_',fecha_corte,'.RData', sep = '')
load(here ( 'datos', 'datos_originales',   nom_ci) )

semanal <- consulta_ws %>% 
  filter(fecha_inicio >= '2024-12-21') %>% 
  select(fecha_inicio, alcaldia_hecho, nombre_sec, colonia_hecho, Nomenclatu, categoria_delito)


# 3. Reemplazar valores nulos, vacíos o NA en 'alcaldia_hecho' por "Sin Alcaldía"
semanal$alcaldia_hecho[semanal$alcaldia_hecho %in% c(NA, "", "NA", "nulo")] <- "Sin Alcaldía"

dia_hoy <- wday(Sys.Date()) 

dia_hoy <- wday(Sys.Date()) 

if (dia_hoy == 2) {
  fecha_actual <- Sys.Date() - 1
  semana_actual <- isoweek(fecha_actual)
  semana_anterior <- isoweek(fecha_actual - 7)  # Una semana antes
  semana_pasada <- isoweek(fecha_actual - 14)  # Dos semanas antes
} else {
  fecha_actual <- Sys.Date() - 1
  semana_actual <- isoweek(fecha_actual)  # Semana ISO actual
  semana_anterior <- isoweek(fecha_actual - 7)  # Semana ISO de hace 7 días
  semana_pasada <- isoweek(fecha_actual - 14)  # Semana ISO de hace 14 días
}


#Aqui vamos a crear un dataframe con los tres periodos semanales
df_semanal <- semanal %>% 
  mutate(
    numero_semana = isoweek(fecha_inicio),
    periodo = case_when(
      numero_semana == semana_anterior ~ "Anterior",
      numero_semana == semana_actual ~ "Actual"
    )
  ) %>% 
  filter(!is.na(periodo)) 


agrupado <- df_semanal %>% 
  filter(categoria_delito == 'Robo a transeúnte en vía pública con violencia') %>% 
  procesar_datos(agrupaciones = c("alcaldia_hecho", "colonia_hecho", "nombre_sec", "Nomenclatu", "categoria_delito", "periodo"), 
                 periodo = c("categoria_delito")) %>% 
  pivotear_por_delito(periodo = "periodo", columnas = c("Robo a transeúnte en vía pública con violencia"))



#Genera Grafica particionada. Se limpian los datos
#datos <- tabla_desglosada_final(agrupado)

datos <- datos %>% slice(-1)

datos <- datos %>%
  filter(Nomenclatu != "TOTALES")

datos <- datos[, -5]
datos <- datos[, -7]
#write.csv(datos, file = "archivo.csv", row.names = FALSE)
datos
datos <- datos %>%
  group_by(Nomenclatu) %>%
  mutate(Nomenclatu = ifelse(Nomenclatu == "SIN CUADRANTE (SIN COLONIA, SIN SECTOR)",
                             paste0(Nomenclatu, "_", row_number()),
                             Nomenclatu)) %>%
  ungroup()


#En esta parte se incluyen ambos periodos en el primer nivel
filtered_data <- datos %>%
  mutate(
    Robos2023 = `Robo a transeúnte en vía pública con violencia_Anterior`,
    Robos2024 = `Robo a transeúnte en vía pública con violencia_Actual`
  )
#ordenados por el valor total
nivel1 <- filtered_data %>%
  group_by(alcaldia_hecho) %>%
  summarise(
    total_delitos_2023 = sum(Robos2023, na.rm = TRUE),
    total_delitos_2024 = sum(Robos2024, na.rm = TRUE),
    Robos2023 = sum(Lesiones2023, na.rm = TRUE),
    Robos2024 = sum(Lesiones2024, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_delitos_2024)) %>%
  mutate(
    name = alcaldia_hecho,
    drilldown_2023 = paste0(alcaldia_hecho, "_2023"),
    drilldown_2024 = paste0(alcaldia_hecho, "_2024"),
    tooltip = paste0(
      "<b>Alcaldía:</b> ", alcaldia_hecho, "<br>",
      "<b>Robos semana actual:</b> ", Robos2024, "<br>",
      "<b>Robos semana pasada:</b> ", Robos2023
      
    )
  )

#ordenados de mayor a menor dentro de cada alcaldía
nivel2 <- filtered_data %>%
  group_by(alcaldia_hecho, Nomenclatu) %>%
  summarise(
    total_delitos_2023 = sum(Robos2023, na.rm = TRUE),
    total_delitos_2024 = sum(Robos2024, na.rm = TRUE),
    Lesiones2023 = sum(Robos2023, na.rm = TRUE),
    Lesiones2024 = sum(Robos2024, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_delitos_2024))

drilldown_nivel2 <- list()

for (periodo in c("2023", "2024")) {
  drilldown_nivel2[[periodo]] <- nivel2 %>%
    group_by(alcaldia_hecho) %>%
    group_split() %>%
    lapply(function(data) {
      data <- data %>% arrange(desc(data[[paste0("total_delitos_", periodo)]]))
      list(
        id = paste0(unique(data$alcaldia_hecho), "_", periodo),
        data = lapply(1:nrow(data), function(i) {
          list(
            name = data$Nomenclatu[i],
            y = data[[paste0("total_delitos_", periodo)]][i],
            customTooltip = paste0(
              "<b>", data$Nomenclatu[i], "</b><br>",
              "Robos semana actual: ", data$Robos2024[i], "<br>",
              "Robos semana pasada: ", data$Robos2023[i]
              
            )
          )
        })
      )
    })
}

grafico <- highchart() %>%
  #Aqui se puede cambiar el contenido del titulo, y el tamaño del mismo y de los ejes
  hc_chart(type = "column", zoomType = "xy") %>%
  hc_title(text = "Total de homicidios y lesiones dolosas por arma de fuego por alcaldía", style = list(fontSize = "24px")) %>%
  hc_xAxis(type = "category", labels = list(style = list(fontSize = "19px"))) %>%
  hc_yAxis(labels = list(style = list(fontSize = "25px"))) %>%
  #Apartir de aqui se puede modificar el tooltip, en esye caso se usa javascript para su creacion
  hc_tooltip(
    useHTML = TRUE,
    style = list(fontSize = "24px"),
    formatter = JS(
      "function() {
        if (this.point.customTooltip) {
          return this.point.customTooltip;
        } else {
          return '<b>' + this.point.name + '</b><br>Total: ' + this.y;
        }
      }"
    )
  ) %>%
  #Aqui se puede configurar todo lo referene al grafico, por ejemplo, el tamaño y referencias de las labels
  hc_plotOptions(
    series = list(
      dataLabels = list(enabled = TRUE, style = list(fontSize = "24px")),
      keys = c("name", "y", "customTooltip", "drilldown")
    )
  ) %>%
  #Esto es cada nivel donde se le puede cambiar los colores y el 
  hc_add_series(
    name = "Total delitos semana pasada",
    color = "#BC955C",
    data = nivel1 %>%
      select(name, total_delitos_2023, drilldown_2023, tooltip) %>%
      rename(y = total_delitos_2023, drilldown = drilldown_2023, customTooltip = tooltip) %>%
      list_parse()
  ) %>%
  hc_add_series(
    name = "Total delitos semana actual",
    color = "#B02858",
    data = nivel1 %>%
      select(name, total_delitos_2024, drilldown_2024, tooltip) %>%
      rename(y = total_delitos_2024, drilldown = drilldown_2024, customTooltip = tooltip) %>%
      list_parse()
  ) %>%
  hc_drilldown(
    series = c(drilldown_nivel2[["2023"]], drilldown_nivel2[["2024"]])
  )
grafico
# Guardar el gráfico como HTML en la ruta especificada
ruta_guardado <- here("salidas/imagenes/grafico_variacion_hom_les.html")
ruta_salida <- here("salidas/imagenes/grafico_variacion_hom_les.png")

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


