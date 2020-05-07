
# Paquetes ----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(highcharter)


url <- "countries/do/do-all"

# Lectura de datos --------------------------------------------------------

# Data de salud pública con los casos acumulados por días
infectados <- read_excel(
    "SIR_provincias/Datos Boletines SP - Provincias.xlsx",
    sheet = "Provincias_CasosConfir", 
    range = "a1:ah47") %>%
    # Llevando la data a formato tidy
    pivot_longer(cols = -Dias, names_to = "provincias", values_to = "infectados")

# Series del Rcero
rcero <- "SIR_provincias/rcero.xlsx" %>%
    read_excel() %>% 
    rename(rcero = infectados)

# Data de los mapas de república domicana en highcharter
mapdata <- get_data_from_map(download_map_data("countries/do/do-all"))

# highchart catalogo provincia
catalogo_provincias <- mapdata %>% select(code = `hc-a2`, provincia = `woe-name`)


# Manipulando los datos ---------------------------------------------------

# Recoding de las provincias para que coincida con highcharter
# Ellos tienen algunos nombres con porblemas, pero es necesario
# que esten iguales para hacer el join
infectados <- infectados %>% 
    mutate(
        provincia_hc = recode(provincias,
                              "El Seibo" = "El Seybo",
                              "Bahoruco" = "Baoruco",
                              "Hermanas Mirabal" = "Hermanas",
                              "Elías Piña" = "La Estrelleta") 
    )


# Data para el mapa
mapdata2 <- mapdata %>% 
    left_join(
        infectados %>% 
            group_by(provincias) %>% 
            filter(Dias == max(Dias)),
        by = c("woe-name" = "provincia_hc")) %>% 
    select(code = `hc-a2`, infectados) %>% 
    filter(!is.na(infectados))


rcero_nested <- rcero %>% 
    group_by(provincia) %>% 
    mutate(fecha = lubridate::ymd(fecha),
           iddate = 1,
           iddate = cumsum(iddate)
           ) %>% 
    ungroup() %>% 
    select(code, iddate, rcero) %>% 
    mutate(tocolor = 1) %>% 
    nest(-code) %>% 
    mutate(
        data = map(data, mutate_mapping, hcaes(x = iddate, y = rcero, color = tocolor), drop = TRUE),
        data = map(data, list_parse)
    ) %>%
    rename(ttdata = data)

mapdata2 <- left_join(mapdata2, rcero_nested)

# Mapa --------------------------------------------------------------------

# Tooltip plot con el Rcero por día 
map_rcero <-  hcmap("countries/do/do-all",
      data = mapdata2, joinBy = c("hc-a2", "code"),
      value = "infectados",
      name = "Infectados",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA",
      borderwidth = 0.01)


map_rcero <-  map_rcero %>% 
    hc_tooltip(useHTML = TRUE, pointFormatter = tooltip_chart(
        width = 300,
        height = 250,
        accesor = "ttdata",
        hc_opts = list(
            title = list(text = "", useHTML = TRUE),
            xAxis = list(type = "category"),
            series = list(list(color = "midnightblue", name = "Rcero")
        ))
    ))
