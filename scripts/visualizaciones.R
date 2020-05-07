
# Paquetes ----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(highcharter)


url <- "countries/do/do-all"

# Lectura de datos --------------------------------------------------------

infectados <- read_excel("SIR_provincias/infectados2.xlsx")

rcero <- "SIR_provincias/rcero.xlsx" %>% read_excel()

mapdata <- get_data_from_map(download_map_data("countries/do/do-all"))

mapdata2 <- mapdata %>% 
    left_join(
        rcero %>% 
            group_by(code) %>% 
            filter(fecha == max(fecha)) %>%
            select(code, infectados),
        by = c("hc-a2" = "code")) %>% 
    select(code = `hc-a2`, infectados) %>% 
    filter(!is.na(infectados))


rcero_nested <- rcero %>% 
    group_by(provincia) %>% 
    mutate(fecha = lubridate::ymd(fecha),
           iddate = 1,
           iddate = cumsum(iddate)
           ) %>% 
    ungroup() %>% 
    select(code, iddate, infectados) %>% 
    nest(-code) %>% 
    mutate(
        data = map(data, mutate_mapping, hcaes(x = iddate, y = infectados), drop = TRUE),
        data = map(data, list_parse)
    ) %>%
    rename(ttdata = data)

mapdata2 <- left_join(mapdata2, rcero_nested)

# Mapa --------------------------------------------------------------------

# Tooltip plot con el Rcero por día 
map_rcero <-  hcmap("countries/do/do-all",
      data = mapdata2, joinBy = c("hc-a2", "code"),
      value = "infectados",
      name = "R cero",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA",
      borderwidth = 0.01)


map_rcero <-  map_rcero %>% 
    hc_tooltip(useHTML = TRUE, pointFormatter = tooltip_chart(
        width = 200,
        height = 200,
        accesor = "ttdata",
        hc_opts = list(
            title = list(text = "", useHTML = TRUE),
            xAxis = list(type = "category")
        )
    ))
