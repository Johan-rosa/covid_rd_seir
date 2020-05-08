
# Paquetes ----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(highcharter)



# Lectura de datos --- -----------------------------------------------------

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


# Manipulando los datos --- ------------------------------------------------

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
        data = map(data, mutate_mapping,
                   hcaes(x = iddate, y = rcero, color = tocolor),
                   drop = TRUE),
        data = map(data, list_parse)
    ) %>%
    rename(ttdata = data)

mapdata2 <- left_join(mapdata2, rcero_nested)


# tema para  los gráficos de ggplot --- ------------------------------------

theme_covid_rd <- theme_minimal() +
    theme(
        strip.text = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(face = "bold", size = 12)
    )

# Mapa --- -----------------------------------------------------------------

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

# Gráficos del Rcero y su promedio movil --- -------------------------------

rcero <- rcero %>% 
    group_by(provincia) %>% 
    mutate(
        code = ifelse(is.na(code), "RD", code),
        fecha = lubridate::ymd(fecha),
        rollmean = c(NA, NA, NA, NA, zoo::rollmean(rcero, 5))) %>% 
    select(provincia, fecha, rcero, rollmean, code) %>% 
    ungroup()


rceroto_toplot <- rcero %>% 
    select(fecha, provincia, rcero) %>% 
    mutate(rcero = round(rcero, 2)) %>% 
    pivot_wider(names_from = provincia, values_from = rcero)

rollmean_toplot <- rcero %>% 
    select(fecha, provincia, rollmean) %>% 
    mutate(rollmean = round(rollmean, 2)) %>% 
    pivot_wider(names_from = provincia, values_from = rollmean)


# Gráfico Nacional 
plot_rcero_rd <- highchart() %>% 
    hc_xAxis(categories = format(rcero_to_plot$fecha, "%B %d")) %>% 
    hc_add_series(name = "Rcero",
                  data = rceroto_toplot$`República Dominicana`,
                  type = "column")  %>% 
    hc_add_series(name = "Promedio movil", data = rollmean_toplot$`República Dominicana`) %>% 
    hc_add_theme(hc_theme_elementary())

# Gráfico Distrito Nacional 
plot_rcero_dn <- highchart() %>% 
    hc_xAxis(categories = format(rcero_to_plot$fecha, "%B %d")) %>% 
    hc_add_series(name = "Rcero",
                  data = rceroto_toplot$`Distrito Nacional`,
                  type = "column")  %>% 
    hc_add_series(name = "Promedio movil", data = rollmean_toplot$`Distrito Nacional`) %>% 
    hc_add_theme(hc_theme_elementary())



# Graficos reproducción de contagios por país -----------------------------



# Scatter del Rcero en diferentes momentos  -------------------------------

rcero <- rcero %>% 
  mutate(fecha = lubridate::ymd(fecha))

# Gráficos del y = cuatro de mayo, x = 21 de marzso

rcero %>% 
  filter(as.character(fecha) %in% c("2020-03-29", "2020-05-04")) %>% 
  mutate(fecha = format(fecha, "%B %d")) %>% 
  pivot_wider(names_from = fecha, values_from = rcero) %>% 
  ggplot(aes(x = `May 04`, y = `March 29`)) +
  geom_point(size = 3, alpha = .8, color = "midnightblue") +
  theme_minimal()



# Guardando outputs -------------------------------------------------------

# Objetos a borrar
erase <- ls()[!ls() %in% c("plot_rcero_rd", "plot_rcero_dn", "map_rcero")]


save.image("presentacion/objetos/graficos_ws")



