
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

rcero_paises <- read_excel("SIR_provincias/r0avanzadas.xlsx") %>% 
  pivot_longer(cols = -fecha, names_to = "pais", values_to = "rcero")


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
    ) %>% left_join(catalogo_provincias, by = c("provincia_hc" = "provincia")) 

# Data para el mapa
mapdata2 <- mapdata %>% 
    left_join(
        infectados %>% 
            group_by(provincias) %>% 
            filter(Dias == max(Dias)),
        by = c("woe-name" = "provincia_hc")) %>% 
    select(code = `hc-a2`, infectados) %>% 
    filter(!is.na(infectados))


infectados_nested <-  infectados %>% 
  group_by(provincias) %>% 
  mutate(flujo_infectados = infectados - lag(infectados),
         rollmean = c(rep(NA, 6), zoo::rollmean(flujo_infectados, 7)),
         rollmean = ifelse(rollmean < 0, 0, rollmean),
         tocolor = 1) %>% 
  ungroup() %>% 
  nest(-code) %>% 
  mutate(data = map(data,mutate_mapping,
                    hcaes(x = Dias, y = rollmean, color = tocolor),
                    drop = TRUE),
         data = map(data, list_parse)
         ) %>% 
  rename(ttdata = data)
  
  
rcero_nested <- rcero %>% 
    group_by(provincia) %>% 
    mutate(fecha = lubridate::ymd(fecha),
           iddate = 1,
           iddate = cumsum(iddate),
           rollmean = c(NA, NA, NA, NA, zoo::rollmean(rcero, 5))
           ) %>% 
    ungroup() %>% 
    select(code, iddate, rcero, rollmean) %>%
    #gather(tocolor, rcero, rcero, rollmean) %>% 
    mutate(tocolor = 1) %>% 
    nest(-code) %>% 
    mutate(
        data = map(data, mutate_mapping,
                   hcaes(x = iddate, y = rollmean, color = tocolor),
                   drop = TRUE),
        data = map(data, list_parse)
    ) %>%
    rename(ttdata = data)

mapdata2 <- left_join(mapdata2, infectados_nested)


# Economias avanzadas -----------------------------------------------------
library(ggrepel)

paises_principales <- c("ESP", "ITA", "USA", "SWE", "CHN", "DEU", "KOR")

labels <- rcero_paises %>%
  mutate(pais = str_remove_all(pais, "R0_|_VA7D")) %>% 
  mutate(fecha = as.Date(fecha)) %>%
  filter(pais %in% paises_principales) %>% 
  filter(fecha == '2020-03-04') %>% 
  mutate(fecha = as.Date("2020-03-01"),
         pais = recode(pais, "ESP" = "España",
                       "ITA" = "Italia",
                       "USA" = "USA",
                       "SWE" = "Suecia",
                       "CHN" = "China",
                       "DEU" = "Alemania",
                       "KOR" = "Korea"))
  
plot_rcero_paises <- rcero_paises %>% 
  mutate(pais = str_remove_all(pais, "R0_|_VA7D"),
         fecha = lubridate::ymd(fecha)) %>% 
  filter(pais %in% paises_principales) %>% 
  filter(fecha > "2020-03-03") %>%
  mutate(pais = recode(pais, "ESP" = "España",
                       "ITA" = "Italia",
                       "USA" = "USA",
                       "SWE" = "Suecia",
                       "CHN" = "China",
                       "DEU" = "Alemania",
                       "KOR" = "Korea")) %>% 
  ggplot(aes(x = fecha, y = rcero, color = pais)) +
  geom_line(size = 1) +
  geom_text(data = labels, aes(x = fecha, y = rcero, label = pais), hjust = 1) +
  theme_minimal() +
  coord_cartesian(xlim = c(as.Date("2020-02-29"), as.Date("2020-05-05"))) +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", size = 12)) +
  ggthemes::scale_color_tableau() +
  labs(x = NULL, y = "R0")

plotly_rcero_paises <-  plotly::ggplotly(plot_rcero_paises)
  

# Spagetti plot -----------------------------------------------------------

rcero_spagetti <- rcero %>% 
  group_by(provincia) %>% 
  mutate(fecha = lubridate::ymd(fecha),
         iddate = 1,
         iddate = cumsum(iddate),
         rollmean = c(NA, NA, NA, NA, zoo::rollmean(rcero, 5)),
         provincia2 = provincia
  ) %>% 
  ungroup() %>% 
  select(provincia, provincia2, fecha, iddate, rollmean)

plot_rcero_spagetti <-  rcero_spagetti %>%
  filter(!is.na(rollmean)) %>% 
  filter(provincia %in% c("Distrito Nacional", "Santo Domingo", "Santiago", "Duarte")) %>% 
  ggplot(aes(x = fecha, y = rollmean)) +
  geom_line(
    data = select(rcero_spagetti, -provincia) %>% 
      filter(!is.na(rollmean)),
    aes(x = fecha, y = rollmean, group = provincia2),
    alpha = 0.7,
    color = "gray"
  ) +
  geom_line(aes(group = provincia), color = "midnightblue", size = 1.2) +
  geom_hline(yintercept = 1, linetype = 2, size = 0.5) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 13)
  ) +
  labs(
    x = "",
    y = "Promedio movil 5 días"
  ) +
  facet_wrap(~provincia)

ggsave("visualizaciones/plot_rcero_spagetti.PNG",
       plot_rcero_spagetti,
       height = 8,
       width = 10,
       dpi = 1000)


# Mapa --- -----------------------------------------------------------------

# Tooltip plot con el Rcero por día 
map_infectados <-  hcmap("countries/do/do-all",
      data = mapdata2, joinBy = c("hc-a2", "code"),
      value = "infectados",
      name = "Infectados",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA",
      borderwidth = 0.01)


map_infectados <-  map_infectados %>% 
    hc_tooltip(useHTML = TRUE, pointFormatter = tooltip_chart(
        width = 300,
        height = 250,
        accesor = "ttdata",
        hc_opts = list(
            title = list(text = "", useHTML = TRUE),
            xAxis = list(type = "category"),
            series = list(list(color = "red", name = "rollmean", big.mark = ","))
            )
    ))



# Heatmap por cada mil habitantes -----------------------------------------
casos_mil <- read_excel("SIR_provincias/resultados_provincias_1.xlsx", sheet = "casos_mil")


heat_map_casos <- casos_mil %>% 
  mutate(fecha = seq(as.Date("2020-03-01"),
                     by = "day",
                     length.out = nrow(.))) %>% 
  pivot_longer(cols = -fecha, names_to = "provincia", values_to = "infectados") %>%
  mutate(provincia = str_remove(provincia, "m_"),
         provincia = fct_reorder(provincia, infectados, .fun = sum),
         infectados = round(infectados, 2)) %>% 
  ggplot(aes(x = fecha, y = provincia, fill = infectados)) +
  geom_tile() +
  labs(y = NULL, x = NULL, fill = "Casos") +
  scale_fill_viridis_c() +
  coord_cartesian(expand = FALSE) +
  theme(axis.text = element_text(size = 14))

plotly_heatmap_casos <- plotly::ggplotly(heat_map_casos)
  
# Gráficos del Rcero y su promedio movil --- -------------------------------

rcero <- rcero %>% 
    group_by(provincia) %>% 
    mutate(
        code = ifelse(is.na(code), "RD", code),
        fecha = lubridate::ymd(fecha),
        rollmean = c(NA, NA, NA, NA, zoo::rollmean(rcero, 5))) %>% 
    select(provincia, fecha, rcero, rollmean, code) %>% 
    ungroup()


rcero_toplot <- rcero %>% 
    select(fecha, provincia, rcero) %>% 
    mutate(rcero = round(rcero, 2)) %>% 
    pivot_wider(names_from = provincia, values_from = rcero)

rollmean_toplot <- rcero %>% 
    select(fecha, provincia, rollmean) %>% 
    mutate(rollmean = round(rollmean, 2)) %>% 
    pivot_wider(names_from = provincia, values_from = rollmean)


# Combinaciones de provincias

rd_dn <- c("República Dominicana", "Distrito Nacional")
grid2 <- c("Duarte", "La Vega", "Monseñor Nouel", "Sánchez Ramírez")
grid3 <- c("Espaillat", "Hermanas Mirabal", "Puerto Plata", "Santiago")
grind4 <- c("La Altagracia", "La Romana", "San Cristóbal", "Santo Domingo")

# Grid RD y Distrito
plot_grid1 <- rcero %>% 
  filter(provincia %in% rd_dn) %>% 
  ggplot(aes(x = fecha)) +
  geom_col(aes(y = rcero), fill = "slateblue3") +
  geom_line(aes(y = rollmean), color = "red", size = 1) +
  facet_wrap(~provincia) +
  geom_hline(yintercept = 1, linetype = 2, size = 0.8) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 15)) +
  labs(x = NULL, y = "R0")


plotly_grid1 <- plotly::ggplotly(plot_grid1)


# Grid 2
plot_grid2 <- rcero %>% 
  filter(provincia %in% grid2) %>% 
  ggplot(aes(x = fecha)) +
  geom_col(aes(y = rcero), fill = "slateblue3") +
  geom_line(aes(y = rollmean), color = "red", size = 1) +
  facet_wrap(~provincia, scales = "free") +
  geom_hline(yintercept = 1, linetype = 2, size = 0.8) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 13)) +
  labs(x = NULL, y = "R0")


plotly_grid2 <-plotly::ggplotly(plot_grid2)


# Grid 3
plot_grid3 <- rcero %>% 
  filter(provincia %in% grid3) %>% 
  ggplot(aes(x = fecha)) +
  geom_col(aes(y = rcero), fill = "slateblue3") +
  geom_line(aes(y = rollmean), color = "red", size = 1) +
  facet_wrap(~provincia, scales = "free") +
  geom_hline(yintercept = 1, linetype = 2, size = 0.8) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 13)) +
  labs(x = NULL, y = "R0")


plotly_grid3 <-plotly::ggplotly(plot_grid3)


plot_grid4 <- rcero %>% 
  filter(provincia %in% grind4) %>% 
  ggplot(aes(x = fecha)) +
  geom_col(aes(y = rcero), fill = "slateblue3") +
  geom_line(aes(y = rollmean), color = "red", size = 1) +
  facet_wrap(~provincia, scales = "free") +
  geom_hline(yintercept = 1, linetype = 2, size = 0.8) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 15)) +
  labs(x = NULL, y = "R0")

plotly_grid4 <-plotly::ggplotly(plot_grid4)

# Graficos reproducción de contagios por país -----------------------------



# Scatter del Rcero en diferentes momentos  -------------------------------
header <- c("date", "Distrito Nacional", "Duarte", "Espaillat", "Hermanas Mirabal",
            "La Altagracia", "La Romana", "La Vega", "Monseñor Nouel", "Puerto Plata", 
            "San Cristobal", "Santo Domingo", "Sánchez Ramírez", "Santiago", "Total")
            


rcero2 <- read_excel("SIR_provincias/resultados_provincias_2_cpm.xlsx", sheet = "R0") %>%
  setNames(header) %>% 
  mutate(date = seq(as.Date("2020-03-25"), by = "day", length.out = nrow(.))) %>% 
  pivot_longer(names_to = "provincia", values_to = "casos", cols = -date)


# Gráficos del y = cuatro de mayo, x = 21 de marzso

scatter1 <- rcero2 %>%  
  filter(as.character(date) %in% c("2020-03-25", "2020-05-02")) %>% 
  mutate(date = format(date, "%B %d")) %>%  
  pivot_wider(names_from = date, values_from = casos) %>% 
  ggplot(aes(text = provincia, y = `May 02`, x = `March 25`)) +
  geom_point(size = 3, alpha = .8, color = "midnightblue") +
  geom_abline(intercept = 0, slope = 1) +
  theme_minimal() +
  labs(x = "21 de Marzo", y = '05 de Mayo') +
  coord_cartesian(xlim = c(0, 7), ylim = c(0, 7))
  

scatter_1plotly <- plotly::ggplotly(scatter1, tooltip = "text")


matix_medidas <- rcero2 %>%
  mutate(
    date = as.character(date),
    cuarentena = case_when(
      date == "2020-03-25" ~ "21 Marzo - 03 Abril",
      date == "2020-04-03" ~ "21 Marzo - 03 Abril",
      date == "2020-04-04" ~ "04 Abril - 18 Abril",
      date == "2020-04-18" ~ "04 Abril - 18 Abril",
      date == "2020-04-05" ~ "05 Abril - 30 Abril",
      date == "2020-04-30" ~ "05 Abril - 30 Abril",
      date == "2020-05-01" ~ "01 Mayo - 17 Mayo",
      date == "2020-05-02" ~ "01 Mayo - 17 Mayo"
    ),
    minmax = case_when(
        date == "2020-03-25" ~ "min",
        date == "2020-04-03" ~ "max",
        date == "2020-04-04" ~ "min",
        date == "2020-04-18" ~ "max",
        date == "2020-04-05" ~ "min",
        date == "2020-04-30" ~ "max",
        date == "2020-05-01" ~ "min",
        date == "2020-05-02" ~ "max"
      )
    ) %>% filter(!is.na(cuarentena))


matix_medidas <- matix_medidas %>% 
  filter(minmax == "min") %>%
  rename(min = casos) %>% 
  left_join(matix_medidas %>% 
              filter(minmax == "max") %>% 
              rename(max = casos) %>% 
              select(cuarentena, provincia, max)
            ) %>% select(-minmax)

 
plot_matix_medidas <- matix_medidas %>% 
  mutate(
    cuarentena = factor(cuarentena,
                        levels = c("21 Marzo - 03 Abril",
                                   "04 Abril - 18 Abril",
                                   "05 Abril - 30 Abril",
                                   "01 Mayo - 17 Mayo")),
    color = max > min
  ) %>% 
  ggplot(aes(x = min, y = max, text = provincia, color = color)) +
  geom_point(size = 3, show.legend = FALSE) +
  facet_wrap(~cuarentena) +
  geom_abline(intercept = 0, slope = 1) +
  coord_cartesian(ylim = c(0, 7), xlim = c(0, 7)) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 12)) +
  labs(x = "Inicio del periodo",
       y = "Final del periodo") +
  scale_color_manual(values = c("midnightblue", "red"))


plot_matrix_plotly <- plotly::ggplotly(plot_matix_medidas, tooltip = "text")


# Time line plot ----------------------------------------------------------

timeline <- tibble(day = seq(as.Date("2020-02-21"), as.Date("2020-04-05"), by = "day")) 
timeline <- timeline %>% 
  mutate(
    
    hecho = case_when(day == "2020-02-26" ~ "Primer caso de\n Covid Latinoamérica\n (Brasil)",
                      day == "2020-03-01" ~ "Primer caso\n importado en RD",
                      day == "2020-03-12" ~ "Comisión\n seguimiento\n Covid-19",
                      day == "2020-03-15" ~ "Elecciones",
                      day == "2020-03-16" ~ "Primera muerte\n por COVID-19",
                      day == "2020-03-18" ~ "Suspensión voluntaria\n de vuelos y cruceros",
                      day == "2020-03-19" ~ "Suspención\n de docencia",
                      day == "2020-03-20" ~ "",
                      day == "2020-03-27" ~ "Horario mobilidad\n de 6am - 5pm"
    ),segment = case_when(day == "2020-02-26" ~ -5,
                        day == "2020-03-01" ~ 5,
                        day == "2020-03-12" ~ 3,
                        day == "2020-03-15" ~ 1,
                        day == "2020-03-16" ~ -3,
                        day == "2020-03-18" ~ 6,
                        day == "2020-03-19" ~ -6,
                        day == "2020-03-20" ~ 0,
                        day == "2020-03-27" ~ 4
                        ),
    color = str_detect(hecho, "muerte|lecciones|importado|atinoam"),
    y = ifelse(is.na(hecho), NA, 1)
  ) 



timeline_plot <-
  timeline %>% 
  ggplot(aes(text = hecho,x = day, y = 0, color = color)) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(yend = segment, y = 0, xend = day)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("midnightblue", "red")) +
  theme_minimal() +
  geom_text(aes(x = day, y = segment * 1.3, label = hecho), size = 4) +
  theme(legend.position = "none",
        #axis.line.x = element_line(color = "black"),
        axis.text.y = element_blank(),
        axis.title = element_blank()) +
  coord_cartesian(ylim = c(-10,10), expand = FALSE)

# timeline_plot <-  
#   timeline_plot +
#   annotate("text", x = as.Date("2020-03-06"), y = -4,
#            label = "Elecciones\n municipales",
#            size = 5) +
#   annotate("text", x = as.Date("2020-03-24"), y = -3,
#            label = 'Horario de mobilidad\n de 6am - 8pm', size = 4)
  
  # annotate("text", x = as.Date("2020-03-05"), y = 8, size = 3.5,
  #          label = " Creación Comisiónn\n seguimiento Covid-19") +
  # annotate("text", x = as.Date("2020-03-06"), y = -8,
  #          label = "Elecciones Municipales", size = 3.5)




timeline_plotly <- plotly::ggplotly(timeline_plot, tooltip = c("text", "day"))


#plotly::subplot(scatter1, plot_matix_medidas)


# Tablas HTML -------------------------------------------------------------

table1 <- structure(
  list(
    provincias = c(
      "Distrito Nacional", "Duarte", "Espaillat", "Hermanas Mirabal",
      "La Altagracia", "La Romana", "La Vega", "Monseñor Nouel",
      "Puerto Plata", "San Cristóbal", "Sánchez Ramírez", "Santiago",
      "Santo Domingo", "R.D.", "China*", "Suecia"),
    dt = c(41L, 41L, 41L, 41L, 41L, 41L, 41L, 41L, 41L, 41L, 41L, 41L,
           41L, 41L, 41L, 41L),
    dr = c(21L, 33L, 33L, 33L, 33L, NA, 40L, 33L, NA, NA, 36L,
           35L, NA, 34L, 24L, 0L),
    x_1_0_1_4 = c(4L, 9L, 3L, 4L, 9L, 0L, 2L, 7L, 0L, 0L, 4L, 6L, 0L, 3L, 18L, 0L),
    x_1_5_1_9 = c(26L, 13L, 14L, 9L, 17L, 13L, 13L, 7L, 14L, 11L, 10L, 13L, 16L, 17L, 2L, 2L),
    x_2_0_2_4 = c(4L, 7L, 9L, 7L, 15L, 20L, 26L, 10L, 6L, 14L, 6L, 22L, 8L, 11L, 2L, 18L),
    x_2_5_3_0 = c(3L, 7L, 6L, 12L, 0L, 8L, 0L, 17L, 21L, 5L, 10L, 0L, 9L, 3L, 7L, 7L),
    x_3 = c(4L, 5L, 9L, 9L, 0L, 0L, 0L, 0L, 0L, 11L, 11L, 0L, 8L, 7L, 12L, 14L)),
  class = "data.frame", row.names = c(NA,-16L)
  )




# Guardando outputs -------------------------------------------------------

# Objetos a borrar
erase <- ls()[!ls() %in% c("plot_rcero_rd", "plot_rcero_dn", "map_rcero", "scatter_1plotly",
                           "timeline_plotly", "plot_matrix_plotly", "table1",
                           "plotly_rcero_paises", "plot_rcero_spagetti")]


save.image("presentacion/objetos/graficos_ws")



