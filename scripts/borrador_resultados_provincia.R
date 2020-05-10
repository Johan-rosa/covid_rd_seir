# Gráfico Nacional hchart
plot_rcero_rd <- highchart() %>% 
    hc_xAxis(categories = format(rcero_toplot$fecha, "%B %d")) %>% 
    hc_add_series(name = "Rcero",
                  data = rcero_toplot$`República Dominicana`,
                  type = "column")  %>% 
    hc_add_series(name = "Promedio movil", data = rollmean_toplot$`República Dominicana`) %>% 
    hc_add_theme(hc_theme_elementary())

# Gráfico Distrito Nacional hchart
plot_rcero_dn <- highchart() %>% 
    hc_xAxis(categories = format(rcero_toplot$fecha, "%B %d")) %>% 
    hc_add_series(name = "Rcero",
                  data = rcero_toplot$`Distrito Nacional`,
                  type = "column")  %>% 
    hc_add_series(name = "Promedio movil", data = rollmean_toplot$`Distrito Nacional`) %>% 
    hc_add_theme(hc_theme_elementary())

# plotly Distrito
plot_distrito <- rcero_toplot %>% 
    ggplot(aes(x = fecha, y = `Distrito Nacional`)) +
    geom_col(fill = "slateblue3", apha = 0.6) +
    geom_line(data = rollmean_toplot, color = "red", size = 1) +
    theme_minimal() +
    labs(x = NULL, y ="R0", title = "Distrito Nacional")

plotly_distrito <- plotly::ggplotly(plot_distrito)

# plotly Santo Domingo
plot_sd <- rcero_toplot %>% 
    ggplot(aes(x = fecha, y = `Santo Domingo`)) +
    geom_col(fill = "slateblue3", apha = 0.6) +
    geom_line(data = rollmean_toplot, color = "red", size = 1) +
    theme_minimal() +
    labs(x = NULL, y ="R0", title = "Santo Domingo")

plotly_sd <- plotly::ggplotly(plot_sd)

# plotly Santiago
plot_santiago <- rcero_toplot %>% 
    ggplot(aes(x = fecha, y = `Santiago`)) +
    geom_col(fill = "slateblue3", apha = 0.6) +
    geom_line(data = rollmean_toplot, color = "red", size = 1) +
    theme_minimal() +
    labs(x = NULL, y ="R0", title = "Santiago")

plotly_santiago <- plotly::ggplotly(plot_santiago)

# plotly Distrito
plot_duarte <- rcero_toplot %>% 
    ggplot(aes(x = fecha, y = `Duarte`)) +
    geom_col(fill = "slateblue3", apha = 0.6) +
    geom_line(data = rollmean_toplot, color = "red", size = 1) +
    theme_minimal() +
    labs(x = NULL, y ="R0", title = "Distrito Nacional")

plotly_duarte <- plotly::ggplotly(plot_duarte)


plotly::subplot(plotly_distrito, plotly_sd, plotly_santiago, plotly_santiago, nrows = 2)



# Plolty grids ------------------------------------------------------------

# Combinaciones de provincias

rd_dn <- c("República Dominicana", "Distrito Nacional")
grid2 <- c("Duarte", "La Vega", "Monseñor Nouel", "Sánchez Ramírez")
grid3 <- c("Espaillat", "Hermanas Mirabal", "Puerto Plata", "Santiago")

# Grid RD y Distrito
plot_rd_dn <- rcero %>% 
    filter(provincia %in% rd_dn) %>% 
    ggplot(aes(x = fecha)) +
    geom_col(aes(y = rcero), fill = "slateblue3") +
    geom_line(aes(y = rollmean), color = "red", size = 1) +
    facet_wrap(~provincia) +
    geom_hline(yintercept = 1, linetype = 2, size = 0.8) +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold", size = 13)) +
    labs(x = NULL, y = "R0")


plotly::ggplotly(plot_rd_dn)


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


plotly::ggplotly(plot_grid2)


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


plotly::ggplotly(plot_grid3)



