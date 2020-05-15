rcero <- readRDS("rcero.RDS")

function(input, output, session) {
    
    output$rceroPlot <- plotly::renderPlotly({
        plot <- rcero %>%
            select(Fecha = fecha, Territorio = provincia, R0 =  rcero) %>% 
            filter(Territorio %in% input$provincia) %>%
            group_by(Territorio) %>% 
            mutate(
                `Promedio movil` = c(rep(NA, 4), zoo::rollmean(R0, 5)),
                `Promedio movil` = round(`Promedio movil`, 2)
            ) %>% 
            filter(!is.na(`Promedio movil`)) %>% 
            ggplot(aes(x = Fecha, y = `Promedio movil`, color = Territorio)) +
            geom_line() +
            geom_hline(yintercept = 1) +
            theme_minimal() +
            labs(x = NULL, y = "Rcero") +
            ggthemes::scale_color_tableau()
        
        return(plotly::ggplotly(plot))
    })
    
    
}

