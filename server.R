library(shiny)
library(dplyr)
library(ggplot2)
library(dbplyr)


# server ################################
server <- function(input, output, session){
  #default cím megadása
  observe({
    type <- input$categoryselect
    default_title <-
      ifelse(input$categoryselect=="Datum",
             paste(input$valueselect,
                   "változása"), paste(input$valueselect,
                                       "megoszlása", input$categoryselect, "szerint"))
    updateTextInput(session, "AbraCime", value = default_title)
    output$plot <- renderPlot({
      plot(1:10, main = default_title, type = input$type)
    })
  })
  
  
  
  # updateSelectizeInput(session, 'AlapNeve', choices = alapok_df$ALAP_NEVE, server = TRUE)
  #fuggveny az arfolyam plothoz
  plot_fuggveny <-
    function(chart_tipus = geom_line(), 
             kivalasztott_ertek = input$valueselect,
             kategoria = input$categoryselect) {
      alapok_df %>%
        filter(ALAP_NEVE %in% input$AlapNeve) %>%
        #azert kell get, mert igy adja vissza azt az oszlopot, amit nevben megadtunk
        ggplot(aes(
          x = get(kategoria),
          y = get(kivalasztott_ertek),
          fill = ALAP_NEVE
        )) +
        chart_tipus + ggtitle(input$AbraCime) + theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = input$col),
          plot.title = element_text(size =
                                      input$Cimsize, family = input$betutipus)
        )
    }
  #arfolyam output
  output$Alap_arfolyama_plot <- plotly::renderPlotly({
    plot_fuggveny(geom_line())
  })
  #tablazat output
  output$Alap_arfolyama_table <- DT::renderDT({
    alapok_df %>% 
      filter(ALAP_NEVE == input$AlapNeve)
  })
  #scatter output
  output$Alap_arfolyama_Scatterplot <- plotly::renderPlotly({
    plot_fuggveny(geom_point())
  })
  
  #barchart output
  output$Alap_arfolyama_colplot <- plotly::renderPlotly({
    plot_fuggveny(geom_col())
  })
  #area output
  output$Alap_arfolyama_areaplot <- plotly::renderPlotly({
    plot_fuggveny(geom_area())
  })
  
  
}
