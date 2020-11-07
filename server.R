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
  
  #server oldali szurok
  output$AlapNeveout = renderUI({
    selectInput(inputId = "AlapNeve",
                label = "AlapNeve:", 
                choices = as.character(unique(categories_df$ALAP_NEVE)),
                selected = "Aberdeen Diversified Growth Alapok Alapja 'B'")
  })
  
  datasub <- reactive({
    categories_df[categories_df$ALAP_NEVE == input$AlapNeve,]
  })
  
  output$Alapkezeloout = renderUI({
    selectInput(inputId = "Alapkezelo", 
                label = "Alapkezelő:", 
                choices = unique(datasub()[,"ALAPKEZELO"]),
                selected = unique(datasub()[,"ALAPKEZELO"])[1])
  })
  
 
  
  # updateSelectizeInput(session, 'AlapNeve', choices = alapok_df$ALAP_NEVE, server = TRUE)
  #fuggveny az arfolyam plothoz
  plot_fuggveny <-
    function(chart_tipus = geom_line(), 
             kivalasztott_ertek = input$valueselect,
             kategoria = input$categoryselect) {
      szurt_isinek <-
        categories_df %>% filter(ALAP_NEVE %in% input$AlapNeve) %>% select(ISIN_KOD)
      szurt_alapok <-
        categories_df %>% filter(ALAP_NEVE %in% input$AlapNeve) %>% select(ALAP_NEVE, ISIN_KOD)
      timeseries_df_merged <- merge(timeseries_df, szurt_alapok, by.x="ISIN_KOD", by.y="ISIN_KOD")

      timeseries_df_merged %>%
        filter(ISIN_KOD %in% szurt_isinek$ISIN_KOD) %>%
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
