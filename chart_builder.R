library(shiny)
library(dplyr)
library(ggplot2)
#install.packages("shinythemes")
#install.packages("plotly")

#import_data
alapok_df<- read.csv("C:/Users/Domonkos/Desktop/bamosz/data_shiny.csv", encoding="UTF-8", stringsAsFactors=FALSE)
names(alapok_df)[1] <- "Datum"
alapok_df$Datum <- alapok_df$Datum %>% as.Date
alapok_df$EFF_HOZAM_1_EVES = readr::parse_number(alapok_df$EFF_HOZAM_1_EVES)/100
alapok_df$EFF_HOZAM_3_EVES = readr::parse_number(alapok_df$EFF_HOZAM_3_EVES)/100


#ui ######################################
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      #felteteles panelre kattintva nyissa csak meg a formazas inputokat
      selectInput('AlapNeve', 'Alap neve', choices=alapok_df$ALAP_NEVE, selected=alapok_df$ALAP_NEVE[1]),
      shinythemes::themeSelector(),
      selectInput("valueselect", "Érték kiválasztása", selected='ARFOLYAM', choices=c("ARFOLYAM", "EFF_HOZAM_1_EVES", "EFF_HOZAM_3_EVES"))
    ),
    mainPanel(
      # egy panelbe rakom line chartot, table-t (felso savon lehet valtani)
      tabsetPanel(
        #line chart
        tabPanel('Line chart', plotly::plotlyOutput('Alap_arfolyama_plot')),
        #table
        tabPanel('Table', DT::DTOutput('Alap_arfolyama_table')),
        #scatter
        tabPanel('Scatter plot', plotly::plotlyOutput('Alap_arfolyama_Scatterplot')),
        #bar
        tabPanel('Bar chart', plotly::plotlyOutput('Alap_arfolyama_colplot'))
        
      ),
      selectInput("formazas", "Formázás", selected='Default', choices=c("Default", "Egyedi")),
      conditionalPanel(
        condition = "input.formazas == 'Egyedi'",
        column(3, colourpicker::colourInput("col", "Select colour", value = "red"), textInput('AbraCime', 'Cím megadása:', "Ábra címe"),
        numericInput("Cimsize","Cím mérete:", "22")),
        column(3, selectInput("betutipus", "Betűtípus", selected='F', choices=c("Times", "Calibri", "Helvetica"))))
    )))
  


# server ################################
server <- function(input, output, session){
  #fuggveny az arfolyam plothoz
  plot_alapok_arfolyama <-
    function(chart_tipus = geom_line(),
             kivalasztott_ertek = input$valueselect) {
      alapok_df %>%
        filter(ALAP_NEVE == input$AlapNeve) %>%
        ggplot(aes(x = Datum, y = get(kivalasztott_ertek))) +
        chart_tipus + ggtitle(input$AbraCime) + theme(panel.background = element_rect(fill = input$col), plot.title = element_text(size =
                                                                                  input$Cimsize, family = input$betutipus))
    }
  
  #arfolyam output
  output$Alap_arfolyama_plot <- plotly::renderPlotly({
    plot_alapok_arfolyama(geom_line())
  })
  #tablazat output
  output$Alap_arfolyama_table <- DT::renderDT({
    alapok_df %>% 
      filter(ALAP_NEVE == input$AlapNeve)
  })
  #scatter output
  output$Alap_arfolyama_Scatterplot <- plotly::renderPlotly({
    plot_alapok_arfolyama(geom_point())
  })
  
  #barchart output
  output$Alap_arfolyama_colplot <- plotly::renderPlotly({
    plot_alapok_arfolyama(geom_col())
  })
  
}
shinyApp(ui = ui, server = server)



