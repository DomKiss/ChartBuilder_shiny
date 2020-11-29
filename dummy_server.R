library(shiny)
library(dplyr)
library(ggplot2)
library(dbplyr)
tail(mtcars)

base_data <- mtcars
# server ################################
server <- function(input, output, session){

  
  plot_fuggveny <-
    function(chart_tipus) {
     
      
      base_data %>%
      
      
        ggplot(aes(
          x = mpg ,
          y = sum(cyl) ,
          label = sum(cyl)
        )) +
        chart_tipus + 
        
        ggtitle(input$title_text) +
        xlab(input$x_text) +
        ylab(input$y_text) +
        geom_text(nudge_y = 1, color = "black") +
        
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = input$col_base),
          plot.title = element_text(size = input$title_size, 
                                    family = input$font_type,
                                    colour = input$title_color,
                                    hjust = input$title_align,
                                    face = ifelse(length(input$title_font) > 1, "bold.italic", ifelse(length(input$title_font) == 0, "plain", input$title_font) ) ),
          axis.title.x = element_text(size = input$x_size, 
                                      family = input$font_type,
                                      colour = input$x_color,
                                      face = ifelse(length(input$x_font) > 1, "bold.italic", ifelse(length(input$x_font) == 0, "plain", input$x_font) ) ),
          axis.title.y = element_text(size = input$y_size, 
                                      family = input$font_type,
                                      colour = input$y_color,
                                      face = ifelse(length(input$y_font) > 1, "bold.italic", ifelse(length(input$y_font) == 0, "plain", input$y_font) ) ),
          legend.text = element_text(size = input$legend_size, 
                                     family = input$legend_type,
                                     colour = input$legend_color,
                                     hjust = input$legend_alignment),
          legend.position = input$jelm_position
          
        )
    }


          
          
  output$Alap_arfolyama_colplot <- plotly::renderPlotly({
    plot_fuggveny(geom_col())
    
  })
  
  output$teszt <- renderText({ 
    input$title_alignment
  })

  
  
  
}

