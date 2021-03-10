library(shiny)
library(dplyr)
library(ggplot2)
library(dbplyr)


base_data <- mtcars
# server ################################
server <- function(input, output, session){

  
  plot_fuggveny <-
    function(chart_tipus) {
     
      
      base_data %>%
      
      
        ggplot(aes(
          x = hp,
          y = sum(cyl) ,
          label = sum(cyl),
          color= mpg,
          shape = factor(cyl),
          fill = mpg
        )) +
        chart_tipus + 
        
        ggtitle(input$title_text) +
        xlab(input$x_text) +
        ylab(input$y_text) +
        
        geom_text(
          hjust = 0.5, 
          position = position_dodge(width = 0.75),
          size = input$label_size, 
          color = input$label_color,
          fontface = ifelse(length(input$label_font) > 1, "bold.italic", ifelse(length(input$label_font) == 0, "plain", input$label_font) ),
          check_overlap = TRUE
        ) +
        
        theme(
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
          
          axis.text.x = element_text(size = input$xscale_size, 
                                     family = input$font_type,
                                     colour = input$xscale_fontcolor,
                                     angle = -input$xscale_angle),
          axis.ticks.x = element_line(color = ifelse(input$xscale_tick == TRUE, input$xscale_color, input$col_base) ),
          axis.line.x = element_line(color = input$xscale_color, size = input$xscale_width),
          
          axis.text.y = element_text(size = input$yscale_size, 
                                     family = input$font_type,
                                     colour = input$yscale_fontcolor,
                                     angle = -input$yscale_angle),
          axis.ticks.y = element_line(color = ifelse(input$yscale_tick == TRUE, input$yscale_color, input$col_base) ),
          axis.line.y = element_line(color = input$yscale_color, size = input$yscale_width),
          
          legend.text = element_text(size = input$legend_size, 
                                     family = input$font_type,
                                     colour = input$legend_color),
          legend.position = input$legend_align,
          
          panel.grid.major = element_line(color = ifelse(input$majorgrid == TRUE, input$majorgrid_color,input$col_base) ),
          panel.grid.minor = element_line(color = ifelse(input$minorgrid == TRUE, input$minorgrid_color,input$col_base) )
        ) 
        #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
        #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
      
    }


          
          
  output$Alap_arfolyama_colplot <- plotly::renderPlotly({
    plot_fuggveny(geom_bar()) 
    
  })
  
  output$teszt <- renderText({ 
    input$title_alignment
  })

  
  
  
}

