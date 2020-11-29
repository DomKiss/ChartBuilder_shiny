custom_theme <- 
theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = input$col),
  plot.title = element_text(size =
                              input$Cimsize, family = input$betutipus)
)