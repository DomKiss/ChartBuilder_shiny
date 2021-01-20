#library(treemapify)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)




#szurok alkalmazasa categories tablan
server <- function(input, output, session) {
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = categories_df,
    vars = c(
      "ALAPKEZELO",
      "ALAP_NEVE",
      "LETETKEZELO",
      "BEFPOL_SZERINTI_KATEGORIA",
      "LETETKEZELO",
      "ALAPTIPUS",
      "ALAPFAJTA",
      "DEVIZALIS_KITETTSEG",
      "FOLDRAJZI_KITETTSEG",
      "EGYEB_KITETTSEG",
      "DEVIZANEM",
      "STATUSZ"
    )
  )

  
#beadom kulon reaktiv valtozoba a datumra szurt timeseries_df-et, es 
  #így mar csak akkor kell ujratoltenie, ha datumban van valtozas

react_ts <- reactive({ sqldf::sqldf(
  paste(
    "select * FROM timeseries_df WHERE DATUM >='",
    input$dateRange[1],
    "' AND DATUM<=", "'",
    input$dateRange[2], "'",
    "",
    sep = ""
  )
) })
  
 
  # leszurt ISINek kivalasztasa a datumra leszurt timeseries tablabol
react_isin_szurt <- reactive({
  react_ts() %>% filter(ISIN_KOD %in% res_mod()$ISIN_KOD)
})  

#ISIN kod, kivalasztott timeseries ertek kivalasztasa egy tablaba, majd ezt fogom mergelni a categories_df-hez  
react_selected <- reactive({
  react_isin_szurt() %>% select(ISIN_KOD, !!!input$valuevalaszt, DATUM)
})

#leszurom a categories tablat
react_categories_szurt_ISIN <- reactive({
  categories_df %>% filter(ISIN_KOD %in% res_mod()$ISIN_KOD)
}) 

#########################kerdes: csinalhatnam azt hogy az elejen mergelem az egeszet, igy utolag mar nem kell uj mergelest csinalni csak selectet

#merge abra_df (mergelem a kategoria szukseges oszlopat a timeseries oszlopokkal), 
#ha datum a tengely, akkor itt ne válassza ki mégegyszer, mert akkro egy uresre kicserelne a mar kivalasztott datum oszlopot, ezert van az ifelse elagazas
abra_df_react_dates_df_nelkul <- reactive(merge(
  if (input$tengelyvalaszt=="DATUM") {
    res_mod() %>% select(ISIN_KOD,!!!input$kategoriavalaszt)
  } else {
    res_mod() %>% select(ISIN_KOD, !!!input$kategoriavalaszt, !!!input$tengelyvalaszt)
  },
  react_selected(),
  by.x = "ISIN_KOD",
  by.y = "ISIN_KOD"
) %>% select(!ISIN_KOD))



#hozzaadom a datum oszlopot (ha napit ad hozzá, akor csak siman a datumot adom hozza, kuonben meg azt az oszlopot amit kivalaszt: utolso het, utolso honap stb)
abra_df_react_non_grouped <- reactive({
  if(input$datumszuro!="Napi"){
  merge(abra_df_react_dates_df_nelkul(), dates_df %>% 
          select(DATUM, which(colnames(dates_df)==input$datumszuro)), by="DATUM", all.x=TRUE, all.y=FALSE) %>% 
    filter(get(colnames(dates_df)[which(colnames(dates_df)==input$datumszuro)])==1)
} else {
  abra_df_react_dates_df_nelkul()
}})

#fuggveny, ami csinal group by tablat, amit majd összegzo fuggveny input alapjan hasznalunk
group_data <-function(df, y, x, fill, fv){
  fv <- get(fv)
  df2 <- as.data.frame(df %>% select(y, x, fill) %>%
    group_by(get(x), get(fill)) %>% 
    dplyr::summarise(sumArfolyam=fv(get(y))))
  df2 <- as.data.frame(df2)
  colnames(df2) <-if (x==fill) {
    c(x, paste(x, "2"), y)
  } else {
    c(x, fill, y)
  }
  
  return(df2)
}

#alkalmazom a group_data fuggvenyt
abra_df_react <-
  reactive({
    group_data(
      abra_df_react_non_grouped(),
      as.character(input$valuevalaszt),
      as.character(input$tengelyvalaszt),
      as.character(input$kategoriavalaszt),
      fv=input$osszegzofv
    )
  })

#mergelem a lekerdezett reactive time-series df valtozot a categories-zal
  output$tabletab <-
    #DT::renderDT(abra_df_react())
    DT::renderDT(DT::datatable(abra_df_react(),
                               selection = "none", 
                               extensions = 'Buttons',
                               option = list(
                                             autoWidth = TRUE, 
                                             dom = 'Blfrtip', 
                                             pageLength = -1,
                                             lengthMenu = list( c(10, 100, 1000, -1), c('10', '100', '1000', 'All') ),
                                             buttons = c('copy', 'csv', 'excel')
                                             )
                               ) 
                 )
  
#GGPLOTOS ABRA FUGGVENY
  plot_fuggveny <-
    function(plot_type = "base",
             kivalasztott_ertek = input$valuevalaszt,
             kategoria = input$kategoriavalaszt,
             x = merged_colnames[which(colnames(abra_df_react()) == input$tengelyvalaszt)],
             y = merged_colnames[which(colnames(abra_df_react()) == input$valuevalaszt)],
             fill = merged_colnames[which(colnames(abra_df_react()) == input$kategoriavalaszt)],
             area = "",
             label = "",
             subgroup=""
             
             ) {
      
      abra_df <- abra_df_react()
      merged_colnames <- colnames(abra_df)
      finished_plot <- 
      ggplot(
        data = abra_df,
        aes(
          x = 
            if (plot_type == "pie") {
              ""
            } else if (input$tengelyvalaszt == "DATUM") {
              as.Date(get(x))
            } else {
              get(x)
            },
          y = get(y),
          fill = get(fill)),
          width = 1,
          stat = "identity"
      )
      
      return(finished_plot)  
      
    }
  
  
  
  proba_plotly <-
    function(
      kivalasztott_ertek = input$valuevalaszt,
      kategoria = input$kategoriavalaszt,
      x = merged_colnames[which(colnames(abra_df_react()) == input$tengelyvalaszt)],
      y = merged_colnames[which(colnames(abra_df_react()) == input$valuevalaszt)],
      fill = merged_colnames[which(colnames(abra_df_react()) == input$kategoriavalaszt)]
    )
      {
      
      
      abra_df <- abra_df_react()
      merged_colnames <- colnames(abra_df)
      
      
      finished_plot <- 
        plotly::plot_ly(
          abra_df,
          x = ~
            if (input$tengelyvalaszt == "DATUM") {
              as.Date(get(x))
            } else {
              get(x)
            },
          y = ~ get(y),
          color = ~ get(fill),
          text = ~ get(y), 
          textposition = 'auto',
          colors = c(
            input$col_1,
            input$col_2,
            input$col_3,
            input$col_4,
            input$col_5
          ),
          type = 'bar'
        )

      return(finished_plot)
    }
  
  
  
  

  
  
  
  
  
  
  
  
  
  #PLOTLY ABRA FUGGVENY
  plot_fuggveny_plotly <-
    function(kivalasztott_ertek = input$valuevalaszt,
             kategoria = input$kategoriavalaszt) {
      abra_df <- abra_df_react()
      merged_colnames <- colnames(abra_df)
      
      
      finished_plot <- 
        plotly::plot_ly(
          abra_df,
          labels = abra_df[, which(colnames(abra_df_react()) == input$kategoriavalaszt)], 
          values = abra_df[, which(colnames(abra_df_react()) == input$valuevalaszt)],
          marker =
            list(colors = c(
              input$col_1,
              input$col_2,
              input$col_3,
              input$col_4,
              input$col_5,
              input$col_6,
              input$col_7,
              input$col_8,
              input$col_9,
              input$col_10
            )
          ),
          sort = T
        )
      
      return(finished_plot)
    }
  
  
  
  
design <-  function(temp_plot){
  

  layout(
    temp_plot,
    margin = list(t=60, r=50),
    
    plot_bgcolor = input$col_base,
    paper_bgcolor = input$col_base,
    
    xaxis = 
      list(
        showgrid = TRUE,
        gridcolor = ifelse(input$xgrid == TRUE, input$xgrid_color,input$col_base),
        
        linecolor = input$xscale_color, 
        linewidth = input$xscale_width,
        
        title = 
          paste("<br>",
                ifelse(length(input$x_font) == 0, input$x_text,
                       case_when(
                         length(input$x_font) > 1 ~ paste("<b><i>", input$x_text, "<i></b>"),
                         input$x_font == "bold" ~ paste("<b>", input$x_text, "<b>"),
                         input$x_font == "italic" ~ paste("<i>", input$x_text, "<i>")
                       )
                ),
                "<br> <br>"),
        titlefont = 
          list(
            family = input$font_type,
            size = input$x_size,
            color = input$x_color
          ),
        
        showticklabels = TRUE,
        tickcolor = ifelse(input$xscale_tick == TRUE, input$xscale_color, input$col_base), 
        tickangle = input$xscale_angle,
        tickfont =
          list(
            family = input$font_type,
            size = input$xscale_size,
            color = input$xscale_fontcolor
          )
      ),
    
    yaxis = 
      list(
        showgrid = TRUE,
        gridcolor = ifelse(input$ygrid == TRUE, input$ygrid_color,input$col_base),
        
        linecolor = input$yscale_color, 
        linewidth = input$yscale_width,
        
        title = 
          paste("<br>",
                ifelse(length(input$y_font) == 0, input$y_text,
                       case_when(
                         length(input$y_font) > 1 ~ paste("<b><i>", input$y_text, "<i></b>"),
                         input$y_font == "bold" ~ paste("<b>", input$y_text, "<b>"),
                         input$y_font == "italic" ~ paste("<i>", input$y_text, "<i>")
                       )
                ),
                "<br> <br>"),
        titlefont = 
          list(
            family = input$font_type,
            size = input$y_size,
            color = input$y_color
          ),
        
        showticklabels = TRUE,
        tickcolor = ifelse(input$yscale_tick == TRUE, input$yscale_color, input$col_base), 
        tickangle = input$yscale_angle,
        tickfont =
          list(
            family = input$font_type,
            size = input$yscale_size,
            color = input$yscale_fontcolor
          )
      ),
    
    
    legend =
      list(
        orientation = unlist(legend_orientation(input$legend_align, input$legend_adjustment))[1],
        x = unlist(legend_orientation(input$legend_align, input$legend_adjustment))[2],
        y = unlist(legend_orientation(input$legend_align, input$legend_adjustment))[3],
        xanchor = "center",
        yanchor = "center",
        
        font =
          list(
            family = input$font_type,
            size = input$legend_textsize,
            color = input$legend_textcolor
          ),
        title = 
          list(
            text = input$legend_titletext,
            font =
              list(
                family = input$font_type,
                size = input$legend_titlesize,
                color = input$legend_titlecolor
              )
          )
      ),
    showlegend = ifelse(input$legend_align == "none", FALSE, TRUE),
    
    title = 
      list(
        text = ifelse(length(input$title_font) == 0, input$title_text,
                      case_when(
                        length(input$title_font) > 1 ~ paste("<b><i>", input$title_text, "<i></b>"),
                        input$title_font == "bold" ~ paste("<b>", input$title_text, "<b>"),
                        input$title_font == "italic" ~ paste("<i>", input$title_text, "<i>")
                      )
        ),
        font =
          list(
            family = input$font_type,
            size = input$title_size,
            color = input$title_color
          ),
        x = input$title_align,
        standoff=20
      )
  )
  
}  
  
  
  

  #   
  # list(
  #   ggtitle(input$title_text),
  #   xlab( input$x_text ),
  #   ylab( input$y_text ),
  #   labs( fill = input$legend_titletext ),
    
    # geom_text(
    #   label = "input$valuevalaszt",
    #   hjust = 0.5,
    #   position = position_dodge(width = 0.75),
    #   size = input$label_size,
    #   color = input$label_color,
    #   fontface = ifelse(length(input$label_font) > 1, "bold.italic", ifelse(length(input$label_font) == 0, "plain", input$label_font) ),
    #   check_overlap = TRUE
    # ),
    
    # theme(
      #panel.background = element_rect(fill = input$col_base),
      # plot.title = element_text(size = input$title_size, 
      #                           family = input$font_type,
      #                           colour = input$title_color,
      #                           hjust = input$title_align,
      #                           face = ifelse(length(input$title_font) > 1, "bold.italic", ifelse(length(input$title_font) == 0, "plain", input$title_font) ) ),
      # axis.title.x = element_text(size = input$x_size,
      #                             vjust=1,
      #                             hjust=1,
      #                             family = input$font_type,
      #                             colour = input$x_color,
      #                             face = ifelse(length(input$x_font) > 1, "bold.italic", ifelse(length(input$x_font) == 0, "plain", input$x_font) ) ),
      # axis.title.y = element_text(size = input$y_size, 
      #                             family = input$font_type,
      #                             colour = input$y_color,
      #                             face = ifelse(length(input$y_font) > 1, "bold.italic", ifelse(length(input$y_font) == 0, "plain", input$y_font) ) ),
      # 
      # axis.text.x = element_text(size = input$xscale_size, 
      #                            family = input$font_type,
      #                            colour = input$xscale_fontcolor,
      #                            angle = -input$xscale_angle),
      # axis.ticks.x = element_line(color = ifelse(input$xscale_tick == TRUE, input$xscale_color, input$col_base) ),
      # axis.line.x = element_line(color = input$xscale_color, size = input$xscale_width),
      # 
      # axis.text.y = element_text(size = input$yscale_size, 
      #                            family = input$font_type,
      #                            colour = input$yscale_fontcolor,
      #                            angle = -input$yscale_angle),
      # axis.ticks.y = element_line(color = ifelse(input$yscale_tick == TRUE, input$yscale_color, input$col_base) ),
      # axis.line.y = element_line(color = input$yscale_color, size = input$yscale_width),
      
      # legend.text = element_text(size = input$legend_textsize,
      #                            family = input$font_type,
      #                            colour = input$legend_textcolor),
      # legend.position = input$legend_align,
      # legend.title = element_text(size = input$legend_titlesize,
      #                             family = input$font_type,
      #                             colour = input$legend_titlecolor),
      
      # panel.grid.major = element_line(color = ifelse(input$majorgrid == TRUE, input$majorgrid_color,input$col_base) ),
      # panel.grid.minor = element_line(color = ifelse(input$minorgrid == TRUE, input$minorgrid_color,input$col_base) )
    # )
    
  # )










legend_orientation <- function( input_orientation, input_legend_align ){
  position <- 
    case_when(
      input_orientation == "right" ~ list('v', 1.1, 0.5),
      input_orientation == "bottom" ~ list('h', 0.5, -input_legend_align)       
    )
  return(position)
}
  



 
    
  #bar output
  output$barplot <- plotly::renderPlotly({ 
    
    proba_plotly() %>% layout(barmode = 'stack' ) %>%
    design()
   
            

  })
  
  #clustered
  output$barplotclust <- plotly::renderPlotly({
    plot_fuggveny()+geom_bar(position="dodge", stat="identity")
  })
  
  
  #savdiagram
  output$savplot <- plotly::renderPlotly({
   
    plot_fuggveny()+geom_col()+coord_flip() 
    
  })
  
  #savdiagram clust
  output$savplotclust <- plotly::renderPlotly({
    plot_fuggveny()+geom_col(position="dodge", stat="identity")+coord_flip()
  })
  
  #point output
  output$pointplot <- plotly::renderPlotly({
    plot_fuggveny()+geom_point()
  })
  
  #area output
  output$areaplot <- plotly::renderPlotly({
    plot_fuggveny()+geom_area() 
  })
  
  #line output
  output$lineplot <- plotly::renderPlotly({
    plot_fuggveny()+geom_line() 
  })
  
  #pie output
  output$pieplot <- plotly::renderPlotly({
    plot_fuggveny_plotly() %>% plotly::add_pie() %>%
      design()
  })
  
  #donut output
  output$donutplot <- plotly::renderPlotly({
    plot_fuggveny_plotly() %>% plotly::add_pie(hole=0.6) %>%

    layout(
      margin = list(t=60, r=50),
      legend =
        list(
          orientation = unlist(legend_orientation(input$legend_align))[1],
          x = unlist(legend_orientation(input$legend_align))[2],
          y = unlist(legend_orientation(input$legend_align))[3],
          font =
            list(
              family = input$font_type,
              size = input$legend_textsize + 3,
              color = input$legend_textcolor
            ),
          title = 
            list(
              text = input$legend_titletext,
              font =
                list(
                  family = input$font_type,
                  size = input$legend_titlesize + 5,
                  color = input$legend_titlecolor
                )
            )
        ),
      showlegend = ifelse(input$legend_align == "none", FALSE, TRUE),
      title = 
        list(
          text = ifelse(length(input$title_font) == 0, input$title_text,
            case_when(
              length(input$title_font) > 1 ~ paste("<b><i>", input$title_text, "<i></b>"),
              input$title_font == "bold" ~ paste("<b>", input$title_text, "<b>"),
              input$title_font == "italic" ~ paste("<i>", input$title_text, "<i>")
            )
          ),
          font =
            list(
              family = input$font_type,
              size = input$title_size + 3,
              color = input$title_color
            ),
          x = input$title_align,
          standoff=20
        )
    )
    
  })
  
  
 
  #TREEMAP
  output$treemapplot <- renderPlot({
    ggplot(abra_df_react(), aes(area = get(colnames(abra_df_react())[which(colnames(abra_df_react()) == input$valuevalaszt)]), fill = get(colnames(abra_df_react())[which(colnames(abra_df_react()) == input$tengelyvalaszt)]), 
                                label = get(colnames(abra_df_react())[which(colnames(abra_df_react()) == input$kategoriavalaszt)]),
                          subgroup = get(colnames(abra_df_react())[which(colnames(abra_df_react()) == input$tengelyvalaszt)]))) +
    geom_treemap() +
    geom_treemap_subgroup_border() +
    geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                                 "black", fontface = "italic", min.size = 0) +
    geom_treemap_text(colour = "white", place = "topleft", reflow = T)
  })
  
}

shinyApp(ui, server)
