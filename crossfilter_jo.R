library(treemapify)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)


ui <- 
  fluidPage(fluidRow(
  column(
    width = 4,
    tags$h3("Szűrők beállítása"),
    bookmarkButton(),
    dateRangeInput('dateRange',
                   label = 'Dátum kivalasztasa',
                   start = Sys.Date() - 200, end = Sys.Date() - 150
    ),
    
    #kategoria, ertek kivalasztasa
    varSelectInput(
      "tengelyvalaszt",
      "Tengely:",
      merge(categories_df, timeseries_df %>% select(ISIN_KOD, DATUM)),
      multiple = FALSE,
      selected = "ALAPKEZELO"
    ),
    
    #datum havi heti stb. illetve datum aggregalva legyen-e?
    conditionalPanel("input.tengelyvalaszt=='DATUM'", 
                     selectInput("datumszuro", "datumszuro", c("UNAP_EV", "UNAP_NEGYEDEV", "UNAP_HONAP", "UNAP_HET", "Napi"), selected="Napi"),
                     ),
    #switch: aggergalva legyen-e
    #conditionalPanel("input.tengelyvalaszt=='DATUM'",materialSwitch(inputId = "datumagr", label = "Időben összevonva", status = "összevonva")),
    conditionalPanel("input.tengelyvalaszt=='DATUM'", materialSwitch(inputId = "datumagr", label = "Időben összevonva", status = "primary")),
    #kategoria, ertek kivalasztasa
    varSelectInput(
      "kategoriavalaszt",
      "Jelmagyarázat:",
      categories_df,
      multiple = FALSE,
      selected = "BEFPOL_SZERINTI_KATEGORIA"
    ),
    varSelectInput(
      "valuevalaszt",
      "Értékek",
      timeseries_df,
      multiple = FALSE,
      selected = "NETTO_ESZKOZERTEK"
    ),
    
    
    selectInput("osszegzofv", "Összegző függvény", c("mean", "sum", "count", "max", "min"), selected="sum"),
    
  
    panel(
      selectizeGroupUI(
        id = "my-filters",
        inline = FALSE,
        params = list(
          ALAPKEZELO = list(inputId = "ALAPKEZELO", title = "Alapkezelő:"),
          ALAP_NEVE = list(inputId = "ALAP_NEVE", title = "Alap neve:"),
          BEFPOL_SZERINTI_KATEGORIA = list(inputId = "BEFPOL_SZERINTI_KATEGORIA", title = "Befektetési politika:"),
          LETETKEZELO = list(inputId = "LETETKEZELO", title = "Letétkezelő"),
          ALAPTIPUS = list(inputId = "ALAPTIPUS", title = "Alapítpus:"),
          ALAPFAJTA = list(inputId = "ALAPFAJTA", title = "Alapfajta:"),
          DEVIZALIS_KITETTSEG = list(inputId = "DEVIZALIS_KITETTSEG", title = "Devizális kitettség:"),
          FOLDRAJZI_KITETTSEG = list(inputId = "FOLDRAJZI_KITETTSEG", title = "Földrajzi kitettség:"),
          EGYEB_KITETTSEG = list(inputId = "EGYEB_KITETTSEG", title = "Egyéb kitettség:"),
          DEVIZANEM = list(inputId = "DEVIZANEM", title = "Devizanem:"),
          STATUSZ = list(inputId = "STATUSZ", title = "Státusz:")
        )
      ),
      status = "primary"
    ),
     
   
  ),
  mainPanel(tabsetPanel(
    #line chart
    tabPanel('Táblázat', DT::DTOutput("tabletab")),
    tabPanel('Halmozott oszlop', plotly::plotlyOutput('barplot')),
    tabPanel('Csoportosított oszlop', plotly::plotlyOutput('barplotclust')),
    tabPanel('Csoportosított sávdiagram', plotly::plotlyOutput('savplot')),
    tabPanel('Halmozott sávdiagram', plotly::plotlyOutput('savplotclust')),
    tabPanel('Pontdiagram', plotly::plotlyOutput('pointplot')),
    tabPanel('Területdiagram', plotly::plotlyOutput('areaplot')),
    tabPanel('Vonaldiagram', plotly::plotlyOutput('lineplot')),
    tabPanel('Kördiagram', plotly::plotlyOutput('pieplot')),
    tabPanel('Fánkdiagram', plotly::plotlyOutput('donutplot')),
    tabPanel('Treemap', plotOutput('treemapplot'))
   
  ))
    #column(width=4, DT::DTOutput("tabletab"), plotly::plotlyOutput('Alap_arfolyama_plot'))
  
))

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
    DT::renderDT(abra_df_react())
  
#GGPLOTOS ABRA FUGGVENY
  plot_fuggveny <-
    function(kivalasztott_ertek = input$valuevalaszt,
             kategoria = input$kategoriavalaszt,
             x = merged_colnames[which(colnames(abra_df_react()) == input$tengelyvalaszt)],
             y = merged_colnames[which(colnames(abra_df_react()) == input$valuevalaszt)],
             fill = merged_colnames[which(colnames(abra_df_react()) == input$kategoriavalaszt)],
             area = "",
             label = "",
             subgroup="") {
      abra_df <- abra_df_react()
      merged_colnames <- colnames(abra_df)
      
      return(
        ggplot(
          data = abra_df,
          aes(
            x = if (input$tengelyvalaszt == "DATUM") {
              as.Date(get(x))
            } else {
              get(x)
            },
            y = get(y),
            fill = get(fill))
          ,
          width = 1,
          stat = "identity"
        ))  #+
        #   stat_summary(fun = input$osszegzofv, geom = chart_tipus)
      #)
    }
  #PLOTLY ABRA FUGGVENY
  plot_fuggveny_plotly <-
    function(kivalasztott_ertek = input$valuevalaszt,
             kategoria = input$kategoriavalaszt) {
      abra_df <- abra_df_react()
      merged_colnames <- colnames(abra_df)
      
      return(plotly::plot_ly(abra_df,
                             labels = abra_df[, which(colnames(abra_df_react()) == input$kategoriavalaszt)], 
                             values = abra_df[, which(colnames(abra_df_react()) == input$valuevalaszt)]))
    }
 
    
  #bar output
  output$barplot <- plotly::renderPlotly({
    plot_fuggveny()+geom_bar(stat='identity')
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
    plot_fuggveny_plotly() %>% plotly::add_pie()
    
  })
  #donut output
  output$donutplot <- plotly::renderPlotly({
    plot_fuggveny_plotly() %>% plotly::add_pie(hole=0.6)
    
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
