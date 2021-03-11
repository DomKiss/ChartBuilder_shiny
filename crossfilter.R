l <- NULL
l$name <- c('b','e','d','b','b','d','e','e','b','b')
l$age <- c(20,20,21,21,20,22,22,30,21,32)
l$gender <- c('Female', 'Female', 'Male', 'Female', 'Male','Male', 
              'Female','Male',"Female","Male")
l <- as.data.frame(l)
l$name <- as.character(l$name)
l$age <- as.numeric(l$age)
l$gender <- as.character(l$gender)


l <- categories_df[3:5]
colnames(l) <- c("name", "age", "gender")
library(shiny)
library(shinyWidgets)
server <- shinyServer(function(input,output){
  
  assign('All Names',unique(sort(l$name)))
  assign("All Ages", unique(sort(l$age)))
  assign('All Genders', unique(sort(l$gender)))
  data1 <- reactive(l[which(l$name %in% if(exists(input$name))
  {get(input$name)}else{input$name}),])
  
  output$table1 <- renderTable(data1())
  output$text1 <- renderPrint(input$name)
  data2 <- reactive(data1()[which(data1()$age %in% if(exists(input$age))
  {get(input$age)}else{input$age}),])
  output$table2 <- renderTable(data2())
  data3 <- reactive(data2()[which(data2()$gender %in% if(exists(input$gender))
  {get(input$gender)}else{input$gender}),])
  
  output$table3 <- renderTable(data3())
  
  
  output$Box1 =  renderUI(
    if((is.null(input$age)) & (is.null(input$gender))){
      pickerInput("name", "Choose Name", choices=c("All Names",unique(sort(l$name))), selected = input$name, multiple=TRUE,  options = list(`live-search`=TRUE,
        `actions-box` = TRUE))
    } else{pickerInput("name", "Choose Name", choices=c("All Names",unique(l[l$gender %in% (if(exists(input$gender)){get(input$gender)}else{input$gender}) & l$age %in% (if(exists(input$age)){get(input$age)}else{input$age}) , "name"])), selected = input$name, multiple=TRUE,  options = list(`live-search`=TRUE,
      `actions-box` = TRUE))
    }
  )
  
  
  
  output$Box2 =  renderUI(
    if((is.null(input$name)) & (is.null(input$gender))){
      selectInput("age", "Choose Age", choices=c("All Ages", unique(sort(l$age))), selected = input$age)
    }else{selectInput("age", "Choose Age", choices=c("All Ages",unique(l[l$gender %in% (if(exists(input$gender)){get(input$gender)}else{input$gender}) & l$name %in% (if(exists(input$name)){get(input$name)}else{input$name}) , "age"])), selected = input$age)}
  )
  
  output$Box3 =  renderUI(
    if((is.null(input$name)) & (is.null(input$age))){
      selectInput("gender", "Choose Gender", choices=c("All Genders", unique(sort(l$gender))), selected = input$gender)
    }else{
      
      selectInput("gender", "Choose Gender", choices=c("All Genders", unique(l[l$name %in% (if(exists(input$name)){get(input$name)}else{input$name}) & l$age %in% (if(exists(input$age)){get(input$age)}else{input$age}), "gender"])), selected = input$gender, multiple = TRUE)
    }
  )
  
  
  
})

ui <-shinyUI(fluidPage(
  uiOutput("Box1"),
  uiOutput("Box2"),
  uiOutput("Box3"),
  tableOutput("table3")
))

shinyApp(ui,server)
