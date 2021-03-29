library(shiny)
jscode <-
  "Shiny.addCustomMessageHandler('mymessage', function(message) { window.location = message;});"
proba_link <-
  "http://befalapok.vizu-tech.com/chartbuilder?_state_id_=8ef11484d87bffd8?companyId=Qomj7lJpGfPOoIgu&uid=qomj7lJpGfPOoIgu&categoryId=qomj7lJpGfPOoIg3"

proba_link2 <-
  "http://befalapok.vizu-tech.com/chartbuilder?companyId=Qomj7lJpGfPOoIgu&uid=qomj7lJpGfPOoIgu&categoryId=qomj7lJpGfPOoIg3"



#ha nem lenne state_id, vágjam le a feleslegesen rarakott uid-t
url_splitter <- function(url) {
  proba_link_splited <-  stringr::str_split(url, "\\&")
  link_state_category <- proba_link_splited[[1]][1]
  link_and_state_id <- paste(unlist(stringr::str_split(as.character(link_state_category), "\\?"))[1],
                             unlist(stringr::str_split(as.character(link_state_category), "\\?"))[2], sep="?")
  link_no_state_id <-
    unlist(stringr::str_split(as.character(link_state_category), "\\?"))[1]
  
  companyID <- ifelse(stringr::str_detect(url, "state_id") == TRUE,
                      unlist(stringr::str_split(as.character(link_state_category), "\\?"))[3],
                      unlist(stringr::str_split(as.character(link_state_category), "\\?"))[2])
  
  companyID <- (stringr::str_split(companyID, "\\=") %>% unlist)[2]
  
  uid <-
    ifelse(
      stringr::str_detect(url, "state_id") == TRUE,
      proba_link_splited[[1]][2],
      proba_link_splited[[1]][2]
    )
  uid <- (stringr::str_split(uid, "\\=") %>% unlist)[2]
  category_id <-
    ifelse(
      stringr::str_detect(url, "state_id") == TRUE,
      proba_link_splited[[1]][3],
      proba_link_splited[[1]][3]
    )
  category_id <- (stringr::str_split(category_id, "\\=") %>% unlist)[2]
  vegso_link <-
    ifelse(
      stringr::str_detect(url, "state_id") == TRUE,
      link_and_state_id,
      link_no_state_id
    )
  return(c(companyID, uid, category_id, vegso_link))
}

ui <- fluidPage(tags$head(tags$script(jscode)))

server <- function(input, output, session) {
  url <-url_splitter(proba_link)[3]
  
  
  session$sendCustomMessage("mymessage", url)
}

shinyApp(ui, server)
