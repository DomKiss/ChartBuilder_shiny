library(httr)
#url post
POST(
  url = "https://vizutechadmin.hunelco.com/v1/power_bi/Qomj7lJpGfPOoIgu/reports",
  content_type_json(),
  body = '{
  "name": "Riport_proba2",
  "url": "mintaurl.hu"
}'
)

#url get
GET1<- GET(url="https://vizutechadmin.hunelco.com/v1/power_bi/Qomj7lJpGfPOoIgu/reports")
content(GET1) 


urk <- "origo.hu"
proba <- paste0('{\n  \"name\": \"onBookmarked_proba\",\n  \"url\": \"', urk, '\"\n}')
lengths(proba)

POST(
  url = "https://vizutechadmin.hunelco.com/v1/power_bi/Qomj7lJpGfPOoIgu/reports",
  content_type_json(),
  body = proba
)
