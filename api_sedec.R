install.packages("httr")
install.packages("jsonlite")
install.packages("tidyverse")
install.packages("GET")
install.packages("RCurl")
install.packages("gt")

library(httr)
library(jsonlite)
library(tidyverse)
library(GET)
library(RCurl)
library(readxl)
library(gt)


#acessando API da SEDEC #para ocorrencias VISTORIADAS

headers = c(
  'apikey' = '$2a$12$npYO5Fi/P1Ra1sytSXna1ek3JWpw2ziLald5shyD88p7o7Ehk6ILu:$2a$12$BhpPo9bp7PF.mkFb6Qmm5.jqcFLBlxwRN1k0GICQDj7R8LqYodAEi'
)

res <- httr::VERB("GET", url = "https://e6s7lhadv2.execute-api.us-east-1.amazonaws.com/v1/ocorrencias-vistoriadas?inicio=2022-06-
01&fim=2022-06-10", 
                  httr::add_headers(headers))

cat(httr::content(res,'text'))

json_sedec_vistorias <- httr::content(res, as="text", encoding = "UTF-8")

bd_sedec_ocorencias_vistoriadas <- fromJSON(json_sedec_vistorias)%>%
  tibble::as_tibble()


#acessando API da SEDEC PARA INTERVENÇÕES REALIZADAS

headers = c(
  'apikey' = '$2a$12$npYO5Fi/P1Ra1sytSXna1ek3JWpw2ziLald5shyD88p7o7Ehk6ILu:$2a$12$BhpPo9bp7PF.mkFb6Qmm5.jqcFLBlxwRN1k0GICQDj7R8LqYodAEi'
)

res_intervencoes <- httr::VERB("GET", url = "https://e6s7lhadv2.execute-api.us-east-1.amazonaws.com/v1/intervencoes-realizadas?status=ASSINADA,PENDENTE", 
                  httr::add_headers(headers))

cat(httr::content(res_intervencoes,'text'))

json_sedec_intervencoes <- httr::content(res_intervencoes, as="text", encoding = "UTF-8")

bd_sedec_intervencoes <- fromJSON(json_sedec_intervencoes)%>%
  tibble::as_tibble()


#acessando API da SEDEC PARA PONTOS DE RISCO 


headers = c(
  "apikey" = "$2a$12$npYO5Fi/P1Ra1sytSXna1ek3JWpw2ziLald5shyD88p7o7Ehk6ILu:$2a$12$BhpPo9bp7PF.mkFb6Qmm5.jqcFLBlxwRN1k0GICQDj7R8LqYodAEi"
)
res_pontos_risco <- getURL("https://e6s7lhadv2.execute-api.us-east-1.amazonaws.com/v1/pontos-de-riscos?risk=R4", 
              .opts=list(httpheader = headers, followlocation = TRUE))

cat(res_pontos_risco)


#uma alternativa para transformar em Tibble
#tenho uma elemento com 10 listas e preciso formar um tibble

json_sedec_pontosrisco <- httr::content(res_pontos_risco, as="text", encoding = "UTF-8")

content(res_pontos_risco)

#NÃO CONSEGUI INTERNAMENTE, ESTOU PEGANDO EXTERNAMENTE COM O NUCELO DE NGEO

PONTOS_DE_RISCO_API_SEDEC <- read_excel("data/PONTOS_DE_RISCO_API_SEDEC.xlsx")







