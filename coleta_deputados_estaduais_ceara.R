# limpando o ambiente
remove(list = ls())


# importando bibliotecas
library(tidyverse)

library(rvest)


# pegando a URL
url <- "https://www.al.ce.gov.br/deputados"


# lendo a URL para os deputados
deputados_alece <- rvest::read_html(x = url) %>%
  html_elements(xpath = "/html/body/main/main/div/div/p[1]/a") %>%
  html_text() %>% 
  as.data.frame()

# tratando a base dos deputados
deputados_alece <- deputados_alece %>% 
  rename(deputados = ".") %>% 
  mutate(indice = seq.int(nrow(deputados_alece)),
         deputados = str_trim(deputados)) 


# lendo a URL para o partido dos deputados
partidos_deputados <- rvest::read_html(url) %>% 
  html_elements(xpath = '/html/body/main/main/div/div/p[2]') %>% 
  html_text() %>%
  as.data.frame()

# tratando a base dos partidos
partidos_deputados <- partidos_deputados %>% 
  rename(partido_deputados = ".") 

partidos_deputados <- partidos_deputados %>% 
  mutate(indice = seq.int(nrow(partidos_deputados)),
         partido_deputados = str_trim(partido_deputados)) 


# juntandoa as bases de dados
deputado_partido <- left_join(x = deputados_alece, y = partidos_deputados, by = "indice")

deputado_partido <- deputado_partido %>% 
  select(indice, deputados, partido = partido_deputados)

remove(partidos_deputados, deputados_alece, url)

# filtrando os partidos PSDB, PDT e PL

filtro_pdt_pl_psdb <- deputado_partido %>% 
  filter(partido %in% c('PSDB', 'PDT', 'PL')) %>% 
  arrange(partido)

janitor::tabyl(filtro_pdt_pl_psdb$deputados)







# juntando o nome e o partido na mesma coluna

pdt_pl_psdb_vigentes <- filtro_pdt_pl_psdb %>% 
  mutate(nome_e_partido = paste0(deputados, " (", partido, ")"))


library(writexl)
writexl::write_xlsx(x = pdt_pl_psdb_vigentes, path = "deputados_estaduais_pl_pdt_psdb.xlsx")


