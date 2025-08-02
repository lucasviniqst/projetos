# limpando o enviroment
remove(list = ls())

# importando bibliotecas necessarias

# pacote para coletar dados de eleições
library(electionsBR)

# para manipular os dados
library(dplyr)

# trabalhar com categorias
library(janitor)

# segundo o GPT são 8 prefeituras com partidos como PDT, PL e PSDB
# 5 pdt
# 1 pl
# 2 psdb


# puxando as informações da eleicao no ceara no ano de 2024
ceara_2024 <- electionsBR::elections_tse(year = 2024, uf = 'CE', type = 'vote_mun_zone')

# visualizando todas as categorias
glimpse(ceara_2024)

# visualizando categorias
janitor::tabyl(ceara_2024$DS_CARGO)

janitor::tabyl(ceara_2024$DS_SIT_TOT_TURNO)

janitor::tabyl(ceara_2024$NR_TURNO)

# criando um novo dataframe selecionando as colunas desejadas

ceara_2024_edit <- ceara_2024 %>% 
  select(NM_URNA_CANDIDATO, DS_CARGO, SG_PARTIDO, NM_PARTIDO, DT_ELEICAO, NM_MUNICIPIO, CD_MUNICIPIO, QT_VOTOS_NOMINAIS, QT_VOTOS_NOMINAIS_VALIDOS, DS_SIT_TOT_TURNO)

# filtrando apenas os prefeitões
prefeitos_2024 <- ceara_2024_edit %>% 
  filter(DS_CARGO == "Prefeito")

# visualizando as situacão total do turno
tabyl(prefeitos_2024$DS_SIT_TOT_TURNO)


# filtrando prefeitos eleitos ou 2 turno
prefeitos_eleitos_2_t <- prefeitos_2024 %>% 
  filter(DS_SIT_TOT_TURNO != 'NÃO ELEITO')


# agrupados os dados das seções eleitorais por municipios
prefeitos_total_votos_distintos <- prefeitos_eleitos_2_t %>% 
  group_by(NM_MUNICIPIO, NM_URNA_CANDIDATO) %>% 
  mutate(total_votos = sum(QT_VOTOS_NOMINAIS_VALIDOS)) %>% 
  distinct(CD_MUNICIPIO, .keep_all = T)


# filtrando os seguintes partidos: PL, PDT e PSDB
prefeitos_pl_pdt_psdb <- prefeitos_total_votos_distintos %>% 
  filter(SG_PARTIDO %in% c('PL', 'PDT', 'PSDB'))


# colando o nome do partido junto do nome do candidato
# além disso removendo o candidato andre fernandes de moura, que foi no segundo turno pelo PL na capital e foi derrotado
prefeitos_pl_pdt_psdb <- prefeitos_pl_pdt_psdb[-1,] %>%
  mutate(candidato_e_partido = paste0(NM_URNA_CANDIDATO, " (", SG_PARTIDO, ")"))

# reordenandoa as colunas
prefeitos_pl_pdt_psdb <- prefeitos_pl_pdt_psdb %>% 
  select(candidato_e_partido, everything())

remove(ceara_2024, ceara_2024_edit, prefeitos_2024, prefeitos_eleitos_2_t, prefeitos_total_votos_distintos)
# salvando em excel 
writexl::write_xlsx(x = prefeitos_pl_pdt_psdb, path = "prefeitos_pl_pdt_psdb_2024.xlsx")

  
  
