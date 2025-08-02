# limpando o diretorio
remove(list = ls())

# removendo notacao cientifica
options(scipen = 999)

# importando bibliotecas ------------------------------------------------------

# ler dados eleitorais
library(electionsBR)

# manipulacao de dados
library(dplyr)

# variaveis categoricas
library(janitor)

# arquivos excel
library(readxl)
library(writexl)


# puxando dados eleitorais dos deputados federais eleitos em 2022 pelo ceara
eleicoes_ceara_2022 <- electionsBR::elections_tse(year = 2022, uf = 'CE', type = "vote_mun_zone")


# visualizando as variaveis
glimpse(eleicoes_ceara_2022)

# visualizando categorias
janitor::tabyl(eleicoes_ceara_2022$DS_CARGO)

janitor::tabyl(eleicoes_ceara_2022$DS_SIT_TOT_TURNO)

janitor::tabyl(eleicoes_ceara_2022$NR_TURNO)

# selecionando as variaveis relevantes
ceara_2022_edit <- eleicoes_ceara_2022 %>% 
  select(NM_URNA_CANDIDATO, DS_CARGO, SG_PARTIDO, NM_PARTIDO, DT_ELEICAO, NM_MUNICIPIO, CD_MUNICIPIO, QT_VOTOS_NOMINAIS, QT_VOTOS_NOMINAIS_VALIDOS, DS_SIT_TOT_TURNO)


# filtrando os deputados federais
deputados_2022 <- ceara_2022_edit %>% 
  filter(DS_CARGO == "Deputado Federal")

# filtrando deputados eleitos
deputados_eleitos <- deputados_2022 %>% 
  filter(DS_SIT_TOT_TURNO != 'NÃO ELEITO')

# removendo os suplentes
deputados_eleitos <- deputados_eleitos %>% 
  filter(DS_SIT_TOT_TURNO != 'SUPLENTE')

# agrupados os dados das seções eleitorais por municipios
# removendo duplicatas dos candidatos
deputados_eleitos_filtrados <- deputados_eleitos %>% 
  group_by(NM_URNA_CANDIDATO, SG_PARTIDO) %>% 
  mutate(total_votos = sum(QT_VOTOS_NOMINAIS_VALIDOS)) %>% 
  distinct(NM_URNA_CANDIDATO, .keep_all = T)

# filtrando os seguintes partidos: PL, PDT e PSDB
deputados_pl_pdt_psdb <- deputados_eleitos_filtrados %>% 
  filter(SG_PARTIDO %in% c('PL', 'PDT', 'PSDB')) %>% 
  arrange(SG_PARTIDO)

# colando o nome do partido junto do nome do candidato
deputados_pl_pdt_psdb <- deputados_pl_pdt_psdb %>%
  mutate(candidato_e_partido = paste0(NM_URNA_CANDIDATO, " (", SG_PARTIDO, ")"))

# reordenandoa as colunas
deputados_pl_pdt_psdb <- deputados_pl_pdt_psdb %>% 
  select(candidato_e_partido, everything())

remove(ceara_2022_edit, deputados_2022, eleicoes_ceara_2022, deputados_eleitos, deputados_eleitos_filtrados)
# salvando em excel 
writexl::write_xlsx(x = deputados_pl_pdt_psdb, path = "deputados_pl_pdt_psdb_2024.xlsx")




