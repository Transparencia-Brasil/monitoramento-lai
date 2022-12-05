# load libs --------------------------------------------------------------------
library(here)
library(tidyverse)
library(lubridate)

# base pedidos
#  |- possui_recurso?
#  |- possui_anexo?
#  |- resultados - bert/nlp

# Possui recurso? --------------------------------------------------------------

# Base de dados de recursos impetrados em 2021 e 2022
recursos_full <- "data/base-cgu-filtrada.rds" %>%
  here() %>%
  readRDS() %>%
  filter(base == "Recursos") %>%
  select(datasets) %>%
  unnest(datasets) %>%
  filter(id_ano %in% c("2021", "2022")) %>%
  select(id_pedido) %>%
  distinct(id_pedido) %>%
  mutate(possui_recurso = TRUE)

# Possui anexo? ----------------------------------------------------------------

# Base de dados de anexos de pedidos em 2021 e 2022
anexos <- "data/lista-de-anexos.rds" %>%
  here() %>%
  readRDS() %>%
  mutate(possui_anexo = if_else(
    map_int(lista_de_urls_de_anexo, nrow) == 0, FALSE, TRUE
  )) %>%
  select(id_pedido, possui_anexo)

# Base BERT/NLP ----------------------------------------------------------------

# Função para ler e empilhar base de dados com resultados do modelo BERT/NLP
gera_base_bert <- function(csv) {
  read_csv(
    csv,
    col_select = c("id_pedido", "resultados"),
    col_types = cols(.default = col_character())
  ) %>% distinct()
}

# Base de dados com resutaltados do modelo BERT/NLP para 2021 e 2022
base_bert <- "data" %>%
  here() %>%
  list.files(pattern = "202[1|2]p-classificado", full.names = T) %>%
  map_df(gera_base_bert)

# Base de pedidos --------------------------------------------------------------

# Função para reduzir número de colunas e tornar base de pedidos mais leve
remove_colunas_nao_usadas_da_tabela_pedidos <- function(df) {
  df %>%
    select(id_pedido, protocolo, orgao, data_registro, decisao) %>%
    distinct()
}

# Base de dados de pedidos em 2021 e 2022
pedidos_full <- "data/base-cgu-filtrada.rds" %>%
  here() %>%
  readRDS() %>%
  filter(base == "Pedidos") %>%
  transmute(datasets = map(datasets, remove_colunas_nao_usadas_da_tabela_pedidos)) %>%
  unnest(datasets) %>%
  filter(year(data_registro) %in% c("2021", "2022")) %>%
  mutate(decisao_valida = decisao %in% c(
    "Acesso Concedido", "Acesso Negado", "Acesso Parcialmente Concedido"
  )) %>%
  left_join(recursos_full) %>%
  left_join(anexos) %>%
  left_join(base_bert) %>%
  mutate(
    possui_recurso = replace_na(possui_recurso, FALSE),
    pedido_valido = if_else(
      decisao_valida & !possui_recurso & !possui_anexo, TRUE, FALSE
    ),
    motivo_pedido_nao_valido = case_when(
      !decisao_valida & !possui_recurso & is.na(possui_anexo) ~ "Não processado",
      !decisao_valida ~ "Decisão (FalaBr/CGU) inválida\n(Pedido descartado)",
      decisao_valida & !possui_recurso & !possui_anexo ~ "Pedido válido",
      decisao_valida & possui_recurso ~ "Pedidos com recursos\n(Pedido descartado)",
      decisao_valida & !possui_recurso & possui_anexo ~ "Pedido com anexo\n(Pedido descartado)",
      TRUE ~ "Não processado"
    ),
    orgao_abrev = case_when(
      # Abreviando órgãos
      orgao == "Transportadora Brasileira Gasoduto Bolívia-Brasil S.A." ~ "TBG",
      orgao == "Petrobras Biocombustível S.A." ~ "PETROBRAS-BIOCOMBUSTÍVEL",
      orgao == "Petrobras Gás S.A." ~ "PETROBRAS-GÁS",
      orgao == "Pré-Sal Petróleo S.A - Empresa Brasileira de Administração de Petróleo e Gás Natural S.A." ~ "PRÉ-SAL",
      orgao == "Companhia de Geração e Transmissão de Energia Elétrica do Sul do Brasil - CGT ELETROSUL" ~ "CGT-ELETROSUL",
      orgao == "Autoridade Portuária de Santos S.A." ~ "AUTORIDADE-PORTUÁRIA",
      orgao == "BB Tecnologia e Serviços" ~ "BB-TEC",
      orgao == "Petrobras Logística de Exploração e Produção S.A." ~ "PETROBRAS-LOGISTICA",
      # a regex abaixo é para capturar a abreviação dos órgãos EBSERH
      str_detect(orgao, "^EBSERH") ~ str_extract(orgao, "[A-Z]+ . ([A-Z]+\\s?.\\s?[A-Z]+)?(sede)?( \\([A-Z]+\\))?"),
      # a regex abaixo é para capturar a abreviação dos demais órgãos
      TRUE ~ str_extract(orgao, "[A-Z-a-z]+\\S*( [A-Z]+)?")
    ),
    # aparando strings
    orgao = str_remove(orgao, "\\s+[A-Z]$") %>% str_squish()
  ) %>%
  filter(motivo_pedido_nao_valido != "Não processado", !is.na(decisao))

saveRDS(pedidos_full, here("data/pedidos-classificados.rds"))
