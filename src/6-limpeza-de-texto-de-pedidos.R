library(tidyverse)
library(here)
library(glue)
library(lubridate)

source(here("src/5-funcao-limpando-texto.R"), encoding = "UTF-8")

base <- "data/base-cgu-filtrada.rds" %>% 
  readRDS()

cria_base_clean <- function(df, interacao, id_ano) {
  
  antes <- Sys.time()
  
  df <- df %>%
    filter(base == interacao) %>%
    select(datasets) %>%
    unnest(datasets) %>%
    filter(year(data_registro) == id_ano)
  
  if (interacao == "Pedidos") {
    df <- df %>% 
      transmute(
        id_pedido = id_pedido,
        data_registro = data_registro,
        data_resposta = data_resposta, 
        orgao = orgao,
        across(c(detalhamento, resposta, resumo), limpando_texto, .names = "{.col}_clean")
      )
  } else {
    df <- df %>% 
      transmute(
        id_pedido = id_pedido,
        id_recurso = id_recurso,
        data_registro = data_registro,
        data_resposta = data_resposta, 
        instancia = instancia,
        across(c(desc_recurso, resposta_recurso), limpando_texto, .names = "{.col}_clean")
      )
  }
  
  saveRDS(df, here(str_glue("data/{tolower(interacao)}-{id_ano}-clean.rds")))
  depois <- Sys.time()
  
  message(str_glue("Interação: {interacao} - {id_ano} limpa e salva !\n duração: {depois - antes}"))
  return(df)

}

p <- walk(2015L:2022L, ~ cria_base_clean(df = base, interacao = "Pedidos", id_ano = .x))
r <- walk(2015L:2022L, ~ cria_base_clean(df = base, interacao = "Recursos", id_ano = .x))

"data" %>%
  here() %>%
  list.files(pattern = "pedidos-\\d{4}-clean", full.names = TRUE) %>%
  map_df(readRDS) %>% 
  saveRDS(here("data/pedidos-clean.rds"))

"data" %>%
  here() %>%
  list.files(pattern = "recursos-\\d{4}-clean", full.names = TRUE) %>%
  map_df(readRDS) %>%
  saveRDS(here("data/recursos-clean.rds"))

