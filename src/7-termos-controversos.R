library(tidyverse)
library(here)

base <- readRDS(here("data/base-cgu-filtrada.rds"))

ids_acesso_negado <- base %>%
  filter(base == "Pedidos") %>%
  select(datasets) %>%
  unnest(datasets) %>%
  filter(decisao == "Acesso Negado") %>%
  pull(id_pedido)

nr_protocolo <- base %>%
  filter(base == "Pedidos") %>%
  select(datasets) %>%
  unnest(datasets) %>%
  filter(decisao == "Acesso Negado") %>%
  select(id_pedido, protocolo)

pedidos  <- readRDS(here("data/pedidos-clean.rds"))
recursos <- readRDS(here("data/recursos-clean.rds"))

pedidos <- pedidos %>%
  filter(id_pedido %in% ids_acesso_negado) %>%
  tidylog::left_join(nr_protocolo) %>%
  transmute(
    id_pedido = id_pedido,
    protocolo = protocolo,
    data_registro = data_registro,
    data_resposta = data_resposta,
    orgao = orgao,
    detalhamento = detalhamento_clean,
    resposta_pedido = resposta_clean,
    resumo = resumo_clean,
    menciona_lai_pedido = str_detect(detalhamento_clean, "LAI"),
    menciona_lai_resposta = str_detect(resposta_clean, "LAI"),
    menciona_dados_pessoais_pedido = str_detect(detalhamento_clean, "dados_pessoais"),
    menciona_dados_pessoais_resposta = str_detect(resposta_clean, "dados_pessoais"),
    menciona_lgpd_pedido = str_detect(detalhamento_clean, "LGPD"),
    menciona_lgpd_resposta = str_detect(resposta_clean, "LGPD"),
    menciona_sigilo_pedido = str_detect(detalhamento_clean, "sigilo"),
    menciona_sigilo_resposta = str_detect(resposta_clean, "sigilo"),
    menciona_artigo_31_pedido = str_detect(detalhamento_clean, "(artigo 31|art 31)"),
    menciona_artigo_31_resposta = str_detect(resposta_clean, "(artigo 31|art 31)")
  )

pedidos %>%
  select(ends_with("_resposta"), -data_resposta) %>%
  filter(
    !(!menciona_lai_resposta &
      !menciona_sigilo_resposta &
      !menciona_dados_pessoais_resposta &
      !menciona_lgpd_resposta &
      !menciona_artigo_31_resposta)
  ) %>%
  filter(
    !(menciona_lai_resposta &
      !menciona_sigilo_resposta &
      !menciona_dados_pessoais_resposta &
      !menciona_lgpd_resposta &
      !menciona_artigo_31_resposta)
  ) %>% 
  count(
    menciona_lai_resposta,
    menciona_sigilo_resposta,
    menciona_dados_pessoais_resposta,
    menciona_lgpd_resposta,
    menciona_artigo_31_resposta,
    sort = TRUE
  ) %>% print(n = Inf)
