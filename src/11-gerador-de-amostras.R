pedidos_full <- "data/1-base-cgu-filtrada.rds" %>%
  here() %>%
  readRDS() %>%
  filter(base == "Pedidos") %>%
  select(datasets) %>%
  unnest(datasets) %>%
  filter(id_ano %in% c("2021", "2022"))

pedidos_full %>%
  filter(str_detect(orgao, "SGPR"))

pedidos <- readRDS(here("data/classificador/pedidos-classificados.rds"))

pedidos_full %>%
  filter(
    id_pedido %in% (pedidos %>%
      filter(orgao_abrev == "SGPR") %>%
        filter(pedido_valido) %>%
        filter(decisao == "Acesso Concedido" & resultados == "Não Atendido") %>%
      pull(id_pedido))
  ) %>%
  filter(
    !str_detect(detalhamento, "fraude|Fraude|miami|Miami|eleições|provas|0?7/03/2020")
  ) %>%
  left_join(select(pedidos, id_pedido, resultados), by = "id_pedido") %>%
  transmute(
    id_pedido,
    `Classificação NLP está correta?` = NA,
    `Qual deveria ser a classificação correta?` = NA,
    `Meio de atendimento` = NA,
    `Justificativa/Detalhes` = NA,
    `Decisão do órgão` = decisao,
    `Classificação Modelo NLP` = resultados,
    `Órgão` = orgao,
    `Conteúdo Pedido` = detalhamento,
    `Conteúdo Resposta` = resposta,
    `Data registro` = data_registro,
    `Link e-sic` = str_glue('=HIPERLINK("http://www.consultaesic.cgu.gov.br/busca/_layouts/15/DetalhePedido/DetalhePedido.aspx?nup={protocolo}"; "LINK E-SIC")')
  ) %>%
  googlesheets4::write_sheet("1gLRXoeQ7OZUL3Fq78KNcZiUcsTbUPVKPFge4OXTrKQ4", sheet = "SGPR - DEMAIS")

sgpr <- read_amostra("SGPR - DEMAIS")
sgpr

read_amostra()




pedidos_full %>%
  filter(
    id_pedido %in% (pedidos %>%
      filter(orgao_abrev == "CEX") %>%
      filter(pedido_valido) %>%
      filter(decisao == "Acesso Concedido" & resultados == "Não Atendido") %>%
      pull(id_pedido))
  ) %>%
  left_join(select(pedidos, id_pedido, resultados), by = "id_pedido") %>%
    sample_n(20) %>%
     transmute(
       id_pedido,
       `Classificação NLP está correta?` = NA,
       `Qual deveria ser a classificação correta?` = NA,
       `Meio de atendimento` = NA,
       `Justificativa/Detalhes` = NA,
       `Decisão do órgão` = decisao,
       `Classificação Modelo NLP` = resultados,
       `Órgão` = orgao,
       `Conteúdo Pedido` = detalhamento,
       `Conteúdo Resposta` = resposta,
       `Data registro` = data_registro,
       `Link e-sic` = str_glue('=HIPERLINK("http://www.consultaesic.cgu.gov.br/busca/_layouts/15/DetalhePedido/DetalhePedido.aspx?nup={protocolo}"; "LINK E-SIC")')
     ) %>%
       googlesheets4::write_sheet("1gLRXoeQ7OZUL3Fq78KNcZiUcsTbUPVKPFge4OXTrKQ4", sheet = "CEX")

pedidos_full %>%
  filter(
    id_pedido %in% (pedidos %>%
      filter(orgao_abrev == "COMAER") %>%
      filter(pedido_valido) %>%
      filter(decisao == "Acesso Concedido" & resultados == "Não Atendido") %>%
      pull(id_pedido))
  ) %>%
  left_join(select(pedidos, id_pedido, resultados), by = "id_pedido") %>%
    sample_n(20) %>%
     transmute(
       id_pedido,
       `Classificação NLP está correta?` = NA,
       `Qual deveria ser a classificação correta?` = NA,
       `Meio de atendimento` = NA,
       `Justificativa/Detalhes` = NA,
       `Decisão do órgão` = decisao,
       `Classificação Modelo NLP` = resultados,
       `Órgão` = orgao,
       `Conteúdo Pedido` = detalhamento,
       `Conteúdo Resposta` = resposta,
       `Data registro` = data_registro,
       `Link e-sic` = str_glue('=HIPERLINK("http://www.consultaesic.cgu.gov.br/busca/_layouts/15/DetalhePedido/DetalhePedido.aspx?nup={protocolo}"; "LINK E-SIC")')
     ) %>%
       googlesheets4::write_sheet("1gLRXoeQ7OZUL3Fq78KNcZiUcsTbUPVKPFge4OXTrKQ4", sheet = "COMAER")


pedidos_full %>%
  filter(str_detect(detalhamento, "ExpoDubai")) %>%
  glimpse()

pedidos %>%
  filter(str_detect(orgao, "Comando"))

rm(pedidos_full)
