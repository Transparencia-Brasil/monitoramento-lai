# helper
`%>%` <- magrittr::`%>%`

{
  #' regex que detecta URL
  rgx_urltag <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

  #' regex que detecta data (ex: dd/mm/aaaa)
  rgx_ddmmyyyy <- "\\b\\d{2}.\\d{2}.\\d{4}\\b"

  #' regex para quando a informação é inexistente:
  rgx_inexistentes <- c(
    "informac...? inexistente",
    "informac...? nao existe",
    "dados? inexistente",
    "dados? nao existe",
    "documentos? inexistente",
    "documentos? nao existe",
    "nao detem (a |as )informac",
    "nao detem (o |os )dado"
  ) %>% paste(collapse = "|")

  #' regex quando o pedido é dezarrazoado ou desproporcional
  rgx_desarrazoado <- c(
    "desarrazoado",
    "desproporcional"
  ) %>% paste(collapse = "|")

  #' regex quando o pedido é fishing expedition
  rgx_fishing <- c(
    "fishing"
  ) %>% paste(collapse = "|")

  #' regex quando o pedido é segurança nacional
  rgx_seguranca_nacional <- c(
    "seguranca nacional",
    "seguranca do estado"
  ) %>% paste(collapse = "|")

  #' regex quando o pedido é sigiloso
  rgx_sigilo <- c(
    "sigilo"
  ) %>% paste(collapse = "|")

  #' regex quando o pedido é processo decisório em curso
  rgx_decisao <- c(
    "processo decisorio em curso",
    "documentos? preparatorio"
  ) %>% paste(collapse = "|")

  #' regex quando o pedido exige trabalho adicional
  rgx_trabalho_adicional <- c(
    "trabalhos? adiciona",
    "tratamentos? adiciona"
  ) %>% paste(collapse = "|")

  #' regex quando o pedido é genérico
  rgx_generico <- c(
    "generico"
  ) %>% paste(collapse = "|")

  #' regex quando pedido alega dados pessoais
  rgx_dados_pessoais <- c(
    "dados? pessoa(l|is)",
    "informacoes pessoais",
    "informacao pessoal",
    "art 31",
    "artigo 31"
  ) %>% paste(collapse = "|")

  #' regex quando pedido menciona LGPD
  rgx_lgpd <- c(
    "lei geral de protecao de dados",
    "lei de protecao de dados pessoais",
    "lgpd",
    "13709",
    "13 709"
  ) %>% paste(collapse = "|")

  #' regex quando órgão não tem competência para responder
  rgx_nao_competencia <- c(
    "nao( sao de| e de| sao| e| tem) competencia"
  ) %>% paste(collapse = "|")

  #' regex quando o anexo está corrompido ou ilegível
  rgx_anexo_corrompido <- c(
    "anexo ilegivel",
    "anexo corrompido",
    "anexo nao consta",
    "nao( foi)? anex",
    "anexo faltante",
    "faltou anexar",
    "faltou o anexo",
    "sem( arquivo)? anexo",
    "arquivo nao encontrado",
    "nao e possivel acessar o anexo"
  ) %>% paste(collapse = "|")

  #' regex resposta incompleta
  rgx_resposta_incompleta <- c(
    "resposta incompleta",
    "parcialmente respondido",
    "faltaram informacoes",
    "faltam informacoes",
    "faltou informar",
    "pedido parcialmente atendido",
    "resposta apresentada nao contempla",
    "nao encaminhou( copia d..?)?( arquivo| documento| normativo)"
  ) %>% paste(collapse = "|")

  #' regex quando há menção à covid 19
  rgx_covid <- c(
    "(novo )?coronavirus",
    "covid.?19"
  ) %>% paste(collapse = "|")

  #' regex quando há menção a lai
  rgx_lai <- c(
    "lei de acesso a informacao",
    "lai",
    "12527",
    "12 527"
  ) %>% paste(collapse = "|")

  rgx_lista_sigilo <- list(

  )

}

#' `tibble` com regex para termos controversos
lista_de_regex <- tibble::tibble(
  regex_id = ls(pattern = "^rgx", envir = .GlobalEnv),
  regex = purrr::map_chr(regex_id, get, envir = .GlobalEnv),
  replacement = stringr::str_remove(regex_id, "^rgx_")
)

#' salva
saveRDS(lista_de_regex, here::here("data/lista-de-regex.rds"))

#' test
readRDS(here::here("data/lista-de-regex.rds"))
