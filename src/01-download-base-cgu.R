library(tidyverse)
library(here)
library(lubridate)

#' cria uma pasta para gerenciar arquivos baixados
exdir <- here("data/temp")
unlink(exdir, recursive = TRUE)
dir.create(exdir)

#' params
years <- 2012:2022
zip_regex <- c("Recursos_Reclamacoes", "Pedidos")
url_base <- "https://dadosabertos-download.cgu.gov.br/FalaBR/Arquivos_FalaBR"

zips_cgu <- tibble(year = rep(years, 2)) %>%
  expand(year, zip_regex) %>%
  mutate(zip_file = str_glue("{zip_regex}_csv_{year}.zip"),
         url = str_glue("{url_base}/{zip_file}"),
         destfile = str_glue("{exdir}/{zip_file}"))

#' download !
download.file_safe <- safely(download.file)
walk2(zips_cgu$url, zips_cgu$destfile, download.file_safe, mode = "wb")

#' unzip ! terminando o processo o caminho dos arquivos ficam armazenados no objeto como `string`
unzip_safe <- safely(unzip)
walk(zips_cgu$destfile, unzip_safe, exdir = exdir)

files_cgu <- here("data/temp") %>%
  list.files(pattern = "csv$",full.names = TRUE) %>%
  as_tibble_col(column_name = "csv") %>%
  mutate(ano = as.integer(str_extract(csv, "\\d+(?=\\.csv)")),
         interacao = str_extract(csv, "(?<=\\d{8}_).+(?=_csv)"),
         base = case_when(
           interacao == "Pedidos" ~ "Pedidos",
           interacao == "Recursos_Reclamacoes" ~ "Recursos",
           TRUE ~ "Solicitantes"
         ))

#' extract data !
read_lai <- function(arquivo) {

  message(glue::glue("get {arquivo}"))

  if (stringr::str_detect(arquivo, "_Pedidos")) {
    colunas <- c(
      "id_pedido",
      "protocolo",
      "esfera",
      "uf",
      "municipio",
      "orgao",
      "situacao",
      "data_registro",
      "prazo",
      "foi_prorrogado",
      "foi_reencaminhado",
      "forma_resposta",
      "origem_da_solicitaca",
      "id_solicitante",
      "assunto",
      "sub_assunto",
      "tag",
      "data_resposta",
      "decisao",
      "especificacao_decisao"
    )
  } else if (stringr::str_detect(arquivo, "_Recursos")) {
    colunas <- c(
      "id_recurso",
      "id_recurso_precedente",
      "id_pedido",
      "id_solicitante",
      "protocolo_pedido",
      "esfera",
      "uf",
      "municipio",
      "orgao",
      "instancia",
      "situacao",
      "data_registro",
      "prazo_atendimento",
      "origem_solicitacao",
      "tipo_recurso",
      "data_resposta",
      "tipo_resposta"
    )
  } else if (stringr::str_detect(arquivo, "Solicitantes")) {
    colunas <- c(
      "id_solicitante",
      "tipo_demandante",
      "data_nascimento",
      "genero",
      "escolaridade",
      "profissao",
      "tipo_pessoa_juridica",
      "pais",
      "uf",
      "municipio"
    )
  } else {
    stop(glue::glue("{arquivo} is not a LAI file!"))
  }

  df <- read_delim(
    delim = ";",
    file = arquivo,
    col_names = colunas,
    col_types = cols(.default = col_character()),
    quote = '\'',
    na = c("", " ", "NA"),
    locale = locale(encoding = "UTF-16LE")
  )

  #' Base de dados de pedidos/recursos pronta para uso
  if (!str_detect(arquivo, "Solicitantes")) {
    df <- df %>%
      mutate(
        ts_registro = data_registro,
        ts_resposta = data_resposta,
        data_registro = dmy(data_registro) %>% floor_date(unit = "month"),
        data_resposta = dmy(data_resposta) %>% floor_date(unit = "month"),
        governo_que_respondeu = case_when(
          data_resposta < as_date("2015-01-01") ~ "Dilma I",
          data_resposta < as_date("2016-05-12") ~ "Dilma II",
          data_resposta < as_date("2019-01-01") ~ "Temer",
          TRUE ~ "Bolsonaro"
        ),
        governo_que_registrou = case_when(
          data_resposta < as_date("2015-01-01") ~ "Dilma I",
          data_registro < as_date("2016-05-12") ~ "Dilma II",
          data_registro < as_date("2019-01-01") ~ "Temer",
          TRUE ~ "Bolsonaro"
        ),
        governo_que_respondeu = factor(governo_que_respondeu, levels = c("Dilma I" ,"Dilma II", "Temer", "Bolsonaro")),
        governo_que_registrou = factor(governo_que_registrou, levels = c("Dilma I", "Dilma II", "Temer", "Bolsonaro"))
      )
  }

  return(df)

}

#' Base de dados simplificada da CGU:
#' * pedidos
#' * solicitantes_pedidos
#' * recursos_reclamacoes
#' * solicitantes_recursos
base_cgu <- files_cgu %>%
  mutate(datasets = map(csv, read_lai)) %>%
  transmute(
    base,
    datasets = map2(datasets, interacao, ~ mutate(.x, interacao = .y)),
    datasets = map2(datasets, ano, ~ mutate(.x, ano = .y))
  ) %>%
  group_by(base) %>%
  nest() %>%
  ungroup() %>%
  transmute(base, datasets = map(data, unnest, datasets))

#' metadados
read_metadados <- function (txt) {
  read_delim(txt, "\n", col_names = "texto",
             col_types = cols(.default = col_character()),
             locale = locale(encoding = "Latin1"))
}

meta <- "https://falabr.cgu.gov.br/publico/DownloadDados/{base_cgu$base}-Formato.txt" %>%
  str_glue() %>%
  map_df(read_metadados) %>%
  mutate(base = str_extract(texto, "(?<=-- CAMPOS: ).+$"),
         texto = str_remove(texto, "-")) %>%
  fill(base) %>%
  filter(str_detect(texto, "\\w"), !str_detect(texto, base)) %>%
  separate(texto, into = c("coluna", "tipo", "descricao"), sep = "(-|:)\\s") %>%
  mutate(across(everything(), str_squish), base = str_to_title(base)) %>%
  transmute(base, coluna, tipo, descricao) %>%
  group_by(base) %>%
  nest() %>%
  ungroup() %>%
  rename(metadados = data)

#' salva
base_cgu %>%
  left_join(meta) %>%
  saveRDS(here("data/base-cgu-atual.rds"))

#' remove temp dir
unlink(exdir, recursive = TRUE)
