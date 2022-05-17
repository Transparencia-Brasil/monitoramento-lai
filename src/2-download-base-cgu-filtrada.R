library(tidyverse)
library(here)
library(lubridate)

#' params
years <- 2015:2022
urls <- "https://dadosabertos-download.cgu.gov.br/FalaBR/Arquivos_FalaBR_Filtrado/Arquivos_csv_{years}.zip"
zip_files <- here("data/Arquivos_csv_{years}.zip")
data_ref <- str_remove_all(Sys.Date(), "-")
exdir <- here("data")

dwld <- tibble(url = str_glue(urls),
               year = years,
               zip_file = str_glue(zip_files),
               exdir = exdir)

#' diretório para guardar dados
dir.create(exdir)

#' download
download.file_safe <- safely(download.file)
download_log <- walk2(dwld$url, dwld$zip_file, ~ download.file_safe(url = .x, destfile = .y, mode = "wb"))

#' unzip
unzip_safe <- safely(unzip)
unzip_log <- walk2(dwld$zip_file, dwld$exdir, ~ unzip_safe(zipfile = .x, exdir = .y))

# read files
read_lai <- function(arquivo) {
  
  message(str_glue("\nget {arquivo}"))
  
  if (str_detect(arquivo, "Pedidos")) {
    colunas <- c(
      "id_pedido",
      "protocolo",
      "esfera",
      "orgao",
      "situacao",
      "data_registro",
      "resumo",
      "detalhamento",
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
      "resposta",
      "decisao",
      "especificacao_decisao"
    )
  } else if (str_detect(arquivo, "Recursos")) {
    colunas <- c(
      "id_recurso",
      "id_recurso_precedente",
      "desc_recurso",
      "id_pedido",
      "id_solicitante",
      "protocolo_pedido",
      "orgao_destinatario",
      "instancia",
      "situacao",
      "data_registro",
      "prazo_atendimento",
      "origem_solicitacao",
      "tipo_recurso",
      "data_resposta",
      "resposta_recurso",
      "tipo_resposta"
    )
  } else if (str_detect(arquivo, "Solicitantes")) {
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
    stop(str_glue("{arquivo} is not a LAI file!"))
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
          data_resposta < as_date("2016-05-12") ~ "Dilma II",
          data_resposta < as_date("2019-01-01") ~ "Temer",
          TRUE ~ "Bolsonaro"
        ),
        governo_que_registrou = case_when(
          data_registro < as_date("2016-05-12") ~ "Dilma II",
          data_registro < as_date("2019-01-01") ~ "Temer",
          TRUE ~ "Bolsonaro"
        ),
        governo_que_respondeu = factor(governo_que_respondeu, levels = c("Dilma II", "Temer", "Bolsonaro")),
        governo_que_registrou = factor(governo_que_registrou, levels = c("Dilma II", "Temer", "Bolsonaro"))
      )
  }
  
  return(df)
  
}

#' local file path
getfiles_lai <- function(base) list.files(here("data"), pattern = base, full.names = T) %>% map_df(read_lai)

#' datasets
datasets <- c(
  "Pedidos",
  "Recursos",
  "Solicitantes"
  )

base_cgu <- datasets %>%
  map(getfiles_lai) %>%
  set_names(datasets) %>%
  enframe(name = "base", value = "datasets")

meta <- "http://www.consultaesic.cgu.gov.br/arquivosRelatorios/PedidosRespostas/Dicionario-Dados-Exportacao.txt" %>% 
  read_delim("\n", col_names = "texto", col_types = cols(.default = col_character())) %>% 
  mutate(base = str_extract(texto, "(?<=-- CAMPOS: ).+$"),
         texto = str_remove(texto, "-")) %>% 
  fill(base) %>% 
  filter(str_detect(texto, "\\w"), !str_detect(texto, base)) %>% 
  separate(texto, into = c("coluna", "tipo", "descricao"), sep = "(-|:)\\s") %>% 
  mutate(across(everything(), str_squish), base = str_to_title(base)) %>% 
  select(base, coluna, tipo, descricao) %>% 
  group_by(base) %>% 
  nest() %>% 
  ungroup() %>% 
  rename(metadados = data)

base_cgu %>% 
  enframe(name = "base", value = "datasets") %>% 
  left_join(meta) %>% 
  saveRDS(here("data/base-cgu-filtrada.rds"))

#' remove extra datasets
"data" %>% 
  here::here() %>% 
  list.files(pattern = "_csv_|zip$", full.names = TRUE) %>%
  purrr::walk(unlink)
