library(tidyverse)

#'TODO: documentar com roxigen

pega_anexos_cgu <- function(protocolo) {
  
  protocolo <- as.character(protocolo)
  url_base <- str_glue("http://www.consultaesic.cgu.gov.br/busca/_layouts/15/DetalhePedido/DetalhePedido.aspx?nup={protocolo}")
  message(url_base)
  
  script_tag <- url_base %>% 
    httr::GET() %>% 
    xml2::read_html() %>% 
    xml2::xml_find_all(xpath = "head") %>% 
    xml2::xml_find_all(xpath = "link") %>% 
    xml2::xml_attr("href") %>% 
    str_subset("busca.dados.Lists.Pedido") %>% 
    str_remove_all(".+\\?List=") %>% 
    paste0("http://www.consultaesic.cgu.gov.br/busca/dados/Lists/Pedido/Item/displayifs.aspx?List=", .) %>% 
    httr::POST() %>% 
    xml2::read_html(options = "NOCDATA") %>% 
    xml2::xml_find_all(xpath = "body/form/script[5]") %>%
    xml2::as_list() %>%
    enframe() %>% 
    unnest(value) %>%
    unnest(value) %>%
    pull(value) %>% 
    str_split("\r\n") %>%
    unlist() %>%
    as_tibble_col() %>%
    filter(str_count(value) == max(str_count(value)))

  attachments <- script_tag %>% 
    str_squish() %>% 
    stringi::stri_extract_all_regex('(?<=").*?(?=")') %>% 
    as_tibble_col() %>% 
    unnest(value) %>% 
    filter(str_count(value) > 10) %>%
    mutate(
      value = stringi::stri_unescape_unicode(value),
      is_url = str_detect(value, "http"),
      is_attachment_url = is_url & str_detect(value, "\\<a href=.+Attachments"),
      is_statement = str_detect(value, "^\\>")
    ) %>% 
    filter(is_attachment_url) %>%
    mutate(
      value = value %>% map_chr(~ xml2::read_xml(.x) %>% xml2::xml_attr("href"))
    ) %>% 
    distinct(value)
    
  return(attachments)
  
}

# Exeplo de uso:
# 
# # base de pedidos
# pedidos <- "data/1-base-cgu-filtrada.rds" %>% 
#   here() %>% 
#   readRDS() %>% 
#   filter(base == "Pedidos") %>% 
#   select(datasets) %>% 
#   unnest(datasets) %>% 
#   distinct()
# 
# # função para coletar url de anexo
# source(here("src/7-coletando-url-anexos.R"))
# pega_anexos_cgu_safely <- safely(pega_anexos_cgu)
# 
# # função para agregar url de anexos coletadas
# anexos_urls <- function(protocolo) {
#   protocolo %>% 
#     map(pega_anexos_cgu_safely) %>% 
#     set_names(protocolo) %>% 
#     map(pluck, 'result') %>% 
#     enframe(name = "protocolo") %>% 
#     unnest(value) %>% 
#     rename(url_anexo = value)
# }
# 
# # guardando tudo em uma tibble
# p <- pedidos %>% 
#   select(id_pedido, protocolo) %>% 
#   mutate(
#     lista_de_urls_de_anexo = map(protocolo, anexos_urls)
#   )