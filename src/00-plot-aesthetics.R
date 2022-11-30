#' Código que apronta objetos e funções que definem o layout dos gráficos com a
#' identidade visual da Transparência Brasil e do Achados e Pedidos:

#' Paleta de cores Achados e pedidos
cores_aep <- c(
  laranja = "#F9A521",
  rosa = "#D81755",
  cinza = "#969696",
  marrom = "#B27D5C"
)

#' Paleta de cores Transparência Brasil
cores_tb <- c(
  laranja = "#F6A323",
  cinza_escuro = "#1d1d1b",
  cinza_claro = "#6f7171",
  cinza_quase_branco = "#ececec",
  azul = "#41ACBD"
)

#' Paleta de cores para decisões de acesso a informação
cores_decisao <- c(
  "Acesso Concedido" = cores_tb[["azul"]],
  "Não se trata de solicitação de informação" = cores_aep[["marrom"]],
  "Acesso Negado" = cores_aep[["rosa"]],
  "Acesso Parcialmente Concedido" = cores_aep[["laranja"]],
  "Pergunta Duplicada/Repetida" = cores_aep[["cinza"]],
  "Órgão não tem competência para responder sobre o assunto" = cores_tb[["cinza_escuro"]],
  "Informação Inexistente" = cores_tb[["cinza_quase_branco"]]
)

#' variações de paleta para combinar `fill`e `color` aesthetics no ggplot
cores_decisao2 <- cores_decisao
cores_decisao2[["Informação Inexistente"]] <- "gray20"

cores_decisao3 <- c("black", "gray90", "gray20", "gray80", "gray20")
names(cores_decisao3) <- names(cores_decisao)[-c(5, 2)]

#' Paleta de cores para difierenciar pedidos LAI de outras manifestações no FalaBr
cores_lai <- tibble(
  c1 = c("Não se trata de solicitação de informação",
         "Pergunta Duplicada/Repetida",
         "Pedidos de acesso a informação via LAI"
  ) %>% str_wrap(25),
  c2 = c("#F9A521", "#969696", "#D81755")
) %>% deframe()

#' Paleta de cores para instâncias recursais
cores_instancia <- c(
  "Primeira Instância" = cores_tb[["azul"]],
  "Segunda Instância" = cores_aep[["laranja"]],
  "CGU" = cores_aep[["marrom"]],
  "CMRI" = cores_aep[["rosa"]]
)

ord_instancia <- ordered(names(cores_instancia), levels = names(cores_instancia))

#' Cores classificação de atendimento
cores_classificacao <- c(
  "Atendido" = cores_tb[["azul"]],
  "Não Atendido" = cores_aep[["rosa"]],
  "Parcialmente Atendido" = cores_aep[["laranja"]]
)

ord_classificacao <- ordered(names(cores_classificacao), levels = c(
  "Atendido", "Parcialmente Atendido", "Não Atendido"
))

#' Paleta de cores para tipos de respostas aos recursos de pedidos LAI
cores_tipo_resposta <- c(
  cores_tb[["azul"]],
  alpha(cores_tb[["azul"]], .4),
  cores_tb[["cinza_claro"]],
  alpha(cores_tb[["laranja"]], .25),
  alpha(cores_tb[["laranja"]], .6),
  cores_tb[["laranja"]],
  cores_aep[["rosa"]]
)

names(cores_tipo_resposta) <- c(
  "Deferido",
  "Parcialmente deferido",
  "Acolhimento",
  "Perda de objeto parcial",
  "Perda de objeto",
  "Não conhecimento",
  "Indeferido"
)

#' Paleta de cores para motivos de recursos abertos pelos solicitantes
cores_motivo_recurso <- c(
  "Informação incompleta" = cores_aep[["rosa"]],
  "Informação recebida não corresponde à\nsolicitada" = cores_aep[["cinza"]],
  "Outros" = cores_tb[["azul"]],
  "Justificativa para o sigilo\ninsatisfatória/não informada" = cores_aep[["marrom"]],
  "Ausência de justificativa legal para\nclassificação" = "gray30",
  "Resposta não foi dada no prazo" = "darkred"
)

#' Helper para aparência de escala percentual em eixos no ggplot
my_lbl <- function(x) scales::percent(x, accuracy = .1, decimal.mark = ",", big.mark = " ")

#' ggplot theme defaults
knitr::knit_hooks$set(inline = function(x) prettyNum(x, big.mark = ".", decimal.mark = ","))

options(
  digits = 1,
  scipen = 999,
  OutDec = ",",
  knitr.kable.NA = "",
  radian.auto_match = FALSE,
  hrbrthemes.loadfonts = TRUE
)

theme_set(theme_minimal())

theme_update(
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "gray97", color = "transparent"),
  axis.line.y = element_blank(),
  axis.line.x = element_line(color = "gray50")
)

hrbrthemes::import_roboto_condensed()
extrafont::loadfonts()

#' opções gerais do relatório
set.seed(1014)

Sys.setenv(LANGUAGE = "pt-br")
Sys.setlocale("LC_TIME", "pt_BR")
