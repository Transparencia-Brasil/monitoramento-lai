# aplica identidade visual da TB/AeP:
cores_aep <- c(
  laranja = "#F9A521",
  rosa = "#D81755",
  cinza = "#969696",
  marrom = "#B27D5C"
)

cores_tb <- c(
  laranja = "#F6A323",
  cinza_escuro = "#1d1d1b",
  cinza_claro = "#6f7171",
  cinza_quase_branco = "#ececec",
  azul = "#41ACBD"
)

cores_decisao <- c(
  "Acesso Concedido" = cores_tb[["azul"]],
  "Não se trata de solicitação de informação" = cores_aep[["marrom"]],
  "Acesso Negado" = cores_aep[["rosa"]],
  "Acesso Parcialmente Concedido" = cores_aep[["laranja"]],
  "Pergunta Duplicada/Repetida" = cores_aep[["cinza"]],
  "Órgão não tem competência para responder sobre o assunto" = cores_tb[["cinza_escuro"]],
  "Informação Inexistente" = cores_tb[["cinza_quase_branco"]]
)

cores_decisao2 <- cores_decisao
cores_decisao2[["Informação Inexistente"]] <- "gray20"

cores_decisao3 <- c("black", "gray90", "gray20", "gray80", "gray20")
names(cores_decisao3) <- names(cores_decisao)[-c(5, 2)] 

cores_lai <- tibble(
  c1 = c("Não se trata de solicitação de informação",
         "Pergunta Duplicada/Repetida",
         "Pedidos de acesso a informação via LAI"
  ) %>% str_wrap(25),
  c2 = c("#F9A521", "#969696", "#D81755")
) %>% deframe()

cores_instancia <- c(
  "Primeira Instância" = cores_tb[["azul"]],
  "Segunda Instância" = cores_aep[["laranja"]],
  "CGU" = cores_aep[["marrom"]],
  "CMRI" = cores_aep[["rosa"]]
)

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

cores_motivo_recurso <- c(
  "Informação incompleta" = cores_aep[["rosa"]],
  "Informação recebida não corresponde à\nsolicitada" = cores_aep[["cinza"]],
  "Outros" = cores_tb[["azul"]],
  "Justificativa para o sigilo\ninsatisfatória/não informada" = cores_aep[["marrom"]],
  "Ausência de justificativa legal para\nclassificação" = "gray30",
  "Resposta não foi dada no prazo" = "darkred"
)