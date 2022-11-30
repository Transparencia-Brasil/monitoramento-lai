here("data/base-cgu-atual.rds") %>%
  readRDS() %>%
  filter(base == "Pedidos") %>%
  select(datasets) %>%
  unnest(datasets) %>%
  filter(protocolo %in% c(
    "03005026266202244",
    "03005282203202276",
    "03005093721202217"
  )) %>%
  googlesheets4::write_sheet("1g8MDLHbZridrSXjwS49tHBGeQvyQYObtZ_eW7vxvsZc", sheet = "Pedidos")

here("data/base-cgu-atual.rds") %>%
  readRDS() %>%
  filter(base == "Recursos") %>%
  select(datasets) %>%
  unnest(datasets) %>%
  filter(protocolo_pedido %in% c(
    "03005026266202244",
    "03005282203202276",
    "03005093721202217"
  )) %>%
  googlesheets4::write_sheet("1g8MDLHbZridrSXjwS49tHBGeQvyQYObtZ_eW7vxvsZc", sheet = "Recursos")
