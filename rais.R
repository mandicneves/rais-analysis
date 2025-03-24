# carregando bibliotecas ####
library(basedosdados)
library(tidyverse)
library(scales)
library(ggthemes)
library(geobr)

# autorizando acesso a basededados ####
file.path(getwd(), ".keys") |> 
  readLines() |> 
  trimws() |> 
  basedosdados::set_billing_id()

# carregando dicionario dos dados e estabelecendo conexao ####
dicionario_rais <- basedosdados::bdplyr("basedosdados.br_me_rais.dicionario") |>
  basedosdados::bd_collect()
dicionario_rais |> glimpse()

dados_rais <- basedosdados::bdplyr("basedosdados.br_me_rais.microdados_estabelecimentos")
dados_rais |> 
  glimpse()

# coletando dados sobre funcionalismo publico ####
tabela_funcionalismo <- dados_rais |> 
  select(
    ano, 
    sigla_uf, 
    natureza_juridica, 
    quantidade_vinculos_ativos
    ) |>
  filter(
    sigla_uf != "IGNORADO",
    natureza_juridica %in% c(
      "1015", "1023", "1031", "1040", "1058", "1066", "1074", "1082"
      )
  ) |>
  group_by(
    ano, 
    sigla_uf, 
    natureza_juridica
    ) |> 
  summarise(
    numero_vinculos = sum(quantidade_vinculos_ativos, na.rm = T),
    .groups = "drop"
  ) |>
  basedosdados::bd_collect()
  
tabela_funcionalismo <- tabela_funcionalismo |>
  mutate(
    tipo = recode(
      natureza_juridica,
      "1015" = "Executivo Federal",
      "1023" = "Executivo Estadual",
      "1031" = "Executivo Municipal",
      "1040" = "Legislativo Federal",
      "1058" = "Legislativo Estadual",
      "1066" = "Legislativo Municipal",
      "1074" = "Judiciário Federal",
      "1082" = "Judiciário Estadual"
    ),
    poder = str_extract(
      string = tipo,
      pattern = "Executivo|Legislativo|Judiciário"
    ),
    esfera = str_extract(
      string = tipo,
      pattern = "Federal|Estadual|Municipal"
    ),
    numero_vinculos = numero_vinculos / 1e6
  )

# grafico de linha da evolucao do funcionalismo publico ####
tabela_funcionalismo |>
  group_by(ano, poder, esfera) |>
  summarise(numero_vinculos = sum(numero_vinculos, na.rm = T), .groups = "drop") |>
  drop_na() |>
  ggplot(aes(x = ano, y = numero_vinculos, color = poder)) +
  geom_line(size = 1.5) +
  facet_wrap(~esfera, scales = "free_y") + 
  scale_y_continuous(
    breaks = scales::breaks_extended(8),
    labels = scales::label_comma(big.mark = ".", decimal.mark = ",")
  ) +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.title = element_text()
  ) +
  labs(
    title = "Evolução do funcionalismo público no Brasil",
    x = NULL,
    y = "Nº de vínculos",
    color = NULL,
    caption = "Dados: RAIS/Base dos Dados"
  )

# grafico mapa de calor ####
coordenadas <- geobr::read_state()

tabela_funcionalismo |> 
  filter(ano == 2023) |>
  group_by(sigla_uf) |>
  summarise(numero_vinculos = sum(numero_vinculos, na.rm =T), .groups = "drop") |>
  left_join(
    coordenadas,
    by = c("sigla_uf" = "abbrev_state")
  ) |>
  ggplot(aes(fill = numero_vinculos, geometry = geom)) + 
  geom_sf(color = "black") + 
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    breaks = scales::breaks_extended(6),
    labels = scales::label_comma(big.mark = ".", decimal.mark = ",")
  ) +
  theme(
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    legend.direction = "vertical",
    legend.background = element_blank()
  ) +
  labs(
    title = "Funcionalismo público no Brasil",
    fill = "Nº de vínculos ativos\n(em milhões) em dez/2020",
    caption = "Dados: RAIS/Base dos dados"
  )

























