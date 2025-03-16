# loading packages ####
library(basedosdados)
library(tidyverse)


# setting inital setup ####
file.path(getwd(), ".keys") |> 
  readLines() |> 
  trimws() |> 
  basedosdados::set_billing_id()

# analysis of labor market evolution by sector in sao paulo city ####

dicionario_rais <- basedosdados::bdplyr("basedosdados.br_me_rais.dicionario") |>
  basedosdados::bd_collect()
dicionario_rais |> glimpse()

dados_rais <- basedosdados::bdplyr("basedosdados.br_me_rais.microdados_vinculos")
dados_rais |> 
  glimpse()
dados_evolucao_mt <- dados_rais |> 
  filter(
    ano >= 2020 & ano <= 2023,
    id_municipio == "3550308"
  ) |> 
  select(
    ano,
    cbo_2002,
    cnae_1,
    vinculo_ativo_3112,
    valor_remuneracao_media_sm
   ) |>
  basedosdados::bd_collect()
  

tabela_total_empregos <- dados_evolucao_mt |>
  group_by(ano, cnae_1) |>
  summarise(total_empregos = sum(vinculo_ativo_3112 |> as.numeric())) |>
  arrange(ano, desc(total_empregos))


tabela_total_empregos |>
  # filter(total_empregos >= mean(tabela_total_empregos$total_empregos)) |>
  ggplot(aes(x = ano, y = total_empregos, color = cnae_1)) +
  geom_area() +
  labs(title = "Evolução do Emprego Formal por Setor",
       x = "Ano",
       y = "Número de Empregos") +
  theme_minimal() +
  theme(legend.position = "none")
