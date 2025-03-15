# loading packages ####
library(basedosdados)
library(dplyr)


# setting inital setup ####
file.path(getwd(), ".keys") |> 
  readLines() |> 
  trimws() |> 
  basedosdados::set_billing_id()

# analysis of labor market evolution by sector
dados_evolucao_mt <- basedosdados::bdplyr("basedosdados.br_me_rais.microdados_vinculos")

dados_evolucao_mt |> glimpse()

