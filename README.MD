# 📊 Análise do Funcionalismo Público no Brasil com Dados da RAIS

## Visão Geral

Este projeto utiliza a base de dados da **RAIS (Relação Anual de Informações Sociais)**, disponibilizada pela plataforma **Base dos Dados**, para analisar a evolução do funcionalismo público no Brasil ao longo dos anos. A partir dos dados, realizamos uma análise exploratória e visualização gráfica da distribuição de vínculos empregatícios no setor público.

Os dados são acessados diretamente da plataforma [**Base dos Dados**](https://basedosdados.org/) utilizando a linguagem **R** e o pacote `basedosdados`.

------------------------------------------------------------------------

## 📂 Estrutura do Projeto

-   `rais.R` → Contém os códigos R utilizados na análise
-   `.keys` → Contém o `ID_PROJECT` do Google Cloud
-   `data/` → Pasta para armazenar os dados baixados (se necessário)
-   `README.md` → Este arquivo, com a explicação detalhada do projeto

------------------------------------------------------------------------

## 🛠️ Configuração e Dependências

### **1️⃣ Instalar Pacotes Necessários**

Antes de rodar o código, instale os pacotes necessários:

``` r
install.packages("basedosdados")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("geobr")
install.packages("ggthemes")
install.packages("scales")
```

### **2️⃣ Autenticação no Google Cloud**

Para acessar os dados da **RAIS** no `basedosdados`, é necessário configurar um projeto no Google Cloud e autenticar a API. Eu adicionei um arquivo `.keys` na pasta contendo meu `ID_PROJECT` e fiz a leitura com o código abaixo:

``` r
library(basedosdados)
file.path(getwd(), ".keys") |> 
  readLines() |> 
  trimws() |> 
  basedosdados::set_billing_id()
```

------------------------------------------------------------------------

## 📊 Coleta de Dados

Utilizamos o pacote `basedosdados` para consultar a base da RAIS e extrair informações sobre o quantitativo de vínculos empregatícios no setor público, categorizados por esfera de governo (Federal, Estadual e Municipal) e poder (Executivo, Legislativo e Judiciário).

Primeiramente, estabelecemos a conexão com a tabela desejada e obtemos um vislumbre de como os dados estão organizados.

Para coletar os dados de fato, podemos aplicar filtros e seleções utilizando as funções do framework `tidyverse` e, em seguida, realizar a extração com a função `bd_collect()` do pacote `basedosdados`.

Alternativamente, é possível especificar uma query **SQL** diretamente para filtrar e selecionar os dados, coletando-os com a função `read_sql()`. Neste primeiro momento, optei pelo uso das funções do `tidyverse` para um tratamento mais flexível e integrado.

``` r
library(basedosdados)
library(tidyverse)

# Estabelecendo conexão e visualizando a estrutura dos dados
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
```

Os principais passos da coleta de dados incluem:

1.  Filtrar os dados para remover registros desconhecidos.
2.  Selecionar apenas as naturezas jurídicas do setor público.
3.  Agregar os vínculos por estado, poder e esfera.

------------------------------------------------------------------------

## 📈 Visualização de Dados

Para facilitar a compreensão da evolução do funcionalismo público, são gerados dois tipos de visualizações:

### 1️⃣ Gráfico de Linha - Evolução dos Vínculos Empregatícios

O gráfico de linha mostra a evolução do número de vínculos ativos entre **2020** e **2023**, diferenciando entre **Executivo, Legislativo e Judiciário**. A visualização utiliza a biblioteca `ggplot2` e o tema `fivethirtyeight`.

``` r
library(ggplot)
library(scales)
library(ggthemes)

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
```

### 2️⃣ Mapa de Calor - Distribuição por Estado

Utilizando o pacote `geobr`, geramos um mapa de calor para visualizar a distribuição geográfica dos vínculos no funcionalismo público em **2023**. Os estados com maior concentração de servidores são destacados com diferentes gradientes de cor.

``` r
library(geobr)

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
```

------------------------------------------------------------------------

## 📌 Contato

Caso tenha dúvidas ou sugestões, fique à vontade para contribuir!

📧 **Email:** [mandicneves\@gmail.com](mailto:mandicneves@gmail.com)

📂 **GitHub:** [github.com/mandicneves](https://github.com/mandicneves)

🛠️ **Contribuições são bem-vindas!** Caso queira colaborar, abra uma issue ou envie um pull request. 🚀
