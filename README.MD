# 📊 Análise da Evolução do Mercado de Trabalho por Setor no Brasil 🇧🇷

Este projeto utiliza os dados da **RAIS (Relação Anual de Informações Sociais)** para analisar a evolução do emprego formal no Brasil ao longo dos anos, explorando a distribuição de empregos por setor, variação salarial e tendências econômicas regionais.  

Os dados são acessados diretamente da plataforma [**Base dos Dados**](https://basedosdados.org/) utilizando a linguagem **R** e o pacote `basedosdados`.

---

O mercado de trabalho é um dos principais indicadores da saúde econômica de um país. A criação e destruição de empregos em diferentes setores refletem mudanças estruturais na economia, avanços tecnológicos, políticas governamentais e crises econômicas.  

Com esta análise, buscamos responder perguntas como:  
✅ **Quais setores estão crescendo e quais estão encolhendo?**  
✅ **Como os salários médios evoluíram ao longo do tempo?**  
✅ **Existe desigualdade regional na geração de empregos?**  

---

## 📂 Estrutura do Projeto  

- `rais.R` → Contém os códigos R utilizados na análise
- `.keys` → Contém o `ID_PROJECT` do Google Cloud
- `data/` → Pasta para armazenar os dados baixados (se necessário)  
- `README.md` → Este arquivo, com a explicação detalhada do projeto  

---

## 🛠️ Configuração e Dependências  

### **1️⃣ Instalar Pacotes Necessários**  

Antes de rodar o código, instale os pacotes necessários:  

```r
install.packages("basedosdados")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("sf")
```

### **2️⃣ Autenticação no Google Cloud**  

Para acessar os dados da **RAIS** no `basedosdados`, é necessário configurar um projeto no Google Cloud e autenticar a API. Eu adicionei um arquivo `.keys` na pasta contendo meu `ID_PROJECT` e fiz a leitura com o código abaixo:

```r
library(basedosdados)
file.path(getwd(), ".keys") |> 
  readLines() |> 
  trimws() |> 
  basedosdados::set_billing_id()
```

---

## 📊 Coleta de Dados  

Usamos o pacote `basedosdados` para consultar a base da RAIS e extrair informações sobre emprego e salário médio por setor e região.  

```r
library(basedosdados)
library(tidyverse)

query <- "
SELECT 
    ano, 
    sigla_uf, 
    cnae_2_subclasse, 
    vinculos_ativos, 
    salario_mensal_med
FROM `basedosdados.br_me_rais.microdados`
WHERE ano BETWEEN 2010 AND 2023
"

dados_rais <- basedosdados::read_sql(query)
```

Explicação:  
✅ **Selecionamos os anos de 2010 a 2023** para capturar uma tendência de longo prazo  
✅ **Filtramos por Estado (sigla_uf) e setor econômico (cnae_2_subclasse)**  
✅ **Extraímos o número total de vínculos ativos e o salário médio mensal**  

---

## 📈 Análise Exploratória  

### **📊 Evolução do emprego por setor**  
Criamos um gráfico para visualizar a evolução do número de empregos por setor ao longo dos anos.  

```r
dados_rais %>%
  group_by(ano, cnae_2_subclasse) %>%
  summarise(total_empregos = sum(vinculos_ativos, na.rm = TRUE)) %>%
  ggplot(aes(x = ano, y = total_empregos, color = cnae_2_subclasse)) +
  geom_line() +
  labs(title = "Evolução do Emprego Formal por Setor",
       x = "Ano",
       y = "Número de Empregos") +
  theme_minimal() +
  theme(legend.position = "none")
```

📌 **O que podemos observar?**  
🔹 Quais setores apresentaram maior crescimento?  
🔹 Como crises econômicas impactaram a geração de empregos?  

---

### **🗺️ Mapa da distribuição de empregos por estado**  

Usamos o pacote `sf` para visualizar a distribuição regional do emprego formal.  

```r
library(ggplot2)
library(sf)

# Carregar shapefile do Brasil
brasil <- st_read("https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/brazil-states.geojson")

# Agregar empregos por estado
emprego_estado <- dados_rais %>%
  group_by(sigla_uf) %>%
  summarise(total_empregos = sum(vinculos_ativos, na.rm = TRUE))

# Criar mapa temático
ggplot(data = brasil) +
  geom_sf(aes(fill = total_empregos), data = emprego_estado) +
  scale_fill_viridis_c() +
  labs(title = "Distribuição do Emprego por Estado") +
  theme_minimal() +
  theme(legend.position = "none")
```

📌 **O que podemos observar?**  
🔹 Quais estados concentram mais empregos?  
🔹 Existe desigualdade regional no mercado de trabalho?  

---

## 🚀 Conclusões e Próximos Passos  

A partir dos gráficos e análises, podemos tirar algumas conclusões sobre o comportamento do emprego formal no Brasil.  

🔎 **Possíveis descobertas:**  
✅ Quais setores tiveram maior expansão nos últimos anos  
✅ Como eventos econômicos impactaram o mercado de trabalho  
✅ Diferenças salariais entre setores  

💡 **Próximos passos:**  
- Aplicar modelos estatísticos para prever tendências futuras  
- Comparar os dados da RAIS com indicadores macroeconômicos  
- Explorar diferenças entre pequenas e grandes empresas  

---

## 📌 Contato  

Caso tenha dúvidas ou sugestões, fique à vontade para contribuir!  

📧 **Email:** seuemail@example.com  
🐦 **Twitter:** @seuperfil  
📂 **GitHub:** [github.com/seuusuario](https://github.com/seuusuario)  

🛠️ **Contribuições são bem-vindas!** Caso queira colaborar, abra uma issue ou envie um pull request. 🚀  
