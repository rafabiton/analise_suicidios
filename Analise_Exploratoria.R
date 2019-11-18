
# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("/Users/rafaelbiton/Google Drive/Cursos/DSA/CIENTISTA_DADOS/10.Experimentos/Suicidio/analise_suicidios")
getwd()

# carregar a biblioteca dplyr para manipulação dos dados
library(dplyr, warn.conflicts = FALSE)
# carrega a biblioteca ggplot2 para criação dos gráficos
library(ggplot2)

# Inicio da Análise Exploratória de Dados

# importa o DataSet para a variavel suicidios
# Dataset foi baixado do Kaggle (https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016)
suicidios <- read.csv("master.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE )

# visualiza o dataSet inteiro
View(suicidios)

# Dividindo os DataSets em dados do Brasil e dados do mundo
suicidios_brasil <- suicidios %>%
  select(country, year, suicides.100k.pop) %>%
  filter (suicidios$country == 'Brazil') %>%
  group_by(year) %>%
  summarise(media_suicidio = mean(suicides.100k.pop)) %>%
  mutate(pais = "Brasil")

View(suicidios_brasil)

# Como não existem dados de 2016 para o Brasil no DataSet
# Vamos retirar este ano do dataframe mundial
suicidios_mundo <- suicidios %>%
  select(country, year, suicides.100k.pop) %>%
  filter(., country != 'Brazil', year != '2016') %>%
  group_by(year) %>%
  summarise(media_suicidio = mean(suicides.100k.pop)) %>%
  mutate(pais = "Resto do Mundo")

# Vamos visualizar como ficou no novo DataFrame com os dados do mundo, sem Brasil
View(suicidios_mundo)

# Aqui juntamos os 2 Data Frames em um único
df_brasil_mundo <- rbind(suicidios_brasil, suicidios_mundo)

# Aqui começa a montagem das camadas do primeiro plot
# que compara a média mundial com a média do Brasil
ggplot(df_brasil_mundo, aes(year, media_suicidio, color = pais), size = 1.5) + 
  ggtitle("Taxa de Homicídios Brasil x Resto do Mundo") +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  ylab("Suicídios/100k pop") +
  xlab("Ano") +
  geom_line()

# Vamos continuar a análise exploratória de dados
# A ideia é identificar quais faixas etárias com maior índice de suicídio

# Dividindo os DataSets em dados do Brasil e dados do mundo, agora por faixa etária
suicidios_brasil_idade <- suicidios %>%
  select(year,country, suicides_no, age) %>%
  filter (suicidios$country == 'Brazil') %>%
  group_by(year, age) %>%
  summarise(total = sum(suicides_no)) %>%
  mutate(pais = "Brasil")


# Vamos montar um barplot para visualização dos dados
ggplot(suicidios_brasil_idade, aes(fill=age, y=total, x=year)) + 
  geom_bar(position="stack", stat="identity")
