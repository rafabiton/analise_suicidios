
# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("/Users/rafaelbiton/Google Drive/Cursos/DSA/CIENTISTA_DADOS/10.Experimentos/Suicidio/analise_suicidios")
getwd()

# instala a biblioteca GGMAP
install.packages("ggmap")
# carregar as bibliotecas necessárias para manipulação dos dados e geração dos plots
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(ggmap)


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
  ggtitle("Número de Suicídios no Brasil") +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  ylab("Total de Suicídios/idade") +
  xlab("Ano") +
  geom_bar(position="stack", stat="identity")



# Dividindo os DataSets em dados do Brasil e dados do mundo, agora por faixa etária
suicidios_mundo_idade <- suicidios %>%
  select(year,country, suicides_no, age) %>%
  filter (suicidios$country != 'Brazil') %>%
  group_by(year, age) %>%
  summarise(total = sum(suicides_no)) %>%
  mutate(pais = "Mundo")


# Vamos montar um barplot para visualização dos dados
ggplot(suicidios_mundo_idade, aes(fill=age, y=total, x=year)) + 
  ggtitle("Número de Suicídios no Mundo") +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  ylab("Total de Suicídios/idade") +
  xlab("Ano") +
  geom_bar(position="stack", stat="identity")


# Dividindo os DataSets em dados do Brasil e dados do mundo, agora por Sexo
suicidios_brasil_sexo <- suicidios %>%
  select(suicides_no, sex) %>%
  filter (suicidios$country == 'Brazil') %>%
  mutate(sex = ifelse(sex == 'female', 'Feminino',
                      ifelse(sex == 'male', 'Masculino', 'no')
  )) %>%
  group_by(sex) %>%
  summarise(total = sum(suicides_no))

# Vamos montar um pichart para visualização dos dados
dfbrasil <- data.frame(valor = c(suicidios_brasil_sexo$total),
                 Group = c(suicidios_brasil_sexo$sex)) %>%
  mutate(Group = factor(Group, levels = c(suicidios_brasil_sexo$sex)),
         cumulative = cumsum(valor),
         midpoint = cumulative - valor / 2,
         label = paste0(Group, " ", round(valor / sum(valor) * 100, 1), "%"))

ggplot(dfbrasil, aes(x = 1, weight = valor, fill = Group)) +
  geom_bar(width = 1, position = "stack") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.3, y = midpoint, label = label)) +
  theme_nothing()


# Dividindo os DataSets em dados do Brasil e dados do mundo, agora por Sexo
suicidios_mundo_sexo <- suicidios %>%
  select(suicides_no, sex) %>%
  filter (suicidios$country != 'Brazil') %>%
  mutate(sex = ifelse(sex == 'female', 'Feminino',
                      ifelse(sex == 'male', 'Masculino', 'no')
                      )) %>%
  group_by(sex) %>%
  summarise(total = sum(suicides_no))


# Vamos montar um piechart para visualização dos dados
df <- data.frame(valor = c(suicidios_mundo_sexo$total),
                 Group = c(suicidios_mundo_sexo$sex)) %>%
  # factor levels need to be the opposite order of the cumulative sum of the values
  mutate(Group = factor(Group, levels = c(suicidios_mundo_sexo$sex)),
         cumulative = cumsum(valor),
         midpoint = cumulative - valor / 2,
         label = paste0(Group, " ", round(valor / sum(valor) * 100, 1), "%"))

ggplot(df, aes(x = 1, weight = valor, fill = Group)) +
  geom_bar(width = 1, position = "stack") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.3, y = midpoint, label = label)) +
  theme_nothing()
