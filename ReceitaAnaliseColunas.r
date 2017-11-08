# Load Libraries
library(readr)   # Provides a fast way to read retangular data (like csv)
library(dplyr)   # Helps in data manipulating tasks
library(sqldf)   # Provides a way to use SQL sentences to search in a data frame
library(Amelia)  # Provides a way to visualize missing data in a data frame

# Set working directory
setwd("/home/eduardo/Área de Trabalho/") # Colocar caminho onde salvou o banco
# Lendo Banco de Dados
raw_data = read_csv("atp_matches_2016.csv") # Nome do arquivo do banco

# ANÁLISES GERAIS
dim(raw_data) # Dimensao da tabela (quantas linhas x colunas)

glimpse(raw_data) # Mostra estrutura (nome das colunas/tipo de dado/visualizacao dos primeiros dados) da tabela

missmap(raw_data) # Isso mostra um gráfico de missing data

# Isso é para retirar linhas TOTALMENTE em branco
indices <- apply(raw_data, 1, function(raw_data) all(is.na(raw_data)))
raw_data = raw_data[!indices, ] # Novo data frame sem as linhas em branco

# ANÁLISE PARA CADA COLUNA
# Para cada coluna da sua tabela primeiro entenda o que ela representa no contexto do seu banco
# Uma vez tendo isso em mente, observe seu conteúdo específico dando atenção para:
# Quão completos estão os dados, quão padronizados estão os dados (lembra que pro computador
# "Cuba" é diferente de "cuba" ou "01/02" é diferente de "01 de fevereiro". Pensa se vale a pena
# tratar esses dados ou se a coluna é lixo), quão distribuídos estão esses dados (dependendo do que
# tu for fazer não vale a pena ter a coluna inteira preenchida com True, por exemplo e só 3 linhas com
# False, isso pode não fazer diferença nenhuma e não te ajudar quando for analisar as relações do banco)

# Conforme for fazendo a analise guarde o nome das colunas em uma variável tipo essa
colunas_interessantes <- c("nomeColuna1", "nomeColuna2") # c() é pra criar um conjunto em R
# Pra depois cortar seu data base original em um data frame que tu vai utilizar
dataFrameLimpo <- subset(raw_data, select = colunas_interessantes)

# Se for bom pra ti, você pode renomear colunas do seu database dessa forma
colnames(raw_data)[9] <- "NovoNome" # 9 é a posição da coluna que quer renomear

# ANALISANDO DADOS DAS COLUNAS
# Você pode pegar só uma coluna dessa forma: bancoDeDados$nomeDaColuna por exemplo:
raw_data$winner_hand

# Você consegue uma lista com os dados que aparecem nessa coluna dessa forma
# Vai mostrar uma única vez cada valor diferente na coluna
unique(raw_data$winner_hand)
# Assim você consegue além da lista de valores um contador de quantos valores diferentes tem e qual o tipo desses valores
# Isso é útil pra tu saber se a coluna tem todos os valores únicos (é chave primária), por exemplo
# NULL é considerado um tipo de "label" (valor) e conta como valor diferente
str(unique(raw_data$winner_hand)) # quantos valores diferentes temos
# Se você identificar valores que aparecem como diferentes mas que deviam ser iguais (como os exemplos que dei
# mais pra cima), ou se eles deveriam ser outro valor mesmo, você pode alterar esses valores na mao, 
# isso tambem serve pra valores NULL. Por exemplo (um exemplo ruim):
raw_data[raw_data$winner_hand == 'R', "winner_hand"] = "L"

# Isso pode ser util para variáveis numéricas, pois dá um resumo de valor máximo, mínimo, média, quartis, etc. da coluna
summary(raw_data$minutes) 

# Isso é importante, mostra a quantidade de valores nulos que existem na coluna
# É importante pois ajuda a decidir se uma coluna é útil ou não, uma coluna com MUITOS valores nulos as vezes precisa ser descartada
# Dependendo da coluna e de quantos valores nulos as vezes vale mais a pena só descartar as linhas que contém valor do database
# As vezes dá pra recuperar os valores nulos de alguma forma
dim(raw_data[is.na(raw_data$winner_hand), ])

# Você pode tirar as linhas nulas de alguma coluna do data frame desse jeito
raw_data <-raw_data[which(!is.na(raw_data$winner_hand)),]

# Isso também é legal, te dá noção da quantidade que cada label tem. Legal de comparar com a quantiade de dados do data frame e ver se é uma quantidade considerável ou não
dim(raw_data[raw_data$winner_hand == 'L', ])

# TRATAMENTO DE COLUNAS QUE VOCÊ QUER USAR
# Transforma os valores no tipo "factor", útil para variáveis categóricas (aquelas que são textuais, mas podem ser tratadas como "número" pois são finitas)
# No caso, as variáveis de suicídio não são categóricas, mas é que já tava no exemplo
raw_data$winner_hand = as.factor(raw_data$winner_hand)
# Como podemos ver
str(raw_data$winner_hand)
