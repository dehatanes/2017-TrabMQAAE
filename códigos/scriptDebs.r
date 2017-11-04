# Load libraries
library(readr)
library(dplyr)
library(ggplot2)
# library(rpython) # Para se quisermos usar os comandos de python em R
library(sqldf)
library(Amelia)

# Set working directory
setwd("C:/Users/Robots/Desktop/R_Project") #Colocar Path onde salvou o script

# Ler Dados
raw.data = read_csv("atp_matches_2016.csv") #Lê a tabela na variável raw.data

# Visualizar dados
head(raw.data,n=1) # Ver as primeiras n linhas
str(raw.data) # Mostrar Estrutura da tabela (nome das colunas e sua composição)
glimpse(raw.data) # Mostra Estrutura de forma mais bonita
dim(raw.data) # Dimensão da tabela (quantas linhas x colunas)

unique(raw.data$surface) # Pega os valores únicos da coluna "surface" da tabela raw.data
raw.data$surface = as.factor(raw.data$surface) # Transforma os valores no tipo "factor"

raw.data[1,] # Linha 1 (todas as colunas)
raw.data[1:10,] # Linha 1 a 10 (todas as colunas)
raw.data[1] # Coluna 1 (todas as linhas)
raw.data[,1] # Coluna 1 (todas as linhas)
raw.data[,1:5] # coluna 1 a 5 (todas as linhas)
raw.data[1,3] # Linha 1 coluna 3
raw.data[1:3,1:3] # Linha 1 a 3, colunas 1 a 3
raw.data[,c(1,3,5)] # c() cria vetores, então estamos acessando as colunas 1, 3 e 5

# Plotando gráfico de missing data
missmap(raw.data)

