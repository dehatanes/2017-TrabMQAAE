# Script para análise e tratamento do Banco de Dados

# Load Libraries
library(readr)   # Provides a fast way to read retangular data (like csv)
library(dplyr)   # Helps in data manipulating tasks
library(ggplot2) # Data visualization with graphs
library(sqldf)   # Provides a way to use SQL sentences to search in a data frame
library(Amelia)  # Provides a way to visualize missing data in a data frame

# Set working directory
setwd("/home/eduardo/Área de Trabalho/") #Colocar Path onde salvou o script

# Lendo Banco de Dados
atp_matches_2016 = read_csv("atp_matches_2016.csv")

# Tamanho do Banco
dim(atp_matches_2016) # Dimensao da tabela (quantas linhas x colunas)

# TRATANDO LINHAS
# Retirando linhas completamente em branco
indices <- apply(atp_matches_2016, 1, function(atp_matches_2016) all(is.na(atp_matches_2016)))
raw.data = atp_matches_2016[!indices, ] # Novo data frame sem as linhas em branco

# TRATANDO COLUNAS
# Conteúdo das colunas
glimpse(raw.data)   # Mostra estrutura (nome das colunas) da tabela

# Podemos retirar as colunas:
# tourney_id, tourney_name, tourney_level, tourney_date, match_num, 
# winner_seed, winner_entry, winner_name, loser_seed, loser_entry, loser_name

# Colunas que quero analisar antes de decidir retirar:
# winner_rank, winner_rank_points, loser_rank, loser_rank_points, score, best_of, round

# Colunas que irao ficar:
# surface (labels), minutes, draw_size, winner_id, loser_id, winner_hand, loser_hand,
# winner_ht, loser_ht, winner_ioc, loser_ioc, winner_age, loser_age, w_ace, l_ace,
# w_df, l_df, w_spvt, l_spvt, w_1stIn, l_1stIn, w_1stWon, l_1stWon, w_2ndWon, l_2ndWon,
# w_SvGms, l_SvGms, w_bpSaved, l_bpSaved, w_bpFaced, l_bpFaced

# ANALISANDO AS COLUNAS
# Winner e Loser Rank
str(unique(raw.data$winner_rank)) # quantos valores diferentes temos
summary(raw.data$winner_rank) 
dim(raw.data[is.na(raw.data$winner_rank), ]) # quantos valores nulos #16
raw.data[is.na(raw.data$winner_rank), "winner_name"] #quais os vencedores com ranking nulo

str(unique(raw.data$loser_rank)) # quantos valores diferentes temos
summary(raw.data$loser_rank) 
dim(raw.data[is.na(raw.data$loser_rank), ]) # quantos valores nulos #46
raw.data[is.na(raw.data$loser_rank), "winner_name"] #quais os perdedores com ranking nulo

# Winner e Loser Rank Points
str(unique(raw.data$winner_rank_points)) # quantos valores diferentes temos
dim(raw.data[is.na(raw.data$winner_rank_points), ]) # quantos valores nulos # 16

str(unique(raw.data$loser_rank_points)) # quantos valores diferentes temos
dim(raw.data[is.na(raw.data$loser_rank_points), ]) # quantos valores nulos # 46


# Best_of
str(unique(raw.data$best_of)) # quantos valores diferentes temos
dim(raw.data[is.na(raw.data$best_of), ]) # quantos valores nulos # Zero
dim(raw.data[raw.data$best_of == 3, ]) # 2280 partidas melhores de 3
dim(raw.data[raw.data$best_of == 5, ]) # 661 partidas melhores de 5

# Score
str(unique(raw.data$score)) # quantos valores diferentes temos
dim(raw.data[is.na(raw.data$score), ]) # quantos valores nulos # Zero

# Round
str(unique(raw.data$round)) # quantos valores diferentes temos
dim(raw.data[is.na(raw.data$round), ]) # quantos valores nulos # Zero
