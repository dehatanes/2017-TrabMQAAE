# Load Libraries
library(readr)   # Provides a fast way to read retangular data (like csv)
library(dplyr)   # Helps in data manipulating tasks
library(sqldf)   # Provides a way to use SQL sentences to search in a data frame
library(Amelia)  # Provides a way to visualize missing data in a data frame
library(caTools) # Para fazer split do dataset entre treino e teste

# Set working directory
setwd("/home/eduardo/Área de Trabalho/") # Colocar caminho onde salvou o banco
# Lendo Banco de Dados
atp_matches_2016 = read_csv("atp_matches_2016.csv") # Nome do arquivo do banco

# ----------------------------------------------
# TRATAMENTO DO BANCO
raw_data = atp_matches_2016

# Retirar linhas TOTALMENTE em branco
indices <- apply(raw_data, 1, function(raw_data) all(is.na(raw_data)))
raw_data = raw_data[!indices, ] # Novo data frame sem as linhas em branco

# Retirar linhas com carpete
raw_data = raw_data[raw_data$surface != 'Carpet', ]

# Total de valores nulos
sapply(raw_data, function(x) sum(is.na(x)))
# Colunas
glimpse(raw_data)

# Manter apenas colunas interessantes e descartar o resto
# surface (char)
# draw_size (int)
# score (char)
# minutes (int)
# w_ace l_ace (int)
# w_df l_df (int)
# w_svpt l_svpt (int)
# w_1stIn l_1stIn (int) 
# w_1stWon l_1stWon (int)
# w_2ndWon l_2ndWon (int)
# w_SvGms l_SvGms (int)
# w_bpSaved l_bpSaved (int)
# w_bpFaced l_bpFaced (int)
colunas_interessantes <- c("surface", "draw_size", "score", "minutes", "w_ace", "l_ace", "w_df", "l_df","w_svpt", "l_svpt", "w_1stIn", "l_1stIn", "w_1stWon", "l_1stWon", "w_2ndWon", "l_2ndWon", "w_SvGms", "l_SvGms", "w_bpSaved", "l_bpSaved", "w_bpFaced", "l_bpFaced")
warm_data = dataFrameLimpo <- subset(raw_data, select = colunas_interessantes)

# Podemos observar que ainda restaram alguns missing datas
sapply(warm_data, function(x) sum(is.na(x)))

# Analisando mais a fundo, percebemos que todas as linhas com 26 valores NULL são as mesmas, o que as tornam descartáveis
# As linhas com a coluna "minutes" faltando também são descartáveis
warm_data = warm_data[!is.na(warm_data$minutes) & !is.na(warm_data$w_ace),]

# Como podemos ver, agora nosso data frame deixou de ser esparso
colSums(is.na(warm_data))

# Isso nos fez perder 39 linhas, o que nos resulta em 2891 linhas aproveitáveis
dim(warm_data)

# ----------------------------------------------
# TRATAMENTO DOS DADOS

# Tratar score. Criar coluna com quantidade de pontos média por set.
warm_data$score

# Tratar minutos. Separar por faixas de tempo.
# Valores sem tratamento
d <- density(warm_data$minutes)
plot(d, main = "Densidade de Tempo de Partida",xlab = "Tempo (min)", ylab = "Densidade")
polygon(d, col = "GREEN", border = "BLUE")
# Sumarizando valores sem tratamento
summary(warm_data$minutes) 
# Separar em faixas de tempo de 10 em 10 min e depois multiplicando por 5 pra distanciar
warm_data$minutes = as.integer(warm_data$minutes / 10) * 3
# Observar resultados do tratamento
table(warm_data$minutes, warm_data$surface)
# Observar resultados do tratamento
d <- density(warm_data$minutes)
plot(d, main = "Densidade de Tempo de Partida",xlab = "Tempo (min)", ylab = "Densidade")
polygon(d, col = "GREEN", border = "BLUE")

# Tratar aces. 
# Transformar em uma variável quantidade de aces na partida. Separar em faixas.
warm_data$qtdAces = warm_data$w_ace + warm_data$l_ace
warm_data$qtdAces = as.integer(warm_data$qtdAces / 2) * 2

# Tratar Double Faults.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdDoubleFaults = warm_data$w_df + warm_data$l_df
warm_data$qtdDoubleFaults = as.integer(warm_data$qtdDoubleFaults / 1) 


# Tratar Pontos Vencido.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdPontosVencidos = warm_data$w_svpt + warm_data$l_svpt
warm_data$qtdPontosVencidos = as.integer(warm_data$qtdPontosVencidos / 20) * 20

# Tratar Acertos no 1º saque.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdAcertos1Saque = warm_data$w_1stIn + warm_data$l_1stIn
warm_data$qtdAcertos1Saque = as.integer(warm_data$qtdAcertos1Saque / 10) * 10

# Tratar Pontos Ganhos no 1º saque.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdGanhos1Saque = warm_data$w_1stWon + warm_data$l_1stWon
warm_data$qtdGanhos1Saque = as.integer(warm_data$qtdGanhos1Saque / 8) * 8

# Tratar Pontos Ganhos no 2º saque.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdGanhos2Saque = warm_data$w_2ndWon + warm_data$l_2ndWon
warm_data$qtdGanhos2Saque = as.integer(warm_data$qtdGanhos2Saque / 2) * 2

# Tratar quantidade de Games de Saque.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qdGamesSaque = warm_data$w_SvGms + warm_data$l_SvGms
warm_data$qdGamesSaque = as.integer(warm_data$qdGamesSaque / 3) * 3

# Tratar quantidade de Pontos Salvos.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qdPontosSalvos = warm_data$w_bpSaved + warm_data$l_bpSaved
warm_data$qdPontosSalvos = as.integer(warm_data$qdPontosSalvos / 1) 

# Tratar quantidade de Break Points Enfrentados.
# Transformar em uma variável quantidade por partida. Separar em faixas.
'
summary(warm_data$w_bpFaced) 
summary(warm_data$l_bpFaced) 
summary(warm_data$w_bpFaced - warm_data$l_bpFaced)
summary(warm_data$w_bpFaced + warm_data$l_bpFaced)

d <- density(warm_data$w_bpFaced)
plot(d, main = "Densidade de Tempo de Partida",xlab = "Tempo (min)", ylab = "Densidade")
polygon(d, col = "GREEN", border = "BLUE")

d <- density(warm_data$l_bpFaced)
plot(d, main = "Densidade de Tempo de Partida",xlab = "Tempo (min)", ylab = "Densidade")
polygon(d, col = "GREEN", border = "BLUE")

d <- density(warm_data$w_bpFaced + warm_data$l_bpFaced)
plot(d, main = "Densidade de Tempo de Partida",xlab = "Tempo (min)", ylab = "Densidade")
polygon(d, col = "GREEN", border = "BLUE")
'
warm_data$qtdBreakPointsEnfr = warm_data$w_bpFaced + warm_data$l_bpFaced
warm_data$qtdBreakPointsEnfr = as.integer(warm_data$qtdBreakPointsEnfr / 2) * 2
'
d <- density(warm_data$qtdBreakPointsEnfr)
plot(d, main = "Densidade de Tempo de Partida",xlab = "Tempo (min)", ylab = "Densidade")
polygon(d, col = "ORANGE", border = "RED")

table(warm_data$qtdBreakPointsEnfr, warm_data$surface)
'

# ----------------------------------------------
# APLICAR WARD HIERARQUICAL CLUSTERING NOS DADOS
newdata = warm_data
cleandata = newdata[,c(2,4,23:31)]
glimpse(cleandata)
d <- dist(cleandata, method = "euclidean") 
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=3) # desenho em tres peda?os
rect.hclust(fit, k=3, border="red")

# APLICAR KMEANS PARA CLUSTERIZAR OS DADOS
newdata_clust = cleandata

set.seed(57)
kclust = kmeans(x = as.matrix(newdata_clust), centers = 3, iter.max = 250)

newdata_clust$cluster = kclust$cluster
newdata_clust$surface = newdata$surface

table(newdata_clust$surface)
table(newdata_clust$surface, newdata_clust$cluster)

# APLICAR ARVORE PARA TENTAR CLASSIFICAR OS DADOS
newdata_clust = warm_data[,c(1:2,4,23:31)]
mask = sample.split(newdata_clust$surface, SplitRatio = 7/10)

train = newdata[mask,]
test  = newdata[!mask,]

library(randomForest,quietly=TRUE)
library(ranger,quietly=TRUE)
set.seed(97)
rf.model = ranger(as.factor(surface) ~ ., data=train, importance = 'impurity')

rf.pred = predict(rf.model,test)
table(test$surface, rf.pred$predictions)

(69 + 2 + 461) / (69 + 2 + 461 + 4 + 44 + 2 + 5 + 190 + 90)

table(test$surface)