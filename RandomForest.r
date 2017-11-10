# Load Libraries
library(readr)   # Provides a fast way to read retangular data (like csv)
library(dplyr)   # Helps in data manipulating tasks
library(sqldf)   # Provides a way to use SQL sentences to search in a data frame
library(Amelia)  # Provides a way to visualize missing data in a data frame
library(caTools) # Para fazer split do dataset entre treino e teste
library(ranger)  # Para usar algoritmo de floresta

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

# Tratar score. 
#Criar coluna com quantidade de sets, quantidade de pontos média por set e se alguém foi retirado da partida.
warm_data$score
warm_data$qtdSets = 0
warm_data$qtdPontosMedia = 0
warm_data$jogadorRetirado = FALSE

# Para cada partida
for (row in 1:nrow(warm_data)){
  score = warm_data[row,'score']
  # Remove coisas desnecessarias no score
  score = gsub("\\s*\\([^\\)]+\\)","",as.character(score))
  # Separo os sets
  sets <- strsplit(score, " ")[[1]]
  # Marco se algum jogador foi retirado na partida
  if ('RET' %in% sets){
    warm_data[row,'jogadorRetirado'] = TRUE
    sets = sets[-length(sets)]
  }
  if ('DEF' %in% sets){
    warm_data[row,'jogadorRetirado'] = TRUE
    sets = sets[-length(sets)]
  }
  # Marco quantidade de sets
  qtdSets = length(sets)
  warm_data[row,'qtdSets'] = qtdSets
  # Tiro a media de pontos por set
  mediaPontos = 0
  for(set in sets){
    pontos = gsub("-"," ",as.character(set))
    pontos <- strsplit(pontos, " ")[[1]]
    pontos = as.integer(pontos)
    totalPontos = sum(pontos)
    mediaPontos = mediaPontos + (totalPontos/qtdSets)
  }
  warm_data[row,'qtdPontosMedia'] = as.integer(mediaPontos)
}

'
table(warm_data$qtdPontosMedia, warm_data$surface) 

d <- density(warm_data$qtdPontosMedia)
plot(d, main = "Densidade de Pontos feitos por set",xlab = "Pontos", ylab = "Densidade")
polygon(d, col = "ORANGE", border = "RED")
'
# Tratar minutos. Separar por faixas de tempo.
# Normalizar pela quantidade de sets
warm_data$minutes = warm_data$minutes / warm_data$qtdSets
# Separar em faixas de tempo de 10 em 10 min e depois multiplicando por 5 pra distanciar
warm_data$minutes = as.integer(warm_data$minutes / 2) * 2
'
# Observar resultados do tratamento
table(warm_data$minutes, warm_data$surface)
# Observar resultados do tratamento
d <- density(warm_data[warm_data$surface == "Clay",]$minutes)
d <- density(warm_data[warm_data$surface == "Hard",]$minutes)
d <- density(warm_data[warm_data$surface == "Grass",]$minutes)

plot(d, main = "Densidade de Tempo de Partida",xlab = "Tempo (min)", ylab = "Densidade")
polygon(d, col = "GREEN", border = "BLUE")
'
# Tratar aces. 
# Transformar em uma variável quantidade de aces na partida. Separar em faixas.
warm_data$qtdAces = (warm_data$w_ace + warm_data$l_ace) / warm_data$qtdSets
warm_data$qtdAces = as.integer(warm_data$qtdAces / 1)
'
summary(warm_data$qtdAces) 

d <- density(warm_data[warm_data$surface == "Clay",]$qtdAces)
d <- density(warm_data[warm_data$surface == "Hard",]$qtdAces)
d <- density(warm_data[warm_data$surface == "Grass",]$qtdAces)

plot(d, main = "Densidade de Qtd. Aces",xlab = "Qtd. Aces", ylab = "Densidade")
polygon(d, col = "ORANGE", border = "RED")

table(warm_data$qtdAces)
table(warm_data$qtdAces, warm_data$surface) 
'
# Tratar Double Faults.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdDoubleFaults = (warm_data$w_df + warm_data$l_df) / warm_data$qtdSets
warm_data$qtdDoubleFaults = as.integer(warm_data$qtdDoubleFaults / 1) 
'
summary(warm_data$qtdDoubleFaults) 

d <- density(warm_data[warm_data$surface == "Clay",]$qtdDoubleFaults)
d <- density(warm_data[warm_data$surface == "Hard",]$qtdDoubleFaults)
d <- density(warm_data[warm_data$surface == "Grass",]$qtdDoubleFaults)

plot(d, main = "Densidade de Qtd de Double Faults por set",xlab = "Qtd Double Faults", ylab = "Densidade")
polygon(d, col = "ORANGE", border = "RED")

table(warm_data$qtdDoubleFaults)
table(warm_data$qtdDoubleFaults, warm_data$surface) 
'

# Tratar Pontos Vencido.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdPontosVencidos = (warm_data$w_svpt + warm_data$l_svpt) / warm_data$qtdSets
warm_data$qtdPontosVencidos = as.integer(warm_data$qtdPontosVencidos / 5) * 5
'
summary(warm_data$qtdPontosVencidos) 

d <- density(warm_data[warm_data$surface == "Clay",]$qtdPontosVencidos)
d <- density(warm_data[warm_data$surface == "Hard",]$qtdPontosVencidos)
d <- density(warm_data[warm_data$surface == "Grass",]$qtdPontosVencidos)

plot(d, main = "Densidade de Qtd de Pontos Vencidos por set",xlab = "Qtd Pontos Vencidos", ylab = "Densidade")
polygon(d, col = "ORANGE", border = "RED")

table(warm_data$qtdPontosVencidos)
table(warm_data$qtdPontosVencidos, warm_data$surface) 
'

# Tratar Acertos no 1º saque.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdAcertos1Saque = (warm_data$w_1stIn + warm_data$l_1stIn) / warm_data$qtdSets
warm_data$qtdAcertos1Saque = as.integer(warm_data$qtdAcertos1Saque / 2) * 2
'
summary(warm_data$qtdAcertos1Saque) 

d <- density(warm_data[warm_data$surface == "Clay",]$qtdAcertos1Saque)
d <- density(warm_data[warm_data$surface == "Hard",]$qtdAcertos1Saque)
d <- density(warm_data[warm_data$surface == "Grass",]$qtdAcertos1Saque)

plot(d, main = "Densidade de Qtd. Acertos no 1º saque",xlab = "Acertos", ylab = "Densidade")
polygon(d, col = "ORANGE", border = "RED")

table(warm_data$qtdAcertos1Saque)
table(warm_data$qtdAcertos1Saque, warm_data$surface) 
'

# Tratar Pontos Ganhos no 1º saque.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdGanhos1Saque = (warm_data$w_1stWon + warm_data$l_1stWon) / warm_data$qtdSets
warm_data$qtdGanhos1Saque = as.integer(warm_data$qtdGanhos1Saque / 2)
'
summary(warm_data$qtdGanhos1Saque) 

d <- density(warm_data[warm_data$surface == "Clay",]$qtdGanhos1Saque)
d <- density(warm_data[warm_data$surface == "Hard",]$qtdGanhos1Saque)
d <- density(warm_data[warm_data$surface == "Grass",]$qtdGanhos1Saque)

plot(d, main = "Densidade Pontos Ganhos 1º saque",xlab = "Pontos Ganhos", ylab = "Densidade")
polygon(d, col = "ORANGE", border = "RED")

table(warm_data$qtdGanhos1Saque)
table(warm_data$qtdGanhos1Saque, warm_data$surface) 
'

# Tratar Pontos Ganhos no 2º saque. # DESCARTADA
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdGanhos2Saque = (warm_data$w_2ndWon + warm_data$l_2ndWon) / warm_data$qtdSets
warm_data$qtdGanhos2Saque = as.integer(warm_data$qtdGanhos2Saque / 1)
'
summary(warm_data$qtdGanhos2Saque) 

d <- density(warm_data[warm_data$surface == "Clay",]$qtdGanhos2Saque)
d <- density(warm_data[warm_data$surface == "Hard",]$qtdGanhos2Saque)
d <- density(warm_data[warm_data$surface == "Grass",]$qtdGanhos2Saque)

plot(d, main = "Densidade Pontos Ganhos 2º saque",xlab = "Pontos Ganhos", ylab = "Densidade")
polygon(d, col = "ORANGE", border = "RED")

table(warm_data$qtdGanhos2Saque)
table(warm_data$qtdGanhos2Saque, warm_data$surface) 
'

# Tratar quantidade de Games de Saque. 
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qdGamesSaque = (warm_data$w_SvGms + warm_data$l_SvGms) / warm_data$qtdSets
warm_data$qdGamesSaque = as.integer(warm_data$qdGamesSaque / 1)
'
summary(warm_data$qdGamesSaque) 

d <- density(warm_data[warm_data$surface == "Clay",]$qdGamesSaque)
d <- density(warm_data[warm_data$surface == "Hard",]$qdGamesSaque)
d <- density(warm_data[warm_data$surface == "Grass",]$qdGamesSaque)

plot(d, main = "Densidade Qtd Games de Saque",xlab = "Games de Saque", ylab = "Densidade")
polygon(d, col = "ORANGE", border = "RED")

table(warm_data$qdGamesSaque)
table(warm_data$qdGamesSaque, warm_data$surface) 
'

# Tratar quantidade de Pontos Salvos.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qdPontosSalvos = (warm_data$w_bpSaved + warm_data$l_bpSaved) / warm_data$qtdSets
warm_data$qdPontosSalvos = as.integer(warm_data$qdPontosSalvos / 1) 
'
summary(warm_data$qdPontosSalvos) 

d <- density(warm_data[warm_data$surface == "Clay",]$qdPontosSalvos)
d <- density(warm_data[warm_data$surface == "Hard",]$qdPontosSalvos)
d <- density(warm_data[warm_data$surface == "Grass",]$qdPontosSalvos)

plot(d, main = "Densidade de Qtd de Pontos Salvos",xlab = "Pontos Salvos", ylab = "Densidade")
polygon(d, col = "ORANGE", border = "RED")

table(warm_data$qdPontosSalvos)
table(warm_data$qdPontosSalvos, warm_data$surface) 
'

# Tratar quantidade de Break Points Enfrentados.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdBreakPointsEnfr = (warm_data$w_bpFaced + warm_data$l_bpFaced) / warm_data$qtdSets
warm_data$qtdBreakPointsEnfr = as.integer(warm_data$qtdBreakPointsEnfr / 1)
'
summary(warm_data$qtdBreakPointsEnfr) 

d <- density(warm_data[warm_data$surface == "Clay",]$qtdBreakPointsEnfr)
d <- density(warm_data[warm_data$surface == "Hard",]$qtdBreakPointsEnfr)
d <- density(warm_data[warm_data$surface == "Grass",]$qtdBreakPointsEnfr)

plot(d, main = "Densidade de Qtd de Break Points Enfrentados",xlab = "Break Points Enfrentados", ylab = "Densidade")
polygon(d, col = "ORANGE", border = "RED")

table(warm_data$qtdBreakPointsEnfr)
table(warm_data$qtdBreakPointsEnfr, warm_data$surface) 
'

# -----------------------------------------
# RANDOM FOREST
newdata = warm_data
newdata_clust = warm_data[,c(1:2,4,23:34)]
glimpse(newdata_clust)

mask = sample.split(newdata_clust$surface, SplitRatio = 7/10)

train = newdata_clust[mask,]
test  = newdata_clust[!mask,]

set.seed(97)
rf.model = ranger(as.factor(surface) ~ ., data=train)

rf.pred = predict(rf.model,test)
table(test$surface, rf.pred$predictions)

acuracia = (99 + 8 + 427) / 867
acuraciaSeFosseChutadoHard = 510 / 867

table(test$surface)
