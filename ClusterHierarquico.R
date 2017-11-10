# Prepare Data
library(readr)
# mydata <- read_csv("D:/Meus Documentos/USP/atp_matches_2016.csv")
mydata<- atp_matches_2016


# sumario dos dados
summary(mydata)

# visualizar a quantidade de NA por coluna
total_na <- sapply(atp_matches_2016, function(x) sum(is.na(x)))
total_na <- data.frame(total_na)

# subset sem os dados mais ofensivos (aqueles > 1000 NA linhas): 43 VAR
colunas_excluidas <- names(mydata) %in% c("winner_entry", "loser_entry", "loser_seed", "winner_seed",
                                          "loser_ht", "winner_ht")
mydata <- mydata[!colunas_excluidas]

# agora retira sets de de superficie de carpete: 2276 LINHAS
# SUBSETS: https://www.statmethods.net/management/subset.html
mydata <- mydata[which (mydata$best_of==3 & mydata$surface != "Carpet"),]

# verifica os missing data: diminui para 27 o m?ximo! D? pra tirar mais ainda!
total_na <- sapply(mydata, function(x) sum(is.na(x)))
total_na <- data.frame(total_na)

# exclui alguns outros inuteis: 39 variaveis agora
colunas_excluidas <- names(mydata) %in% c("loser_rank", "winner_rank", "loser_rank_points",
                                           "winner_rank_points")
mydata <- mydata[!colunas_excluidas]

colunas_interessantes <- c("tourney_name", "surface", "draw_size", "winner_hand",
                           "winner_age", "loser_hand", 
                           "loser_age", "score", "best_of", "minutes", "w_ace", "l_ace", 
                           "w_df", "l_df","w_svpt", "l_svpt", "w_1stIn", "l_1stIn", "w_1stWon", 
                           "l_1stWon", "w_2ndWon", "l_2ndWon", "w_SvGms", "l_SvGms", "w_bpSaved", 
                           "l_bpSaved", "w_bpFaced", "l_bpFaced")
mydata = subset(mydata, select = colunas_interessantes)



# verifica os missing data: diminui para 20 o m?ximo de NA por coluna! 
total_na <- sapply(mydata, function(x) sum(is.na(x)))
total_na <- data.frame(total_na)


# Considero todos os dados importantes, dessa forma nao da mais para tirar coluna
# Solucao eh tirar todos os missing data das linhas: 2236 LINHAS
mydata <- na.omit(mydata)

# observe o novo resultado de NA: zero! sucesso!
anyNA(mydata)
rm(total_na, colunas_excluidas, colunas_interessantes)



# ----------------------------------------------------------------
# TRATANDO OS DADOS ESCOLHIDOS
# ----------------------------------------------------------------

# Separar as pontuações

# crio colunas para cada um dos sets e para diferencas e medias
mydata$first_set <- lapply(strsplit(as.character(mydata$score), "\\ "), "[", 1)
mydata$first_diferenca <- NA
mydata$first_media <- NA
mydata$second_set <- lapply(strsplit(as.character(mydata$score), "\\ "), "[", 2)
mydata$second_diferenca <- NA
mydata$second_media <- NA
mydata$third_set <- lapply(strsplit(as.character(mydata$score), "\\ "), "[", 3)
mydata$third_diferenca <- NA
mydata$third_media <- NA


# cria funcao para calcular diferencas e medias
calculaSet <- function(set){
  resultado <- list()
  dif_list <- c()
  med_list <- c()
  for (i in 1:length(set)){
    # pega a pontuação dos sets
    primeiro <- as.numeric(substr(set[i], start=1, stop=1))
    segundo <- as.numeric(substr(set[i], start=3, stop=3))
    
    # calcula a diferença
    diferenca <- primeiro - segundo
    if (!is.na(diferenca))
      if (diferenca < 0) diferenca = diferenca * -1
    dif_list <- c(dif_list, diferenca)
    
    # calcula a media
    media <- (primeiro+segundo)/2
    med_list <- c(med_list, media)
  }
  # concatena as listas de resultados e retorna
  resultado <- c(dif_list, "*", med_list)
  return (resultado)
}


# utiliza a funcao pra o 1 set
resultado <- calculaSet(mydata$first_set)
posicao = match("*", resultado)
mydata$first_diferenca = resultado[1: posicao-1]
mydata$first_media = resultado[(posicao+1):length(resultado)]

# utiliza a funcao pra o 2 set
resultado <- calculaSet(mydata$second_set)
posicao = match("*", resultado)
mydata$second_diferenca = resultado[1: posicao-1]
mydata$second_media = resultado[(posicao+1):length(resultado)]

# utiliza a funcao pra o 3 set
resultado <- calculaSet(mydata$third_set)
posicao = match("*", resultado)
mydata$third_diferenca = resultado[1: posicao-1]
mydata$third_media = resultado[(posicao+1):length(resultado)]

# apaga da memoria o lixo
rm(resultado, posicao, calculaSet)

# tira a coluna score original
mydata <- mydata[!names(mydata) %in% "score"]
mydata <- mydata[!names(mydata) %in% c("tourney_name", "first_set", "second_set", "third_set")]




# Mãos do jogador vencedor: 0 para R, 1 para L
mydata$winner_hand[mydata$winner_hand == "R"] <- 0
mydata$winner_hand[mydata$winner_hand == "L"] <- 1

# Mãos do jogador perdedor: 0 para R, 1 para L
mydata$loser_hand[mydata$loser_hand == "R"] <- 0
mydata$loser_hand[mydata$loser_hand == "L"] <- 1

warm_data <- mydata
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
warm_data$qtdGamesSaque = warm_data$w_SvGms + warm_data$l_SvGms
warm_data$qtdGamesSaque = as.integer(warm_data$qtdGamesSaque / 3) * 3

# Tratar quantidade de Pontos Salvos.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdPontosSalvos = warm_data$w_bpSaved + warm_data$l_bpSaved
warm_data$qtdPontosSalvos = as.integer(warm_data$qtdPontosSalvos / 1) 

# Tratar quantidade de Pontos Enfrentados
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdPontosEnfrentados= warm_data$w_bpFaced + warm_data$l_bpFaced
warm_data$qtdPontosEnfrentados = as.integer(warm_data$qtdPontosSalvos / 1) 

# Tratar Diferencas e Medias
warm_data$qtdDiferenca <- NULL
for (i in 1:length(warm_data$first_diferenca)){
  diferenca <- c()
  if (is.na(warm_data$third_diferenca[i])){
    if (is.na(warm_data$second_diferenca[i]) == TRUE) 
      warm_data$qtdDiferenca[i] =as.integer(warm_data$first_diferenca[i])
    else
      warm_data$qtdDiferenca[i] = (as.integer(warm_data$first_diferenca[i]) + as.integer(warm_data$second_diferenca[i]))/2
  } else 
    warm_data$qtdDiferenca[i] = (as.integer(warm_data$first_diferenca[i]) + as.integer(warm_data$second_diferenca[i]) + as.integer(warm_data$third_diferenca[i]))/3
}

warm_data$qtdMedia <- NULL
for (i in 1:length(warm_data$first_media)){
  diferenca <- c()
  if (is.na(warm_data$third_media[i])){
    if (is.na(warm_data$second_media[i]) == TRUE) 
      warm_data$qtdMedia[i] =as.integer(warm_data$first_media[i])
    else
      warm_data$qtdMedia[i] = (as.integer(warm_data$first_media[i]) + as.integer(warm_data$second_media[i]))/2
  } else 
    warm_data$qtdMedia[i] = (as.integer(warm_data$first_media[i]) + as.integer(warm_data$second_media[i]) + as.integer(warm_data$third_media[i]))/3
}


# Tratar idades
warm_data$qdIdades = (warm_data$winner_age + warm_data$loser_age)/2

colnames(warm_data)

# Limpa o dataset pro cluster
warm_data = warm_data[!names(warm_data) %in% c("first_diferenca", "second_diferenca",
                                               "third_diferenca", "first_media", "second_media",
                                               "third_media", "w_bpSaved", "l_bpSaved",
                                               "w_SvGms", "l_SvGms", "w_2ndWon", "l_2ndWon",
                                               "w_1stWon", "l_1stWon", "w_1stIn", "l_1stIn",
                                               "w_svpt", "l_svpt", "w_df", "l_df", 
                                               "w_ace", "l_ace", "best_of", "w_bpSaved",
                                               "l_bpSaved", "w_bpFaced", "l_bpFaced",
                                               "winner_age", "loser_age", "winner_hand", "loser_hand")]

# ----------------------------------------------------------------
# CLUSTER HIERARQUICO
# ----------------------------------------------------------------
library(zoom)
warm_data <- na.omit(warm_data)

# prepare hierarchical cluster
hc = hclust(dist(warm_data))
png(
  "test.png",
  width     = 15,
  height    = 10,
  units     = "in",
  res       = 1200,
  pointsize = 4
)
par(
  mar      = c(5, 5, 2, 2),
  xaxs     = "i",
  yaxs     = "i",
  cex.axis = 2,
  cex.lab  = 2
)

plot(hc, lty = 1,  axes = FALSE, hang= -1, main ="Dendograma", cex= 0.8, font=10,
     ylab = "Altura", xlab = "Clusters")
axis(side = 2, at = seq(0, 400, 100), col = "#F38630", labels = FALSE, 
     lwd = 2, las = 2)
dev.off()

                                                      

d <- dist(warm_data, method = "euclidean") 
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
plot(fit, main="Dendograma", hang = -1, cex = 0.6, ylab = "Altura", xlab = "Clusters")
plot(fit, ylim = c(0, 200), hang = -1, cex = 0.6)
mtext(seq(0, 400, 100), side = 2, at = seq(0, 400, 100), line = 1, 
      col = "#A38630", las = 2)
zm()
clusterCut <- cutree(hc, k=4) # desenho em tres pedaco
table(clusterCut, warm_data$surface)



rect.hclust(hc, k=4, border="red")


