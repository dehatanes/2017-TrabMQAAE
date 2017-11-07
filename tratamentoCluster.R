# Prepare Data
library(readr)
# mydata <- read_csv("D:/Meus Documentos/USP/atp_matches_2016.csv")
# mydata<- atp_matches_2016


# sumario dos dados
summary(mydata)

# visualizar a quantidade de NA por coluna
total_na <- sapply(mydata, function(x) sum(is.na(x)))
total_na <- data.frame(total_na)

# subset sem os dados mais ofensivos (aqueles > 1000 NA linhas): 43 VAR
colunas_excluidas <- names(mydata) %in% c("winner_entry", "loser_entry", "loser_seed", "winner_seed",
                                          "loser_ht", "winner_ht")
newdata <- mydata[!colunas_excluidas]

# agora retira sets de de superficie de carpete: 2276 LINHAS
# SUBSETS: https://www.statmethods.net/management/subset.html
newdata <- newdata[which (newdata$best_of==3 & newdata$surface != "Carpet"),]

# verifica os missing data: diminui para 27 o máximo! Dá pra tirar mais ainda!
total_na <- sapply(newdata, function(x) sum(is.na(x)))
total_na <- data.frame(total_na)

# exclui alguns outros inuteis: 39 variaveis agora
colunas_excluidas <- names(newdata) %in% c("loser_rank", "winner_rank", "loser_rank_points",
                                          "winner_rank_points")
newdata <- newdata[!colunas_excluidas]

# verifica os missing data: diminui para 20 o máximo de NA por coluna! 
total_na <- sapply(newdata, function(x) sum(is.na(x)))
total_na <- data.frame(total_na)


# Considero todos os dados importantes, dessa forma nao da mais para tirar coluna
# Solucao eh tirar todos os missing data das linhas: 2236 LINHAS
newdata <- na.omit(newdata)

# observe o novo resultado de NA: zero! sucesso!
anyNA(mydata)
anyNA(newdata)

# vamos fazer o cluster hierarquico
# distancia euclidianana
# metodo completo = faz link entre clusters similares


# Ward Hierarchical Clustering
d <- dist(newdata, method = "euclidean") 
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=3) # desenho em tres pedaços
rect.hclust(fit, k=3, border="red")









   