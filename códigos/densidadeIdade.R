d<-density(atp_matches_2016$winner_age, na.rm = TRUE)
plot(d, main = "Densidade da Idade dos Jogadores", xlab = "Idade (anos)", ylab = "Densidade")
polygon(d, col = "RED", border = "BLUE")

