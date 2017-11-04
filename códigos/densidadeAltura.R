d<-density(atp_matches_2016$winner_ht, na.rm = TRUE)

plot(d, main = "Densidade da Altura dos Jogadores", xlab = "Altura (cm)", ylab = "Densidade")
polygon(d, col = "RED", border = "BLUE")

