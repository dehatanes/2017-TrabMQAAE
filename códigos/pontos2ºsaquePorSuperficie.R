mean.surfaces=tapply( atp_matches_2016$w_2ndWon+atp_matches_2016$l_2ndWon, atp_matches_2016$surface, mean, na.rm=TRUE)
sd.surfaces=tapply( atp_matches_2016$w_2ndWon+atp_matches_2016$l_2ndWon, atp_matches_2016$surface, sd, na.rm=TRUE)
mids=barplot(mean.surfaces, main = "M�dias de Pontos Ganhos com o 2� Saque", col = c("darkolivegreen3","salmon","chartreuse3","dodgerblue2"), names.arg = c("Carpet", "Clay","Grass","Hard"), ylim = c(0,80), ylab = "N�mero de Pontos", xlab = "Superf�cies", space = 1)
arrows(mids, mean.surfaces-sd.surfaces, mids, mean.surfaces+sd.surfaces, angle = 90, code=3)