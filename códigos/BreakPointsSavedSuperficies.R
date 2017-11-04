mean.surfaces=tapply( atp_matches_2016$w_bpSaved+atp_matches_2016$l_bpSaved, atp_matches_2016$surface, mean, na.rm=TRUE)
sd.surfaces=tapply( atp_matches_2016$w_bpSaved+atp_matches_2016$l_bpSaved, atp_matches_2016$surface, sd, na.rm=TRUE)
mids=barplot(mean.surfaces, main = "Médias de Break Points Salvos", col = c("darkolivegreen3","salmon","chartreuse3","dodgerblue2"), names.arg = c("Carpet", "Clay","Grass","Hard"), ylim = c(0,25), ylab = "Número de Pontos", xlab = "Superfícies", space = 1)
arrows(mids, mean.surfaces-sd.surfaces, mids, mean.surfaces+sd.surfaces, angle = 90, code=3)