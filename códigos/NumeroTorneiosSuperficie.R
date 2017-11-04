for(i in 2691:length(atp_matches_2016$tourney_name))
  atp_matches_2016[i, 2] <- "Davis Cup"

count.tourney_surfaces = tapply(atp_matches_2016$tourney_name, atp_matches_2016$surface, FUN = function(x) length(unique(x)))

barplot(count.tourney_surfaces, main = "Número de Torneios por Superfície", col = c("darkolivegreen3","salmon","chartreuse3","dodgerblue2"), names.arg = c("Carpet", "Clay","Grass","Hard"), ylim = c(0,40), ylab = "Número de Torneios", xlab = "Superfícies", space = 1)

