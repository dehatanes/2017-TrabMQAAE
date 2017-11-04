count.matches_surfaces = tapply(atp_matches_2016$surface, atp_matches_2016$surface, FUN = function(x) length(x))
barplot(count.matches_surfaces, main = "Número de Jogos por Superfície", col = c("darkolivegreen3","salmon","chartreuse3","dodgerblue2"), names.arg = c("Carpet", "Clay","Grass","Hard"), ylim = c(0,2000), ylab = "Número de Jogos", xlab = "Superfícies", space = 1)

