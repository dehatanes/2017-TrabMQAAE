df <- stack(atp_matches_2016, select=c("winner_ht","loser_ht")) 
df2<-stack(atp_matches_2016, select=c("w_ace","l_ace"))
boxplot(df2$values~df$values, main="Distribução do Número de Aces pela Altura", ylab="Número de Aces", xlab="Altura (cm)", na.rm=TRUE)