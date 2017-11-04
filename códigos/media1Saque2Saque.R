df <- stack(mean1.surfaces)
df2 <- stack(mean2.surfaces)
df3 <- rbind(df$values,df2$values) 
barplot(df3, main="Médias de Pontos Ganhos com o 1º e 2º saque",
        xlab="Superfícies", ylab="Número de Pontos", legend = rownames(df3) ,
        col=c("darkblue","red"), names=c("Carpet","Clay","Grass","Hard"), ylim = c(0, 100), 
        beside=TRUE)
legend("topright",             
            fill = c("darkblue", "red"),
            legend = c("1º saque", "2º saque"), cex = 0.52, pt.cex=50,ncol = 2)
