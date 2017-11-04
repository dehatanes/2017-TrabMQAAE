library(ggplot2)

attach(atp_matches_2016_1_)

# gera media e desvio padrao do tempo por superficie



bd <- atp_matches_2016_1_

# pega todas as superficies e minutos menos as vazias
vetorSurface <- (unique( na.exclude(bd$surface)))
vetorMinutes <- (unique( na.exclude(bd$minutes)))

# salva em um subset
novaBase <- subset(bd, surface == vetorSurface)


# imprime o grafico
geraGrafico <- function(banco, titulo, eixoX, eixoY, tipo){
  grafico <- ggplot(banco, aes(x=surface, y=minutes))
  grafico <- grafico + labs(title = titulo, x= eixoX,y= eixoY)
  grafico <- grafico + theme_classic() 
  
  if (tipo == 1)
    grafico <- grafico + geom_boxplot()
  else if (tipo == 2)
    grafico <- grafico + geom_line()
  
  return (grafico)
}
print(geraGrafico(novaBase, "Tempo de partida por superficie", "Superfície", "Minutos", 1))



baseCarpet <- subset(bd, surface == vetorSurface[4])
agg <- mean(baseCarpet$minutes)
print(geraGrafico(agg, "Média de tempo de partida por superficie", "Superfície", "Minutos", 2))




detach(atp_matches_2016_1_)



