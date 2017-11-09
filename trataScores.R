# Separar as pontuações
# criando um tipo de dado nomeado Score para teste
# coloco nele a pontuacao do dataset normal
library(readr)
mydata <- atp_matches_2016
# rm (mydata)
mydata <- na.omit(mydata$score)
mydata <- data.frame(mydata)



# crio colunas para cada um dos sets e para diferencas e medias
mydata$first_set <- lapply(strsplit(as.character(mydata$mydata), "\\ "), "[", 1)
mydata$first_diferenca <- NA
mydata$first_media <- NA
mydata$second_set <- lapply(strsplit(as.character(mydata$mydata), "\\ "), "[", 2)
mydata$second_diferenca <- NA
mydata$second_media <- NA
mydata$third_set <- lapply(strsplit(as.character(mydata$mydata), "\\ "), "[", 3)
mydata$third_diferenca <- NA
mydata$third_media <- NA


# cria funcao para calcular diferencas e medias
calculaSet <- function(set){
  resultado <- list()
  dif_list <- c()
  med_list <- c()
  for (i in 1:length(set)){
      # pega a pontuação dos sets
      primeiro <- as.numeric(substr(set[i], start=1, stop=1))
      segundo <- as.numeric(substr(set[i], start=3, stop=3))
      
      # calcula a diferença
      diferenca <- primeiro - segundo
      if (!is.na(diferenca))
        if (diferenca < 0) diferenca = diferenca * -1
      dif_list <- c(dif_list, diferenca)
      
      # calcula a media
      media <- (primeiro+segundo)/2
      med_list <- c(med_list, media)
  }
  # concatena as listas de resultados e retorna
  resultado <- c(dif_list, "*", med_list)
  return (resultado)
}


# utiliza a funcao pra o 1 set
resultado <- calculaSet(mydata$first_set)
posicao = match("*", resultado)
mydata$first_diferenca = resultado[1: posicao-1]
mydata$first_media = resultado[(posicao+1):length(resultado)]

# utiliza a funcao pra o 2 set
resultado <- calculaSet(mydata$second_set)
posicao = match("*", resultado)
mydata$second_diferenca = resultado[1: posicao-1]
mydata$second_media = resultado[(posicao+1):length(resultado)]

# utiliza a funcao pra o 3 set
resultado <- calculaSet(mydata$third_set)
posicao = match("*", resultado)
mydata$third_diferenca = resultado[1: posicao-1]
mydata$third_media = resultado[(posicao+1):length(resultado)]

# apaga da memoria o lixo
rm(resultado, posicao, calculaSet)
