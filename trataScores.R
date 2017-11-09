# Separar as pontuações
# criando um tipo de dado nomeado Score para teste
# coloco nele a pontuacao do dataset normal
library(readr)
mydata <- read_csv("D:/Meus Documentos/USP/atp_matches_2016.csv")
# rm (scores)
scores <- na.omit(mydata$score)
scores <- data.frame(scores, stringsAsFactors = FALSE)



# crio colunas para cada um dos sets e para diferencas e medias
scores$first_set <- lapply(strsplit(as.character(scores$scores), "\\ "), "[", 1)
scores$first_diferenca <- NA
scores$first_media <- NA
scores$second_set <- lapply(strsplit(as.character(scores$scores), "\\ "), "[", 2)
scores$second_diferenca <- NA
scores$second_media <- NA
scores$third_set <- lapply(strsplit(as.character(scores$scores), "\\ "), "[", 3)
scores$third_diferenca <- NA
scores$third_media <- NA




# cria funcao para calcular e registrar as diferencas e medias
calculaSet <- function(set, ordem){
  resultado <- list("diferenca", "media")
  for (i in 1:length(set)){
      # pega a pontuação dos sets
      primeiro <- as.numeric(substr(set[i], start=1, stop=1))
      segundo <- as.numeric(substr(set[i], start=3, stop=3))
      
      # calcula a diferença
      diferenca <- primeiro - segundo
      if (!is.na(diferenca) && diferenca < 1) diferença <- diferenca * (-1)
      
      # calcula a media
      media <- (primeiro+segundo)/2
      
      # registra esse resultado no dataframe
      #resultado <- c(resultado, diferenca, media)
      switch (ordem,
        "1" = {
          scores$first_diferenca[i] <- diferenca
          scores$first_media <- media
          # cat("\n", i, " dif: ", diferenca, " media: ", media)
        },
        "2" = {
          scores$second_diferenca[i] <- diferenca
          scores$second_media[i] <- media
        },
        "3" = {
          scores$third_diferenca[i] <- diferenca
          scores$third_media[i] <- media
        }, stop
      )
  }
  return (resultado)
}


# utiliza a funcao pra o 1 set
q <- calculaSet(scores$first_set, "1")

print (q)
# utiliza a funcao pra o 2 set
calculaSet(scores$second_set, 2)
# utiliza a funcao pra o 3 set
calculaSet(scores$third_set, 3)

