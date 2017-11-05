# Load Libraries
library(readr)   # Provides a fast way to read retangular data (like csv)
library(dplyr)   # Helps in data manipulating tasks
library(ggplot2) # Data visualization with graphs
library(sqldf)   # Provides a way to use SQL sentences to search in a data frame
library(Amelia)  # Provides a way to visualize missing data in a data frame

# Set working directory
setwd("/home/eduardo/Área de Trabalho/MQA Codes/") #Colocar Path onde salvou o script

# Lendo Banco de Dados
atp_matches_2016 = read_csv("atp_matches_2016.csv")

# Tratando dados faltantes
for(i in 2691:length(atp_matches_2016$tourney_name))
  atp_matches_2016[i, 2] <- "Davis Cup"

# Maneiras de visualizar a estrutura do banco no console
dim(atp_matches_2016)       # Dimensao da tabela (quantas linhas x colunas)
glimpse(atp_matches_2016)   # Mostra estrutura (nome das colunas) da tabela
head(atp_matches_2016, n=1) # Ver as primeiras n linhas

# Maneiras de visualizar os dados no console
unique(atp_matches_2016$surface) # Pega os valores ?nicos da coluna "surface" da tabela raw.data
atp_matches_2016$surface = as.factor(atp_matches_2016$surface) # Transforma os valores no tipo "factor"

# Maneiras de visualizar linhas e colunas
atp_matches_2016[1,]        # Linha 1 (todas as colunas)
atp_matches_2016[1:10,]     # Linha 1 a 10 (todas as colunas)
atp_matches_2016[1]         # Coluna 1 (todas as linhas)
atp_matches_2016[,1]        # Coluna 1 (todas as linhas)
atp_matches_2016[,1:5]      # coluna 1 a 5 (todas as linhas)
atp_matches_2016[1,3]       # Linha 1 coluna 3
atp_matches_2016[1:3,1:3]   # Linha 1 a 3, colunas 1 a 3
atp_matches_2016[,c(1,3,5)] # c() cria vetores, entao estamos acessando as colunas 1, 3 e 5

# PLOTS
# Grafico de missing data
missmap(atp_matches_2016)

# Boxplot - Altura dos jogadores vencedores e perdedores
boxplot(atp_matches_2016$winner_ht, atp_matches_2016$loser_ht, main="Altura dos Jogadores", ylab= "Altura (cm)", names = c("Vencedores", "Perdedores") ,  boxwex = 0.2, varwidth=TRUE, na.rm = TRUE)

# Boxplot - Idade dos jogadores vencedores e perdedores
boxplot(atp_matches_2016$winner_age, atp_matches_2016$loser_age, main="Idade dos Jogadores", ylab= "Idade (anos)", names = c("Vencedores", "Perdedores") ,  boxwex = 0.2, varwidth=TRUE, na.rm = TRUE)

# Barplot - Numero de torneios por superficie
count.tourney_surfaces = tapply(atp_matches_2016$tourney_name, 
                                atp_matches_2016$surface, FUN = function(x) length(unique(x)))
barplot(count.tourney_surfaces, main = "Número de Torneios por Superfície", 
        col = c("darkolivegreen3","salmon","chartreuse3","dodgerblue2"), 
        names.arg = c("Carpet", "Clay","Grass","Hard"), ylim = c(0,40), 
        ylab = "Número de Torneios", xlab = "Superfícies", space = 1)

# Boxplot - Aces por altura dos jogadores
df <- stack(atp_matches_2016, select=c("winner_ht","loser_ht")) 
df2 <- stack(atp_matches_2016, select=c("w_ace","l_ace"))
boxplot(df2$values~df$values, main="Distribução do Número de Aces pela Altura", 
        ylab="Número de Aces", xlab="Altura (cm)", na.rm=TRUE)

# Barplot - Quantidade de jogos por superficie
count.matches_surfaces = tapply(atp_matches_2016$surface, atp_matches_2016$surface, 
                                FUN = function(x) length(x))
barplot(count.matches_surfaces, main = "Número de Jogos por Superfície", 
        col = c("darkolivegreen3","salmon","chartreuse3","dodgerblue2"), 
        names.arg = c("Carpet", "Clay","Grass","Hard"), ylim = c(0,2000), 
        ylab = "Número de Jogos", xlab = "Superfícies", space = 1)

# Barplot - Quantidade de pontos disputados por superficie
mean.surfaces = tapply(atp_matches_2016$w_svpt+atp_matches_2016$l_svpt, 
                       atp_matches_2016$surface, mean, na.rm=TRUE)
sd.surfaces = tapply(atp_matches_2016$w_svpt+atp_matches_2016$l_svpt, 
                     atp_matches_2016$surface, sd, na.rm=TRUE)
mids = barplot(mean.surfaces, main = "Médias de Pontos Disputados", 
               col = c("darkolivegreen3","salmon","chartreuse3","dodgerblue2"), 
               names.arg = c("Carpet", "Clay","Grass","Hard"), ylim = c(0,350), 
               ylab = "Número de Pontos", xlab = "Superfícies", space = 1)
arrows(mids, mean.surfaces-sd.surfaces, mids, mean.surfaces+sd.surfaces, angle = 90, code=3)

# Barplot - Ace por superficie
mean.surfaces = tapply(atp_matches_2016$w_ace +  atp_matches_2016$l_ace, 
                     atp_matches_2016$surface, mean, na.rm=TRUE)
sd.surfaces = tapply(atp_matches_2016$w_ace + atp_matches_2016$l_ace, 
                       atp_matches_2016$surface, sd, na.rm=TRUE)
mids = barplot(mean.surfaces, main = "Médias de Aces", 
               col = c("darkolivegreen3","salmon","chartreuse3","dodgerblue2"), 
               names.arg = c("Carpet", "Clay","Grass","Hard"), 
               ylim = c(0,35), ylab = "Número de Aces", xlab = "Superfícies", 
               space = 1)
arrows(mids, mean.surfaces - sd.surfaces, mids, 
       mean.surfaces + sd.surfaces, angle = 90, code=3)

# Barplot - Breakpoints enfrentados por superficie
mean.surfaces = tapply(atp_matches_2016$w_bpFaced + atp_matches_2016$l_bpFaced,
                     atp_matches_2016$surface, mean, na.rm=TRUE)
sd.surfaces = tapply(atp_matches_2016$w_bpFaced + atp_matches_2016$l_bpFaced, 
                     atp_matches_2016$surface, sd, na.rm=TRUE)
mids = barplot(mean.surfaces, main = "Médias de Break Points Enfrentados", 
               col = c("darkolivegreen3","salmon","chartreuse3","dodgerblue2"), 
               names.arg = c("Carpet", "Clay","Grass","Hard"), ylim = c(0,25), 
               ylab = "Número de Pontos", xlab = "Superfícies", space = 1)
arrows(mids, mean.surfaces-sd.surfaces, mids, 
       mean.surfaces+sd.surfaces, angle = 90, code=3)

# Barplot - Break Point Salvos por Superfície
mean.surfaces = tapply(atp_matches_2016$w_bpSaved + atp_matches_2016$l_bpSaved, 
                       atp_matches_2016$surface, mean, na.rm=TRUE)
sd.surfaces = tapply(atp_matches_2016$w_bpSaved+atp_matches_2016$l_bpSaved,
                     atp_matches_2016$surface, sd, na.rm=TRUE)
mids = barplot(mean.surfaces, main = "Médias de Break Points Salvos", 
               col = c("darkolivegreen3","salmon","chartreuse3","dodgerblue2"), 
               names.arg = c("Carpet", "Clay","Grass","Hard"), ylim = c(0,25), 
               ylab = "Número de Pontos", xlab = "Superfícies", space = 1)
arrows(mids, mean.surfaces-sd.surfaces, mids, mean.surfaces+sd.surfaces, angle = 90, code=3)

# Barplot - Duplas faltas por superficie
mean.surfaces = tapply(atp_matches_2016$w_df+atp_matches_2016$l_df, 
                       atp_matches_2016$surface, mean, na.rm=TRUE)
sd.surfaces = tapply( atp_matches_2016$w_df+atp_matches_2016$l_df, 
                      atp_matches_2016$surface, sd, na.rm=TRUE)
mids = barplot(mean.surfaces, main = "Médias de Duplas Faltas Cometidas", 
               col = c("darkolivegreen3","salmon","chartreuse3","dodgerblue2"), 
               names.arg = c("Carpet", "Clay","Grass","Hard"), ylim = c(0,25), 
               ylab = "Número de Pontos", xlab = "Superfícies", space = 1)
arrows(mids, mean.surfaces-sd.surfaces, mids, mean.surfaces+sd.surfaces, angle = 90, code=3)

# Barplot - Pontos de 1o saque por superficie
mean1.surfaces = tapply(atp_matches_2016$w_1stWon+atp_matches_2016$l_1stWon,
                        atp_matches_2016$surface, mean, na.rm=TRUE)
sd.surfaces=tapply(atp_matches_2016$w_1stWon+atp_matches_2016$l_1stWon, 
                   atp_matches_2016$surface, sd, na.rm=TRUE)
mids = barplot(mean1.surfaces, main = "Médias de Pontos Ganhos com o 1º Saque",
               col = c("darkolivegreen3","salmon","chartreuse3","dodgerblue2"), 
               names.arg = c("Carpet", "Clay","Grass","Hard"), ylim = c(0,150), 
               ylab = "Número de Pontos", xlab = "Superfícies", space = 1)
arrows(mids, mean1.surfaces-sd.surfaces, mids, mean1.surfaces+sd.surfaces, angle = 90, code=3)

# Barplot - Pontos de 2o saque por superficie
mean2.surfaces = tapply(atp_matches_2016$w_2ndWon+atp_matches_2016$l_2ndWon, 
                        atp_matches_2016$surface, mean, na.rm=TRUE)
sd.surfaces=tapply(atp_matches_2016$w_2ndWon+atp_matches_2016$l_2ndWon, 
                   atp_matches_2016$surface, sd, na.rm=TRUE)
mids=barplot(mean2.surfaces, main = "Médias de Pontos Ganhos com o 2o Saque", 
             col = c("darkolivegreen3","salmon","chartreuse3","dodgerblue2"), 
             names.arg = c("Carpet", "Clay","Grass","Hard"), ylim = c(0,80), 
             ylab = "Número de Pontos", xlab = "Superfúcies", space = 1)
arrows(mids, mean2.surfaces-sd.surfaces, mids, mean2.surfaces+sd.surfaces, angle = 90, code=3)

# Barplot - Comparacao entre 1o saque e 2o saques
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

# Densidade - Altura dos jogadores
d <- density(atp_matches_2016$winner_ht, na.rm = TRUE)
plot(d, main = "Densidade da Altura dos Jogadores", 
     xlab = "Altura (cm)", ylab = "Densidade")
polygon(d, col = "RED", border = "BLUE")

# Densidade - Idade dos jogadores
d <- density(atp_matches_2016$winner_age, na.rm = TRUE)
plot(d, main = "Densidade da Idade dos Jogadores", 
     xlab = "Idade (anos)", ylab = "Densidade")
polygon(d, col = "RED", border = "BLUE")
