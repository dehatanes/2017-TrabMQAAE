# Load Libraries
library(readr)   # Provides a fast way to read retangular data (like csv)
library(dplyr)   # Helps in data manipulating tasks
library(sqldf)   # Provides a way to use SQL sentences to search in a data frame
library(Amelia)  # Provides a way to visualize missing data in a data frame
library(caTools) # Para fazer split do dataset entre treino e teste
library(ranger)  # Para usar algoritmo de floresta

# Set working directory
setwd("/home/eduardo/Área de Trabalho/") # Colocar caminho onde salvou o banco
# Lendo Banco de Dados
atp_matches_2016 = read_csv("atp_matches_2016.csv") # Nome do arquivo do banco

# ----------------------------------------------
# TRATAMENTO DO BANCO
raw_data = atp_matches_2016

# Retirar linhas TOTALMENTE em branco
indices <- apply(raw_data, 1, function(raw_data) all(is.na(raw_data)))
raw_data = raw_data[!indices, ] # Novo data frame sem as linhas em branco


# Retirar linhas com carpete
raw_data = raw_data[raw_data$surface != 'Carpet', ]

# Total de valores nulos
sapply(raw_data, function(x) sum(is.na(x)))
# Colunas
glimpse(raw_data)

# Manter apenas colunas interessantes e descartar o resto
# surface (char)
# draw_size (int)
# score (char)
# minutes (int)
# w_ace l_ace (int)
# w_df l_df (int)
# w_svpt l_svpt (int)
# w_1stIn l_1stIn (int) 
# w_1stWon l_1stWon (int)
# w_2ndWon l_2ndWon (int)
# w_SvGms l_SvGms (int)
# w_bpSaved l_bpSaved (int)
# w_bpFaced l_bpFaced (int)
colunas_interessantes <- c("surface", "draw_size", "score", "minutes", "w_ace", "l_ace", "w_df", "l_df","w_svpt", "l_svpt", "w_1stIn", "l_1stIn", "w_1stWon", "l_1stWon", "w_2ndWon", "l_2ndWon", "w_SvGms", "l_SvGms", "w_bpSaved", "l_bpSaved", "w_bpFaced", "l_bpFaced")
warm_data = dataFrameLimpo <- subset(raw_data, select = colunas_interessantes)

# Podemos observar que ainda restaram alguns missing datas
sapply(warm_data, function(x) sum(is.na(x)))

# Analisando mais a fundo, percebemos que todas as linhas com 26 valores NULL são as mesmas, o que as tornam descartáveis
# As linhas com a coluna "minutes" faltando também são descartáveis
warm_data = warm_data[!is.na(warm_data$minutes) & !is.na(warm_data$w_ace),]

# Como podemos ver, agora nosso data frame deixou de ser esparso
colSums(is.na(warm_data))

# Isso nos fez perder 39 linhas, o que nos resulta em 2891 linhas aproveitáveis
dim(warm_data)

# ----------------------------------------------
# MANIPULAÇÃO DOS DADOS

# Quantos jogos por superficie
table(warm_data$surface, warm_data$draw_size)

# Draw Size
counts <- table(warm_data$draw_size, warm_data$surface)
barplot(counts, 
        main="Tamanho da Quadra por Superficie",
        xlab="Superficie", 
        ylab='Quantidade',
        col=c("green","blue","yellow","red","pink"),
        legend = rownames(counts), beside=FALSE) 

# Tratar score. 
#Criar coluna com quantidade de sets, quantidade de pontos média por set e se alguém foi retirado da partida.
warm_data$score
warm_data$qtdSets = 0
warm_data$qtdPontosMedia = 0
warm_data$jogadorRetirado = FALSE

# Para cada partida
for (row in 1:nrow(warm_data)){
  score = warm_data[row,'score']
  # Remove coisas desnecessarias no score
  score = gsub("\\s*\\([^\\)]+\\)","",as.character(score))
  # Separo os sets
  sets <- strsplit(score, " ")[[1]]
  # Marco se algum jogador foi retirado na partida
  if ('RET' %in% sets){
    warm_data[row,'jogadorRetirado'] = TRUE
    sets = sets[-length(sets)]
  }
  if ('DEF' %in% sets){
    warm_data[row,'jogadorRetirado'] = TRUE
    sets = sets[-length(sets)]
  }
  # Marco quantidade de sets
  qtdSets = length(sets)
  warm_data[row,'qtdSets'] = qtdSets
  # Tiro a media de pontos por set
  mediaPontos = 0
  for(set in sets){
    pontos = gsub("-"," ",as.character(set))
    pontos <- strsplit(pontos, " ")[[1]]
    pontos = as.integer(pontos)
    totalPontos = sum(pontos)
    mediaPontos = mediaPontos + (totalPontos/qtdSets)
  }
  warm_data[row,'qtdPontosMedia'] = as.integer(mediaPontos)
}

summary(warm_data$qtdPontosMedia)

g <- ggplot(warm_data, aes(x = surface, y = qtdPontosMedia)) + 
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) +
  scale_x_discrete(name = "Superficie") +
  scale_y_continuous(name = "Quantidade de Pontos", 
                     breaks = seq(0, 20, 2), limits=c(0, 20)) +
  ggtitle("Boxplot - Quantidade de Pontos por Set média") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9))
g

g <- ggplot(warm_data, aes(x = surface, y = qtdSets)) + 
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) +
  scale_x_discrete(name = "Superficie") +
  scale_y_continuous(name = "Quantidade de Sets", 
                     breaks = seq(0, 7, 1), limits=c(0, 7)) +
  ggtitle("Boxplot - Quantidade de Sets") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9))
g


# Tratar minutos. Separar por faixas de tempo.
# Normalizar pela quantidade de sets
warm_data$minutes = warm_data$minutes / warm_data$qtdSets
# Separar em faixas de tempo de 10 em 10 min e depois multiplicando por 5 pra distanciar

# Observar resultados do tratamento
summary(warm_data$minutes, warm_data$surface)

# Grafico
g <- ggplot(warm_data, aes(x = surface, y = minutes)) + 
     geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) +
     scale_x_discrete(name = "Superficie") +
     scale_y_continuous(name = "Tempo (minutos)", 
                        breaks = seq(0, 175, 5), limits=c(0, 175)) +
     ggtitle("Boxplot - Tempo Medio da Partida por Superficie") +
     theme(panel.border = element_blank(),
           panel.grid.major = element_line(colour = "#d3d3d3"),
           panel.grid.minor = element_blank(),
           plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
           text=element_text(family = "Tahoma"),
           axis.line = element_line(size=0.5, colour = "black"),
           axis.text.x = element_text(colour="black", size = 11),
           axis.text.y = element_text(colour="black", size = 9))
# Ignora Outliers
limitesY = boxplot.stats(warm_data$minutes)$stats[c(1, 5)]
g.zoom = g + coord_cartesian(ylim = limitesY*1.05)
g.zoom

# Tratar aces. 
# Transformar em uma variável quantidade de aces na partida. Separar em faixas.
warm_data$qtdAces = (warm_data$w_ace + warm_data$l_ace) / warm_data$qtdSets

summary(warm_data$qtdAces) 

# Grafico
g <- ggplot(warm_data, aes(x = surface, y = qtdAces)) + 
     geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) +
     scale_x_discrete(name = "Superficie") +
     scale_y_continuous(name = "Quantidade Media de Aces por Set", 
                        breaks = seq(0, 24, 2), limits=c(0, 24)) +
     ggtitle("Boxplot - Quantidade de Aces por Superficie") +
     theme(panel.border = element_blank(),
           panel.grid.major = element_line(colour = "#d3d3d3"),
           panel.grid.minor = element_blank(),
           plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
           text=element_text(family = "Tahoma"),
           axis.line = element_line(size=0.5, colour = "black"),
           axis.text.x = element_text(colour="black", size = 11),
           axis.text.y = element_text(colour="black", size = 9))
g

# Tratar Double Faults.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdDoubleFaults = (warm_data$w_df + warm_data$l_df) / warm_data$qtdSets

summary(warm_data$qtdDoubleFaults) 

# Grafico
g <- ggplot(warm_data, aes(x = surface, y = qtdDoubleFaults)) + 
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) +
  scale_x_discrete(name = "Superficie") +
  scale_y_continuous(name = "Quantidade Media de Double Faults por Set", 
                     breaks = seq(0, 10, 1), limits=c(0, 10)) +
  ggtitle("Boxplot - Quantidade de Double Faults por Superficie") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9))
g

# Tratar Pontos Vencido.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdPontosVencidos = (warm_data$w_svpt + warm_data$l_svpt) / warm_data$qtdSets

summary(warm_data$qtdPontosVencidos) 

# Grafico
g <- ggplot(warm_data, aes(x = surface, y = qtdPontosVencidos)) + 
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) +
  scale_x_discrete(name = "Superficie") +
  scale_y_continuous(name = "Quantidade Media de Pontos Vencidos por Set", 
                     breaks = seq(0, 102, 10), limits=c(0, 102)) +
  ggtitle("Boxplot - Quantidade de Pontos Vencidos por Superficie") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9))
g

# Tratar Acertos no 1º saque.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdAcertos1Saque = (warm_data$w_1stIn + warm_data$l_1stIn) / warm_data$qtdSets

summary(warm_data$qtdAcertos1Saque) 

# Grafico
g <- ggplot(warm_data, aes(x = surface, y = qtdAcertos1Saque)) + 
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) +
  scale_x_discrete(name = "Superficie") +
  scale_y_continuous(name = "Quantidade Media de Acertos 1º Saque", 
                     breaks = seq(0, 68, 5), limits=c(0, 68)) +
  ggtitle("Boxplot - Quantidade de Acertos no 1º Saque") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9))
g

# Tratar Pontos Ganhos no 1º saque.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdGanhos1Saque = (warm_data$w_1stWon + warm_data$l_1stWon) / warm_data$qtdSets

summary(warm_data$qtdGanhos1Saque) 

# Grafico
g <- ggplot(warm_data, aes(x = surface, y = qtdGanhos1Saque)) + 
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) +
  scale_x_discrete(name = "Superficie") +
  scale_y_continuous(name = "Quantidade Media de PG no 1º Saque", 
                     breaks = seq(0, 53, 5), limits=c(0, 53)) +
  ggtitle("Boxplot - Quantidade de Pontos Ganhos no 1º Saque") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9))
g

# Tratar Pontos Ganhos no 2º saque. # DESCARTADA
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdGanhos2Saque = (warm_data$w_2ndWon + warm_data$l_2ndWon) / warm_data$qtdSets

summary(warm_data$qtdGanhos2Saque) 

# Grafico
g <- ggplot(warm_data, aes(x = surface, y = qtdGanhos2Saque)) + 
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) +
  scale_x_discrete(name = "Superficie") +
  scale_y_continuous(name = "Quantidade Media de PG no 2º Saque", 
                     breaks = seq(0, 30, 5), limits=c(0, 30)) +
  ggtitle("Boxplot - Quantidade de Pontos Ganhos no 2º Saque") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9))
g

# Tratar quantidade de Games de Saque. 
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qdGamesSaque = (warm_data$w_SvGms + warm_data$l_SvGms) / warm_data$qtdSets

summary(warm_data$qdGamesSaque) 

# Grafico
g <- ggplot(warm_data, aes(x = surface, y = qdGamesSaque)) + 
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) +
  scale_x_discrete(name = "Superficie") +
  scale_y_continuous(name = "Quantidade Media de GS", 
                     breaks = seq(0, 17, 2), limits=c(0, 17)) +
  ggtitle("Boxplot - Quantidade de Games de Saque") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9))
g

# Tratar quantidade de Pontos Salvos.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qdPontosSalvos = (warm_data$w_bpSaved + warm_data$l_bpSaved) / warm_data$qtdSets

summary(warm_data$qdPontosSalvos) 

# Grafico
g <- ggplot(warm_data, aes(x = surface, y = qdPontosSalvos)) + 
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) +
  scale_x_discrete(name = "Superficie") +
  scale_y_continuous(name = "Quantidade Media de Pontos Salvos", 
                     breaks = seq(0, 13, 2), limits=c(0, 13)) +
  ggtitle("Boxplot - Quantidade de Pontos Salvos por Superficie") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9))
g

# Tratar quantidade de Break Points Enfrentados.
# Transformar em uma variável quantidade por partida. Separar em faixas.
warm_data$qtdBreakPointsEnfr = (warm_data$w_bpFaced + warm_data$l_bpFaced) / warm_data$qtdSets

summary(warm_data$qtdBreakPointsEnfr) 

# Grafico
g <- ggplot(warm_data, aes(x = surface, y = qtdBreakPointsEnfr)) + 
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) +
  scale_x_discrete(name = "Superficie") +
  scale_y_continuous(name = "Quantidade Media de BP Enfrentados", 
                     breaks = seq(0, 15, 2), limits=c(0, 15)) +
  ggtitle("Boxplot - Quantidade de Break Points Enfrentados") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9))
g

# -----------------------------------------
# RANDOM FOREST
newdata = warm_data

colunas_interessantes <- c("surface", "draw_size", "minutes", "qtdPontosMedia",  "qtdAces", "qtdGanhos1Saque", "qdPontosSalvos", "l_2ndWon", "qtdBreakPointsEnfr")
newdata_clust =  subset(newdata, select = colunas_interessantes)

glimpse(newdata_clust)

mask = sample.split(newdata_clust$surface, SplitRatio = 7/10)

train = newdata_clust[mask,]
test  = newdata_clust[!mask,]

set.seed(97)
rf.model = ranger(as.factor(surface) ~ ., data=train)

rf.pred = predict(rf.model,test)
table(test$surface, rf.pred$predictions)

acuracia = (89 + 13 + 428) / 867
acuraciaSeFosseChutadoHard = 510 / 867

table(test$surface)
