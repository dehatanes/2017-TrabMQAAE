# Ler Dados
atp_matches_2016 = read_csv("atp_matches_2016.csv")

#seleção das partidas de 3 sets e que não usam carpet como piso
tresSets <- atp_matches_2016[which (atp_matches_2016$best_of==3 & atp_matches_2016$surface != "Carpet" & atp_matches_2016$w_ace != "NA" & atp_matches_2016$l_bpFaced!="NA"),]


#criação de varivel categorica para mão do vencedor
w_hand <- tresSets[12]
w_hand$right_hand<-0
w_hand$right_hand[w_hand$winner_hand=="R"]<-1

#criação dos atributos do vencedor
w_atributos <- data.frame( tresSets$winner_ht, w_hand$right_hand, tresSets$winner_age, tresSets$winner_rank)
w_atributos <- data.frame(w_atributos, tresSets[32:40])

#remoção dos casos NA
w_atributos <- w_atributos[which(w_atributos$tresSets.winner_ht!="NA" & w_atributos$tresSets.winner_rank!="NA"),]
w_atributos$winner<-1

#renomeação das variaveis
names(w_atributos)[1]<-"p_ht"
names(w_atributos)[2]<-"right_hand"
names(w_atributos)[3]<-"p_age"
names(w_atributos)[4]<-"p_rank"
names(w_atributos)[5]<-"p_ace"
names(w_atributos)[6]<-"p_df"
names(w_atributos)[7]<-"p_svpt"
names(w_atributos)[8]<-"p_1stIn"
names(w_atributos)[9]<-"p_1stWon"
names(w_atributos)[10]<-"p_2ndWon"
names(w_atributos)[11]<-"p_SvGms"
names(w_atributos)[12]<-"p_bpSaved"
names(w_atributos)[13]<-"p_bpFaced"



#criação de varivel categorica para mão do perdedor
l_hand <- tresSets[22]
l_hand$right_hand<-0
l_hand$right_hand[l_hand$loser_hand=="R"]<-1

#criação dos atributos do vencedor
l_atributos <- data.frame( tresSets$loser_ht, l_hand$right_hand, tresSets$loser_age, tresSets$loser_rank)
l_atributos <- data.frame(l_atributos, tresSets[41:49])

#remoção dos casos NA
l_atributos <- l_atributos[which(l_atributos$tresSets.loser_ht!="NA" & l_atributos$tresSets.loser_rank!="NA"),]
l_atributos$winner<-0

#renomeação das variaveis
names(l_atributos)[1]<-"p_ht"
names(l_atributos)[2]<-"right_hand"
names(l_atributos)[3]<-"p_age"
names(l_atributos)[4]<-"p_rank"
names(l_atributos)[5]<-"p_ace"
names(l_atributos)[6]<-"p_df"
names(l_atributos)[7]<-"p_svpt"
names(l_atributos)[8]<-"p_1stIn"
names(l_atributos)[9]<-"p_1stWon"
names(l_atributos)[10]<-"p_2ndWon"
names(l_atributos)[11]<-"p_SvGms"
names(l_atributos)[12]<-"p_bpSaved"
names(l_atributos)[13]<-"p_bpFaced"

#combinado w_atributos com l_atributos
p_atributos<-rbind(w_atributos, l_atributos)

#embaralha os dados
p_atributos_sf<-p_atributos[sample(nrow(p_atributos)),]

library("caTools", lib.loc="~/R/win-library/3.4")
#divisao do dataset, 80% para treino e 20% para teste
divisao<- sample.split(p_atributos_sf, SplitRatio = 0.8)
treino<-subset(p_atributos_sf,divisao=="TRUE")
teste<-subset(p_atributos_sf,divisao=="FALSE")

#criaçao do modelo usando regressao logistica 
model<-glm(winner ~., treino, family = binomial(link = "probit"))

#otmizaçao do modelo removendo a variavel mebos significativa
model<-glm(winner ~.-right_hand-p_age-p_1stIn, treino, family = binomial(link = "probit"))

#testando o modelo
res<-predict(model, teste, type = "response")

#construindo uma tabela com o resultado do teste do modelo, com o limiar de 0.5
matriz_conf<-table(ValorReal=teste$winner, ValorPrevisto=res>0.5)

#para determinar qual é o limiar ideal para o modelo
res<-predict(model, treino, type = "response")

#para determinar qual é o ponto de corte ideal para o modelo
library("ROCR", lib.loc="~/R/win-library/3.4")
ROCRPred<-prediction(res, treino$winner)
ROCRPref<-performance(ROCRPred, "tpr", "fpr")
plot(ROCRPref,colorize=TRUE,print.cutoffs.at=seq(0.05,by=0.05))


res<-predict(model, teste, type = "response")
matriz_conf<-table(ValorReal=teste$winner, ValorPrevisto=res>0.55)
