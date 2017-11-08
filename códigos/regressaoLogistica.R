library("caTools", lib.loc="~/R/win-library/3.4")
#divisao do dataset, 80% para treino e 20% para teste
divisao<- sample.split(p_atributos_sf, SplitRatio = 0.8)
treino<-subset(p_atributos_sf,divisao=="TRUE")
teste<-subset(p_atributos_sf,divisao=="FALSE")

#criaçao do modelo usando regressao logistica 
model<-glm(winner ~ p_ht+right_hand+p_age+p_rank+p_ace, treino, family = "binomial")
model<-glm(winner~., treino, family = "binomial")

#otmizaçao do modelo removendo a variavel mebos significativa
model<-glm(winner~. -p_1stIn, treino, family = "binomial")

#testando o modelo
res<-predict(model, teste, type = "response")

#construindo uma tabela com o resultado do teste do modelo, com o limiar de 0.5
matriz_conf<-table(ValorReal=teste$winner, ValorPrevisto=res>0.5)

#para calcular a previsao do modelo deve-se somar a diagonal principal da matriz_conf e dividir
#pela soma de todos os elementos da matriz_conf. Quando eu fiz deu 0.8023088. 

#para determinar qual é o limiar ideal para o modelo

res<-predict(model, treino, type = "response")

#para determinar qual é o ponto de corte ideal para o modelo
library("ROCR", lib.loc="~/R/win-library/3.4")
ROCRPred<-prediction(res, treino$winner)
ROCRPref<-performance(ROCRPred, "tpr", "fpr")
plot(ROCRPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
#usando 0.51 como limiar obtive 0.8037518
matriz_conf<-table(ValorReal=teste$winner, ValorPrevisto=res>0.51)