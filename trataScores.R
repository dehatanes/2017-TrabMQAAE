# Separar as pontuações
# criando um tipo de dado nomeado Score para teste
# coloco nele a pontuacao do dataset normal

scores <- mydata$score
scores <- data.frame(scores)

# crio colunas para cada um dos sets
scores$first_set <- lapply(strsplit(as.character(scores$scores), "\\ "), "[", 1)
scores$second_set <- lapply(strsplit(as.character(scores$scores), "\\ "), "[", 2)
scores$third_set <- lapply(strsplit(as.character(scores$scores), "\\ "), "[", 3)

# comeco a calcular a diferenca das pontuacoes por set
scores$diferenca <- strsplit(as.character(scores$first_set),'-') 

# criou um vetor com dois valores como caractere na coluna
vetor <- scores$diferenca

# ideia: achar a difrenca entre ambos
vals <- numeric()

for (i in 1:length(vetor)){
  if (is.na(vetor[i]))    next 
  if (('\\(' %in% vetor[i])==TRUE)  print(vetor[i])
  vals <- append(vals, Reduce("-", vetor[i]))
}
print(vals)



resultado <- (sum(scores$diferenca))
scores$diferenca <- lapply(sum(as.numeric(scores$diferenca)))
funcao_soma <- function(vetor){
  operando1 = as.numeric(out)
}
out <- unlist(out, use.names=FALSE)
do.call(sum, out)

soma <- function(lista){
  sum()
}

first_number <- strsplit(as.character(scores$first_set), "\\-")
first_number <- lapply(first_number, sum(x))
do.call(sum, first_number)
second_number <- strsplit(as.character(scores$first_set), "\\-")


scores <- data.frame(do.call(strsplit(as.character(scores$scores),' ')))

library(stringr)