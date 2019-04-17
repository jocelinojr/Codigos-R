

####################################
# Método 1 para criar  uma data frame a aprtir de um conjunto de números 
# aleatórios

set.seed(1014)
# cria o data.frame vazio
df <- data.frame()
# faz isso 6 vezes e acumula num data.frame
# pega uma amostra de 6 dentre o vetor com números de 1 a 10 e o -99

for (i in 1:6){
    df <- rbind(df, sample(c(1:10, -99), 6, rep=TRUE))
}
# nomeia as colunas do data.frame
names(df) <- letters[1:6]



###################################################
# Método 2 : Usando a funcção replicate

set.seed(1014)
df <- data.frame(replicate(7, sample(c(1:10, -99), 6, rep=TRUE)))
names(df) <- letters[1:7]

