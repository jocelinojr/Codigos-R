

####################################
# M�todo 1 para criar  uma data frame a aprtir de um conjunto de n�meros 
# aleat�rios

set.seed(1014)
# cria o data.frame vazio
df <- data.frame()
# faz isso 6 vezes e acumula num data.frame
# pega uma amostra de 6 dentre o vetor com n�meros de 1 a 10 e o -99

for (i in 1:6){
    df <- rbind(df, sample(c(1:10, -99), 6, rep=TRUE))
}
# nomeia as colunas do data.frame
names(df) <- letters[1:6]



###################################################
# M�todo 2 : Usando a func��o replicate

set.seed(1014)
df <- data.frame(replicate(7, sample(c(1:10, -99), 6, rep=TRUE)))
names(df) <- letters[1:7]
