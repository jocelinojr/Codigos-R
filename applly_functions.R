############################################
# Apply Functions
# Wheather data Challenge



getwd()
setwd(file.path(getwd(), "Weather_Data"))

list.files()

###############################################
# Carregando os arquivos que iremos trabalhar
# IMPORTANTE: temos que indicar que a primeira coluna cont�m os rownames, sen�o ela ser� importada
# como uma coluna no dataframe
chicago <- read.csv("Chicago-F.csv", row.names = 1)
new_york <- read.csv("NewYork-F.csv", row.names=1)
houston <- read.csv("Houston-F.csv", row.names = 1)
san_fran <- read.csv("SanFrancisco-F.csv", row.names = 1)



# convertendo os data frames para matrizes para facilitar nosso trabalho mais a frente
chicago <- as.matrix(chicago)
new_york <- as.matrix(new_york)
houston <- as.matrix(houston)
san_fran <- as.matrix(san_fran)


# coloca as matrizes numa lista
weather <- list(chicago=chicago, new_york=new_york, houston=houston, san_fran=san_fran)


# usando a fun��o Apply para obter as m�dias das m�tricas nos dataframes
apply(chicago, 1, mean)
apply(houston, 1, mean)
apply(new_york, 1, mean)
apply(san_fran, 1, mean)

# Fun��o que retorna a m�dia das linhas de uma matriz
rowMeans(chicago)

# usando lapply para pegar de uma vez s� a m�dias das linhas com rowMeans
m <- lapply(weather, rowMeans)

d <- as.data.frame(m)
d
# Awesome functions to use with lapply
# rowMeans()
# colMeans()
# rowSums()
# colSums()

  














