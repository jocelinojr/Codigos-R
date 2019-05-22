library(tidyverse)
completa_zeros <- function(vetor){
  
  zeros <- rep(0, 19-length(vetor))  
  c(vetor, zeros)

}



manu <- c(1,3,7,8,9,10,15)
craveiro <- c(1, 3, 5, 7, 9, 13, 15, 16, 18)
andre <- c(16, 1, 3, 8)
jocelino <- c(16, 15, 11)
juliana <- c(1,3,16,8)
fabio <- c(1, 3, 8, 16, 7, 15, 18)
adera <- c(1,2,7)
ronaldo <- c(15, 3, 7, 8, 9)
lineu <- c(1,5,11,13,18)
eric <- c(1,3,9,15,16,18)
dion <- c(1,7,2,9,8,3,18)


adera <- completa_zeros(adera)#
andre <- completa_zeros(andre)#
craveiro <- completa_zeros(craveiro)#
dion <- completa_zeros(dion)#
eric <- completa_zeros(eric)#
fabio <- completa_zeros(fabio)#
manu <- completa_zeros(manu)#
jocelino <- completa_zeros(jocelino)#
juliana <- completa_zeros(juliana)#
ronaldo <- completa_zeros(ronaldo)#
lineu <- completa_zeros(lineu)#

lugares <- seq(19)

matriz <- rbind(manu,
                craveiro,
                andre, 
                jocelino,
                juliana, 
                fabio,
                adera, 
                ronaldo,
                lineu,
                eric, 
                dion) 

df <- as.data.frame(t(matriz))
df <- gather(df, "pessoa", "lugar")

# exclui os lugares de numero 0
df <- filter(df, lugar >0)

# sumariza o dataset com as mesas escolhidas
qde_por_mesa <- count(df, lugar)
# ordena as mesas de forma decrescente por escolha
qde_por_mesa <- arrange(qde_por_mesa, desc(n))

# pega as mesas escolhidas 
mesas <- distinct(qde_por_mesa, lugar)
set.seed(184)
# copia o dataset com os dados para poder ir sorteando e apagando
df_sorteio_orig <- df
df_sorteio_orig$sorteado <- "N"
sorteados <- data.frame()

# loop nos lugares para ir sorteando
for (mesa in mesas$lugar) {
  # pega os candidatos da mesa da vez excluindo quem já foi sorteado anteriormente
  m <- filter(df_sorteio_orig, lugar == mesa, sorteado == "N")
  # se tem candidato para essa mesa...faz o sorteio
  # sorteia 1
  s <- sample_n(m, 1)
  # se teve gente sorteada...guarda na lista de sorteados
  if (nrow(s) != 0) {
    sorteados <- rbind(sorteados, s)
    # marca o sorteado para não entrar no próximo sorteio
    df_sorteio_orig[df_sorteio_orig$pessoa == s$pessoa, "sorteado"] <- "S"
  }

} # for

# apaga a coluna de controle
sorteados$sorteado <- NULL
# pega as mesas que não foram sorteadas
mesas_nao_sorteadas <-  mesas$lugar[!(mesas$lugar %in% sorteados$lugar)]
remanescente <- filter(df_sorteio_orig, sorteado == "N")


for (p in distinct(remanescente, pessoa)) {
  lugar <- sample(mesas_nao_sorteadas, 1, replace = FALSE)
  linha <- data.frame(pessoa=p, lugar=lugar) 
  sorteados <- rbind(sorteados, linha)
  # apaga essa opção
  mesas_nao_sorteadas[lugar] <- 0
}



mesas_livres <- lugares[!(lugares %in% sorteados$lugar)]



write.csv(sorteados, "sorteados.csv", row.names = FALSE)


lugares_indesejados <- c(4,6,12,14,17,19)
