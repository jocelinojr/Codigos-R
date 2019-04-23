library(lubridate)
library(tidyverse)

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





# R for Data Science ------------------------------------------------------
x <- 5
ifelse(x>10, print("Maior que 10"), print("Menor que"))




# Greeting function 
greetings <- function(hora_atu){
 if (hora_atu < 12){
   print("good morning")
 } else if (hora_atu > 12 && hora_atu < 18){
   print("good afternoon")
 } else {
   print("good evening")
 }
 
}





# testa a função de saudações
greetings(8)


# fizzbuzz
fizzbuz <- function(numero){
  resto_3 <- numero %% 3
  resto_5 <- numero %% 5
  # se é divisível por 3 e por 5
  if (resto_3 == 0 & resto_5 == 0){
    return("fizzbuzz")
  } else if (resto_3 == 0) {
    return("fizz")
  } else if (resto_5 == 0) {
    return("buzz")
  }
    
} # if


  
fizzbuz(6)  


str_c(sQuote(head(serv_pb$NOME, 20)), collapse = ",")





























