###################################################################
# Instalando Packages 

install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tibble")
install.packages("devtools")



# carregando/ativando pacotes importantes para manipulação de dados
library(magrittr)
library(dplyr)
library(tidyr)
library(tibble)
library(devtools)
library(readxl)
library(readr)




2*4
# cria uma array!
1:20

mean(1:20)
?mean
median(1:20)
meus_numeros <- c(1, 3, 15, 124, 289.54, 48,23)
mean(meus_numeros)
median(meus_numeros)

nome = "abc"
class(nome)
j = "jocelino  mendes da silva  Junior"
class(j)
length(j)


#  tipo numeric (números reais)
i = 10.5
k = 55
j = 22
class(i)
class(k)
class(j)

# tipos inteiros
inteiro1 = 50L
class(inteiro1)
inteiro1*2

# tipo lógico
verdade = TRUE
mentira = FALSE

# type cast 
print(as.character(verdade))

print(as.integer(verdade))
print(as.integer(mentira))

x <- 2

if (x == 2) { Sys.time() 
  }

if (verdade == mentira){ 
  print('Entrou')}

#########################################################
# EXERCÍCIOS
########################################################

# calculando o número de ouro

gold_number <- (1 + sqrt(5)) / 2
gold_number
  
div_zero <-  1 / 0
print(div_zero) 

# expressões que retornam Nan
0 /0
log(-1)

00 %% 3

5 + (3 * 10) %/% 3 == 15

######################################################
# O operador PIPE



# Exercícios do material.curso-r.com/pipe/
# reescrever usando o pipe

#1
round(mean(sum(1:10)/3), digits = 1)

# com o pipe
1:10 %>% 
  sum %>%
  divide_by(3) %>%  
  round(digits = 1)


#2
x <- rnorm(100)
x.pos <- x[x>0]
media <- mean(x.pos)
saida <- round(media, 1)

# com o pipe
set.seed(137)
rnorm(100) %>% extract(. > 0) %>% mean %>% round(digits = 2)


#3
2 %>% add(2) %>% c(6, NA) %>% mean(na.rm = T) %>% equals(5)

# Importando arquivos

fundef <- read_table2(file = "fundef.txt", sep)

fundef <- read_excel("fundef.xls")

# Manipulando e arrumando os dados


# baixando e instalando o pacote de dados para começar a diversão
devtools::install_github("abjur/abjData")
# problemas na instalação desse pacote...vamos usar o do fundef mesmo


fundef %>% select(Municipio, `Valor Recebido (R$)`)





