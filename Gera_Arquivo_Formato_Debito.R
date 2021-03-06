#####################################################################
# Pega um arquivo tipo csv (OU TEXTO) contendo colunas (data e valor)
# e o converte em um arquivo no formato do sistema d�bito


getwd()
list.files()
debitos <- read.csv2("debito.csv")
debitos
str(debitos)

# inicia o contador
i <- 0
# cria nosso empty vector
x <- vector(mode = "character")


# loop no arquivo e escreve cada linha
for (row in 1: nrow(debitos)) {
  
  i <-  i+1

  valor <- debitos[row, "valor"]
  date <- debitos[row, "data"]

  
  data_vez <- paste("Data", as.character(i), "=", date, sep = "")
  valor_vez <- paste("Valor", as.character(i), "=", valor, sep = "")
  tipo_vez <- paste("Tipo", as.character(i), "=D", sep="")
  
  # escreve os dados no vector
  x[length(x)+1] <- data_vez
  x[length(x)+1] <- valor_vez
  x[length(x)+1] <- tipo_vez

  # mostra nossa sa�da
  print(data_vez)
  print(valor_vez)
  print(tipo_vez)
  

}


# cria o data.frame para escrever o arquivo sa�da
meus_dados <- data.frame(x)

write.table(meus_dados, "saida_tx.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)



