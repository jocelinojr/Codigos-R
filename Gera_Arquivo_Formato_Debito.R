getwd()
list.files()
debitos <- read.csv2("debito.txt")
debitos
str(debitos)

dados <- data.frame(coluna = "")



# inicia o contador
i <- 0
x <- ""
y <- ""
z <- ""

# loop no arquivo e escreve cada linha
for (row in 1: nrow(debitos)) {
  i <-  i+1

  valor <- debitos[row, "valor"]
  date <- debitos[row, "data"]
  
  data_vez <- paste("Data", as.character(i), "=", date, sep = "")
  valor_vez <- paste("Valor", as.character(i), "=", valor, sep = "")
  tipo_vez <- paste("Tipo", as.character(i), "=D", sep="")
  x[i] <- data_vez
  y[i] <- valor_vez
  z[i] <- tipo_vez
  

  print(data_vez)
  print(valor_vez)
  print(tipo_vez)


}


meus_dados <- data.frame(x, y, z)


for (row in 1: nrow(meus_dados)) {
   print(meus_dados[row,"x"])
  
}
  
  




write.table(meus_dados, "saida_tx.txt")

#write.table(debitos, file = "saida.txt")

