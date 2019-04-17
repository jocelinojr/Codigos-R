install.packages("RPostgreSQL")
library(RPostgreSQL)
library(tidyverse)
library(dplyr)

glimpse(iris)

###############################
# Verificar o uso dessas funções

#dbQuoteIdentifier()
# dbQuoteString


############################################################
# Função que altera os nomes para ficarem no padrão SQL PostGree
dbSafeNames <- function(nomes){
  nomes <- gsub("[^a-z0-9]+","_", tolower(nomes) )
  nomes = make.names(nomes, unique=TRUE, allow_=TRUE)
  nomes = gsub('.','_',nomes, fixed=TRUE)  
  return(nomes)

}


gravaTabelaPostgree <- function(tabela, nome_tabela){
  # Muda os nomes das colunas para inserir no banco
  colnames(tabela) <-  dbSafeNames(colnames(tabela))
  pg <-  dbDriver("PostgreSQL")
  con <- dbConnect(drv = pg, user="postgres", password="root", host="10.10.15.73", port=5432, dbname="SEC_PB")
  # cria a tabela no banco
    dbWriteTable(con, name = nome_tabela, tabela, row.names=FALSE)
  dbDisconnect(con)

}


filtraTabela <- function(nome_tabela, campos_resultado, condicao){
  
  pg <-  dbDriver("PostgreSQL")
  con <- dbConnect(drv = pg, user="postgres", password="bs@pv215", host="localhost", port=5432, dbname="SEC_PB")
  # monta a query
  sql <- paste("SELECT", campos_resultado, "FROM", nome_tabela, "WHERE", condicao)
  print(sql)
  resultado <- dbSendQuery(con, sql)
  dados_fetched <- dbFetch(resultado)
  dbClearResult(resultado)
  dbDisconnect(con)
  return(dados_fetched)

}



getwd()
list.files(pattern = "*.csv")

serv_federais <- read.csv("Servidores Federais.csv", sep = ";")


rownames(serv_federais)


# Insere a tabela no banco
gravaTabelaPostgree(tabela = serv_federais, nome_tabela = "servidores_federais")


pg <-  dbDriver("PostgreSQL")
con <- dbConnect(drv = pg, user="postgres", password="bs@pv215", host="localhost", port=5432, dbname="SEC_PB")
f <- dbListFields(con, name = "servidores_federais")


mar <-  filtraTabela(nome_tabela = "servidores_federais", 
                      campos_resultado = "nome, cpf, descricao_cargo",
                      condicao = "nome like 'MAR%' ")


View(mar)




















