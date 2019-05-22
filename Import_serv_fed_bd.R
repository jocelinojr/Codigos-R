library(tidyverse)
library(lubridate)
library(odbc)
library(RPostgres)
library(roxygen2)

source("Utils.R")


gravaTabelaPostgree <- function(tabela, conexao, nome_tabela){
  
  # Muda os nomes das colunas para inserir no banco
  colnames(tabela) <-  adequa_nomes_campos_postgre(colnames(tabela))
  
  dbWriteTable(conexao, name = nome_tabela, tabela, row.names=FALSE, overwrite = TRUE)
  
}


pega_cpfs <- function(nomes_servidores, conexao, tabela, campos_select, campo_where){
  #--------------------------------------------------------
  # Vai ate o banco dos servidores fornecido pela CGU e pega os CPFs
  # completos dos servidores federais na Paraiba
  
  
  # se nao informou o campo..nao executa
  if ((campos_select == "" || campos_select == "")) {
    stop("Informe os campos para  execução")
  }
  
  # seta a op??o de quotes para o ap?strofo simples para funcionar no SQL Server
  options(useFancyQuotes = FALSE)
  nomes <- str_c(sQuote(nomes_servidores), collapse = ',')
  nomes <- str_c("(", nomes, ")")
  sql <- str_c("SELECT", campos_select, "FROM", tabela, sep=" ")
  sql <- paste(sql,"WHERE", campo_where ,"IN",nomes)
  resultado <- dbSendQuery(conexao, sql)
  serv_cpf <- dbFetch(resultado)
  # libera os recursos da query
  dbClearResult(resultado)
  # retorna o data frame com os dados
  serv_cpf
}

pega_serv_fed_pb <- function(conexao, tabela){
  #---------------------------------------------------------------------
  # vai até o banco de dados onde gravamos os servidores federais com 
  # cpf e traz eles como um data frame

  sql <- str_c("SELECT * FROM", tabela, sep=" ")
  resultado <- dbSendQuery(conexao, sql)
  serv_pb <- dbFetch(resultado)
  # libera os recursos da query
  dbClearResult(resultado)
  # retorna o data frame com os dados
  select(serv_pb, num_cpf, everything())  

}

#--------------------------------------------
# Main function
import_serv_fed_bd <- function(nome_arq, banco=c("postgre", "labcontas")){
  
  #' @title Importa o arquivo de servidores federais baixado do Portal da Transparencia
  #' @name import_serv_fed_bd
  #' @description Importa o arquivo de servidores federais baixado do Portal da Transparencia
  #'              para um Banco de Dados. A principio PostGre ou SQL Server (LABCONTAS)
  #' @param nome_arq Nome do arquivo .csv que foi baixado do Portal da Transparencia 
  #' @param banco direciona para qual banco iremos importar o arquivo. 
  #' @details O banco de dados ja deve estar configurado. Se for no Labcontas o alias 
  #' odbc ja deve estar criado
  #' O arquivo original com os dados vem de:
  #' http://www.portaltransparencia.gov.br/download-de-dados/servidores   
  #' Após baixar o zip, extrair o arquivo que contém o Cadastro
  #' @author Jocelino Junior
  #' @export     

  cat("ATENÇÃO: Os dados da tabela serão sobrescritos.", "\n")
  cat("importando para o banco de dados", banco,"...", "\n")
  

  # Carrega o arquivo
  serv_federais <- read.csv(nome_arq, sep = ";")
  
  # Pega a lista dos servidores federais na Para?ba com os campos que interessam
  serv_pb <- serv_federais %>% 
    filter(UF_EXERCICIO == "PB") %>%
    select(CPF, NOME, MATRICULA, FUNCAO, ORG_LOTACAO,
           ORG_EXERCICIO, UORG_LOTACAO, SITUACAO_VINCULO)
  
  # conecta com o LABCONTAS
  con <- dbConnect(odbc(), "LABCONTAS")  

  # retira os caracteres estranhos do CPF
  serv_pb$CPF <- str_remove_all(serv_pb$CPF, pattern = "[*\\.\\-]")
  
  
  # A partir da lista de nomes dos servidores federais na PB vai ? tabela de servidores da CGU no LABCONTAS e obt?m o CPF completo deles
  serv_pb_cpfs <- pega_cpfs(serv_pb$NOME, 
                            conexao = con, 
                            tabela = "[BD_SERVIDOR].[dbo].[SERVIDOR_CGU]",
                            campos_select = "CO_CPF, NOME_ORIGEM",
                            campo_where = "NOME_ORIGEM")
  
  # remove as duplicidades
  serv_pb_cpfs <- distinct(serv_pb_cpfs)

  # Cria uma coluna que contem uma parte do CPF para poder fazer o join com os servidores da PB
  serv_pb_cpfs$CPF_PARTE <- substr(serv_pb_cpfs$CO_CPF, 4, 9)

  
  # junta os datasets (left join)
  # Left join para preservar todos os servidores da PB
  serv_pb_join_cgu <-  merge(serv_pb, serv_pb_cpfs, by.x = c("CPF", "NOME"), by.y=c("CPF_PARTE", "NOME_ORIGEM"), all.x=TRUE)
  
  # filtra todos os que nao foram encontrados o CPF
  serv_pb_sem_cpf <- filter(serv_pb_join_cgu, is.na(CO_CPF))
  
  # Vai no LABCONTAS na tabela de CPF buscar os CPFs desses mais de 2.000 que est?o sem
  serv_pb_novos_cpfs <- pega_cpfs(serv_pb_sem_cpf$NOME, 
                                  con, 
                                  "[BD_RECEITA].[dbo].[CPF]",
                                  "NUM_CPF, NOME",
                                  "NOME")
  
  
  # Cria uma coluna que contem uma parte do CPF para poder fazer o join com os servidores da PB que ainda nao temos o CPF
  serv_pb_novos_cpfs$CPF_PARTE <- substr(serv_pb_novos_cpfs$NUM_CPF, 4, 9)

  # junta os datasets (left join)
  # Left join para preservar todos os servidores da PB
  serv_pb_join_cpf_receita <-  merge(serv_pb_sem_cpf, serv_pb_novos_cpfs, by.x = c("CPF", "NOME"), by.y=c("CPF_PARTE", "NOME"), all.x=TRUE)
  
  # remove a coluna CO_CPF 
  serv_pb_join_cpf_receita$CO_CPF <- NULL
  apenas_cpf <-  filter(serv_pb_join_cgu, !is.na(CO_CPF))
  apenas_cpf <-  rename(apenas_cpf, NUM_CPF=CO_CPF)
  

  # pega apenas os que tem CPF e junta com os que acabamos de pegar no banco da Receita
  serv_pb_completo <- rbind(apenas_cpf, serv_pb_join_cpf_receita)

  # verifica qual banco de dados de destino iremos usar
  if (banco == "postgre"){
    # pega nossa conexao com o banco no servidor
    con_post_gree <- conecta_postgre(host = "10.10.15.73", dbname = "SEC_PB")
    # Apaga o campo que tinha apenas o cpf parcial, deixa o CPF completo
    serv_pb_completo$CPF <- NULL
    # grava no banco postgre sobreescrevendo a tabela já existente
    gravaTabelaPostgree(serv_pb_completo, con_post_gree, "serv_federais_pb")

  }
  
  num_import <- nrow(serv_pb_completo)                    
  # fecha as conexoes com os bancos de dados
  dbDisconnect(con_post_gree)  
  dbDisconnect(con)
  
  # limpa os objetos da memória
  rm(serv_pb)
  rm(serv_pb_join_cpf_receita)
  rm(serv_pb_novos_cpfs)
  rm(serv_pb_sem_cpf)
  rm(serv_pb_join_cgu)
  rm(serv_pb_cpfs)
  rm(serv_pb_completo)
  rm(apenas_cpf)
  
  cat(num_import, " Servidores Federais importados com Sucesso!")

}



# Início ------------------------------------------------------------------


# Origem dos dados
# http://www.portaltransparencia.gov.br/download-de-dados/servidores 
# 
# import_serv_fed_bd(file.path(getwd(), "data", "Servidores_Federais.csv"),
#                      "postgre")
# 
# pega nossa conexao com o banco no servidor
# con_post_gree <- conecta_postgre(host = "10.10.15.73", dbname = "SEC_PB")
# serv_pb_cpf <- pega_serv_fed_pb(con_post_gree, "serv_federais_pb")
# View(serv_pb_cpf)









