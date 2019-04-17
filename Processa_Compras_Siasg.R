library(tidyverse)
library(dplyr)
library(lubridate)
library(odbc)
library(RSQLite)

getwd()
setwd("C:/Users/jocelinoms/Documents/R")
list.files(pattern = "*.csv")

siasg <- read.csv("SIASG_30082018.csv", sep = ";", stringsAsFactors = TRUE)
fornec <- read.csv("fornecedores_SIASG.csv")
compras <- read.csv("201901_Compras.csv", sep=";")



compras <- as.tibble(compras)
glimpse(compras)
View(compras)

# Transforma��es nos dados

# converte as datas
compras$Data.In�cio.Vig�ncia <- as.Date(dmy(compras$Data.In�cio.Vig�ncia))
compras$Data.Fim.Vig�ncia <- as.Date(dmy(compras$Data.Fim.Vig�ncia))
compras$Data.Assinatura.Contrato <- as.Date(dmy(compras$Data.Assinatura.Contrato))
compras$Data.Publica��o.DOU <- as.Date(dmy(compras$Data.Publica��o.DOU))
# converte os valores num�ricos
compras$Valor.Final.Compra <- as.numeric(gsub(",", ".", compras$Valor.Final.Compra))
compras$Valor.Inicial.Compra <- as.numeric(gsub(",", ".", compras$Valor.Inicial.Compra))

# retira os sigilosos
compras <- compras[compras$CNPJ.Contratado != -11,]

# limpa o campo objeto removendo a palavra objeto e o campo Fundamento legal tb
compras$Objeto <- gsub("Objeto:", "", compras$Objeto)
compras$Fundamento.Legal <- gsub("Fundamento legal:", "", compras$Fundamento.Legal, ignore.case = TRUE)

cnpjs_sql <- NULL

trata_cnpj <- function(x){
    cat("x1 � isso:", x[1])
    # se o cnpj � num�rico
    if (grepl(pattern = "[A-z]", x)) {
      # acumula o cnpj na nossa lista de cnpjs para mandar ao SQL Server
      cnpjs_sql <- paste(cnpjs_sql, "'", x, "'" ,",", sep = "")
      cat("Vari�vel:", cnpjs_sql)
    } # if
    
}


lapply(cnpj_nomes, trata_cnpj)
#mostra a vari�vel montada
print(cnpjs_sql)


# pega apenas o CNPJ
############
cnpj_nomes <- compras %>% select(CNPJ.Contratado)
# converte para caracter
cnpj_nomes$CNPJ.Contratado <- as.character((cnpj_nomes$CNPJ.Contratado))


# transforma a coluna em vetor
v <- pull(cnpj_nomes, CNPJ.Contratado)
class(v)


cnpjs <- cnpj_nomes$CNPJ.Contratado

# verifica se temos cnpjs com algo al�m de n�meros

i <- 0
cnpjs_sql <- NULL
for (cnpj in cnpjs) {
  # se n�o temostemos alguma letra...monta
  if (!grepl(pattern = "[A-z]", cnpj)) {
    # limita a 20 cnpjs...por enquanto
    if (i < 20) {
      # acumula o cnpj na nossa lista de cnpjs para mandar ao SQL Server
      cnpjs_sql <- paste(cnpjs_sql, "'", cnpj, "'" ,",", sep = "")
      i <- i + 1 
    } # if
  } # if
} # for

# apaga a �ltima v�rgula
substr(cnpjs_sql, nchar(cnpjs_sql) , nchar(cnpjs_sql)) <- " "
# remove o espa�o em branco
cnpjs_sql <- str_trim(cnpjs_sql)
cnpjs_sql <- paste("(", cnpjs_sql, ")", sep="")
 

# Monta o Sql para trazer os dados das empresas
sql <- paste("SELECT * FROM [BD_RECEITA].[dbo].[CNPJ] WHERE NUM_CNPJ IN", cnpjs_sql)

######
# Vai at� o LABCONTAS para trazer os dados

inicio <- Sys.time()
# Conecta no LABCONTAS
con <- dbConnect(odbc(),"LABCONTAS")
resultado <- dbSendQuery(conn = con, sql)
CNPJ <- dbFetch(resultado)
# libera os recursos da query
dbClearResult(resultado)
dbDisconnect(con)
fim <- Sys.time()
tempo <- fim - inicio
print(paste("SQL Server:", tempo))


View(CNPJ)
#######################################
# Faz a mesma consulta no Sqllite

sqlite <- paste("SELECT * FROM cnpj_dados_cadastrais_pj WHERE cnpj IN", cnpjs_sql)

inicio <- Sys.time()
conlite <- dbConnect(SQLite(), dbname="bd_dados_qsa_cnpj.db")
resultadolite <- dbSendQuery(conlite, sqlite)
CNPJ_LITE <- dbFetch(resultadolite)
dbClearResult(resultadolite)
dbDisconnect(conlite)
fim <- Sys.time()
tempo <- fim - inicio
print(paste("SQL lite:", tempo))

View(CNPJ_LITE)



str(CNPJ)
View(CNPJ)

# Convertendo para Factory
CNPJ$NUM_CEP <- factor(CNPJ$NUM_CEP)
CNPJ$COD_MUNICIPIO <- factor(CNPJ$COD_MUNICIPIO)
CNPJ$SIGLA_UF <- factor(CNPJ$SIGLA_UF)
CNPJ$COD_ATIVIDADE_ECON_PRINCIPAL <- factor(CNPJ$COD_ATIVIDADE_ECON_PRINCIPAL)
# Converte as datas
CNPJ$DATA_SITUACAO_CADASTRAL <- as.Date(ymd(CNPJ$DATA_SITUACAO_CADASTRAL))
CNPJ$DATA_ABERTURA_ESTABELECIMENTO <- as.Date(ymd(CNPJ$DATA_ABERTURA_ESTABELECIMENTO))












  









