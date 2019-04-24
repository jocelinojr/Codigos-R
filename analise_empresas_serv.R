library(tidyverse)
library(dplyr)
library(lubridate)
library(odbc)

#---------------------------------------------------------------------------
# Conecta no LABCONTAS e retorna a conexão
#---------------------------------------------------------------------------
conecta_labcontas <- function(alias){
  # Conecta no LABCONTAS
  dbConnect(odbc(), alias)

}


# ---------------------------------------------------------------------
# VERSÃO OBSOLETA
# 'Pega o CPF do servidor no Banco BD_SERVIDORES_CGU
# '@param x vetor com o cpf e o nome do servidor que iremos buscar
# VERSÃO OBSOLETA - DEMORA DEMAIS INDO PEGAR UMA A UM
# pega_cpf <- function(x) {
#   # guarda o cpf e o nome do servidor vindos do data.frame
#   cpf <- x[1]
#   nome <- x[2]
#   nome_serv <- paste("'", nome, "'", sep = "")
#   sql <- paste("SELECT CO_CPF FROM [BD_SERVIDOR].[dbo].[SERVIDOR_CGU] WHERE NOME_ORIGEM =", nome_serv)
#   print(sql)  
#   resultado <- dbSendQuery(con, sql)
#   servidor <- dbFetch(resultado)
#   # libera os recursos da query
#   dbClearResult(resultado)
#   
#   
#   # # se só encontramos 1...deu tudo certo
#   if (nrow(servidor) == 1) {
#     return(servidor$CO_CPF)
#   } # if
#   else {
#     return("***")
#     # compara o CPF do servidor trazido do banco da CGU
#     # str_detect(servidor$CO_CPF, "031184")
# 
#   } # else
#   
# } # func

#--------------------------------------------------------
# Vai até o banco dos servidores fornecido pela CGU e pega os CPFs
# completos dos servidores federais na Paraíba
pega_cpfs <- function(nomes_servidores, conexao, tabela, campos_select, campo_where){
  
  # seta a opção de quotes para o apóstrofo simples para funcionar no SQL Server
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


  
# Início ------------------------------------------------------------------

# Pega a lista dos servidores federais na Paraíba com os campos que interessam
serv_pb <- serv_federais %>% 
           filter(UF_EXERCICIO == "PB") %>%
           select(CPF, NOME, MATRICULA, FUNCAO, ORG_LOTACAO,
                  ORG_EXERCICIO, UORG_LOTACAO, SITUACAO_VINCULO)


# conecta com o banco
con <- conecta_labcontas("LABCONTAS")

# retira os caracteres estranhos do CPF
serv_pb$CPF <- str_remove_all(serv_pb$CPF, pattern = "[*\\.\\-]")


# A partir da lista de nomes dos servidores federais na PB vai à tabela de servidores da CGU no LABCONTAS e obtém o CPF completo deles
serv_pb_cpfs <- pega_cpfs(serv_pb$NOME, 
                          conexao = con, 
                          tabela = "[BD_SERVIDOR].[dbo].[SERVIDOR_CGU]",
                          campos_select = "CO_CPF, NOME_ORIGEM",
                          campo_where = "NOME_ORIGEM")

# remove as duplicidades
View(serv_pb_cpfs) <- distinct(serv_pb_cpfs)


# verifica se temos algum CPF que não tenha 11 dígitos, assim podemos pegar um pedaço do CPF
if (!any(nchar(serv_pb_cpfs$CO_CPF) != 11)) {
  # Cria uma coluna que contém uma parte do CPF para poder fazer o join com os servidores da PB
  serv_pb_cpfs$CPF_PARTE <- substr(serv_pb_cpfs$CO_CPF, 4, 9)
}

glimpse(serv_pb_cpfs)


# junta os datasets (left join)
# Left join para preservar todos os servidores da PB
serv_pb_join_cgu <-  merge(serv_pb, serv_pb_cpfs, by.x = c("CPF", "NOME"), by.y=c("CPF_PARTE", "NOME_ORIGEM"), all.x=TRUE)

# verifica que há uma diferença entre os datasets após a junção 
nrow(serv_pb_cpfs) - nrow(serv_pb_join_cgu)

# filtra todos os que não foram encontrados o CPF
serv_pb_sem_cpf <- filter(serv_pb_join_cgu, is.na(CO_CPF))


# Vai no LABCONTAS na tabela de CPF buscar os CPFs desses mais de 2.000 que estão sem
serv_pb_novos_cpfs <- pega_cpfs(serv_pb_sem_cpf$NOME, 
                                con, 
                                "[BD_RECEITA].[dbo].[CPF]",
                                "NUM_CPF, NOME",
                                "NOME")


View(serv_pb_novos_cpfs)

# Cria uma coluna que contém uma parte do CPF para poder fazer o join com os servidores da PB que ainda não temos o CPF
serv_pb_novos_cpfs$CPF_PARTE <- substr(serv_pb_novos_cpfs$NUM_CPF, 4, 9)


# junta os datasets (left join)
# Left join para preservar todos os servidores da PB
serv_pb_join_cpf_receita <-  merge(serv_pb_sem_cpf, serv_pb_novos_cpfs, by.x = c("CPF", "NOME"), by.y=c("CPF_PARTE", "NOME"), all.x=TRUE)

nrow(serv_pb_join_cpf_receita)
nrow(serv_pb_join_cgu)

apenas_cpf <-  filter(serv_pb_join_cgu, !is.na(CO_CPF))
apenas_cpf <-  rename(apenas_cpf, NUM_CPF=CO_CPF)

# pega apenas os que tem CPF e junta com os que acabamos de pegar no banco da Receita
serv_pb_completo <- rbind(apenas_cpf, serv_pb_join_cpf_receita)


servidore_orgao <- serv_pb_completo %>% 
  group_by(ORG_LOTACAO) %>%
  summarize(qde=n())

write_csv(servidore_orgao, "servidores_pb_por_orgao.csv")

head(servidore_orgao, 200)

filter(serv_pb_completo, is.na(NUM_CPF))

View(serv_pb_completo)
rm(serv_pb_join_final)
View(serv_pb_join_cgu)
view(serv_pb_sem_cpf)


# OBSOLETO
# vai até a tabela de SERVIDORES fornecida pela CGU para pegar o CPF completo dos servidores
# serv_pb$CPF_COMPLETO <-  apply(serv_pb[, c("CPF","NOME"), drop=FALSE], 1, pega_cpf)
#apply(serv_pb[, c("CPF","NOME"), drop=FALSE], 1, monta_sql_nomes)

























