library(tidyverse)
library(lubridate)
library(odbc)
source("Utils.R")
library(blogdown)



pega_dados_cnpj_empresas <- function(con, cnpjs ){
  
  #------------------------------------------------------
  # Vai a tabela de CNPJ da Receita Federal buscar os dados das empresas 
  # que contrataram e estão no SIASG
  
  options(useFancyQuotes = FALSE)
  
  cnpjs <- str_c(sQuote(cnpjs), collapse = ',')
  
  cnpjs <- str_c("(", cnpjs, ")")
  
  sql <- str_c("SELECT NUM_CNPJ, NOME FROM [BD_RECEITA].[dbo].[CNPJ]",
               "WHERE NUM_CNPJ IN", 
               cnpjs,
               sep=" ")
  
  resultado <- dbSendQuery(con, sql)
  
  cnpjs_receita <- dbFetch(resultado)

  # libera os recursos da query
  dbClearResult(resultado)
  
  # retorna o data frame com os dados
  cnpjs_receita
  
}

pega_cpfs_socios <- function(con, cnpjs){
  
  # Pega o CPF dos sócios a partir de uma lista de CNPJS
  

  options(useFancyQuotes = FALSE)
  
  cnpjs <- str_c(sQuote(cnpjs), collapse = ',')
  
  cnpjs <- str_c("(", cnpjs, ")")

  sql <- str_c("SELECT NUM_CNPJ_EMPRESA, NUM_CPF, NOME FROM [BD_RECEITA].[dbo].[SOCIO]",
               "WHERE NUM_CNPJ_EMPRESA IN", 
               cnpjs,
               sep=" ")
  
  resultado <- dbSendQuery(con, sql)
  
  dados_socios <- dbFetch(resultado)
  
  # libera os recursos da query
  dbClearResult(resultado)
  
  # junta com os dados da empresa da Receita para pegar o nome da empresa
  dados_socios <- merge(dados_socios, cnpjs_receita, by.x= "NUM_CNPJ_EMPRESA", by.y = "NUM_CNPJ")
  
  # retorna o data frame com os dados
  dados_socios
  
  
}


# Vai a tabela de contratos do siasg em busca dos dados dos contratados em determinados anos
# passados como parâmetro
pega_cnpjs_siasg <- function(con, ano, uf="PB"){
  
  options(useFancyQuotes = FALSE)
  ano <- str_c(sQuote(ano), collapse = ",")
  ano <- str_c("(", ano, ")")
  uf <- sQuote(uf)
  sql <- str_c("SELECT * FROM contratos_portal_siasg WHERE ano IN ", 
               ano,
               "AND uf_contratado =", uf)
  resultado <- dbSendQuery(con, sql)
  contratados <-  dbFetch(resultado)
  dbClearResult(resultado)
  contratados
  
}


# vai ao LABCONTAS pegar os dados das compras para a PB ou outra unidade desejada
pega_aquisicoes_labcontas <- function(con, ano, uf="PB"){
  options(useFancyQuotes = FALSE)
  ano <- str_c(sQuote(ano), collapse = ",")
  ano <- str_c("(", ano, ")")
  uf <- sQuote(uf)

  sql <- str_c("SELECT VL_TOT_AQUISICAO, NO_UNDD_UNIDADE, ID_CMPR_COMPRA FROM  
                [BDA_AQUISICOES].[dbo].[TB_COMPRA] WHERE ANO_COMPRA IN ", 
               ano,
               "AND ID_UNDD_LCAL_UF = ", uf)
  resultado <- dbSendQuery(con, sql)
  contratados <-  dbFetch(resultado)
  dbClearResult(resultado)
  contratados
  
}


# vai na tabela que tem os itens das compras por fornecedor
pega_itens_compras_labcontas <- function(con, compras_id){
  
  options(useFancyQuotes = FALSE)
  compras <- str_c(sQuote(compras_id), collapse = ",")
  compras <- str_c("(", compras, ")")
  sql <- str_c("SELECT ID_FRND_FORNECEDOR, BASE_CNPJ_FORNECEDOR, ID_CMPR_COMPRA,
                NO_FRND_FORNECEDOR, VL_PRECO_UNIT_HOMOLOG, VL_PRECO_TOTAL_HOMOLOG, QT_AQUIS_ITEM_FORN FROM 
               [BDA_AQUISICOES].[dbo].[TB_COMPRA_ITEM_FORN] WHERE ID_CMPR_COMPRA IN ", compras)
  
  resultado <- dbSendQuery(con, sql)
  contratados <-  dbFetch(resultado)
  dbClearResult(resultado)
  contratados
  
}


# Cruza uma lista de CPFs com a base de servidores federais na PB
cruza_socios_servidores_pb <- function(con, socios){
  
  sql <- "SELECT * FROM serv_federais_pb"
  resultado <- dbSendQuery(con, sql)
  serv_pb <- dbFetch(resultado)
  dbClearResult(resultado)
  
  # faz o cruzamento dos servidores federais com os s?cios das empresas
  serv_pb_socios <- merge(socios, serv_pb, by.x = "NUM_CPF", by.y = "num_cpf")
  serv_pb_socios

}


# pega um dataframe com duas colunas sendo uma categ?rica e outra num?rica
# retornando um gr?fico de barras para mostrar as quantidades
gera_grafico_barras <- function() {
  
}



# Inicio ------------------------------------------------------------------


# # conecta no banco Postgre
#con_postgre <- conecta_postgre(host = "10.10.15.73", dbname = "SEC_PB")
# 
# # pega os contratos  no siasg na Para?ba para um ou mais anos
#contr <- pega_cnpjs_siasg(con_postgre, ano=c("2018", "2019"))
# 




# 
# 
# # agrupa para ver quem foram os maiores contratados 
# contratos_postgre_group <-  contr %>% 
#                             group_by(nome_do_fornecedor_contratado) %>%
#                             summarize(total = sum(valor_contratado)) %>%
#                             arrange(-total) 
# 
# contratos_postgre_group$total_format <- formata_numero(contratos_postgre_group$total, cifrao = TRUE)
# 
# # conecta no LABCONTAS
# con_lab <- dbConnect(odbc(), "LABCONTAS")  
# 
# # pega as compras no LABCONTAS
# aquisicoes_labcontas <- pega_aquisicoes_labcontas(con_lab, c("2018")) 
# # pega os itens dessas compras para poder sumarizar por fornecedor 
# compras <- unique(aquisicoes_labcontas$ID_CMPR_COMPRA)
# itens_compras <- pega_itens_compras_labcontas(con_lab,compras)
# 
# # retira as compras NA
# itens_compras_full <- filter(itens_compras, !is.na(itens_compras$QT_AQUIS_ITEM_FORN))
# 
# 
# 
# # Agrupa por fornecedor
# fornec_compras_group <- itens_compras_full %>%
#                     group_by(NO_FRND_FORNECEDOR) %>%
#                     summarise(total_fornec = sum(VL_PRECO_TOTAL_HOMOLOG)) %>%
#                     arrange(desc(total_fornec))
# 
# fornec_compras_group$total_format <- formata_numero(fornec_compras_group$total_fornec)  
# 
# 
# # faz o gr?fico das compras
# p_compras <- ggplot(head(fornec_compras_group), 
#             mapping = aes(x=reorder(NO_FRND_FORNECEDOR, total_fornec), y=total_fornec) )
# plot_compras <- p_compras + geom_bar(stat = "identity", width = .8, fill="#FF9999") + 
#   coord_flip() + guides(fill=FALSE) +
#   scale_y_continuous(labels = formata_moeda) + theme_light() + 
#   labs(title = "Maiores Fornecedores de ?rg?os Federais na Para?ba",
#        subtitle = "Compras: 2018 a 2019",
#        caption = "Fonte: Comprasnet",
#        x = "",
#        y = "Total Fornecido")
# 
# plot_compras
# 
# 
# 
# # faz o gr?fico dos contratos
# p_contratos <- ggplot(head(contratos_postgre_group),mapping = aes(x=reorder(nome_do_fornecedor_contratado,total), y=total) )
# plot_contratos <- p_contratos +
#                   geom_bar(stat = "identity", width = .8, fill="#FF9933") +
#                   coord_flip() +
#                   guides(fill=FALSE) +
#                   scale_y_continuous(labels = formata_moeda) +
#                   theme_light() +
#                   labs(title = "Maiores Contratos com ?rg?os federais na Para?ba",
#                        subtitle = "Contratos: 2018 a 2019",
#                        caption = "Fonte: Siasg",
#                        x = "",
#                        y = "Total Contratado")
# 
# plot_contratos
# 
# 
# 
# 
# 
# cnpjs_receita <-  pega_dados_cnpj_empresas(con_lab, contr$fornecedor_contratado)
# View(cnpjs_receita)
# 
# 
# # pega os dados dos s?cios no LABCONTAS
# socios <- pega_cpfs_socios(con_lab, cnpjs_receita$NUM_CNPJ)
# 
# # cruza com os servidores no estado
# serv_pb_socios <- cruza_socios_servidores_pb(con_postgre, socios)
# 
# # junta com os contratos dessas empresas para ver o montante contratado no per?odo
# serv_pb_socios_contratos <- merge(serv_pb_socios, contr, by.x = "NUM_CNPJ_EMPRESA", by.y = "fornecedor_contratado", all.x = TRUE)
# 
# serv_pb_socios_contratos
# 
# # sumariza por servidor os valores recebisos em contratos federais
# g <-  serv_pb_socios_contratos %>%
#   group_by(NOME.x, NOME.y) %>%
#   summarize(recebido = sum(valor_contratado)) %>%
#   arrange(recebido)
# 
# 
# # desconect dos bancos de dados
# dbDisconnect(con_postgre)
# dbDisconnect(con_lab)











# compras <- read.csv(file.path(getwd(), "data", "201901_Compras.csv"), sep=";")
# 
# # Arruma os nomes das colunas em nosso dataset
# colnames(compras) <- adequa_nomes_campos_postgre(colnames(compras))
# compras <- converte_campos_data(compras, FUN = dmy)
# compras <- converte_campos_num(compras)
# compras <- compras[compras$cnpj_contratado != -11,]
# # limpa o campo objeto removendo a palavra objeto e o campo Fundamento legal tb
# compras$objeto <- gsub("Objeto:", "", compras$objeto)
# compras$fundamento_legal <- gsub("Fundamento legal:", "", compras$fundamento_legal, ignore.case = TRUE)
# 
# 
# # conecta no Banco de dados
# con <- dbConnect(odbc(), "LABCONTAS")
# compras$cnpj_contratado <- as.character(compras$cnpj_contratado)
# 
# # pegas apenas os CNPJs (deixa os CPFs de lado) e remove os duplicados da lista de 
# # cnpjs que iremos usar para pegar os socios
# cnpjs_pesq <-  unique(compras[nchar(compras$cnpj_contratado) == 14, "cnpj_contratado"])
# 
# # dados do banco CNPJ da receita federal
# cnpjs_receita <- pega_dados_cnpj_empresas(cnpjs_pesq, con)
# 
# 
# # pega os sócios dessas empresas 
# dados_socios <- pega_cpfs_socios(cnpjs_receita$NUM_CNPJ)
# 
# 
# dados_socios %>% count(NUM_CNPJ_EMPRESA)
# 
# 
# # close the connection
# dbDisconnect(con)
# 
# 
# 
# 
# # verifica se temos cnpjs com algo al?m de n?meros
# 
# i <- 0
# cnpjs_sql <- NULL
# for (cnpj in cnpjs) {
#   # se n?o temostemos alguma letra...monta
#   if (!grepl(pattern = "[A-z]", cnpj)) {
#     # limita a 20 cnpjs...por enquanto
#     if (i < 20) {
#       # acumula o cnpj na nossa lista de cnpjs para mandar ao SQL Server
#       cnpjs_sql <- paste(cnpjs_sql, "'", cnpj, "'" ,",", sep = "")
#       i <- i + 1 
#     } # if
#   } # if
# } # for
# 
# # apaga a ?ltima v?rgula
# substr(cnpjs_sql, nchar(cnpjs_sql) , nchar(cnpjs_sql)) <- " "
# # remove o espa?o em branco
# cnpjs_sql <- str_trim(cnpjs_sql)
# cnpjs_sql <- paste("(", cnpjs_sql, ")", sep="")
#  
# 
# # Monta o Sql para trazer os dados das empresas
# sql <- paste("SELECT * FROM [BD_RECEITA].[dbo].[CNPJ] WHERE NUM_CNPJ IN", cnpjs_sql)
# 
# ######
# # Vai at? o LABCONTAS para trazer os dados
# 
# inicio <- Sys.time()
# # Conecta no LABCONTAS
# con <- dbConnect(odbc(),"LABCONTAS")
# resultado <- dbSendQuery(conn = con, sql)
# CNPJ <- dbFetch(resultado)
# # libera os recursos da query
# dbClearResult(resultado)
# dbDisconnect(con)
# fim <- Sys.time()
# tempo <- fim - inicio
# print(paste("SQL Server:", tempo))
# 
# 
# View(CNPJ)
# #######################################
# # Faz a mesma consulta no Sqllite
# 
# sqlite <- paste("SELECT * FROM cnpj_dados_cadastrais_pj WHERE cnpj IN", cnpjs_sql)
# 
# inicio <- Sys.time()
# conlite <- dbConnect(SQLite(), dbname="bd_dados_qsa_cnpj.db")
# resultadolite <- dbSendQuery(conlite, sqlite)
# CNPJ_LITE <- dbFetch(resultadolite)
# dbClearResult(resultadolite)
# dbDisconnect(conlite)
# fim <- Sys.time()
# tempo <- fim - inicio
# print(paste("SQL lite:", tempo))
# 
# View(CNPJ_LITE)
# 
# 
# 
# str(CNPJ)
# View(CNPJ)
# 
# # Convertendo para Factory
# CNPJ$NUM_CEP <- factor(CNPJ$NUM_CEP)
# CNPJ$COD_MUNICIPIO <- factor(CNPJ$COD_MUNICIPIO)
# CNPJ$SIGLA_UF <- factor(CNPJ$SIGLA_UF)
# CNPJ$COD_ATIVIDADE_ECON_PRINCIPAL <- factor(CNPJ$COD_ATIVIDADE_ECON_PRINCIPAL)
# # Converte as datas
# CNPJ$DATA_SITUACAO_CADASTRAL <- as.Date(ymd(CNPJ$DATA_SITUACAO_CADASTRAL))
# CNPJ$DATA_ABERTURA_ESTABELECIMENTO <- as.Date(ymd(CNPJ$DATA_ABERTURA_ESTABELECIMENTO))












  










