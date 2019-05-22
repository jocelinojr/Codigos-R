library(lubridate)
library(RPostgres)


##############################
# fun√ß√µes utilit√°rias

#' @title Faz a adequacao dos nomes dos campos de um dataset para o padrao do Postgre SQL
#' @name adequa_nomes_campos_postgre
#' @description Coloca os nomes dos campos de um dataset no padrao do Postgre SQL
#' @param nomes nomes dos campos a serem conformados ao padrao 
#' @author Jocelino Junior
#' @return Vector com os nomes dos campos devidamente formatados
#' @export              
adequa_nomes_campos_postgre <- function(nomes){
  
  # primeiro, remove todos os acentos
  nomes <- iconv(nomes, to="ASCII//TRANSLIT")  
  
  nomes <- gsub("[^a-z0-9]+","_", tolower(nomes) )
  
  nomes = make.names(nomes, unique=TRUE, allow_=TRUE)
  
  nomes = gsub('.','_',nomes, fixed=TRUE)  
  

}


#' @title Varre um dataframe convertendo os campos com nome data para date
#' @name converte_campos_data
#' @description Percorre um dataframe e verifica se o seu nome cont√©m "data", caso sim,
#'  converte o seu tipo para date
#' @param dataframe a ser mudado
#' @author Jocelino Junior
#' @return dataframe mudado
#' @export              
converte_campos_data <- function(tabela, FUN=dmy){
  
  # percorre as colunas da tabela emn busca de um campo data
  for (coluna in colnames(tabela)) {
    
    if (grepl("data", tolower(coluna))){
      
      tabela[, coluna] <-  as.Date(FUN(tabela[, coluna]))
      
    }
    
  }
  
  tabela

}


#' @title Varre um dataframe convertendo os campos de valor para num√©rico
#' @name converte_campos_valor
#' @description Percorre um dataframe e verifica se o seu nome cont√©m "valor", caso sim,
#'  converte o seu tipo para numeric
#' @param dataframe a ser mudado
#' @author Jocelino Junior
#' @return dataframe mudado
#' @export              
converte_campos_num <- function(tabela){
  # percorre as colunas da tabela emn busca de um campo data
  for (coluna in colnames(tabela)) {
    
    if (grepl("valor", tolower(coluna))){
      
      tabela[, coluna] <-  as.numeric(gsub(",", ".", tabela[, coluna]))
      
    }
    
  }
  
  tabela
  
  
  
  
}



# FunÁ„o para conectar ao banco de dados postgres
conecta_postgre <- function(host, dbname, user="postgres", password="root"){
  
  dbConnect(RPostgres::Postgres(), user = user,password = password, host = host, port = 5432, dbname = dbname)
  
}


# Recebe um vetor com valores numÈricos e formata-os para um formato de melhor vizualizaÁ„o com a opÁ„o de 
# incluir o R$ (default)
formata_numero <- function(valores, cifrao=FALSE) {
  
 preform <- valores %>%
             as.numeric() %>%
             format(nsmall=2, decimal.mark = ",", big.mark = ".", scientific = FALSE) %>%
             str_trim()
 
 simb <- ifelse(cifrao, "R$ ", "")
 str_c(simb, preform)
 

}

# Recebe um vetor com valores numÈricos e formata-os para um formato de moeda
formata_moeda <- function(valores) {
  
  preform <- valores %>%
    as.numeric() %>%
    format(nsmall=2, decimal.mark = ",", big.mark = ".", scientific = FALSE) %>%
    str_trim()
  
  str_c("R$ ", preform)

}


  
  
