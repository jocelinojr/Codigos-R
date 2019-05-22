# ==============================================================
# Contratos PB
# O arquivo pode ser baixado no link : http://paineldecompras.planejamento.gov.br/QvAJAXZfc/opendoc.htm?document=paineldecompras.qvw&lang=en-US&host=QVS%40srvbsaiasprd04&anonymous=true
# ==============================================================

# Incluido por jocelino
library(readxl)
source("Utils.R")
library(dplyr)
library(tidyverse)

# Guardando uma string com o caminho do arquivo .xlsx
caminho_arquivo <- "data/contratos_portal_siasg.xlsx"

# Usando a função read_excel para ler o arquio .xlsx, onde col_types será o tipo de cada coluna
contratos <- read_excel(caminho_arquivo, 
                        col_types = c("text", "text", "text", 
                                      "text", "text", "numeric", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text"))

colnames(contratos) <- adequa_nomes_campos_postgre(colnames(contratos))

# Tratamento da tabela
contratos[['data_fim_da_vigencia']] <- as.Date(contratos[['data_fim_da_vigencia']], "%d/%m/%Y")

# separa o campo uasg em duas colunas 
l <- str_split(string = contratos$uasg_contratante, pattern = "-", n = 2)
cod_uasg <- sapply(l, "[[", 1)
desc_uasg <- sapply(l, "[[", 2)
contratos$cod_uasg_contratante <- cod_uasg
contratos$desc_uasg_contratante <- desc_uasg
#apaga a UASG contratante antiga
contratos$uasg_contratante <- NULL

# Função para inserir os contratos no banco de dados
gravarTabelaContratos <- function(tabela, nome_tabela){
  
  colnames(tabela) <- adequa_nomes_campos_postgre(colnames(tabela))
  
  conexao <- conecta_postgre(host = "10.10.15.73", dbname = "SEC_PB")
  
  dbWriteTable(conexao, nome_tabela, tabela, overwrite = TRUE, row.names = FALSE)
  
  query <-  dbSendQuery(conexao, 'ALTER TABLE contratos_portal_siasg ADD COLUMN id_contrato SERIAL PRIMARY KEY')
  dbClearResult(query)
  
  query <- dbSendQuery(conexao, 'CREATE INDEX idx_contratos on contratos_portal_siasg (id_contrato)')
  dbClearResult(query)
  
  dbDisconnect(conexao)
  
  
}


# Início ------------------------------------------------------------------



gravarTabelaContratos(contratos, "contratos_portal_siasg")




