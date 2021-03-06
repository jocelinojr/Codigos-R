#########################################################################

# instala pacotes
install.packages("xlsx")
install.packages("officer")
install.packages("WordR")


# Carrega as bibliotecas

library(readxl)
library(dplyr)
library(magrittr)
library(tidyverse)

library(xlsx)
library(officer)
library(WordR)

# seta a pasta de trabalho
getwd()
list.files()



################################################################################
# Defini��o das fun��es


# Pega os registros apenas dos empenhos de Bernardino Batista relativos aos documentos informados
selecBernardino <- function(nome_arq) {
  
  
  bernadino <- read_excel(nome_arq)
  str(bernadino)
  empenhos <- c("0002343", "0002344", "0002345", "0002346", "0002347", "0002348", 
                "0002349", "0002350", "0002389","0002390", "0002391", "0002392", 
                "0002393", "0002394", "0002395", "0002396", "0002431", "0002445")
  
  selec_berna <- bernadino %>%
    filter(`Empenho n�` %in% empenhos)
  sum(selec_berna$Pago)  
  # exporta a tabela para csv
  write.table(selec_berna, file="2502052_Bernardino_filtro.csv", sep=";", row.names = FALSE, dec = ",")

}



juntaDetalhes <- function(caminho_R, caminho_detalhes, arq_ibge){
  
  # Junta os arquivos de detalhes de pagamentos
  
  
  setwd(caminho_R)

  # taking a look at the folder content
  list.files()
  
  # cria nossos data.frames para manipular os dados
  df_detalhes <- data.frame()
  df_exc <- data.frame()
  
  
  # l� o arquivo que tem os c�digos do IBGE dos munic�pios
  munic_ibge <- read_excel(arq_ibge)
  # seleciona apenas as colunas que nos interessam
  munic_ibge <- select(munic_ibge, CODIBGE, Municipio)
  
  # Muda a pasta de trabalho para onde est�o os arquivos de detalhes
  # "C:/Users/tcujjunior/Documents/R/Detalhes Pagamentos"
  setwd(caminho_detalhes)
  
  
  # nome do arquivo de detalhes
  arq_detalhes <- "detalhes.csv"
  
  # verifica se temos o arquivo de detalhes j� gerado...caso sim, apaga-o
  if (file.exists(arq_detalhes)) {
    print("Apagando arquivo antigo de detalhes...")
    file.remove(arq_detalhes)
    print(paste("Arquivo ", arq_detalhes, " apagado com sucesso!"))
  } else {
    print("Arquivo de detalhes sendo gerado pela primeira vez...") 
  }
  
  
  for (arq in list.files()){
    print(arq)
    # pega o numero do municipio
    cod_mun <- str_sub(arq, 1, 7)
    # pega o nome do munic�pio
    nome_df <- munic_ibge %>% filter(CODIBGE == cod_mun) %>% select(Municipio)  
    nome_str <- nome_df$Municipio
    print(paste(cod_mun, nome_str))
    # l� o arquivo
    df_exc <- read_excel(arq)
    # inclui uma coluna para o codigo do municipio
    df_exc$CODIBGE <- cod_mun
    df_exc$Municipio <- nome_str
    # acumula  no data.frame
    df_detalhes <- rbind(df_detalhes, df_exc) 
    
  }
  
  # gera o arquivo de sa�da com os detalhes reunidos
  write.table(df_detalhes, file="detalhes.csv", sep=";", row.names = FALSE, dec = ",", fileEncoding = 'iso-8859-1')
  
  
}


processDiversos <- function(){
  # Diversos c�digos de manipula��o que foram usados em um dado momento
  
  
  # l� o arquivo 
  processos <- read_excel("fundef_processos.xls")
  class(processos)
  
  # sumariza os dados por vara
  (by_vara <- processos %>% 
      filter(!is.na(Vara)) %>%
      group_by(Vara) %>% 
      summarise(quant = n()) %>%
      arrange(Vara, quant))
  
  # exporta para uma planilha
  write.xlsx(by_vara, file = "por_vara.xls")
  
  (proces_by_vara <- processos %>%
      filter(`Recebeu recurso?` == "sim") %>%
      select(Processo, Vara, Municipio, `Valor  (R$)`) %>%
      arrange(Vara))
  
  
  # exporta para um csv
  write.csv(x =proces_by_vara,  "processos_vara.csv")
  write_excel_csv(x = proces_by_vara, "processos_vara_Excel.xls")
  
  
  ##############################################################
  municipios <- read_excel("Municipios.xls")
  advogados <- read_excel("Advogados.xls")
  detalhes <- read_excel("Detalhe_Gastos.xls")
  
  str(municipios)
  str(advogados)
  
  #  junta as tabelas com join
  join_mun_adv <- inner_join(municipios, advogados, by=c("Municipio", "Municipio"))
  str(join_mun_adv)
  
  # pega apenas os campos que queremos
  adv_new <- (join_mun_adv %>%
                select(CODIBGE.x, Municipio,`CPF/CNPJ Adv`, Advogado, `Valor Honor�rio`, Observa��o.y)) 
  
  write_excel_csv(adv_new, "advogados_new.csv")
  
  
  mun_reduc <- municipios %>% select(CODIBGE, Municipio)
  
  join_det_mun <- inner_join(mun_reduc, detalhes)
  write.table(join_det_mun, "")
  write.table(join_det_mun, file="new_detalhe.csv", sep=";", row.names = FALSE, dec = ",")
  
  
}


VerificaPagRemunera��o <- function(){
  #########################################################################################
  # L� o arquivo de detalhes para ver quem pagou Remunera��o na educa��o
  #########################################################################################
  
  
  # filtra 
  filtro_remunera <- df_detalhes %>% 
    filter(Fun��o == "Educa��o" & (Classifica��o %in% c("319011", "319091") )) %>%
    group_by(CODIBGE, Municipio) %>%
    summarise(quant = n())
  
  
  
  # Carrega o arquivo de Munic�pios que ser� usado no painel
  getwd()
  setwd("C:/Users/tcujjunior/Documents/R/")
  list.files()
  
  munic_painel <- read_excel("Municipios.xls")
  # converte o c�digo do IBGE para caracter
  munic_painel$CODIBGE <- as.character(munic_painel$CODIBGE)
  
  
  # junta com o data.frame que tem os municipios que pagaram remunera��o
  join_munic_remunera <- left_join(munic_painel, filtro_remunera, by="CODIBGE")
  
  join_munic_remunera$`Pagou Remuneracao`  <-  as.integer(join_munic_remunera$quant > 0)
  
  
  
  
  
  credores <- df %>%
    group_by(`Nome do Credor`) %>%
    summarise(Pago = sum(Pago)) %>%
    #   summarise(Pago = prettyNum(sum(Pago), scientific=FALSE, big.mark='.', decimal.mark = ',')) %>%
    arrange(desc(Pago))
  
  
  
}



#______________________________________________________________________________
# preenche os c�digos do IBGE na planilha de Advogados para alimentar o painel
#______________________________________________________________________________
colocaIBGE <- function(caminho_arq, datasetAlvo){
  
  
  # seta a pasta de trabalho, onde se encontram os arquivos
  setwd(caminho_arq)
  list.files()

  # carrega a planilha de ADVOGADOS
  ibge <- read_excel("IBGE.xls")
  
  # converte o nome dos munic�pios para sem acento e maisuculo em ambas as tabelas
  datasetAlvo$Municipio.Clean <- iconv(datasetAlvo$Municipio, from= "UTF-8",  to="ASCII//TRANSLIT")
  ibge$Municipio.Clean <- str_to_upper(iconv(ibge$de_Ente, from= "UTF-8",  to="ASCII//TRANSLIT"))
  
  # junta as tabelas 
  join_ibge <- inner_join(datasetAlvo, ibge, by=c("Municipio.Clean", "Municipio.Clean"))
  
  # copia o c�digo do IBGE 
  join_ibge$CODIBGE <- join_ibge$cd_IBGE
  
  
  # escreve nosso arquivo de sa�da
  write.table(advogados_new, file="Advogados.csv", sep=";", row.names = FALSE, dec = ",")
  
  
}





#______________________________________________________________________________
# preenche os c�digos do IBGE na planilha de Advogados para alimentar o painel
#______________________________________________________________________________
colocaIBGEAdv <- function(caminho_arq){
  
  
  # seta a pasta de trabalho, onde se encontram os arquivos
  setwd(caminho_arq)
  list.files()
  
  
  # carrega a planilha de ADVOGADOS
  advogados <- read_excel("Advogados.xls")
  ibge <- read_excel("IBGE.xls")
  
  # converte o nome dos munic�pios para sem acento e maisuculo em ambas as tabelas
  advogados$Municipio.Clean <- iconv(advogados$Municipio, from= "UTF-8",  to="ASCII//TRANSLIT")
  ibge$Municipio.Clean <- str_to_upper(iconv(ibge$de_Ente, from= "UTF-8",  to="ASCII//TRANSLIT"))
  
  # junta as tabelas 
  join_ibge_adv <- inner_join(advogados, ibge, by=c("Municipio.Clean", "Municipio.Clean"))
  
  # copia o c�digo do IBGE 
  join_ibge_adv$CODIBGE <- join_ibge_adv$cd_IBGE
  
  # Seleciona apenas os campos que queremos
  advogados_new <- select(join_ibge_adv, -cd_Ente, -cd_IBGE, -de_Ente, -Id_Ente)
  
  # escreve nosso arquivo de sa�da
  write.table(advogados_new, file="Advogados.csv", sep=";", row.names = FALSE, dec = ",")
  
  
}





########################################################################
# Insere os munic�pios que vieram da planilha da AGU e que ainda n�o tinham 
# sido contemplados na planilha de an�lise geral


insereMuncipiosAGU <- function(){
  
  print("Iniciando...")
  
  # loads the major list of towns
  munic_origi <- read_excel("Analise_Municipios_Prec_Fundef_v8.xlsx", sheet = "Lista Municipios")
  munic_origi
  
  # loads the AGU list
  agu_list <- read_excel("Embargos e A��es.xlsx")
  agu_list
  
  
  ##################################################
  # COloca o codigo do IBGE na lista da AGU

  # carrega a planilha de IBGE
  ibge <- read_excel("IBGE.xlsx")
  # converte o nome dos munic�pios para sem acento e maisuculo em ambas as tabelas
  agu_list$Municipio.Clean <- str_to_upper(iconv(agu_list$Munic�pio, from= "UTF-8",  to="ASCII//TRANSLIT"))
  ibge$Municipio.Clean <- str_to_upper(iconv(ibge$de_Ente, from= "UTF-8",  to="ASCII//TRANSLIT"))
  # junta as tabelas 
  join_agu_ibge <- left_join(agu_list, ibge, by=c("Municipio.Clean", "Municipio.Clean"))
  # seleciona apenas o que queremos
  agu_list_tidy <-  join_agu_ibge %>% select(-Id_Ente, -cd_Ente, -de_Ente)
  # muda o tipo do c�digo do processo para char
  agu_list_tidy$`Embargos � Execu��o` <- as.character(agu_list_tidy$`Embargos � Execu��o`)
  
  # Cria uma data.frame vazio com 20 vari�veis
  new_df <- data.frame("", 
                        agu_list_tidy$`Embargos � Execu��o`, 
                        "", 
                        agu_list_tidy$cd_IBGE, 
                        agu_list_tidy$Municipio.Clean, 
                        agu_list_tidy$`Valor Embargos`,
                        "", 
                        "",
                        "",
                        "",
                        "",
                        "",
                        "",
                        "",
                        "",
                        "",
                        "",
                        agu_list_tidy$Observa��o,
                        "",
                        "")
  
  
  names(new_df) <- c("PAG", 
                      "Processo TRF5", 
                      "Vara", 
                      "CODIBGE", 
                      "Municipio", 
                      "Valor  (R$)",
                      "Data recebimento", 
                      "Doc Recebido",
                      "Recebeu recurso",
                      "Utilizou conta FUNDEB ou espec�fica",
                      "Gastou fora MDE",
                      "Pagou honor�rios com recursos dos precat�rios",
                      "Pagou efetivos da �poca",
                      "Recurso/bloq do Prec",
                      "Honor�rios em discuss�o",
                      "Possui Procuradoria",
                      "Pagou Remuneracao",
                      "Observa��o",
                      "Auditor",
                      "Analise Feita")
  
  
  # converte os tipos de dados Factors
  new_df$PAG <- as.character(new_df$PAG)
  new_df$`Processo TRF5` <- as.character(new_df$`Processo TRF5`)
  new_df$Vara<- as.character(new_df$Vara)
  new_df$CODIBGE <- as.character(new_df$CODIBGE)
  new_df$Municipio<- as.character(new_df$Municipio)
  new_df$`Valor  (R$)`<- as.double(new_df$`Valor  (R$)`)
  new_df$`Data recebimento`<- NA
  new_df$`Doc Recebido`<- as.character(new_df$`Doc Recebido`)
  new_df$`Doc Recebido`<- "agu"
  new_df$`Recebeu recurso`<- as.character(new_df$`Recebeu recurso`)
  new_df$`Recebeu recurso`<- "n�o"
  new_df$`Utilizou conta FUNDEB ou espec�fica`<- as.character(new_df$`Utilizou conta FUNDEB ou espec�fica`)
  new_df$`Gastou fora MDE`<- as.character(new_df$`Gastou fora MDE`)
  new_df$`Pagou honor�rios com recursos dos precat�rios`<- as.character(new_df$`Pagou honor�rios com recursos dos precat�rios`)
  new_df$`Pagou efetivos da �poca`<- as.character(new_df$`Pagou efetivos da �poca`)
  new_df$`Recurso/bloq do Prec`<- as.character(new_df$`Recurso/bloq do Prec`)
  new_df$`Honor�rios em discuss�o`<- as.character(new_df$`Honor�rios em discuss�o`)
  new_df$`Possui Procuradoria`<- as.character(new_df$`Possui Procuradoria`)
  new_df$`Pagou Remuneracao`<- as.character(new_df$`Pagou Remuneracao`)
  new_df$Observa��o<- as.character(new_df$Observa��o)
  new_df$Auditor<- as.character(new_df$Auditor)
  new_df$`Analise Feita`<- as.character(new_df$`Analise Feita`)
  
  
  
  # junta os data.frames
  munic_binded <-  rbind(munic_origi, new_df)
  
  write.table(munic_binded, file="munic_com_agu.csv", sep=";", row.names = FALSE, dec = ",")
  
  print("Dados da AGU juntados aos demais municipios com sucesso!")
  
  
  
  
  
  
  
}
















#####################################################################################################
# Inicia o c�digo de execu��o

# pega nossa pasta atual
getwd()

# junta os arquivos de detalhes
juntaDetalhes("C:/Users/jocelinoms/Documents/R", 
              "C:/Users/jocelinoms/Documents/R/Detalhes Pagamentos", 
              arq_ibge = "IBGE.XLSX")

# trabalha dados em Bernardino
selecBernardino("2502052_Bernardino.xls")




###################################
# Coloca o codigo IBGE nos de Placido


# carrega a planilha de ADVOGADOS
placido <- read_excel("mun_placido.xls")
ibge <- read_excel("IBGE.xls")

# converte o nome dos munic�pios para sem acento e maisuculo em ambas as tabelas
placido$Municipio.Clean <- iconv(placido$Municipio, from= "UTF-8",  to="ASCII//TRANSLIT")
ibge$Municipio.Clean <- str_to_upper(iconv(ibge$de_Ente, from= "UTF-8",  to="ASCII//TRANSLIT"))

# junta as tabelas 
join_ibge_pla <- inner_join(placido, ibge, by=c("Municipio.Clean", "Municipio.Clean"))

# copia o c�digo do IBGE 
join_ibge_pla$CODIBGE <- join_ibge_pla$cd_IBGE


# Seleciona apenas os campos que queremos
join_ibge_pla

# escreve nosso arquivo de sa�da
write.table(join_ibge_pla, file="placido.csv", sep=";", row.names = FALSE, dec = ",")





#############################################################################
# Remove a �ltima linha de TOTAIS dos arquivos de detalhes antes de junt�-los
#############################################################################
# N�O FUNCIONOU!


setwd("C:/Users/tcujjunior/Documents/R/Limpeza")
getwd()

# taking a look at the folder content
list.files()

# cria nossos data.frames para manipular os dados
df_old <- data.frame()
df_new <- data.frame()


for (arq in list.files()){
  print(arq)
  # l� o arquivo
  df_old <- read_excel(arq)

}

# A leitura s� funciona quando o arquivo excel est� no formato tabular
df_old <- read_excel("2500734_Amparo (2).xls")
df_old



# gera o arquivo de sa�da com os detalhes reunidos
write.table(df_detalhes, file="detalhes.csv", sep=";", row.names = FALSE, dec = ",", fileEncoding = 'iso-8859-1')





#####################################################################################
# Adiciona (join) os municipios informados pela AGU ao dataset ja lavantado
insereMuncipiosAGU()







