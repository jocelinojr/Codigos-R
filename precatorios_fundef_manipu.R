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
setwd("C:/Users/tcujjunior/Documents/R")



for (x in list.files()) {
  print(str_sub(x, 1, 8))
  
}




# lê o arquivo 
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


documento <- read_docx()
documento



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
    select(CODIBGE.x, Municipio,`CPF/CNPJ Adv`, Advogado, `Valor Honorário`, Observação.y)) 

write_excel_csv(adv_new, "advogados_new.csv", )


mun_reduc <- municipios %>% select(CODIBGE, Municipio)

join_det_mun <- inner_join(mun_reduc, detalhes)
write.table(join_det_mun, "")
write.table(join_det_mun, file="new_detalhe.csv", sep=";", row.names = FALSE, dec = ",")



############################################
# Pega os registros apenas dosempenhos de Bernardino Batista relativos aos documentos informados


bernadino <- read_excel("2502052_Bernardino.xls")
str(bernadino)




empenhos <- c("0002343", "0002344", "0002345", "0002346", "0002347", "0002348", 
              "0002349", "0002350", "0002389","0002390", "0002391", "0002392", 
              "0002393", "0002394", "0002395", "0002396", "0002431", "0002445")


selec_berna <- bernadino %>%
                  filter(`Empenho nº` %in% empenhos)


sum(selec_berna$Pago)  

# exporta a tabela para csv
write.table(selec_berna, file="2502052_Bernardino_filtro.csv", sep=";", row.names = FALSE, dec = ",")




###################################################################
# Junta os arquivos de detalhes de pagamentos
###################################################################


setwd("C:/Users/tcujjunior/Documents/R")
getwd()

# taking a look at the folder content
list.files()

# cria nossos data.frames para manipular os dados
df_detalhes <- data.frame()
df_exc <- data.frame()


# lê o arquivo que tem os códigos do IBGE dos municípios
munic_ibge <- read_excel('IBGE_2.xls')
# seleciona apenas as colunas que nos interessam
munic_ibge <- select(munic_ibge, CODIBGE, Municipio)

getwd()
setwd("C:/Users/tcujjunior/Documents/R/Detalhes Pagamentos")


# nome do arquivo de detalhes
arq_detalhes <- "detalhes.csv"

# verifica se temos o arquivo de detalhes já gerado...caso sim, apaga-o
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
  # pega o nome do município
  nome_df <- munic_ibge %>% filter(CODIBGE == cod_mun) %>% select(Municipio)  
  nome_str <- nome_df$Municipio
  print(paste(cod_mun, nome_str))
  # lê o arquivo
  df_exc <- read_excel(arq)
  # inclui uma coluna para o codigo do municipio
  df_exc$CODIBGE <- cod_mun
  df_exc$Municipio <- nome_str
  # acumula  no data.frame
  df_detalhes <- rbind(df_detalhes, df_exc) 

}

# gera o arquivo de saída com os detalhes reunidos
write.table(df_detalhes, file="detalhes.csv", sep=";", row.names = FALSE, dec = ",", fileEncoding = 'iso-8859-1')


#########################################################################################
# Lê o arquivo de detalhes para ver quem pagou Remuneração na educação
#########################################################################################


# filtra 
filtro_remunera <- df_detalhes %>% 
                  filter(Função == "Educação" & (Classificação %in% c("319011", "319091") )) %>%
                  group_by(CODIBGE, Municipio) %>%
                  summarise(quant = n())



# Carrega o arquivo de Municípios que será usado no painel
getwd()
setwd("C:/Users/tcujjunior/Documents/R/")
list.files()

munic_painel <- read_excel("Municipios.xls")
# converte o código do IBGE para caracter
munic_painel$CODIBGE <- as.character(munic_painel$CODIBGE)


# junta com o data.frame que tem os municipios que pagaram remuneração
join_munic_remunera <- left_join(munic_painel, filtro_remunera, by="CODIBGE")

join_munic_remunera$`Pagou Remuneracao`  <-  as.integer(join_munic_remunera$quant > 0)





credores <- df %>%
   group_by(`Nome do Credor`) %>%
   summarise(Pago = sum(Pago)) %>%
#   summarise(Pago = prettyNum(sum(Pago), scientific=FALSE, big.mark='.', decimal.mark = ',')) %>%
               arrange(desc(Pago))


write.table(df, file="sao_caiana_join.csv", sep=";", row.names = FALSE, dec = ",")


##################################
# trabalha com as transferencias de São José de Caiana

list.files()

transf_caiana <- read_excel('transf_sao_caiana.xls')
str(transf_caiana)

# total de transfe^rncias
sum(transf_caiana %>% 
  filter(Transf == "s") %>%
  select(Valor))

transf_caiana %>% 
      group_by(Transf) %>%
      summarise(Total = sum(Valor))
      




###########################################################################
# preenche os códigos do IBGE de uma planilha

getwd()
setwd("C:/Users/tcujjunior/Documents/R")
list.files()


# carrega a planilha de ADVOGADOS
advogados <- read_excel("Advogados.xls")
ibge <- read_excel("IBGE.xls")

# converte o nome dos municípios para sem acento e maisuculo em ambas as tabelas
advogados$Municipio.Clean <- iconv(advogados$Municipio, from= "UTF-8",  to="ASCII//TRANSLIT")
ibge$Municipio.Clean <- str_to_upper(iconv(ibge$de_Ente, from= "UTF-8",  to="ASCII//TRANSLIT"))

# junta as tabelas 
join_ibge_adv <- inner_join(advogados, ibge, by=c("Municipio.Clean", "Municipio.Clean"))

# copia o código do IBGE 
join_ibge_adv$CODIBGE <- join_ibge_adv$cd_IBGE

str(join_ibge_adv)

# Seleciona apenas os campos que queremos
advogados_new <- select(join_ibge_adv, -cd_Ente, -cd_IBGE, -de_Ente, -Id_Ente)

# escreve nosso arquivo de saída
write.table(advogados_new, file="Advogados.csv", sep=";", row.names = FALSE, dec = ",")

###################################
# Coloca o codigo IBGE nos de Placido


# carrega a planilha de ADVOGADOS
placido <- read_excel("mun_placido.xls")
ibge <- read_excel("IBGE.xls")

# converte o nome dos municípios para sem acento e maisuculo em ambas as tabelas
placido$Municipio.Clean <- iconv(placido$Municipio, from= "UTF-8",  to="ASCII//TRANSLIT")
ibge$Municipio.Clean <- str_to_upper(iconv(ibge$de_Ente, from= "UTF-8",  to="ASCII//TRANSLIT"))

# junta as tabelas 
join_ibge_pla <- inner_join(placido, ibge, by=c("Municipio.Clean", "Municipio.Clean"))

# copia o código do IBGE 
join_ibge_pla$CODIBGE <- join_ibge_pla$cd_IBGE


# Seleciona apenas os campos que queremos
join_ibge_pla

# escreve nosso arquivo de saída
write.table(join_ibge_pla, file="placido.csv", sep=";", row.names = FALSE, dec = ",")







#############################################################################
# Remove a última linha de TOTAIS dos arquivos de detalhes antes de juntá-los
#############################################################################


setwd("C:/Users/tcujjunior/Documents/R/Limpeza")
getwd()

# taking a look at the folder content
list.files()

# cria nossos data.frames para manipular os dados
df_old <- data.frame()
df_new <- data.frame()


for (arq in list.files()){
  print(arq)
  # lê o arquivo
  df_old <- read_excel(arq)

}

# A leitura só funciona quando o arquivo excel está no formato tabular
df_old <- read_excel("2500734_Amparo (2).xls")
df_old

?read_excel


# gera o arquivo de saída com os detalhes reunidos
write.table(df_detalhes, file="detalhes.csv", sep=";", row.names = FALSE, dec = ",", fileEncoding = 'iso-8859-1')














