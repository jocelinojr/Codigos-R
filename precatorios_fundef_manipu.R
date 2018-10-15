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



setwd("C:/Users/tcujjunior/Documents/R/Detalhes")
getwd()

df <- data.frame()
df_exc <- data.frame()

list.files()

for (arq in list.files()){
  print(arq)
  # pega o numero do municipio
  cod_mun <- str_sub(arq, 1, 7)
  nome_mun <- str_sub(arq, 9, -1)
  # lê o arquivo
  df_exc <- read_excel(arq)
  # inclui uma coluna para o codigo do municipio
  df_exc$CODIBGE <- cod_mun
  # acumula  no data.frame
  df <- rbind(df, df_exc) 

  }

(df %>%
   group_by(`Nome do Credor`) %>%
   summarise(Pago = prettyNum(sum(Pago), scientific=FALSE, big.mark='.', decimal.mark = ',')))


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
      








