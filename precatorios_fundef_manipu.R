#########################################################################

# instala pacotes
install.packages("xlsx")


# Carrega as bibliotecas

library(readxl)
library(dplyr)
library(magrittr)
library(tidyverse)
library(xlsx)

getwd()

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


        




