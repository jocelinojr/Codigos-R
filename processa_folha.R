library(tidyverse)
library(readxl)
library(plotly)
source("Utils.R")




# carrega um arquivo criado a partir de dados do SAGRES com o total recebido a título de Fundeb
# valores pelo estado da Paraíba e pelo município em estudo
carrega_transf_fundeb <- function(){
  
  transf_fundeb <- read.csv("data/Transferencias_fundeb_2017-2019_sagres.csv", sep=";")
  
  # emilina os registros com NA
  transf_fundeb <-  na.omit(transf_fundeb)
  
  transf_fundeb$valor <- as.character(transf_fundeb$valor)
  
  transf_fundeb$valor <- as.numeric( gsub(",", ".", gsub("\\.", "", transf_fundeb$valor))) 
  
  transf_fundeb$ano <- factor(transf_fundeb$ano)
  
  fundeb_ano <-  transf_fundeb %>% 
    group_by(ano, ente) %>% 
    summarise(total =  sum(valor))
  
  fundeb_ano

}

# Carrega o arquivo do TCE-PB contendo a folha de pagamento 
# Por default, carrega-se o arquivo do estado. Caso queira um município,
# deve-se informar esfera="Municipal" e o nome do município
# O arquivo foi obtido junto ao TCE-PB via SSH acessando a pasta 
# compartilhada com os órgãos parceiros da controle 
carrega_folha_pagto <- function(nome_arq, 
                                esfera="Estadual",
                                munic = "",
                                ano="",
                                mes="",
                                cargo="profess",
                                cod_orgao="") {
  
  arq_folha  <- file.path(getwd(), nome_arq)
  
  folha <- read.csv(arq_folha, sep = ";")
  
  # filtra apenas os registros da esfera que queremos
  folha <- filter(folha, ESFERA == esfera)
  
  # converte os tipos de dados
  folha$NM_SERVIDOR <-as.character(folha$NM_SERVIDOR)
  folha$NM_ORGAO <- as.character(folha$NM_ORGAO)
  folha$DS_CARGO <- as.character(folha$DS_CARGO)
  
  # se queremos de um municipio, filtra por ele
  if (esfera == "Municipal"){
    folha <- subset(folha, grepl(tolower(munic), tolower(folha$NM_ORGAO)))
  }
  folha$DS_CARGO <- as.character(folha$DS_CARGO)
  # pega apenas o cargo de professor
  folha <-  subset(folha, grepl(cargo, tolower(folha$DS_CARGO)))
  # se queremos um ano especifico, filtra
  if (ano != ""){
    folha <- subset(folha, A_ANO == ano)
  }
    
  # se queremos um mes especifico
  if (mes != ""){
    folha <- subset(folha, A_MES == mes)  
  }
  
  # Código do órgão
  if (cod_orgao != ""){
    folha <- subset(folha, COD_ORGAO == cod_orgao)  
  }
  
  
  # volta o cargo para factor para poder fazer estatísticas
  folha$DS_CARGO <- as.factor(folha$DS_CARGO)
  
  folha
  
}



# Carrega os dados vindos do SIOPE. 
# acessíveis em:
# para o Estado
# https://www.fnde.gov.br/siope/consultarRemuneracaoEstadual.do
# Para os municípios
# https://www.fnde.gov.br/siope/consultarRemuneracaoMunicipal.do
# deve-se converter o arquivo de .xls para .csv, uma vez que estava havendo 
# problemas para ler o campo com os valores no .xls direto
carrega_siope <- function(pasta_siope){
  
  # cria nosso objeto data frame vazio para acumlar 
  siope <- data.frame()
  
  # loop nos arquivos da pasta de dados para juntá-los num só
  for (arq in list.files(pasta_siope)){
    
    nome_arq <- file.path(pasta_siope, arq)
    # pega o arquivo da vez
    dados_arq <- read.csv(nome_arq, sep = ";") 
    # concatena na variável acumuladora
    siope <- rbind(siope, dados_arq)
  }
  

  # ajusta o nome dos campos
  nomes <- adequa_nomes_campos_postgre(names(siope))
  names(siope) <- nomes

  siope$total <- as.character(siope$total_a_b_c_)

  siope$total <- as.numeric( gsub(",", ".", gsub("\\.", "", siope$total))) 
  
  siope

}


# faz todo o processamento para podermos ter um dataframe com
# os dados da folha do estado e do siope para fins de comparação. Salva o arquivo em RDS
gera_folha_siope_estado <- function(){
  #############################################################
  # FOLHA DE PAGAMENTO - Origem TCE-PB
  #############################################################
  
  # carrega a folha em RDS
  folha_estado <-  readRDS("data/folha_pb.rds")
  
  # se já não pegamos em RDS, processa o arquivo original
  if (!is.data.frame(folha_estado)) {
    
    # pega a folha do estado de 2015 a 2018
    folha_estado <- carrega_folha_pagto("data/TCE-PB-Folha_2015-2018.csv", 
                                        esfera = "Estadual",
                                        munic = "",
                                        ano = "", 
                                        mes = "")

    # carrega a folha de 2019
    folha_estado_19 <- carrega_folha_pagto("data/TCE-PB-Folha_2019.csv", 
                                           esfera = "Estadual",
                                           munic = "",
                                           ano = "", 
                                           mes = "")

    
    # filtra apenas 2017 e 2018, já que no SIOPE só temos esses dois anos
    folha_estado <- filter(folha_estado, A_ANO %in% c(2017, 2018))
    folha_estado$A_ANO <- factor(folha_estado$A_ANO)
    
    folha_estado_19$A_ANO <- factor(folha_estado_19$A_ANO)
    
    # Filtra apenas os 4 primeiros meses do ano para poder comparar com o siope
    folha_estado_19 <- filter(folha_estado_19, A_MES %in% c(1,2,3,4))
    
    folha_estado <- rbind(folha_estado, folha_estado_19)
    
    # salva em RDS
    saveRDS(folha_estado, file = "data/folha_pb.rds")  
    
        
  }


  # valor pago a cada ano
  folha_estado_por_ano <-  folha_estado %>% 
    group_by(A_ANO) %>% 
    summarise(total = sum(VL_REMUNERACAO_TOTAL))
  
  # renomeia as colunas
  names(folha_estado_por_ano) <- c("ano", "total")
  folha_estado_por_ano$origem <- "folha de pagamento"
  
  
  #############################################################
  # SIOPE
  #############################################################
  
  
  
  # pega os dados do SIOPE da folha estadual
  siope_estado <- carrega_siope(file.path(getwd(), "data", "siope","estado"))
  
  # transforma o ano em factor
  siope_estado$ano <- factor(siope_estado$ano)
  
  # coloca os meses na ordem
  siope_estado$mes <- factor(siope_estado$mes, levels = c("Janeiro", 
                                                          "Fevereiro",
                                                          "Março",
                                                          "Abril",
                                                          "Maio",
                                                          "Junho",
                                                          "Julho",
                                                          "Agosto",
                                                          "Setembro",
                                                          "Outubro",
                                                          "Novembro",
                                                          "Dezembro"))
  
  
  # removendo os vazios (NA) do dataset
  siope_estado <- na.omit(siope_estado)
  
  # agrupa o SIOPE por ano para compararmos com a folha do estado
  siope_estado_ano <- siope_estado %>% 
    group_by(ano) %>% 
    summarise(total = sum(total, na.rm = TRUE))
  
  
  # acrescenta uma coluna para identificar que é do SIOPE
  siope_estado_ano$origem <- "siope"
  
  # junta os datasets para podermos vizualizar no gráfico juntos
  siope_folha_join <- rbind(siope_estado_ano, folha_estado_por_ano)
  
  # salva o arquivo gerado para RDS a fim de ter melhor desempenho
  saveRDS(siope_folha_join, "data/folha_siope_pb_sumario.rds")

}

# retorna um dataframe com o arquivo dos dados sumarizados Folha x Siope
pega_folha_siope_estado <- function(){
  
  folha_siope_sumup <- readRDS("data/folha_siope_pb_sumario.rds")
  
  # se não temos o arquivo, avisa
  try(if(!is.data.frame(folha_siope_sumup)) stop("arquivo RDS com sumário Folha x Siope não encontrado!"))  
  
  folha_siope_sumup  

}


gera_folha_siope_municip <- function(){
  #######################################################################
  # FOLHA DE PAGAMENTO
  
  # pega a folha de Brejo do Cruz de 2015 a 2018
  folha_brejo <- carrega_folha_pagto("data/TCE-PB-Folha_2015-2018.csv", 
                                     esfera = "Municipal",
                                     munic = "",
                                     ano = "", 
                                     mes = "",
                                     cod_orgao = "201036")
  
  
  # carrega a folha de 2019
  folha_brejo_19 <- carrega_folha_pagto("data/TCE-PB-Folha_2019.csv", 
                                        esfera = "Municipal",
                                        munic = "",
                                        ano = "", 
                                        mes = "",
                                        cod_orgao = "201036")
  
  
  # filtra apenas 2017 e 2018, já que no SIOPE só temos esses dois anos
  folha_brejo <- filter(folha_brejo, A_ANO %in% c(2017, 2018))
  folha_brejo$A_ANO <- factor(folha_brejo$A_ANO)
  
  folha_brejo_19$A_ANO <- factor(folha_brejo_19$A_ANO)
  
  # Filtra apenas os 4 primeiros meses do ano para poder comparar com o siope
  folha_brejo_19 <- filter(folha_brejo_19, A_MES %in% c(1,2,3,4))
  
  folha_brejo <- rbind(folha_brejo, folha_brejo_19)
  
  # salva em RDS
  saveRDS(folha_brejo, file = "folha_brejo.rds")  
  
  
  # valor pago a cada ano
  folha_brejo_ano <-  folha_brejo %>% 
    group_by(A_ANO) %>% 
    summarise(total = sum(VL_REMUNERACAO_TOTAL))
  
  # renomeia as colunas
  names(folha_brejo_ano) <- c("ano", "total")
  folha_brejo_ano$origem <- "folha de pagamento"
  
  
  
  ########################################################################
  # SIOPE
  
  # pega os dados de Brejo do Cruz
  siope_brejo <- carrega_siope(file.path(getwd(), "data", "siope", "brejo do cruz"))
  
  
  # removendo os vazios (NA) do dataset
  siope_brejo <- na.omit(siope_brejo)
  
  # agrupa o SIOPE por ano para compararmos com a folha do brejo
  siope_brejo_ano <- siope_brejo %>% 
    group_by(ano) %>% 
    summarise(total = sum(total, na.rm = TRUE))
  
  
  # acrescenta uma coluna para identificar que é do SIOPE
  siope_brejo_ano$origem <- "siope"
  
  # junta os datasets para podermos vizualizar no gráfico juntos
  siope_brejo_join <- rbind(siope_brejo_ano, folha_brejo_ano)
  
  siope_brejo_join
  
  
}



# Início ------------------------------------------------------------------








# gráfico dos valores da folha do brejo
# p <- ggplot(siope_brejo_join, mapping = aes(ano, total))
# g <-  p + 
#   geom_bar(stat = "identity", aes(fill=origem), position = "dodge") + 
#   scale_y_continuous(labels = formata_moeda) +
#   theme_light() +
#   labs(title = "Folha de pagamento x SIOPE (Brejo do Cruz)",
#        subtitle = "2017 a 2019",
#        caption = "Fonte: SAGRES-PB",
#        x = "Ano",
#        y = "Total pago no ano")
# g
# 







































