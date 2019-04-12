# atualiza nosso R
install.packages("installr")


library(installr)

updateR()



devtools::install_github("georgevbsantiago/qsacnpj")
library(qsacnpj)
entes <- qsacnpj::tab_cnpj_entes_publicos_br

# pesquisando textos com grep

grep("tribunal de CONTAS da uniao", entes$nome_empresarial, ignore.case = TRUE)
grepl("tribunal de contas da", entes$nome_empresarial, ignore.case = TRUE)

entes[grep("tribunal de CONTAS da uniao", entes$nome_empresarial, ignore.case = TRUE), ]
entes %>% subset(grepl("tribunal de contas da", nome_empresarial, ignore.case = TRUE))
  

