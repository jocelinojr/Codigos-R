#--------------------------------
# This file is for practice the exercises of the Exploratory Data Analysis
# from the book R for Data Science

library(tidyverse)
library(ggthemes)
library(scales)
library(nycflights13)
install.packages("blogdown")


# Begining ----------------------------------------------------------------

summary(diamonds)

px <- ggplot(data = diamonds, mapping = aes(x=x))
px + geom_histogram(binwidth = 0.5)
diamonds %>%
  count(cut_width(x, 0.5))

py <- ggplot(data = diamonds, mapping = aes(x=y))
py + geom_histogram(binwidth = 0.5) + theme_light() + labs(title = "Y dimension") + 
  coord_cartesian(xlim = c(0, 15))


diamonds %>%
  count(cut_width(y, 0.5))

theme_set(theme_light())
pz <- ggplot(data = diamonds, mapping = aes(x=z))
pz + geom_histogram(binwidth = 0.5) + labs(title = "Z dimension")


diamonds %>%
  count(cut_width(carat, 0.10))

nrow(filter(diamonds, carat==1))


#################################################
# resposta do execício dada no git hib
# https://jrnold.github.io/r4ds-exercise-solutions/



diamonds %>%
  # cria uma nova coluna com o número da linha
  mutate(id=row_number()) %>%
  select(x, y, z, id) %>% 
  gather(variable, value, -id) %>%
  ggplot(aes(x=value)) + 
  geom_density() +
  geom_rug() + 
  facet_grid(variable~.)


# Exercise number 2 - exploring the distribution of price

p <- ggplot(data = diamonds, mapping = aes(x=price, fill=cut))
p + geom_histogram(binwidth = 100) + 
   scale_x_continuous(labels=scales::dollar)

diamonds %>%
  count(cut_width(price, 1000))

# seeing better the correlation between cut and price
p <- ggplot(diamonds, mapping = aes(x=cut, y=price))
p + geom_boxplot() + theme_light() + coord_flip()

diamonds %>%
  group_by(cut) %>%
  summarize(min_preco=min(price),
            quant_25=quantile(price, 0.25),
            mediana_preco=median(price),
            quant_75=quantile(price,0.75),
            max_preco=max(price),
            preco_medio=mean(price)) %>%
  arrange(mediana_preco)


glimpse(flights)
head(flights)  
minutos <- head(flights$sched_dep_time, 20) %% 100
horas <- head(flights$sched_dep_time, 20) %/% 100

horas
minutos

2 + 4/3

horas + minutos / 60

# Visualizando os vôos cancelados

nyc_alterado <- flights %>% 
  mutate(cancelado = ifelse(is.na(dep_time), "SIM", "NÃO"),
         # pega a hora usando o operador de divisão inteira na divisão por 100
         sched_hour = sched_dep_time %/% 100,
         sched_min = sched_dep_time %% 100,
         sched_dep_time_2 = sched_hour + sched_min/60) %>%
  select(sched_dep_time_2, sched_dep_time, cancelado)  

ggplot(data = nyc_alterado, mapping = aes(sched_dep_time_2)) + 
  geom_freqpoly(mapping = aes(color=cancelado), binwidth=1/4)


# Usando Box Plots
ggplot(data = nyc_alterado, mapping = aes(x = cancelado, y=sched_dep_time_2)) +
  geom_boxplot() + labs(x = "Status: Cancelado", y = "Hora da Partida")  + 
  coord_flip()






# for (i in 1: nrow(matriz)) {
# 
#   for (j in 1: ncol(matriz)){
#     print(matriz[i, j])
#     
#   }
# 
# } # for 2

# matriz["andre", matriz["andre", 1]] <- matriz["andre", 1]





write.csv(qde_lugares, "lugares.csv")

p <- ggplot(qde_lugares, mapping = aes(x=lugar, y=n) )
p + geom_bar(stat = "identity")



# # função que realiza o sorteio
# sorteia_mesas <- function(df_sorteio, mesas) {
#   sorteados <- data.frame()
#   
#   # loop nos lugares para ir sorteando 
#   for (mesa in mesas$lugar) {
#     # pega os candidatos da mesa da vez excluindo quem já foi sorteado anteriormente
#     m <- filter(df_sorteio, lugar == mesa, sorteado == "N")  
#     # sorteia 1
#     s <- sample_n(m, 1)
#     # se teve gente sorteada...guarda na lista de sorteados
#     if (nrow(s) != 0) {
#       sorteados <- rbind(sorteados, s)
#     }
#     
#     # marca o sorteado para não entrar no próximo sorteio
#     df_sorteio[df_sorteio$pessoa == s$pessoa, "sorteado"] <- "S"
#     
#   } # for
#   
#   sorteados$sorteado <- NULL
#   # retorna o dataframe dos sorteados juntamente com o das opções para ver se não ficou ninguem sem sorteio 
#   retorno <- list("a"=sorteados, "b"=df_sorteio)
#   return(retorno)
#   
# 
# }











  
  

















  

















