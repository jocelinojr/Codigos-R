################################################################################
install.packages("nycflights13")


###############################################################################
library(tidyverse)
library(nycflights13)
library(dplyr)
library(magrittr)


###############################################################################
# dados do ambiente

# pega qual é a pasta atual de traballho
getwd()
# lista os arquivos da pasta atual
list.files()



# Open the dataset in the RStudio Viewer
View(flights)




# The six verbs of the data manipulation grammar (language) of dplyr
# filter, select,  arrange, mutate, summarise, group_by

# filter, assigns the result dataset to a variable and print it out by the using of parenthesis
(jan <-  filter(flights, month==1, day==1))


##############################################
# HOT TIP ON LEADING WITH REAL NUMBERS

# the square root of two raised to 2 should be equal to two., however...
sqrt(2) ^ 2 == 2
# instead, we should use, near
near(sqrt(2) ^ 2, 2)

# using boolean operators
# finding all 2013 january flights
jan_dez_2013 <- filter(flights, month == 1 | month == 12)


###########################################################
# AWESOME OPERATOR!
nov_dez <- filter(flights, month %in% c(11, 12))

(jfk_lga_ewr <- filter(flights, origin %in% c("JFK", "LGA", "EWR")))

###################################################################
# The NA problem. NA is contagious!
NA > 5

df <- tibble(x = c(1, NA, 3))


df_1 = filter(df, x>1)
df_2 <- filter(df, is.na(x)|x>1)
# criando  tabelas 
df2 <- tibble(x = c(1, 4, 10), y = c(9, 10, 12.5))


?flights

#######################################################################
# Exercises from R for Data Science


# Find all flights that had an arrival delay of two or more hours
arr_dly_2_more <- filter(flights, arr_delay >= 120)
# Flew to houston
flew_houston <- filter(flights, dest %in% c("IAH", "HOU"))

# Departed in summer (July, August or Spetember)
summer_months <- c(7, 8, 9)
summer_departs <- filter(flights, month %in% summer_months)

m# com o pipe
summer_departs1 <-  flights %>% filter(month %in% summer_months) 

# Arrived more than two hours late, but didn't leave late
arr_2late_dept_on <-  filter(flights, arr_delay > 120 & dep_delay <=0)

# Departed between midnight and 6 am (inclusive)
departed_mid_six <- filter(flights, between(dep_time, 1, 600) & !is.na(dep_time))

# arrange (changing the order of the rows)
# the most delayed flights
(arrange(flights, desc(dep_delay)))

# the flights that left earliest
(arrange(flights, dep_delay))


########################################
# Select

(select(flights, year, dep_time, day))

# com o pipe
flights %>% select(., year, dep_time, day)



# we can select a range of columns names!
(select(flights, day:arr_time))

# we can even use regular expressions to select columns names!

?flights
# cria uma nova variável com a diferença 
(flights %>% 
    select(arr_time, dep_time, air_time, arr_delay, dep_delay) %>%
    mutate(air_calc = arr_time - dep_time) %>%
    mutate(air_dif =  air_calc - air_time)
  )


# investigando a relação entre distância percorrida e atraso médio
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest, 
                   qtde = n(), 
                   distancia_media = mean(distance, na.rm = TRUE),
                   atraso_medio = mean(arr_delay, na.rm = TRUE))
# pega os maiores atrasos
delay <- filter(delay, qtde > 20, dest !="HNL")

ggplot(data = delay, mapping = aes(x = distancia_media, y =atraso_medio)) +
  geom_point(aes(size=qtde), alpha = 1/2) +
  geom_smooth(se= FALSE)
  

#################################################################################
# trabalhando no arquivo de convênios do portal da transparência


##################################################################################
# Para ler o arquivo corretamente, é preciso:
# 1 - Usar csv2, para csv separados por ";"
# 2 - muda o locale para modificar a codificação do arquivo para ISO-8859-1

convenios <- read_csv2("20180907_Convenios.csv", locale = locale(encoding = 'ISO-8859-1'))

# filtra apenas os da PB
convenios_PB <- convenios %>%
  filter(UF == "PB")

# conhecendo nosso data.frame
str(convenios_PB)

# converte nosso dataframe para uma tibble
conv_pb_ti <- as_tibble(convenios_PB)
conv_pb_ti

# column names that doesn't fit in R paterns can be refered to using backticks ``
str(conv_pb_ti)

conv_pb_ti$`DATA FINAL VIGÊNCIA` <- as.date(conv_pb_ti$`DATA FINAL VIGÊNCIA`)
View(conv_pb_ti)


por_muni <- conv_pb_ti %>%
  group_by(`NOME MUNICÍPIO`) %>%
  filter(`SITUAÇÃO CONVÊNIO` == "EM EXECUÇÃO")  %>%
  #filter(`VALOR LIBERADO` > 1000000) %>%
  summarise(qt_conv = n(), total_libe = sum(`VALOR LIBERADO`)) %>%
  arrange(desc(total_libe))




ggplot(data=por_muni, mapping = aes(x = qt_conv, y = total_libe )) +
  geom_point(alpha = 1/2) +
  geom_smooth(se= FALSE)










