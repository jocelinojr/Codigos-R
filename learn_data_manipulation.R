###############################################################################
library(tidyverse)
library(nycflights13)
library(dplyr)
library(magrittr)



##############################################################################
# Code begins


# Open the dataset in the RStudio Viewer
View(flights)


# Taking a look in the dataset
flights

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

# com o pipe
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














