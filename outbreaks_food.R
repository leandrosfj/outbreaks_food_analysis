# Exercício em R: Outbreaks - TI e Saúde
# Análise Exploratória de Dados (EDA) e Teste de Hipóteses
# 05/09/2019
# Leandro Silva Ferreira Jr.

# Carregando pacotes
  library(dplyr)
  library(data.table)
  library(ggplot2)
  library(RColorBrewer)
  library(rworldmap)
  library(tidyr)
  library(scales)

# Setando diretório
  getwd()
  setwd("~/Documentos/")
  getwd()
  dir()

# Salvando dados no objeto no tipo dataframe
  outbreaks <- read.csv('outbreaks.csv',stringsAsFactors = F, header = T)
  
#(1) - Inspeção do banco de dados

# Verficando classe
  class(outbreaks)

# Dimensão
  dim(outbreaks)

# Nome das colunas
  colnames(outbreaks)

# Tipo de dados em cada coluna
  str(outbreaks)
  
# Quantidade de dados não disponíveis
  na1 <- sum(ifelse(grepl("*\\d", outbreaks$Year), FALSE, TRUE))
  na2 <- sum(ifelse(grepl("*\\W", outbreaks$Month), TRUE, FALSE))
  na3 <- sum(ifelse(grepl("*\\W", outbreaks$State), TRUE, FALSE))
  na4 <- sum(ifelse(grepl("*\\w", outbreaks$Location), FALSE, TRUE))
  na5 <- sum(ifelse(grepl("*\\w", outbreaks$Food), FALSE, TRUE))
  na6 <- sum(ifelse(grepl("*\\W", outbreaks$Ingredient), FALSE, TRUE))
  na7 <- sum(ifelse(grepl("*\\W", outbreaks$Species), FALSE, TRUE))
  na8 <- sum(ifelse(grepl("*\\w", outbreaks$Serotype.Genotype), FALSE, TRUE))
  na9 <- sum(ifelse(grepl("*\\w", outbreaks$Status), FALSE, TRUE))
  na10 <- sum(ifelse(grepl("*\\d", outbreaks$Illnesses), FALSE, TRUE))
  na11 <- sum(ifelse(grepl("*\\d", outbreaks$Hospitalizations), FALSE, TRUE))
  na12 <- sum(ifelse(grepl("*\\d", outbreaks$Fatalities), FALSE, TRUE))
  
#(NA) Total
  na_total = sum(na1,na2,na3,na4,na5,na6,na7,na8,na9,na10,na11,na12)
  sprintf('Total de dados indisponíveis: %d', na_total)

sprintf('Não foi encontrado nenhum outro registro indicador de dados não disponíveis')

sprintf('Porcentagem de dados não disponíveis em cada variável:')

  #Year
    sprintf("%.2f%%", na1/dim(outbreaks)[1]*100)
  #Month
    sprintf("%.2f%%", na2/dim(outbreaks)[1]*100)
  #State
    sprintf("%.2f%%", na3/dim(outbreaks)[1]*100)
  #Location
    sprintf("%.2f%%", na4/dim(outbreaks)[1]*100)
  #Food
    sprintf("%.2f%%", na5/dim(outbreaks)[1]*100)
  #Ingredient
    sprintf("%.2f%%", na6/dim(outbreaks)[1]*100)
  #Species
    sprintf("%.2f%%", na7/dim(outbreaks)[1]*100)
  #Serotype/Genotype
    sprintf("%.2f%%", na8/dim(outbreaks)[1]*100)
  #Status
    sprintf("%.2f%%", na9/dim(outbreaks)[1]*100)
  #Illnesses
    sprintf("%.2f%%", na10/dim(outbreaks)[1]*100)
  #Hospitalizations
    sprintf("%.2f%%", na11/dim(outbreaks)[1]*100)
  #Fatalities
    sprintf("%.2f%%", na12/dim(outbreaks)[1]*100)

#Número de valores distintos em cada variável
nlevels(factor(outbreaks$Year))    
nlevels(factor(outbreaks$Month))
nlevels(factor(outbreaks$State))
nlevels(factor(outbreaks$Location))
nlevels(factor(outbreaks$Food))
nlevels(factor(outbreaks$Ingredient))
nlevels(factor(outbreaks$Species))
nlevels(factor(outbreaks$Serotype.Genotype))
nlevels(factor(outbreaks$Status))
nlevels(factor(outbreaks$Illnesses))
nlevels(factor(outbreaks$Hospitalizations))
nlevels(factor(outbreaks$Fatalities))

sprintf("As variáveis com mais valores distintos são Food, Ingredient e Illnesses")

outbreaks <- outbreaks %>%
  separate(col = "Location", into = c("Location", "Location2"), sep = "([\\;\\/\\,\\;])") %>%
  separate(col = "Food", into = c("Food", "Food2"), sep = "([\\;\\/\\,\\;])") %>%
  separate(col = "Species", into = c("Species", "Species2"), sep = "([\\;\\/])")

#Organizando meses em ordem numérica
outbreaks$month_n <- ""
outbreaks$month_n <- as.integer(outbreaks$month_n,na.rm=TRUE)
outbreaks$month_n[which(outbreaks$Month == "January")] <- 1
outbreaks$month_n[which(outbreaks$Month == "February")] <- 2
outbreaks$month_n[which(outbreaks$Month == "March")] <- 3
outbreaks$month_n[which(outbreaks$Month == "April")] <- 4
outbreaks$month_n[which(outbreaks$Month == "May")] <- 5
outbreaks$month_n[which(outbreaks$Month == "June")] <- 6
outbreaks$month_n[which(outbreaks$Month == "July")] <- 7
outbreaks$month_n[which(outbreaks$Month == "August")] <- 8
outbreaks$month_n[which(outbreaks$Month == "September")] <- 9
outbreaks$month_n[which(outbreaks$Month == "October")] <- 10
outbreaks$month_n[which(outbreaks$Month == "November")] <- 11
outbreaks$month_n[which(outbreaks$Month == "December")] <- 12

########################################################################

sprintf("A variável escolhida para análise de dados e plotagem dos gráficos foi Fatalities")

#(2) - Gráficos

sprintf("Número de mortes por ano devido à alimentos contaminados")
year_data <- outbreaks %>%
  group_by(Year) %>%
  summarise(num_cases = sum(Fatalities,na.rm = T)) %>%
  arrange(desc(num_cases))

ggplot(data=year_data,
       aes(x=reorder(Year, Year),
           y=num_cases,
           fill=num_cases))+
      geom_bar(stat='identity') +
      geom_text(aes(x = reorder(Year, Year), 
                y = num_cases, label = num_cases), nudge_y = 1) + 
      labs(x = "Ano", 
          y = "Mortes", 
          title = "Número de mortes por ano devido à alimentos contaminados") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_continuous(name= "Número de mortes:", labels=comma) +
      scale_y_continuous(labels =  comma) 


sprintf("Número de mortes por mês devido à alimentos contaminados")
month_data <- outbreaks %>%
  select(Month, Fatalities, month_n) %>%
  group_by(month_n, Month) %>%
  summarise(num_cases = sum(Fatalities,na.rm = T)) %>%
  arrange(order(month_n))

ggplot(data=month_data,
       aes(x=reorder(Month, order(month_n)),
           y=num_cases,
           fill=num_cases))+
  geom_bar(stat='identity') +
  geom_text(aes(x = reorder(Month, order(month_n)), 
                y = num_cases, label = num_cases), nudge_y = 1) + 
  labs(x = "Mês", 
       y = "Mortes", 
       title = "Número de mortes por mês devido à alimentos contaminados") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_continuous(name= "Número de mortes:", labels=comma) +
  scale_y_continuous(labels =  comma) 


sprintf("Número de mortes por estado devido à alimentos contaminados")
state_data <- outbreaks %>%
  select(State, Fatalities) %>%
  filter(!is.na(Fatalities) & Fatalities>0) %>%
  group_by(State) %>%
  summarise(num_cases = sum(Fatalities,na.rm = T)) %>%
  arrange(desc(num_cases))

ggplot(data=state_data,
       aes(x=reorder(State, desc(num_cases)),
           y=num_cases,
           fill=num_cases))+
  geom_bar(stat='identity') +
  geom_text(aes(x = reorder(State, desc(num_cases)), 
                y = num_cases, label = num_cases), nudge_y = 3) + 
  labs(x = "Estado", 
       y = "Mortes", 
       title = "Número de mortes por estado devido à alimentos contaminados") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_continuous(name= "Número de mortes:", labels=comma) +
  scale_y_continuous(labels =  comma) 

sprintf("Número de mortes por localidade devido à alimentos contaminados")
state_data <- outbreaks %>%
  filter(!is.na(Fatalities) & !is.na(Location) & Fatalities>0) %>%
  select(Location, Fatalities) %>%
  group_by(Location) %>%
  summarise(num_cases = sum(Fatalities,na.rm = T)) %>%
  arrange(desc(num_cases))

ggplot(data=state_data,
       aes(x=reorder(Location, desc(num_cases)),
           y=num_cases,
           fill=num_cases))+
  geom_bar(stat='identity') +
  geom_text(aes(x = reorder(Location, desc(num_cases)), 
                y = num_cases, label = num_cases), nudge_y = 3) + 
  labs(x = "Localidade", 
       y = "Mortes", 
       title = "Número de mortes por localidade devido à alimentos contaminados") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_continuous(name= "Número de mortes:", labels=comma) +
  scale_y_continuous(labels =  comma) 


sprintf("Número de mortes devido à alimentos contaminados")
state_data <- outbreaks %>%
  select(Food, Fatalities) %>%
  filter(!is.na(Fatalities) & !is.na(Food) & Fatalities>0) %>%
  group_by(Food) %>%
  summarise(num_cases = sum(Fatalities,na.rm = T)) %>%
  arrange(desc(num_cases))

ggplot(data=state_data,
       aes(x=reorder(Food, desc(num_cases)),
           y=num_cases,
           fill=num_cases))+
  geom_bar(stat='identity') +
  geom_text(aes(x = reorder(Food, desc(num_cases)), 
                y = num_cases, label = num_cases), nudge_y = 3) + 
  labs(x = "Alimento", 
       y = "Mortes", 
       title = "Número de mortes devido à alimentos contaminados") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_continuous(name= "Número de mortes:", labels=comma) +
  scale_y_continuous(labels =  comma) 

sprintf("Número de mortes por espécie devido à alimentos contaminados")
state_data <- outbreaks %>%
  select(Species, Fatalities) %>%
  filter(!is.na(Fatalities) & !is.na(Species) & Fatalities>0) %>%
  group_by(Species) %>%
  summarise(num_cases = sum(Fatalities,na.rm = T)) %>%
  arrange(desc(num_cases))

ggplot(data=state_data,
       aes(x=reorder(Species, desc(num_cases)),
           y=num_cases,
           fill=num_cases))+
  geom_bar(stat='identity') +
  geom_text(aes(x = reorder(Species, desc(num_cases)), 
                y = num_cases, label = num_cases), nudge_y = 3) + 
  labs(x = "Espécie", 
       y = "Mortes", 
       title = "Número de mortes por espécie devido à alimentos contaminados") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_continuous(name= "Número de mortes:", labels=comma) +
  scale_y_continuous(labels =  comma) 



########################################################################

#(3) - Teste de Hipóteses

#   Foi observado um número de mortes acima da média no ano de 2011, além disso também foi observado um número alto de mortes no mês de Julho. 
#   A Hipótese a ser testada é de que esses dados estejam relacionados a um mesmo evento de surto.
#   Para o teste, inicialmente foi plotado um gráfico dos casos de morte por mês no ano de 2011:

hp1 <- outbreaks %>%
  select(Month, Fatalities, month_n, Year) %>%
  filter(Year == (2011)) %>%
  group_by(month_n, Month) %>%
  summarise(num_cases = sum(Fatalities,na.rm = T)) %>%
  arrange(order(month_n))

ggplot(data=hp1,
       aes(x=reorder(Month, order(month_n)),
           y=num_cases,
           fill=num_cases))+
  geom_bar(stat='identity') +
  geom_text(aes(x = reorder(Month, order(month_n)), 
                y = num_cases, label = num_cases), nudge_y = 1) + 
  labs(x = "Mês", 
       y = "Mortes", 
       title = "Número de mortes devido à alimentos contaminados (2011)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_continuous(name= "Número de mortes:", labels=comma) +
  scale_y_continuous(labels =  comma) 

#   A partir do resultado é possível concluir a hipótese de que os dados têm relação à um mesmo surto, porém
#   foram feitas mais algumas análises para comprovar:

#   Plot do gráfico das mortes em Julho de 2011 mostrando os alimentos:

hp2 <- outbreaks %>%
  select(Food, Fatalities, Year, Month) %>%
  filter(Year == (2011) & Month == "July") %>%
  filter(!is.na(Fatalities) & !is.na(Food) & Fatalities>0) %>%
  group_by(Food) %>%
  summarise(num_cases = sum(Fatalities,na.rm = T)) %>%
  arrange(desc(num_cases))

ggplot(data=hp2,
       aes(x=reorder(Food, desc(num_cases)),
           y=num_cases,
           fill=num_cases))+
  geom_bar(stat='identity') +
  geom_text(aes(x = reorder(Food, desc(num_cases)), 
                y = num_cases, label = num_cases), nudge_y = 3) + 
  labs(x = "Alimento", 
       y = "Mortes", 
       title = "Número de mortes devido à alimentos contaminados (Julho de 2011)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_continuous(name= "Número de mortes:", labels=comma) +
  scale_y_continuous(labels =  comma) 

#   Plot do gráfico das mortes em Julho de 2011 mostrando as espécies:

hp3 <- outbreaks %>%
  filter(Year == (2011) & Month == "July") %>%
  filter(!is.na(Fatalities) & !is.na(Species) & Fatalities>0) %>%
  group_by(Species) %>%
  summarise(num_cases = sum(Fatalities,na.rm = T)) %>%
  arrange(desc(num_cases))

ggplot(data=hp3,
       aes(x=reorder(Species, desc(num_cases)),
           y=num_cases,
           fill=num_cases))+
  geom_bar(stat='identity') +
  geom_text(aes(x = reorder(Species, desc(num_cases)), 
                y = num_cases, label = num_cases), nudge_y = 3) + 
  labs(x = "Espécie", 
       y = "Mortes", 
       title = "Número de mortes por espécie devido à alimentos contaminados (Julho de 2011") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_continuous(name= "Número de mortes:", labels=comma) +
  scale_y_continuous(labels =  comma) 

#   A partir do plot dos dois últimos gráficos foi possível perceber a relação direta das variáveis:
#   33 Pessoas morreram em Julho de 2011, devido à mesma espécie, após comerem o mesmo tipo de alimento
#   Isso satisfaz a hipótese sugerida inicialmente

