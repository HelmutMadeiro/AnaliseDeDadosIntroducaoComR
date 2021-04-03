# qual o seu tipo de dado
dados

install.packages('dplyr')


library(dplyr)

head(dados)
#tratando dados qualitativos e ordinais

# criando um dataframe com apenas uma coluna
#select
select(dados,AnosdeEstudo)

#criando um dataframe com apenas uma coluna e trazendo uma coluna sem repeticão 
#unique
unique(select(dados,AnosdeEstudo))


#criando um dataframe com apenas uma coluna e trazendo uma coluna sem repeticão e ordenado 
#arrange
arrange(unique(select(dados,AnosdeEstudo)),AnosdeEstudo)

#criando um vetor de dados
c(arrange(unique(select(dados,AnosdeEstudo)),AnosdeEstudo))


#variaveis qualitativas nominais

c(arrange(unique(select(dados,UF)),UF))

c(arrange(unique(select(dados,Sexo)),Sexo))

c(arrange(unique(select(dados,Cor)),Cor))

  
#variaveis qualitativas discretas

#interpulação de string
#sprintf com a ajuda do %s podemos manipular aonde colocar nossa variavel
sprintf('De %s até %s anos',min(dados$Idade),max(dados$Idade))

library(glue)
glue('De {min(dados$Idade)} até {max(dados$Idade)} anos')




















































