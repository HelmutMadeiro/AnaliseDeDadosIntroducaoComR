dados

install.packages("dplyr")
library('dplyr')

unique(select(dados,AnosdeEstudo))

arrange(unique(select(dados,AnosdeEstudo)),AnosdeEstudo)

#criando um vetor
c(1,2,3,4)

#variaveis qualitativas ordinais
#variaveis que podem ser ordenadas ou hierarquizadas

c(arrange(unique(select(dados,AnosdeEstudo)),AnosdeEstudo))

#variaveis qualitativas nominais 
#variaveis que nao podem ser ordenas ou hieraquizadas

c(arrange(unique(select(dados,UF)),UF))

c(arrange(unique(select(dados,Sexo)),Sexo))

c(arrange(unique(select(dados,Cor)),Cor))

#Variáveis quantitativas discretas
#??? Variáveis que representam uma contagem onde os valores possíveis formam um conjunto finito ou enumerável.

sprintf('De %s ate %s Anos', min(dados$Idade),max(dados$Idade))

library(glue)

glue('De {min(dados$Idade)} ate {max(dados$Idade)} Anos')

#Variáveis quantitativas contínuas
#$??? Variáveis que representam uma contagem ou mensuração que assumem valores em uma escala contínua (números reais).


sprintf('De %s ate %s metros', min(dados$Altura),max(dados$Altura))

glue('De {min(dados$Altura)} ate {max(dados$Altura)} Metros')

#                      Ordinais
#          Qualitativas 
#                      nominais
#                      
#Variaveis             
#                      Discretas 
#          Quantitativas          
#                      Continuas



#cria uma tabela
table(dados$Sexo)

#Você já deve estar lembrando o "c" que é aquele que a cada parâmetro c(,1,2,3,4) Ele junta e faz um vetor. 
#Esse aqui cbind() não deixa de fazer a mesma coisa, só que cada parâmetro que eu passar para ele, ele vai transformar em uma coluna.
# prop.table () pega a porcentagem 

dist_freq_qualitativas <- cbind(freq = table(dados$Sexo),percent = prop.table(table(dados$Sexo))*100)

#criamos um vetor com os nomes colunas e atribuimos com o colnames() na nossa variavel
colnames(dist_freq_qualitativas)<-c('Frequencia','Porcentagem %')
dist_freq_qualitativas

#criamos um vetor com os nomes das linhas e atribuimos com o linhas 0 e 1 na nossa variavel
rownames(dist_freq_qualitativas)<-c('Masculino','Feminino')
dist_freq_qualitativas





















