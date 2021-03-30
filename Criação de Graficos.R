

#ajustando e renomento os nomes das colunas 

install.packages("plyr")
library(plyr)

duracao <- rename(duracao,replace = c("user_id" = "aluno", "course_id" = "curso", "timeToFinish" = "dias")) 
duracao

#deixando o R decidir qual seria o grafico para essa analise
plot(duracao$dias)



#iniciando a criação de uma exportação do grafico 
jpeg("histograma.jpg")

hist(duracao$dias # buscando o banco e a variavel
     , breaks = 20 #Buscando o total de colunas para exibir
     , main = "Histograma do Tempo" # colocando nome do grafico
     ,ylab = "Quantidade" # colocando o nome do eixo Y
     , xlab = "Tempo"  # colocando o nome do eixo Y
     ,ylim = c(0,2500)  # colocando um limite no eixo Y
     ,xlim = c(0,600)  # colocando um limite no eixo Y
     ,col = "blue") # colocando cor no grafico 

#fechando e salvando a exportação da imagem
dev.off()

#mostra a media em dias, na.rm = t remove do calculo os registro N/A
mean(duracao$dias,na.rm = T)
 
#mostra a mediana 
median(duracao$dias,na.rm = T)

summary(duracao$dias)


dim(duracao)[1]

length(unique(duracao$curso))
length(unique(duracao$aluno))
summario_estatistico <- aggregate(duracao$dias,list(duracao$curso), mean, na.rm = T)

popularidade = count(aulas, vars = 'course_id')
head(popularidade)

popularidade=rename(popularidade, c("course_id" = "curso", "freq"="frequencia"))
popularidade <- rename(popularidade, c('course_id'= 'curso','freq'='popularidade'))

summario_estatistico <- rename (summario_estatistico, replace = c('Group.1'='curso','x'= 'dias'))
summario_estatistico <- rename (summario_estatistico, c( 'Curso'='curso'))

popularidade <-rename (popularidade,c('frequencia'='popularidade'))
popularidade_e_duracao <- merge(summario_estatistico ,popularidade ,by = 'curso')


plot(popularidade_e_duracao$dias,popularidade_e_duracao$popularidade)

modelo.linear <- lm(popularidade_e_duracao$popularidade~popularidade_e_duracao$dias)
abline(lm(popularidade_e_duracao$popularidade~popularidade_e_duracao$dias))


#dispersão ponto suavização, para modelos nao linear

scatter.smooth(popularidade_e_duracao$dias,popularidade_e_duracao$popularidade
               , col = 'blue',ylab = "Popularidade",xlab ="Dias",main ="Popularidade dos cursos")


#grammar of graphics
install.packages('ggplot2')
library(ggplot2)

#começaremos a criar o gráfico, ao qual daremos o nome de grafico e atribuiremos (<-) a ele ggplot, que é o comando básico da biblioteca. 
#jogamos para uma variavel o resultado do ggplot aonde colocamos o bd as variaveis resposta e explicativas 
grafico <- ggplot(popularidade_e_duracao,aes(dias,popularidade))
grafico

#Notem que adicionamos (+) elementos ao gráfico, transformando o objeto (grafico) inicial, 
#somando a ele grafico e geom_point. Esse último comando é utilizado para informar 
#ao programa que estamos trabalhando com um modelo suave e não linear. Em seguida, abrimos e fechamos parênteses. 
#Há uma etimologia do termo, que expressa "geometria de ponto", mas não nos aprofundaremos nisso.

grafico <-grafico+geom_point()
grafico


#Falta a curva, que não está no gráfico. Para inseri-la, repetiremos o procedimento anterior, 
#adicionando (+) a curva ao gráfico em uma nova linha do R Script:
grafico <- grafico + geom_smooth()
grafico



