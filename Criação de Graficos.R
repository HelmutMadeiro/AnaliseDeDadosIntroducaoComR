

#ajustando e renomento os nomes das colunas 

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
