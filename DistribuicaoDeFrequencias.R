#DistribuicaoDeFrequencias

#sexo 0 = Homem 1 = Mulher 
#pega o dataframe e transfforma em uma table 
table(dados$Sexo)


dist_freq_qualitativas 
prop.table(table(dados$Sexo))*100

#cbind pega a table e tomba
dist_freq_qualitativas <- cbind( freq = table(dados$Sexo),porcent = prop.table(table(dados$Sexo))*100)
dist_freq_qualitativas

#com uma variavel
# hora de colocar nomes nas colunas, colnames passa a variavel e depois o vetor com os nomes
# hora de colocar nomes nas linas, rownames passa a variavel e depois o vetor com os nomes
colnames(dist_freq_qualitativas )<- c('Frequencia', 'Porcentagem(%)')
rownames(dist_freq_qualitativas )<- c('Masculino', 'Feminino')
dist_freq_qualitativas

frequencia <-  table(dados$Sexo,dados$Cor)
frequencia


colnames (frequencia) <- c('Indigina','Branco','Preta','Amarela','Parda')
rownames(frequencia) <-c('Masculino', 'Feminino')

frequencia <- cbind(frequencia)
percentual <- prop.table(frequencia)*100


#paramentros para TApply : o que vc quer medir, a lista , a medicao 
medias <- tapply(dados$Renda,list(dados$Sexo,dados$Cor),mean)
colnames (medias) <- c('Indigina','Branco','Preta','Amarela','Parda')
rownames(medias) <-c('Masculino', 'Feminino')
medias


min(dados$Renda)
max(dados$Renda)

#Criando uma variavel classe aonde recebe um vetor que indica o salario
classes <- c('0','1576','3152','7880','15760','200000')

#Criando uma variavel labels aonde recebe um vetor que indica o Classe 

labels <- c('E','D','C','B','A')

#criar uma tabela de frequecia

#"cut". Essa fun��o vai fazer o qu�? 
#N�s passamos as classes, ele vai dentro do nosso dado e vai verificar. 
#Aquela vari�vel renda pertence a que classe? 
#Pertence a essa, ent�o vou pegar esse label e vou jogar para esse.



#classes <- c(min(dados$Altura), 1.65, 1.75, max(dados$Altura))

#labels <- c('1 - Baixa', '2 - M�dia', '3 - Alta')

#frequencia <- table(
  #cut(
    #x = dados$Altura,
    #breaks = classes,
    #labels = labels,
    #include.lowest = TRUE
    #)
  #)

#percentual <- prop.table(frequencia) * 100

#dist_freq_altura <- cbind('Frequ�ncia' = frequencia, 'Porcentagem (%)' = percentual)

#dist_freq_altura[
  #  order(row.names(dist_freq_altura)),
#]


frequencia <- table(
                      cut(
                      x=dados$Renda,
                      breaks = classes,
                      labels = labels,
                      include.lowest = TRUE
                      )
  )

#Pegando a porcentagem da frequencia
percentual <- prop.table(frequencia)*100

#tombando a informa��o
dist_freq_quantitativas_personalizadas <- cbind('Frequencia' = frequencia,'Percentual(%)' = percentual)

#ordenando por ordem alfabetica 
dist_freq_quantitativas_personalizadas[
  order (row.names(dist_freq_quantitativas_personalizadas)),
]




#regra Sturges
#A f�rmula da regra de Sturges n�o est� correta. 
 #  Com essa f�rmula, podemos definir um n�mero �timo de classes, baseado no total de observa��es da nossa vari�vel.

#classes com Amplitude fixas


n <- nrow(dados)
n #numero de dataset (observa��es)

k <- 1+(10/3)* log10(n)

k <- round(k)

k # numero de classes 

#contruindo a label 
labels <- c(
  '      0.00 |-|  11,764.70', 
  ' 11,764.70  -|  23,529.40', 
  ' 23,529.40  -|  35,294.10', 
  ' 35,294.10  -|  47,058.80', 
  ' 47,058.80  -|  58,823.50', 
  ' 58,823.50  -|  70,588.20', 
  ' 70,588.20  -|  82,352.90', 
  ' 82,352.90  -|  94,117.60', 
  ' 94,117.60  -| 105,882.00', 
  '105,882.00  -| 117,647.00', 
  '117,647.00  -| 129,412.00', 
  '129,412.00  -| 141,176.00', 
  '141,176.00  -| 152,941.00', 
  '152,941.00  -| 164,706.00', 
  '164,706.00  -| 176,471.00', 
  '176,471.00  -| 188,235.00', 
  '188,235.00  -| 200,000.00'
)



#alocando cada k dentro de uma label
frequencia <- table(
  cut(
    x=dados$Renda,
    breaks = k,
    labels = labels,
    include.lowest = TRUE
  )
)

frequencia


#pegando o percentual 
percentual <- prop.table(frequencia)*100

percentual

#tombando a iforma��o e juntando as variaveis 
dist_freq_quantitativas_amplitude_fixa <- cbind('Frequencia' = frequencia,'Percentual(%)' = percentual)
dist_freq_quantitativas_amplitude_fixa

#O HISTOGRAMA � a representa��o gr�fica de uma distribui��o de frequ�ncias.

hist(
  x = dados$Altura,
  breaks = 'Sturges',
  col = 'lightblue',
  main = 'Histograma das Alturas',
  xlab = 'Altura',
  ylab = 'Frequ�ncias',
  prob = TRUE,
  las = 1
)

library(ggplot2)

ggplot(dados, aes(x = Altura)) + 
         geom_histogram(binwidth = 0.02, color = "black", alpha = 0.9) + 
         ylab("Frequ�ncia") + 
         xlab("Alturas") + 
         ggtitle('Histograma das Alturas') +
         theme(
           plot.title = element_text(size = 14, hjust = 0.5),
           axis.title.y = element_text(size = 12, vjust = +0.2),
           axis.title.x = element_text(size = 12, vjust = -0.2),
           axis.text.y = element_text(size = 10),
           axis.text.x = element_text(size = 10)
         )

formatos <-          theme(
  plot.title = element_text(size = 14, hjust = 0.5),
  axis.title.y = element_text(size = 12, vjust = +0.2),
  axis.title.x = element_text(size = 12, vjust = -0.2),
  axis.text.y = element_text(size = 10),
  axis.text.x = element_text(size = 10)
)

ggplot(dados, aes(x = Altura, y =..density..)) + 
  geom_histogram(binwidth = 0.02, color = "black", alpha = 0.8) + 
  geom_density(color = 'green') +
  ylab("Frequ�ncia") + 
  xlab("Alturas") + 
  ggtitle('Histograma das Alturas') +
  formatos


bar_chart <- data.frame(dist_freq_quantitativas_personalizadas)
bar_chart



ggplot(bar_chart, aes(x = rownames(bar_chart), y = bar_chart$Frequencia)) + 
  geom_bar(stat = "identity") + 
  ylab("Frequ�ncia") + 
  xlab("Classes de Renda") + 
  ggtitle('Gr�fico Classes de Renda') +
  formatos






#O Histograma � a representa��o gr�fica de uma distribui��o de frequ�ncias. 
#� um gr�fico formado por um conjunto de ret�ngulos, colocados lado a lado, 
#onde a �rea de cada ret�ngulo � proporcional � frequ�ncia da classe que ele representa


#Com o Histograma, podemos ter uma melhor ideia de como os dados se distribuem.









