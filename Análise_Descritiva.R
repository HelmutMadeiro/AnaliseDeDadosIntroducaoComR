Geral


formatos <-          theme(
  plot.title = element_text(size = 14, hjust = 0.5),
  axis.title.y = element_text(size = 12, vjust = +0.2),
  axis.title.x = element_text(size = 12, vjust = -0.2),
  axis.text.y = element_text(size = 10),
  axis.text.x = element_text(size = 10)
)


install.packages('dplyr')
library(dplyr)


library(ggplot2)

dados



classes <- c(
  min(dados$Renda),2*788,5*788,15*788,25*788, max(dados$Renda)
)
classes

labels <- c('E','D','C','B','A')
labels

frequencia <- table(
  cut(
    x=dados$Renda,
    breaks = classes,
    labels = labels,
    include.lowest = TRUE
  )
)
frequencia

percentual <- prop.table(frequencia)*100
percentual


dist_freq_renda <- cbind('Frequencia' = frequencia, 'Porcentagem' = percentual)
dist_freq_renda

dist_freq_renda[order(row.names(dist_freq_renda)),]

bar_chart <- data.frame(dist_freq_renda)
bar_chart

ggplot(bar_chart,aes(x=row.names(bar_chart),y=bar_chart$Frequencia))+
  geom_bar(stat ="identity" )+
  ylab("Frequencia")+
  xlab("Classes de Renda")+
  ggtitle('Grafico Classes de Renda') +
  formatos 

ggplot(dados,aes(x = Idade))+
  geom_histogram(bins = 50)+
  ylab('Frequencia')+
  xlab('Idade')+
  ggtitle('Histograma de Idades')+
  formatos

ggplot(dados,aes(x = Altura))+
  geom_histogram(bins = 50)+
  ylab('Frequencia')+
  xlab('Idade')+
  ggtitle('Histograma de Idades')+
  formatos

ggplot(dados,aes(x = Renda))+
  geom_histogram(bins = 50)+
  ylab('Frequencia')+
  xlab('Idade')+
  ggtitle('Histograma de Idades')+
  formatos

ggplot(dados[dados$Renda<20000,],aes(x = Renda))+
  geom_histogram(bins = 50)+
  ylab('Frequencia')+
  xlab('Idade')+
  ggtitle('Histograma de Idades')+
  formatos

sexo = c( 'Masculino', 'Feminino')
cor  = c('Indígena','Branca','Preta','Amarela','Parda')

anos_de_estudo = c(
  'Sem instrução e menos de 1 ano', 
  '1 ano', 
  '2 anos', 
  '3 anos', 
  '4 anos', 
  '5 anos', 
  '6 anos', 
  '7 anos', 
  '8 anos', 
  '9 anos', 
  '10 anos', 
  '11 anos', 
  '12 anos', 
  '13 anos', 
  '14 anos', 
  '15 anos ou mais', 
  'Não determinados'
)

dados$Cat.Sexo <- factor(dados$Sexo)
levels(dados$Cat.Sexo) <- sexo

dados$Cat.AnosdeEstudo <- factor(dados$AnosdeEstudo)
levels(dados$Cat.AnosdeEstudo) <- anos_de_estudo

dados$Cat.Cor <- factor(dados$Cor)
levels(dados$Cat.Cor) <- cor

dados


frequencia <- table(dados$Cat.Sexo,dados$Cat.Cor)
frequencia <- cbind(frequencia)
frequencia

percentual <- cbind(prop.table(frequencia)*100)
percentual

mean(dados$Renda)
median(dados$Renda)

Moda <- function(x){
  frequencias <- table(x)
  return(names(frequencias)[frequencias==max(frequencias)])
}
as.numeric(Moda(dados$Renda))


var(dados$Renda)
sd(dados$Renda)

medias <- tapply(dados$Renda,list(dados$Cat.Sexo,dados$Cat.Cor),mean)
medias

medianas <- tapply(dados$Renda,list(dados$Cat.Sexo,dados$Cat.Cor),median)
medianas

medianas <- tapply(dados$Renda,list(dados$Cat.Sexo,dados$Cat.Cor),max)
medianas

varianca <- tapply(dados$Renda,list(dados$Cat.Sexo,dados$Cat.Cor),var)
varianca

Desvio <- tapply(dados$Renda,list(dados$Cat.Sexo,dados$Cat.Cor),sd)
Desvio


ggplot(data = dados[dados$Renda < 10000,], aes(x=Cat.Cor,y = Renda, fill = Cat.Sexo))+
  geom_boxplot(size = 0.2)+
  coord_flip()+
  ylab("R$")+
  xlab("Cor")+
  guides(fill = guide_legend(title = 'Sexo'))+
  ggtitle('Box-plot de Renda por Sexo e Cor') +
  formatos


length(dados$Renda[dados$Renda <= 788])/ length(dados$Renda)*100

quantile(dados$Renda, .99)



ggplot(data = dados[dados$Renda<10000,] , aes(x=Cat.AnosdeEstudo,y = Renda, fill =Cat.Sexo ))+
  geom_boxplot(size = 0.2)+
  coord_flip()+
  ylab("R$")+
  xlab("Cor")+
  guides(fill = guide_legend(title = 'Sexo'))+
  ggtitle('Box-plot de Renda por Sexo e Cor') +
  formatos
