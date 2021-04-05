#Medidas Separatrizes

dados


quantile(dados$Renda,c(0.25,0.5,0.75))

decis <- c()
for (i in 1:9){
  decis <- c(decis,i/10)
}
quantile(dados$Renda,decis)



centis <- c()
for (i in 1:99){
  centis <- c(centis,i/100)
}
quantile(dados$Renda,centis)

library(ggplot2)

ggplot(data = dados, aes(x = Idade)) + 
  geom_histogram(
    aes(y = cumsum(..count..)/sum(..count..)), 
    bins = 10
  ) + 
  geom_freqpoly(
    aes(y = cumsum(..count..)/sum(..count..)), 
    color = 'green'
  )

quantile(dados$Idade,decis)

length(dados$Idade[dados$Idade<=40])/ length(dados$Idade)*100 #pelo total de pessoas


788/2

length(dados$Renda[dados$Renda<=788/2])/length(dados$Renda)*100


#box plot

sexo = c(
  'Masculino', 
  'Feminino'
)
cor = c(
  'Indígena', 
  'Branca', 
  'Preta', 
  'Amarela', 
  'Parda'
)
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
levels(dados$Cat.Sexo ) <- sexo


formatos <-          theme(
  plot.title = element_text(size = 14, hjust = 0.5),
  axis.title.y = element_text(size = 12, vjust = +0.2),
  axis.title.x = element_text(size = 12, vjust = -0.2),
  axis.text.y = element_text(size = 10),
  axis.text.x = element_text(size = 10)
)

ggplot(data = dados, aes(x = Cat.Sexo, y = Altura)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = '#3274A1') + 
  coord_flip() +
  ylab("Metros") + 
  xlab("") + 
  ggtitle('Box-plot Alturas') +
  formatos

ggplot(data = dados, aes(x = Cat.Sexo, y = Altura, group = Sexo)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("Metros") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Alturas X Sexo') +
  formatos


ggplot(data = dados[dados$Renda < 10000,], aes(x = Cat.Sexo, y = Renda)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange"))  + 
  coord_flip() +
  ylab("Metros") + 
  xlab("") + 
  ggtitle('Box-plot Alturas') +
  formatos


dados$Cat.AnosdeEstudo  <- factor(dados$AnosdeEstudo ) 
levels(dados$Cat.AnosdeEstudo ) <- anos_de_estudo 

head(dados)

ggplot(data = dados, aes(x = "", y = AnosdeEstudo)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = '#3274A1') + 
  coord_flip() +
  ylab("Anos") + 
  xlab("") + 
  ggtitle('Box-plot Anos de Estudo') +
  formatos


ggplot(data = dados, aes(x =Cat.Sexo, y = AnosdeEstudo)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("Anos") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Anos de Estudo X Sexo') +
  formatos



dados$Cat.Cor  <- factor(dados$Cor ) 
levels(dados$Cat.Cor ) <- cor 

dados

ggplot(data = dados, aes(x =Cat.Cor, y = AnosdeEstudo)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c("Blue","Red","Green","Purple", "orange")) + 
  coord_flip() +
  ylab("Anos") + 
  xlab("Cor") + 
  ggtitle('Box-plot Anos de Estudo X Cor') +
  formatos












