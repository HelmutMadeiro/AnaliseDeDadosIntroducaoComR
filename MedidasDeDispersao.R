#medidas de dispersão




materias <- c('Matemática', 'Português', 'Inglês', 'Geografia', 'História', 'Física', 'Química')
Fulano <- c(8, 10, 4, 8, 6, 10, 8)
Beltrano <- c(10, 2, 0.5, 1, 3, 9.5, 10)
Sicrano <- c(7.5, 8, 7, 8, 8, 8.5, 7)



df <- data.frame(
  Fulano,
  Beltrano,
  Sicrano,
  row.names = materias 
) 

df
notas_fulano <- data.frame(Fulano = df$Fulano,row.names = row.names(df))
notas_fulano

nota_media_fulano <- mean(notas_fulano$Fulano)
nota_media_fulano

notas_fulano$Desvio <- notas_fulano$Fulano - nota_media_fulano
notas_fulano

sum(notas_fulano$Desvio)

notas_fulano$Desvio.Absoluto <- abs(notas_fulano$Desvio)

notas_fulano


ggplot(data = notas_fulano, aes(x = row.names(notas_fulano), y = Fulano)) + 
  geom_point() + 
  geom_hline(yintercept = mean(notas_fulano$Fulano), color = 'red') + 
  geom_segment(aes(x = 1, y = 10, xend = 1, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 2, y = 8, xend = 2, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 3, y = 6, xend = 3, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 4, y = 4, xend = 4, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 5, y = 8, xend = 5, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 6, y = 10, xend = 6, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 7, y = 8, xend = 7, yend = mean(notas_fulano$Fulano)))



mean(notas_fulano$Desvio.Absoluto)

library(DescTools)


MeanAD(df$Fulano)



#variança




notas_fulano$Desvio2 <- notas_fulano$Desvio ^2 
notas_fulano

#vamanho da varianca
sum(notas_fulano$Desvio2)/ (nrow(notas_fulano) -1)


varianca <- var(notas_fulano$Fulano)
varianca


#desviopadrão

sqrt(varianca)

desvio_padrao <- sd(notas_fulano$Fulano)
desvio_padrao

df

summary(df)


Moda <- function(x){
  freq <- table(x)
  return(names(freq)[freq==max(freq)])
}


Moda(df$Fulano)
Moda(df$Sicrano)

sd(df$Fulano)
sd(df$Sicrano)


dataset <- data.frame( 
  Sexo = c('H', 'M', 'M', 'M', 'M', 'H', 'H', 'H', 'M', 'M'), 
  Idade = c(53, 72, 54, 27, 30, 40, 58, 32, 44, 51) 
) 

sd(dataset$Idade)

dataset <- data.frame( 
  Sexo = c('H', 'M', 'M', 'M', 'M', 'H', 'H', 'H', 'M', 'M'),
  Idade = c(53, 72, 54, 27, 30, 40, 58, 32, 44, 51)
) 

sd(dataset$Idade[dataset$Sexo =='M'])

aggregate(list(Idade = dataset$Idade), list(Sexo = dataset$Sexo), sd)[2, 2] 
