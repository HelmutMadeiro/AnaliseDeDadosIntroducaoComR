




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

(8+ 10+ 4+ 8+ 6+ 10+ 8)/7

mean(df$Fulano)
mean(dados$Renda)

aggregate( list(renda =dados$Renda),list(sexo=dados$Sexo),mean)


#Mediana 

#primeiro ordernar os elementos 

df_fulano <- df[order(df$Fulano),]
df_fulano

n = nrow(df_fulano)
n
elemento_md <- (n+1)/2
elemento_md 

df_fulano[elemento_md,]

median(df$Fulano)

set.seed(101)
sample(nrow(df),6)
df_beltrando  <- df[sample(sample(nrow(df),6)),]
df_beltrando

n<- nrow(df_beltrando)
n
df_beltrando <- df_beltrando[order(df_beltrando$Beltrano),]
df_beltrando

elemento_md = n/2
elemento_md

mean(df_beltrando[c(elemento_md,elemento_md +1),]$Beltrano)

median(df_beltrando$Beltrano)


Moda <- function(x){
  freq <- table(x)
  return(names(freq)[freq==max(freq)])
}

teste <- c(1,2,3,4,23,2,1,3,3,21,1,2,32,4,4,2)

Moda(teste)
Moda(df$Fulano)
Moda(dados$Renda)
Moda(dados$Altura)


#relação  entre media mediana e Moda()

#Avaliando a variável RENDA \ Assimetrica a Direita


ggplot(dados[dados$Renda < 20000, ], aes(x = Renda, y = ..density..)) + 
  geom_histogram(binwidth = 500) + 
  geom_density(color = 'green')

moda <- as.numeric(Moda(dados$Renda))
moda

mediana <- median(dados$Renda)
mediana


media <- mean(dados$Renda)
media

#Avaliando a variável ALTURA \ Simetrica

ggplot(dados, aes(x = Altura, y = ..density..)) + 
  geom_histogram() + 
  geom_density(color = 'green')


moda <- as.numeric(Moda(dados$Altura))
moda

mediana <- median(dados$Altura)
mediana

media <- mean(dados$Altura)
media



#Avaliando a variável ANOS DE ESTUDO  \ Assimetrica a Esquerda

ggplot(dados, aes(x = AnosdeEstudo, y = ..density..)) + 
  geom_histogram() + 
  geom_density(color = 'green')

moda <- as.numeric(Moda(dados$AnosdeEstudo))
moda

mediana <- median(dados$AnosdeEstudo)
mediana

media = mean(dados$AnosdeEstudo)
media
