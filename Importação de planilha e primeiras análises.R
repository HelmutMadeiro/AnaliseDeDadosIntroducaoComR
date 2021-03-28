
#user._id, código atribuído ao aluno;
#course_id, código do curso;
#section_id, código para os vídeos.

#para que o banco reconheca o nome ddas variaveis contidas nele
#joga para a memoria 
#o que altara na memoria nao altera no arquivo
attach(aulas)  
section_id
options(max.print = 40000) # altera o numero de informações que sera exibida
head(section_id) # Se quisermos visualizar apenas os primeiros valores da variável
sort(section_id) # Poderemos ordenar a exibição dos valores

aulas[33137,3] <- 3255

sort(aulas$section_id)

unique(aulas$section_id)

length(unique(aulas$section_id))
 
table(aulas$section_id)

sort(table(aulas$section_id))
table(aulas$course_id)

install.packages("plyr")


library(plyr)
auxiliar<- count(aulas, vars = "course_id")
View(auxiliar)
write.csv(auxiliar,"popularidade.csv") # exportar a informação para excel








