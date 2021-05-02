## Avaliacao Tecnica estagio processamento - QUAEST 
## Candidata: Nathalia de Oliveira Moraes Tavares


library(tidyverse)
library(haven)
library(descr)
library(readr)
library(readxl)
library(ggplot2)

rm(list = ls())

#Ler o banco de dados em excel
bd_surveyquaest <- read_xlsx("bd_surveyquaest.xlsx")
glimpse(bd_surveyquaest)
head(bd_surveyquaest)

#backup
bk = bd_surveyquaest 

#selecionar apenas as variaveis sexo, aval_gov e voto1 para responder as perguntas:
bd = bd_surveyquaest %>% select(sexo, voto1, aval_gov)

#Transformar as tres colunas em factor e usar labels para organizar
bd$voto1 <- factor(bd$voto1, levels = c("Candidato 1", "Candidato 2", "Candidato 3", "Candidato 4", "Candidato 5", "Candidato 6", 
                                        "Candidato 7", "Candidato 8", "Candidato 9", "Candidato 10", "Candidato 11", "Candidato 12",
                                        "Candidato 13", "Candidato 14", "Ninguém/Branco/Nulo", "NS/NR"),
                   labels = c("Candidato 1", "Candidato 2", "Candidato 3", "Candidato 4", "Candidato 5", "Candidato 6", 
                              "Candidato 7", "Candidato 8", "Candidato 9", "Candidato 10", "Candidato 11", "Candidato 12",
                              "Candidato 13", "Candidato 14", "Ninguém/Branco/Nulo", "NS/NR"))

bd$sexo <- factor(bd$sexo, levels = c("Masculino", "Feminino"),
                  labels = c("Masculino", "Feminino"))


bd$aval_gov <- factor(bd$aval_gov, levels = c("Ótima", "Boa", "Regular positiva", 
                                          "Regular negativa", "Ruim", "Péssima", "NS/NR"),
                      labels = c("Ótima", "Boa", "Regular positiva", 
                                 "Regular negativa", "Ruim", "Péssima", "NS/NR"))


#Pergunta 1) Qual o % de intenção de voto para cada candidato?
freq(bd$voto1, plot = FALSE)

#Plotar o resultado
ggplot(bd, aes(x=voto1)) + 
  geom_bar() + 
  labs(x="Candidatos", y="Porcentagem", title="% de Intenção de voto por candidato") +
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

#Resposta 1)
# O candidato com a maior porcentagem de intenções de voto é o Candidato 2, com 52,3%.    
# Em segundo lugar ficou o candidato 1, com 4,2% e em terceiro o candidato 8, com 2,6%. Em seguida o candidato 5, teve 2.5% e o candidato 10, 2,0% das intenções de voto.  
# Os candidatos 9, 3, 4 e 6 tiveram, respectivamente, 1,9%, 1,6%, 1,2% e 1,0%.    
# Os candidatos com menor porcentagem de intenções de voto foram: o candidato 7, com 0,7%, os candidatos 12 e 14, ambos com 0,6% e por fim, os candidatos 11 e 13 com 0,3%.    
# 14% dos entrevistados não responderam ou não souberam responder(NS/NR) e 14,2% não votariam em ninguém ou votariam em Branco/Nulo.  

#Pergunta 2) Qual o candidato com maior % de intenção de voto entre as MULHERES?

prop.table(table(bd$voto1, bd$sexo),2) * 100 #comparar as duas variàveis

#Resposta 2)
# De acordo com a coluna "Feminino", o candidato com maior poncentagem de intenção de votos
# entre as Mulheres é o Candidato 2 com 54,12%.


#Pergunta 3) O candidato que lidera as intenções de voto é o candidato de situação ou oposição ao governo ?
prop.table(table(bd$voto1 == "Candidato 2", bd$aval_gov),2) * 100 #comparar as duas variàveis

# Resposta 3)
# Considerando a linha de dados `TRUE`, observa-se que 85,08% dos respondentes que prentendem votar no Candidato 2 
# consideram o atual governo como "Ótimo", 67,72% como "Bom" e 33,71% consideram como "Regular positivo", 
# formado maioria entre os entrevistados. Contra apenas 9,45% que consideram "Regular negativo", 
# 2,04% consideram "Ruim" e 1,56% consideram a atuação "Péssima". 
# Dessa forma, conclui-se que o Candidato 2 não é de oposição e sim situação em relação ao governo atual.

