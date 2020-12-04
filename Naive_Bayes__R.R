base = read.csv('census.csv')
base$X = NULL
table(base$sex)
unique(base$sex)
base$sex = factor(base$sex, levels = c(' Female', ' Male'), labels = c(0, 1))
base[is.na(base$sex)]
#dividir a base de dados em treinamento e teste
library('caTools')
set.seed(1)
divisao<-sample.split(base$income,SplitRatio = 0.85)
base_treinamento<-subset(base, divisao==TRUE)
base_teste<-subset(base, divisao==FALSE)
#Naive Bayes
library(e1071)
classificador<-naiveBayes(x=base_treinamento[-15],y=base_treinamento$income)
previsoes<-predict(classificador,newdata = base_teste[-15])
matriz_confusao<-table(base_teste[,15],previsoes)
#Precisão de 82,16%
library(caret)
confusionMatrix(matriz_confusao)