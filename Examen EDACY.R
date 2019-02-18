# charger la base de donnees
film1<-read.csv("file:///C:/Users/user/Desktop/Examen Edacy/Classeur1.csv" ,sep = ";")

str(film1) # on a une base de 09 observation et de 07 variables
#verifions les types de donnee et la distibution des variabes Types,Prix,desabonnement

class(film1$Type)
table(film1$Type)
class(film1$Prix)
film1$Prix<-as.factor(film1$Prix)
table(film1$prix)
class(film1$desabonnement)
#### Verifier la correlation entre les valeurs numeriques ####
cor(film1[c(,'annee','Type', 'prix')])
# visualisation des donnees avec le packages ggplot2
library(ggplot2)
ggplot(film1, aes(x = Prix, fill = desabonnement)) +
  geom_bar() +
  xlab("Prix") +
  ylab("Type") +
  labs(fill = "desabonnement")
# exploitation du model
library(caret)
library(caTools)
# entrainer et tester le model
set.seed(123)

sample <- sample.split(film1$desabonnement,SplitRatio=0.70)
trainData <- subset(film1,sample==TRUE)
testData <- subset(film1,sample==FALSE)
# model de regression logistique
filmmodel <- glm(desabonnement ~ .,family=binomial(link="logit"),data=trainData)
print(summary(filmmodel))
