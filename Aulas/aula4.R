titanic <- read.table(file = "titanic.txt", sep = ",", header = TRUE)

titanic <- titanic[,-c(1,9:12)]
str(titanic)

titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic $Pclass)
titanic$Sex <- as.factor(titanic $Sex)

summary(titanic)

ggplot(data = titanic, aes(x = Survived))+
  geom_bar(fill = "blue")+
  theme_minimal()

ggplot(data = titanic, aes(x = Survived, fill = Sex))+
  geom_bar()+
  theme_minimal()


ggplot(data = titanic, aes(fill = Survived, x = Sex))+geom_bar()+
  scale_fill_manual(values = c("0" = "#331F40", "1" = "#6A2473"))


