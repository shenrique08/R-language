# QUESTÃO 01

# PARTE INICIAL (CARREGANDO O ARQUIVO)
diabetes_data <- read.csv("diabetes.txt", sep = ";", header = TRUE)

# 80% treinamento e 20% de teste

set.seed(123)  # para reprodutibilidade
train_index <- sample(seq_len(nrow(diabetes_data)), size = 0.7 * nrow(diabetes_data))
train_data <- diabetes_data[train_index, ]
test_data <- diabetes_data[-train_index, ]


#                          QUESTÃO (A)

library(ggplot2)

# 1 -> pacientes com diabestes, 2 -> sem diabetes
# distribuição da idade
ggplot(train_data, aes(x = Age, fill = factor(Diabetic))) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Distribuição da Idade por Status de Diabetes", x = "Idade", fill = "Diabetes")


# concentração de Glicose no Plasma
ggplot(train_data, aes(x = PlasmaGlucose, fill = factor(Diabetic))) +
  geom_histogram(binwidth = 10, position = "dodge") +
  labs(title = "Distribuição da Glicose por Status de Diabetes", x = "Glicose", fill = "Diabetes")


# índice de Massa Corporal
ggplot(train_data, aes(x = BMI, fill = factor(Diabetic))) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(title = "Distribuição do IMC por Status de Diabetes", x = "IMC", fill = "Diabetes")


# Pressão Sanguínea Distólica
ggplot(train_data, aes(x = DiastolicBloodPressure, fill = factor(Diabetic))) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Distribuição da Pressão Sanguínea Diastólica por Status de Diabetes", x = "Pressão Sanguínea", fill = "Diabetes")



# Análise dos resultados: A partir desses gráficos, observa-se diferençascomo:

# Idade: Pacientes com diabetes tendem a ser mais velhos.
# Concentração de Glicose no Plasma: Pacientes com diabetes têm glicose plasmática mais alta, sugerindo que este é um fator importante para o diagnóstico.
# Índice de Massa Corporal (IMC): O IMC mais elevado também aparece associado ao grupo com diabetes.
# Pressão Sanguínea Diastólica: Não há um padrão tão claro, mas valores mais elevados de pressão sanguínea diastólica podem ocorrer com maior frequência em pacientes com diabetes.



#                             QUESTÃO B


library(rpart)
library(rpart.plot)

# Modelo de árvore de decisão
tree_model <- rpart(Diabetic ~ Pregnancies + PlasmaGlucose + DiastolicBloodPressure + TricepsThickness + SerumInsulin + BMI + DiabetesPedigree + Age, 
                    data = train_data, method = "class")


# Visualizar a árvore de decisão
rpart.plot(tree_model, type = 3, extra = 101, fallen.leaves = TRUE, main = "Árvore de Decisão para Diagnóstico de Diabetes")


diagnostico_diabetes <- function(pregnancies, bmi, serum_insulin, age, plasma_glucose) {
  # Primeiro nível da árvore
  if (pregnancies < 2) {
    if (bmi < 22) {
      if (serum_insulin < 52) {
        return(0)  # Paciente sem diabetes
      } else {
        if (age < 36) {
          return(0)  # Paciente sem diabetes
        } else {
          return(1)  # Paciente com diabetes
        }
      }
    } else {
      if (age < 36) {
        if (plasma_glucose < 96) {
          return(0)  # Paciente sem diabetes
        } else {
          return(1)  # Paciente com diabetes
        }
      } else {
        return(1)  # Paciente com diabetes
      }
    }
  } else {
    if (bmi < 33) {
      if (age < 24) {
        return(0)  # Paciente sem diabetes
      } else {
        return(1)  # Paciente com diabetes
      }
    } else {
      if (age < 27) {
        return(0)  # Paciente sem diabetes
      } else {
        return(1)  # Paciente com diabetes
      }
    }
  }
}


# Aplicar a função de diagnóstico em cada paciente do conjunto de teste
test_data$predicted <- mapply(diagnostico_diabetes, 
                              test_data$Pregnancies, test_data$BMI, 
                              test_data$SerumInsulin, test_data$Age, 
                              test_data$PlasmaGlucose)

# Calcular a acurácia
accuracy <- mean(test_data$predicted == test_data$Diabetic)
print(paste("Acurácia do modelo:", round(accuracy * 100, 2), "%"))




#                   QUESTÃO C

library(randomForest)

# Converter a variável "Diabetic" em fator para garantir a classificação
train_data$Diabetic <- as.factor(train_data$Diabetic)
test_data$Diabetic <- as.factor(test_data$Diabetic)

# Criar o modelo de floresta aleatória
rf_model <- randomForest(Diabetic ~ ., data = train_data)

# Calcular a acurácia no conjunto de teste
rf_predictions <- predict(rf_model, test_data)
accuracy_rf <- mean(rf_predictions == test_data$Diabetic)
print(paste("Acurácia do modelo de floresta aleatória:", accuracy_rf))



#                   QUESTÃO D

# Taxa de acertos para pacientes com diabetes (sensibilidade)
library(caret)

# Calcular a matriz de confusão para a árvore de decisão
conf_matrix_tree <- confusionMatrix(as.factor(test_data$predicted), test_data$Diabetic)
sensitivity_tree <- conf_matrix_tree$byClass["Sensitivity"]

# Calcular a matriz de confusão para a floresta aleatória
conf_matrix_rf <- confusionMatrix(rf_predictions, test_data$Diabetic)
sensitivity_rf <- conf_matrix_rf$byClass["Sensitivity"]

print(paste("Sensibilidade da árvore de decisão:", round(sensitivity_tree, 4)))
print(paste("Sensibilidade da floresta aleatória:", round(sensitivity_rf, 4)))




#                   QUESTÃO E


# Em resumo, enquanto a árvore de decisão seja útil pela sua simplicidade, a floresta aleatória se destacou em termos de desempenho, apresentando uma sensibilidade significativamente maior. Essa análise sugere que, para este conjunto de dados, a floresta aleatória é a melhor escolha para o diagnóstico de diabetes, pois minimiza a chance de falsos negativos, o que pode ser vital para o tratamento e acompanhamento adequado dos pacientes.







