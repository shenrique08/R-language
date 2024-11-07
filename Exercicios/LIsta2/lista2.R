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




#                    ********* QUESTÃO 2 *************

#(A)

cerebelo_data <- read.csv("cerebelo.csv")


# Gráfico de dispersão do peso do cerebelo em relação ao peso do corpo
plot(cerebelo_data$Body_g, cerebelo_data$Cerebellum_g,
     xlab = "Peso do Corpo (g)",
     ylab = "Peso do Cerebelo (g)",
     main = "Dispersão: Peso do Cerebelo vs Peso do Corpo",
     pch = 19, col = "blue")

# Gráfico de dispersão dos valores log-transformados
plot(cerebelo_data$Log_body, cerebelo_data$Log_cerebellum,
     xlab = "Log(Peso do Corpo)",
     ylab = "Log(Peso do Cerebelo)",
     main = "Dispersão: Log(Peso do Cerebelo) vs Log(Peso do Corpo)",
     pch = 19, col = "red")



# A escala logarítmica tende a mostrar uma relação mais linear entre as variáveis, sugerindo que o aumento do peso do cerebelo é proporcional ao aumento do peso do corpo.

# O primeiro gráfico utiliza valores originais e pode apresentar uma curva crescente

# No segundo gráfico, a transformação logarítmica geralmente lineariza a relação, facilitando a análise de proporcionalidade.





# *******  (B) ********



# Calcular o coeficiente de correlação entre o peso do cerebelo e o peso do corpo
cor_original <- cor(cerebelo_data$Body_g, cerebelo_data$Cerebellum_g)
cat("Correlação (valores originais):", cor_original, "\n")

# Calcular o coeficiente de correlação entre os valores log-transformados
cor_log <- cor(cerebelo_data$Log_body, cerebelo_data$Log_cerebellum)
cat("Correlação (valores log-transformados):", cor_log, "\n")



# *******   (C) ********


# Calcular o coeficiente de correlação entre o logaritmo do peso do cerebelo e o logaritmo do peso do corpo
cor_log <- cor(cerebelo_data$Log_body, cerebelo_data$Log_cerebellum)
cat("Correlação (valores log-transformados):", cor_log, "\n")



# *******   (D) ********


# Coeficiente com valores originais: A correlação entre os pesos do corpo e do cerebelo nos dados originais pode ser menor ou apresentar uma relação menos linear, especialmente se houver uma grande variação nos tamanhos dos animais, já que os dados sem transformação podem não seguir uma relação direta.


# Coeficiente com valores log-transformados: A correlação entre os logaritmos dos pesos geralmente será mais próxima de 1 (ou seja, mais alta), indicando uma relação linear mais forte. A transformação logarítmica reduz as diferenças extremas nos dados e revela proporcionalidades, sugerindo que o peso do cerebelo aumenta em proporção ao peso do corpo.



# *******   (E) ********


# Ajustar o modelo de regressão linear usando os valores log-transformados
modelo_log <- lm(Log_cerebellum ~ Log_body, data = cerebelo_data)

# Visualizar o resumo do modelo para interpretação dos resultados
summary(modelo_log)

# Equação da reta de regressão
coeficientes <- coef(modelo_log)
cat("Equação da reta: Log_cerebellum =", coeficientes[1], "+", coeficientes[2], "* Log_body", "\n")

# Gráfico com a reta de regressão adicionada
plot(cerebelo_data$Log_body, cerebelo_data$Log_cerebellum,
     xlab = "Log(Peso do Corpo)",
     ylab = "Log(Peso do Cerebelo)",
     main = "Dispersão: Log(Peso do Cerebelo) vs Log(Peso do Corpo) com Reta de Regressão",
     pch = 19, col = "red")

# Adicionar a reta de regressão ao gráfico
abline(modelo_log, col = "blue", lwd = 2)



# *******   (F) ********


# Obter os resíduos do modelo de regressão
residuos <- residuals(modelo_log)

# Teste de Shapiro-Wilk para verificar normalidade dos resíduos
shapiro_test <- shapiro.test(residuos)
cat("Valor p do teste de Shapiro-Wilk:", shapiro_test$p.value, "\n")

# Gráfico Q-Q dos resíduos
qqnorm(residuos)
qqline(residuos, col = "blue", lwd = 2)



# Caso o valor p do teste de Shapiro-Wilk seja alto e o gráfico Q-Q mostre os resíduos alinhados, pode-se concluir que os resíduos seguem uma distribuição Normal. Se o valor p for baixo ou o gráfico Q-Q mostrar desvios significativos, isso indica uma possível violação da normalidade dos resíduos.



# *******   (F) ********




peso_corpo <- 100000


log_peso_corpo <- log10(peso_corpo)

# Usar a equação da reta de regressão para prever o logaritmo do peso do cerebelo
log_peso_cerebelo_pred <- coeficientes[1] + coeficientes[2] * log_peso_corpo

# Transformar a previsão de volta para a escala original em gramas
peso_cerebelo_pred <- 10^log_peso_cerebelo_pred
cat("Peso previsto do cerebelo:", peso_cerebelo_pred, "gramas\n")




#                    ********* QUESTÃO 3 *************




# (A)



dados <- read.csv("olive.txt", sep = ",", header = TRUE)

dados_num <- dados[, 2:9]

# Padronizar os dados
dados_padronizados <- scale(dados_num)

head(dados_padronizados)



# (B)


# Construir o modelo K-means com k = 3
set.seed(123)  
modelo_kmeans <- kmeans(dados_padronizados, centers = 3)


dados$cluster_k3 <- as.factor(modelo_kmeans$cluster)
library(ggplot2)

# Criar o gráfico de barras
ggplot(dados, aes(x = cluster_k3, fill = region)) +
  geom_bar() +
  labs(title = "Distribuição dos Azeites por Cluster e Região",
       x = "Cluster K-means",
       y = "Quantidade de Azeites") +
  theme_minimal()



# o gráfico de barras exibe um padrão regional em relação aos clusters ou se as distribuição dos azeites nas regiões estão misturados entre os clusters.

# Por exemplo, norte da Itália é forte o cluster 3. Já para o sul, o cluster 2 ficou forte. Enquanto que no cluster 1 ficou equilibrado


# (C)



set.seed(123) 
modelo_kmeans_4 <- kmeans(dados_padronizados, centers = 4)

# Adicionar os clusters ao conjunto de dados
dados$cluster_k4 <- as.factor(modelo_kmeans_4$cluster)


ggplot(dados, aes(x = cluster_k4, fill = region)) +
  geom_bar() +
  labs(title = "Distribuição dos Azeites por Cluster (k = 4) e Região",
       x = "Cluster K-means (k = 4)",
       y = "Quantidade de Azeites") +
  theme_minimal()


modelo_kmeans_5 <- kmeans(dados_padronizados, centers = 5)


dados$cluster_k5 <- as.factor(modelo_kmeans_5$cluster)

# Criar o gráfico de barras para k = 5
ggplot(dados, aes(x = cluster_k5, fill = region)) +
  geom_bar() +
  labs(title = "Distribuição dos Azeites por Cluster (k = 5) e Região",
       x = "Cluster K-means (k = 5)",
       y = "Quantidade de Azeites") +
  theme_minimal()




# o gráfico de barras exibe um padrão regional em relação aos clusters ou se as distribuição dos azeites nas regiões estão misturados entre os clusters.

# Por exemplo, norte da Itália é forte o cluster 3. Já para o sul, o cluster 2 ficou forte. Enquanto que no cluster 1 ficou equilibrado. Para k 4 o norte da Itália e para k 5 apareceu Sardina


