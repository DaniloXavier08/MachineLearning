# Nome dos integrantes do grupo:
# - Danilo Xavier Silva
# - Patrícia Cristina Fernandes

# Funções de apoio -------------------------------------------------------------

# Calcula a loss de um modelo dado os valores preditos e os labels
getLoss <- function(y_true, y_pred){
  y_true <- as.numeric(y_true) - 1
  
  totalLoss <- 0
  eps <- 1e-9
  # Recall: length(y_true) == length(y_pred)
  # loss = (1-y)*log2(1 - p + eps)) + y*log(p + eps)
  # eps is used for numerical stability, it is very close to 0.
  # Supose we have y = 1 and p = 1 (perfect prediction), the loss (without eps)
  # would be 0*log2(0) + 1*log(1). It would result in NaN
  # because of 0*log2(0). With eps: 0*log2(1e-9) + 1*log(1 + 1e-9) 
  for(i in 1:length(y_true)){
    loss <- -1*((1 - y_true[i])*log2(1 - y_pred[i] + eps) + y_true[i]*log2(y_pred[i] + eps))
    totalLoss <- totalLoss + loss
  }
  totalLoss <- totalLoss/(length(y_true))
  return(totalLoss)
}


# Escreve a funcao de hipotese dada as features continuas e o 
# respectivo grau polinomial
getHypothesis <- function(feature_names, degree){
  
  hypothesis_string <- "hypothesis <- formula(target ~ "
  for(d in 1:degree){
    for(i in 1:length(feature_names)){
      hypothesis_string <- paste(hypothesis_string, 
                                 "I(", feature_names[i], "^", d, ") + ",
                                 sep = "")
    }
  }
  hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
  hypothesis_string <- paste(hypothesis_string, ")")
  hypothesis <- eval(parse(text=hypothesis_string))
  return(hypothesis)
}

# Calcula a matriz de confusão relativa 
calculaMatrizConfusaoRelativa <- function(cm){
  
  # Aplicamos a transposição para garantir que a referencia
  # fique nas linhas e a predicao nas colunas
  cm_absolute = t(cm$table)
  
  # SEMPRE construam e reportem a matriz de confusao relativa!
  cm_relative = cm_absolute
  
  cm_relative[1,1] = round(cm_absolute[1,1]/sum(cm_absolute[1,]), digits=2)
  cm_relative[1,2] = round(cm_absolute[1,2]/sum(cm_absolute[1,]), digits=2)
  cm_relative[2,1] = round(cm_absolute[2,1]/sum(cm_absolute[2,]), digits=2)
  cm_relative[2,2] = round(cm_absolute[2,2]/sum(cm_absolute[2,]), digits=2)
  
  return(cm_relative)  
}

# ------------------------------------------------------------------------------


set.seed(42)
setwd("~/MDC/0615 - Aprendizado de Maquina Supervisionado I/Trabalho 2")

library(glmnet)
library(caret)
library(pROC)

# Carregando datasets
trainSet <- read.csv("proteins_training_set.csv")
valSet <- read.csv("proteins_validation_set.csv")

summary(trainSet)
summary(valSet)

# 1) Inspecionando

dim(trainSet)
dim(valSet)

any(is.na(trainSet))
any(is.na(valSet))

# Removing duplicates 
dataTrain <- unique(trainSet)
dataTrain <- unique(valSet)

dim(trainSet)
dim(valSet)


# 2) Frequencia das classes

table(trainSet$target)
table(valSet$target)

# Balanceamento das classes
trainSetNo <- trainSet[trainSet$target == 0,]
trainSetYes <- trainSet[trainSet$target == 1,] 

dim(trainSetYes)
dim(trainSetNo)

randomNoIdx <- sample(1:nrow(trainSetNo), size=1.4*nrow(trainSetYes))
subsamplingNo <- trainSetNo[randomNoIdx,]
trainSet <- rbind(trainSetYes, subsamplingNo)

table(trainSet$target)

# 3) Normalização
  
## Normalizacao Z-norma 
mean_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, mean)
mean_features

sd_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, sd)
sd_features

trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, mean_features, "-")
trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, sd_features, "/")
summary(trainSet)

valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, mean_features, "-")
valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, sd_features, "/")
summary(valSet)

# 4) Regressao Logistica baseline

hypothesis <- formula(target ~ start_position + end_position + chou_fasman + 
                        emini + kolaskar_tongaonkar + parker + isoelectric_point
                      + aromaticity + hydrophobicity + stability)

x_train <- model.matrix(hypothesis, trainSet)
x_train
y_train <- trainSet$target
y_train

model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE,
                maxit = 1e+05, alpha=0, lambda = 1e-2)

trainPred <- predict(model, newx = x_train, type="response")
trainPred

trainClassPred <- trainPred

trainClassPred[trainPred >= 0.5] <- 1
trainClassPred[trainPred < 0.5] <- 0
trainClassPred

cm <- confusionMatrix(data = as.factor(trainClassPred), 
                      reference = as.factor(trainSet$target), 
                      positive='1')

cm$table 

# Matriz de confusão relativa Treino
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative


# TPR = TP / (TP+FN)
tpr <- cm$table[2,2] / sum(cm$table[2,])
tpr

# TNR = TN / (TN + FP)
tnr <- cm$table[1,1] / sum(cm$table[1,])
tnr

# Acuracia balanceada
acc_bal_train_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_train_baseline


# Predicao no conjunto de validacao
x_val <- model.matrix(hypothesis, valSet)
y_val <- valSet$target
valPred <- predict(model, newx = x_val, type="response")

valClassPred <- valPred
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$target), 
                      positive='1')

cm$table 

# Matriz de confusão relativa conjunto de Validação 
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative


# TPR = TP / (TP+FN)
tpr <- cm$table[2,2] / sum(cm$table[2,])
tpr

# TNR = TN / (TN + FP)
tnr <- cm$table[1,1] / sum(cm$table[1,])
tnr

# Acuracia balanceada
acc_bal_val_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_val_baseline


# ROC Curve for baseline
ROC <- roc(valSet$target, valPred[,1], direction="<")
ROC

plot(ROC, col="blue", lwd=2, main="ROC")


# 5) Soluções alternativas em regressão logistica

# Combinação de features

cor(trainSet[1:(ncol(trainSet)-1)])

f01 <- formula(target ~ . + (start_position + end_position + chou_fasman + emini
                             + kolaskar_tongaonkar + parker + isoelectric_point 
                             + aromaticity + hydrophobicity + stability))

f02 <- formula(target ~ . + (start_position + end_position + chou_fasman + emini
                             + kolaskar_tongaonkar + parker + isoelectric_point 
                             + aromaticity + hydrophobicity + stability)^2)

f03 <- formula(target ~ . + (start_position + end_position + chou_fasman + emini
                             + kolaskar_tongaonkar + parker + isoelectric_point 
                             + aromaticity + hydrophobicity + stability)^3)

f04 <- formula(target ~ . + (start_position + end_position + chou_fasman + emini
                             + kolaskar_tongaonkar + parker + isoelectric_point 
                             + aromaticity + hydrophobicity + stability)^4)


formulas <- c(f01, f02, f03, f04)

acc_train <- c()
acc_val_comb_features <- c()

i <- 1
for(f in formulas){  
  #print(i)
  
  # Applying hypothesis and training the model
  x_train <- model.matrix(f, trainSet)
  y_train <- trainSet$target
  model <- glmnet(x_train, y_train,  family="binomial", 
                  standardize = FALSE, maxit = 1e+05, 
                  alpha=0, lambda = 1e-2)
  
  trainPred <- predict(model, newx = x_train, type="response")
  
  #converting to class
  trainClassPred <- trainPred
  
  #### THRESHOLD ####
  # Threshold = 0.5 
  trainClassPred[trainPred >= 0.5] <- 1
  trainClassPred[trainPred < 0.5] <- 0
  #trainClassPred
  
  cm <- confusionMatrix(data = as.factor(trainClassPred), 
                        reference = as.factor(trainSet$target), 
                        positive='1')
  
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  
  # Validation
  x_val <- model.matrix(f, valSet)
  y_val <- valSet$target
  valPred <- predict(model, newx = x_val, type="response")
  
  #converting to class
  valClassPred <- valPred
  
  #### THRESHOLD ####
  # Threshold = 0.5 
  valClassPred[valPred >= 0.5] <- 1
  valClassPred[valPred < 0.5] <- 0
  
  cm <- confusionMatrix(data = as.factor(valClassPred), 
                        reference = as.factor(valSet$target), 
                        positive='1')
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  acc_train[i] <- acc_bal_train 
  acc_val_comb_features[i] <- acc_bal_val 
  
  # Matriz de confusão relativa Conjunto Validação
  print(cm_relative)
  
  # Acuracia balanceada Conjunto Validação
  cat("Acuracia Balanceada: ", acc_bal_val,"\n")
  
  i <- i + 1
}



# Modelos Polinomiais

acc_train <- c()
acc_val_poly <- c()

feature_names <- colnames(trainSet)[1:(ncol(trainSet)-1)]

for(i in 1:4){
  
  print(i)
  hypothesis <- getHypothesis(feature_names, i)
  
  # Applying hypothesis and training the model
  x_train <- model.matrix(hypothesis, trainSet)
  y_train <- trainSet$target
  model <- glmnet(x_train, y_train,  family="binomial", 
                  standardize = FALSE, maxit = 1e+05, 
                  alpha=0, lambda = 1e-2)
  
  trainPred <- predict(model, newx = x_train, type="response")
  
  #converting to class
  trainClassPred <- trainPred
  
  #### THRESHOLD ####
  # Threshold = 0.5 
  trainClassPred[trainPred >= 0.5] <- 1
  trainClassPred[trainPred < 0.5] <- 0
  
  cm <- confusionMatrix(data = as.factor(trainClassPred), 
                        reference = as.factor(trainSet$target), 
                        positive='1')
  
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  # Validation
  x_val <- model.matrix(hypothesis, valSet)
  y_val <- valSet$target
  valPred <- predict(model, newx = x_val, type="response")
  
  #converting to class
  valClassPred <- valPred
  
  #### THRESHOLD ####
  # Threshold = 0.5 
  valClassPred[valPred >= 0.5] <- 1
  valClassPred[valPred < 0.5] <- 0
  
  cm <- confusionMatrix(data = as.factor(valClassPred), 
                        reference = as.factor(valSet$target), 
                        positive='1')
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  acc_train[i] <- acc_bal_train 
  acc_val_poly[i] <- acc_bal_val 
  
  # Matriz de confusão relativa Conjunto Validação
  print(cm_relative)
  
  # Acuracia balanceada Conjunto Validação
  cat("Acuracia Balanceada: ", acc_bal_val,"\n")
}


# 6)

acc_train <- c()
acc_val_lambda <- c()

lambda_values <- c(1.0, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6)

i <- 1
for(l in lambda_values){
  
  print(l)
  # Applying hypothesis and training the model
  x_train <- model.matrix(hypothesis, trainSet)
  y_train <- trainSet$target
  model <- glmnet(x_train, y_train,  family="binomial", 
                  standardize = FALSE, maxit = 1e+05, 
                  alpha=0, lambda = l)
  
  trainPred <- predict(model, newx = x_train, type="response")
  
  #converting to class
  trainClassPred <- trainPred
  
  #### THRESHOLD ####
  # Threshold = 0.5 
  trainClassPred[trainPred >= 0.5] <- 1
  trainClassPred[trainPred < 0.5] <- 0
  
  cm <- confusionMatrix(data = as.factor(trainClassPred), 
                        reference = as.factor(trainSet$target), 
                        positive='1')
  
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  
  # Validation
  x_val <- model.matrix(hypothesis, valSet)
  y_val <- valSet$target
  valPred <- predict(model, newx = x_val, type="response")
  
  #converting to class
  valClassPred <- valPred
  
  #### THRESHOLD ####
  # Threshold = 0.5 
  valClassPred[valPred >= 0.5] <- 1
  valClassPred[valPred < 0.5] <- 0
  
  cm <- confusionMatrix(data = as.factor(valClassPred), 
                        reference = as.factor(valSet$target), 
                        positive='1')
  
  cm_relative <- calculaMatrizConfusaoRelativa(cm)
  acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
  
  acc_train[i] <- acc_bal_train 
  acc_val_lambda[i] <-acc_bal_val 
  i <- i + 1
  
}

plot(acc_train, xlab="Regularization factor (lambda)", ylab="Acc Balanced", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(acc_train, acc_val_lambda)),
            max(c(acc_train, acc_val_lambda))))

axis(1, at=1:length(lambda_values), labels=lambda_values, 
     cex.axis=0.5, las=2)
points(acc_val_lambda, pch="*", col="blue")
points(rep(acc_bal_val_baseline, length(acc_val_lambda)), pch="o", col="green")

lines(acc_train, col="red", lty=2)
lines(acc_val_lambda, col="blue", lty=2)
lines(rep(acc_bal_val_baseline, length(acc_val_lambda)), col="green", lty=2)
legend(5, 0.54, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)

# Underfitting - primeiro e segundo valor de lambda (1 e 0.1)

# Ponto otimo -  lambda = 0.001
which.max(acc_val_lambda)
best_lambda<- lambda_values[which.max(acc_val_lambda)]
best_lambda

# Overfitting - a partir do sexto valor de lambda (1e-05 e 1e-06)


# Conjunto de Teste ------------------------------------------------------------

testSet <- read.csv("proteins_test_set.csv")

summary(testSet)
dim(testSet)
any(is.na(testSet))

testSet <- unique(testSet)
dim(testSet)

testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, mean_features, "-")
testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, sd_features, "/")
summary(testSet)

# 4)

hypothesis <- formula(target ~ start_position + end_position + chou_fasman + 
                        emini + kolaskar_tongaonkar + parker + isoelectric_point
                      + aromaticity + hydrophobicity + stability)

x_train <- model.matrix(hypothesis, trainSet)
x_train
y_train <- trainSet$target
y_train

model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE,
                maxit = 1e+05, alpha=0, lambda = 1e-2)

trainPred <- predict(model, newx = x_train, type="response")

x_test <- model.matrix(hypothesis, testSet)
y_test <- testSet$target
testPred <- predict(model, newx = x_test, type="response")

testClassPred <- testPred
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$target), 
                      positive='1')

# Matriz de confusão relativa 
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

# TPR = TP / (TP+FN)
tpr <- cm$table[2,2] / sum(cm$table[2,])
tpr

# TNR = TN / (TN + FP)
tnr <- cm$table[1,1] / sum(cm$table[1,])
tnr

# Acuracia balanceada
acc_bal_test <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_test



# 5)

max(acc_val_comb_features) # 0.64
max(acc_val_poly)          # 0.625

# Melhor modelo: Combinacao de features

i <- which.max(acc_val_comb_features)
f <- formulas[[i]]

x_train <- model.matrix(f, trainSet)
y_train <- trainSet$target
model <- glmnet(x_train, y_train,  family="binomial", 
                standardize = FALSE, maxit = 1e+05, 
                alpha=0, lambda = 1e-2)

x_test <- model.matrix(f, testSet)
y_test <- testSet$target
testPred <- predict(model, newx = x_test, type="response")

testClassPred <- testPred
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0


cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$target), 
                      positive='1')

# Matriz de confusão relativa 
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

# TPR = TP / (TP+FN)
tpr <- cm$table[2,2] / sum(cm$table[2,])
tpr

# TNR = TN / (TN + FP)
tnr <- cm$table[1,1] / sum(cm$table[1,])
tnr

# Acuracia balanceada
acc_bal_test_comb_features <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_test_comb_features


# 6) 

i<- which.max(acc_val_lambda)
i

best_lambda <- lambda_values[i]
best_lambda

x_train <- model.matrix(hypothesis, trainSet)
y_train <- trainSet$target

model <- glmnet(x_train, y_train,  family="binomial", 
                standardize = FALSE, maxit = 1e+05, 
                alpha=0, lambda = best_lambda)

trainPred <- predict(model, newx = x_train, type="response")

x_test <- model.matrix(hypothesis, testSet)
y_test <- testSet$target
testPred <- predict(model, newx = x_test, type="response")

testClassPred <- testPred
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$target), 
                      positive='1')


# Matriz de confusão relativa 
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

# TPR = TP / (TP+FN)
tpr <- cm$table[2,2] / sum(cm$table[2,])
tpr

# TNR = TN / (TN + FP)
tnr <- cm$table[1,1] / sum(cm$table[1,])
tnr

# Acuracia balanceada
acc_bal_test_lambda <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_test_lambda




# 7) SARS-----------------------------------------------------------------------

SARStestSet <- read.csv("SARS_test_set.csv")

summary(SARStestSet)
dim(SARStestSet)
any(is.na(SARStestSet))

SARStestSet <- unique(SARStestSet)
dim(SARStestSet)

SARStestSet[,1:(ncol(SARStestSet)-1)] <- sweep(SARStestSet[,1:(ncol(SARStestSet)-1)], 2, mean_features, "-")
SARStestSet[,1:(ncol(SARStestSet)-1)] <- sweep(SARStestSet[,1:(ncol(SARStestSet)-1)], 2, sd_features, "/")
summary(SARStestSet)

max(acc_val_comb_features, acc_val_poly, acc_val_lambda)

i <- which.max(acc_val_comb_features)
f <- formulas[[i]]

x_train <- model.matrix(f, trainSet)
y_train <- trainSet$target
model <- glmnet(x_train, y_train,  family="binomial", 
                standardize = FALSE, maxit = 1e+05, 
                alpha=0, lambda = 1e-2)

x_SARStest <- model.matrix(f, SARStestSet)
y_SARStest <- SARStestSet$target
SARStestPred <- predict(model, newx = x_SARStest, type="response")

SARStestClassPred <- SARStestPred
SARStestClassPred[SARStestPred >= 0.5] <- 1
SARStestClassPred[SARStestPred < 0.5] <- 0


cm <- confusionMatrix(data = as.factor(SARStestClassPred), 
                      reference = as.factor(SARStestSet$target), 
                      positive='1')

# Matriz de confusão relativa 
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

# TPR = TP / (TP+FN)
tpr <- cm$table[2,2] / sum(cm$table[2,])
tpr

# TNR = TN / (TN + FP)
tnr <- cm$table[1,1] / sum(cm$table[1,])
tnr

# Acuracia balanceada
acc_bal_SARS_test_comb_features <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_SARS_test_comb_features
