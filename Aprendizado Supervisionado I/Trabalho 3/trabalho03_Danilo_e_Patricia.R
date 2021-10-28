# Nome dos integrantes do grupo:
# - Danilo Xavier Silva
# - Patricia Cristina Fernandes


####### Código de apoio ao Trabalho 03 da disciplina INF-0615 #######

# Funcao que calcula a matriz de confusao relativa para 3 classes
calculaMatrizConfusaoRelativa <- function(cm){
    
    # Aplicamos a transposição para garantir que a referencia
    # fique nas linhas e a predicao nas colunas
    cm_absolute = t(cm$table)
    
    # SEMPRE construam e reportem a matriz de confusao relativa!
    cm_relative = cm_absolute
    
    cm_relative[1,] = round(cm_absolute[1,]/sum(cm_absolute[1,]), digits=2)
    cm_relative[2,] = round(cm_absolute[2,]/sum(cm_absolute[2,]), digits=2)
    cm_relative[3,] = round(cm_absolute[3,]/sum(cm_absolute[3,]), digits=2)
    
    return(cm_relative)  
}


# Leitura da base de treinamento+validacao
train_val_set <- read.csv("train_val_set_patient_status_covid19.csv", stringsAsFactors = T)

####### ======= O TRABALHO COMEÇA A PARTIR DAQUI ======= #######

# Configurem o valor da semente.
set.seed(8)

# Bibliotecas

library(reshape2)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

# Removendo duplicatas

dim(train_val_set)
train_val_set <- unique(train_val_set)
dim(train_val_set)

# Treino 80% / Validação 20%
randomTrainIndexes <- sample(1:nrow(train_val_set), size=0.8*nrow(train_val_set))
trainSet <- train_val_set[randomTrainIndexes, ]
valSet  <- train_val_set[-randomTrainIndexes, ] 

merge(trainSet, valSet)
any(is.na(trainSet))
any(is.na(valSet))

dim(trainSet)
dim(valSet)

summary(trainSet)
summary(valSet)

table(trainSet$label)
table(valSet$label)

# Não está balanceado

# Balanceamento das classes
trainSetDead <- trainSet[trainSet$label == "dead",]
trainSetTreatment <- trainSet[trainSet$label == "onTreatment",]
trainSetRecovered <- trainSet[trainSet$label == "recovered",] 

dim(trainSetDead)
dim(trainSetTreatment)
dim(trainSetRecovered)

# Undersampling
randomTreatmentIdx <- sample(1:nrow(trainSetTreatment), size=3*nrow(trainSetDead))
subsamplingTreatment <- trainSetTreatment[randomTreatmentIdx,]

randomRecoveredIdx <- sample(1:nrow(trainSetRecovered), size=3*nrow(trainSetDead))
subsamplingRecovered <- trainSetRecovered[randomRecoveredIdx,]

trainSet <- rbind(trainSetDead, subsamplingTreatment, subsamplingRecovered)
table(trainSet$label)


# 2)

treeModel_baseline <- rpart(formula=label ~ age + sex + country + latitude + longitude + 
                       date_onset_symptoms + date_admission_hospital + 
                       date_confirmation + lives_in_Wuhan + travel_history_dates + 
                       travel_history_location + chronic_disease_binary + 
                       date_death_or_discharge + travel_history_binary,
                   data=trainSet, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, xval = 10),
                   parms= list(split="information"))


printcp(treeModel_baseline)
summary(treeModel_baseline)

# Treinamento

train_pred <- predict(treeModel_baseline, trainSet, type="class")
cm <- confusionMatrix(data = as.factor(train_pred), 
                      reference = as.factor(trainSet$label))

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_train_baseline <- (cm_relative[1,1] + cm_relative[2,2] + cm_relative[3,3])/3
acc_train_baseline

# Validação

val_pred <- predict(treeModel_baseline, valSet, type="class")
cm <- confusionMatrix(data = as.factor(val_pred), 
                      reference = as.factor(valSet$label))

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_val_baseline <- (cm_relative[1,1] + cm_relative[2,2] + cm_relative[3,3])/3
acc_val_baseline


# 3)

number_of_depths = 20
accPerDepth <- data.frame(depth=numeric(number_of_depths), 
                          accTrain=numeric(number_of_depths), 
                          accVal=numeric(number_of_depths), stringsAsFactors = FALSE)
summary(accPerDepth)
acc_val_size_variation <- c()

for (maxDepth in 1:number_of_depths){
    cat(maxDepth)
    treeModel <- rpart(formula=label ~ age + sex + country + latitude + longitude + 
                           date_onset_symptoms + date_admission_hospital + 
                           date_confirmation + lives_in_Wuhan + travel_history_dates + 
                           travel_history_location + chronic_disease_binary + 
                           date_death_or_discharge + travel_history_binary, 
                           data=trainSet, method="class",
                           control=rpart.control(minsplit=2, cp=0.0, 
                                             maxdepth=maxDepth, xval = 0),
                       parms= list(split="information"))
    
    # Avaliando no conjunto de treino
    train_pred <- predict(treeModel, trainSet, type="class")
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                                reference = as.factor(trainSet$label))
    
    cm_relative_train <- calculaMatrizConfusaoRelativa(cm_train)
    acc_bal_train <- (cm_relative_train[1,1] + cm_relative_train[2,2] + cm_relative_train[3,3])/3
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(treeModel, valSet, type="class")
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                              reference = as.factor(valSet$label))
    
    cm_relative_val <- calculaMatrizConfusaoRelativa(cm_val)
    acc_bal_val <- (cm_relative_val[1,1] + cm_relative_val[2,2] + cm_relative_val[3,3])/3
    
    acc_val_size_variation[maxDepth] <- acc_bal_val
    
    accPerDepth[maxDepth,] <- c(maxDepth, acc_bal_train, 
                              acc_bal_val)
}

accPerDepth <- melt(accPerDepth, id="depth")  # convert to long format
ggplot(data=accPerDepth, aes(x=depth, y=value, colour=variable)) + geom_line() + geom_point() +
    geom_hline(aes(yintercept = acc_val_baseline, linetype = "Baseline"), colour = "green", size=1) + 
    scale_linetype_manual(name="", values=c("dashed"))

max(acc_val_size_variation)
which.max(acc_val_size_variation)

# 4)
# -----

# Conjunto de Features 1:
# age, country, date_confirmation , chronic_disease_binary e travel_history_binary

treeModel_ft_sel_1 <- rpart(formula=label ~ age + country	+ date_confirmation + 
                                chronic_disease_binary + travel_history_binary, 
                            data=trainSet, method="class",
                            control=rpart.control(minsplit=2, cp=0.0, xval = 10),
                            parms= list(split="information"))

summary(treeModel_ft_sel_1)
printcp(treeModel_ft_sel_1)

# -----

train_pred_ft_sel_1 <- predict(treeModel_ft_sel_1, trainSet, type="class")
cm <- confusionMatrix(data = as.factor(train_pred_ft_sel_1), 
                      reference = as.factor(trainSet$label))

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_train_ft_sel_1 <- (cm_relative[1,1] + cm_relative[2,2] + cm_relative[3,3])/3
acc_train_ft_sel_1

# -----

val_pred_ft_sel_1 <- predict(treeModel_ft_sel_1, valSet, type="class")
cm <- confusionMatrix(data = as.factor(val_pred_ft_sel_1), 
                      reference = as.factor(valSet$label))

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_val_ft_sel_1 <- (cm_relative[1,1] + cm_relative[2,2] + cm_relative[3,3])/3
acc_val_ft_sel_1

# ----

### Verificando importancia das variáveis ###
importance_per_features <- treeModel_baseline$variable.importance
importance_per_features

relative_importance <- importance_per_features/sum(importance_per_features)
relative_importance


# Conjunto de Features 2:
# date_death_or_discharge, longitude, date_admission_hospital, country, 
# travel_history_dates e lives_in_Wuhan

treeModel_ft_sel_2 <- rpart(formula=label ~ date_death_or_discharge + longitude + date_admission_hospital + country + travel_history_dates + lives_in_Wuhan,
                            data=trainSet, method="class",
                            control=rpart.control(minsplit=2, cp=0.0, xval = 10),
                            parms= list(split="information"))

summary(treeModel_ft_sel_2)
printcp(treeModel_ft_sel_2)

# -----

train_pred_ft_sel_2 <- predict(treeModel_ft_sel_2, trainSet, type="class")
cm <- confusionMatrix(data = as.factor(train_pred_ft_sel_2), 
                      reference = as.factor(trainSet$label))

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_train_ft_sel_2 <- (cm_relative[1,1] + cm_relative[2,2] + cm_relative[3,3])/3
acc_train_ft_sel_2

# -----

val_pred_ft_sel_2 <- predict(treeModel_ft_sel_2, valSet, type="class")
cm <- confusionMatrix(data = as.factor(val_pred_ft_sel_2), 
                      reference = as.factor(valSet$label))

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_val_ft_sel_2 <- (cm_relative[1,1] + cm_relative[2,2] + cm_relative[3,3])/3
acc_val_ft_sel_2

# -----


# 5) Random Forest




# Acurácias de treinamento e de validação variando com o número de 
# árvores na floresta aleatória
nTreeList = c(1,5,10,15,20,30,40,50,75,100)#, 250, 500, 1000)
accPerNTrees <- data.frame(ntree=numeric(length(nTreeList)), 
                           accTrain=numeric(length(nTreeList)), 
                           accVal=numeric(length(nTreeList)))

acc_val_rf <- c()

set.seed(42)
for (i in 1:length(nTreeList)){
    cat(i)
    rfModel <- randomForest(formula=label ~ age + sex + country + latitude + longitude + 
                                date_onset_symptoms + date_admission_hospital + 
                                date_confirmation + lives_in_Wuhan + travel_history_dates + 
                                travel_history_location + chronic_disease_binary + 
                                date_death_or_discharge + travel_history_binary, 
                            data=trainSet, ntree=nTreeList[i])#, mtry=7)
    
    # Avaliando no conjunto de treino
    train_pred <- predict(rfModel, trainSet, type="class")
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                                reference = as.factor(trainSet$label))
    
    cm_relative_train <- calculaMatrizConfusaoRelativa(cm_train)
    cm_relative_train
    acc_bal_train <- (cm_relative_train[1,1] + cm_relative_train[2,2] + cm_relative_train[3,3])/3
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(rfModel, valSet, type="class")
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                              reference = as.factor(valSet$label))
    
    cm_relative_val <- calculaMatrizConfusaoRelativa(cm_val)
    acc_bal_val <- (cm_relative_val[1,1] + cm_relative_val[2,2] + cm_relative_val[3,3])/3
    
    acc_val_rf[i] <- acc_bal_val
    
    accPerNTrees[i,] = c(nTreeList[i], 
                         acc_bal_train, 
                         acc_bal_val)
}

accPerNTrees <- melt(accPerNTrees, id="ntree")  # convert to long format
ggplot(data=accPerNTrees, aes(x=ntree, y=value, colour=variable)) + geom_line() + geom_point() +
    geom_hline(aes(yintercept = acc_val_baseline, linetype = "Baseline"), colour = "green", size=1) + 
    scale_linetype_manual(name="", values=c("dashed"))

max(acc_val_rf)
nTreeList[which.max(acc_val_rf)]



# Leitura da base de Teste. Descomentem as linhas abaixo quando o 
# conjunto de teste estiver disponível.

test_set <- read.csv("test_set_patient_status_covid19.csv", stringsAsFactors = T) # Descomentar

# As duas linhas abaixo são um trick para corrigir os "levels" na
# coluna country. Ele apenas adiciona 1 exemplo de treino na primeira
# linha do teste e depois retira-o para obter o test_set original. 
# Nao se preocupem, eh apenas para nivelamento interno do R. 
# Certifiquem-se de executar os comandos na seguinte ordem:
# linha 38, linha 47 e linha 48 quando a base de teste estiver disponivel

temporary_test <- rbind(train_val_set[1,], test_set) # Descomentar
test_set <- temporary_test[-1,] # Descomentar

dim(test_set)

# 2 ---

test_pred <- predict(treeModel_baseline, test_set, type="class")
cm <- confusionMatrix(data = as.factor(test_pred), 
                      reference = as.factor(test_set$label))

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal_test_baseline <- (cm_relative[1,1] + cm_relative[2,2] + cm_relative[3,3])/3
acc_bal_test_baseline

# 3 ---

best_tree <- which.max(acc_val_size_variation) # 6
best_tree

treeModel_best <- rpart(formula=label ~ age + sex + country + latitude + longitude + 
                       date_onset_symptoms + date_admission_hospital + 
                       date_confirmation + lives_in_Wuhan + travel_history_dates + 
                       travel_history_location + chronic_disease_binary + 
                       date_death_or_discharge + travel_history_binary, 
                   data=trainSet, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, 
                                         maxdepth=best_tree, xval = 0),
                   parms= list(split="information"))


test_pred <- predict(treeModel_best, test_set, type="class")
cm_test <- confusionMatrix(data = as.factor(test_pred), 
                            reference = as.factor(test_set$label))

cm_relative_test <- calculaMatrizConfusaoRelativa(cm_test)
cm_relative_test

acc_bal_test <- (cm_relative_test[1,1] + cm_relative_test[2,2] + cm_relative_test[3,3])/3
acc_bal_test

# 4 ---

max(acc_val_ft_sel_1, acc_val_ft_sel_2) # 2


test_pred_ft_sel <- predict(treeModel_ft_sel_2, test_set, type="class")
cm <- confusionMatrix(data = as.factor(test_pred_ft_sel), 
                      reference = as.factor(test_set$label))

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_test_ft_sel <- (cm_relative[1,1] + cm_relative[2,2] + cm_relative[3,3])/3
acc_test_ft_sel


# 5 ---

best_rf <- nTreeList[which.max(acc_val_rf)]
best_rf

best_rfModel <- randomForest(formula=label ~ age + sex + country + latitude + longitude + 
                               date_onset_symptoms + date_admission_hospital + 
                               date_confirmation + lives_in_Wuhan + travel_history_dates + 
                               travel_history_location + chronic_disease_binary + 
                               date_death_or_discharge + travel_history_binary, 
                           data=trainSet, ntree=best_rf)


test_pred <- predict(best_rfModel, test_set, type="class")
cm_test <- confusionMatrix(data = as.factor(test_pred), 
                            reference = as.factor(test_set$label))

cm_relative_test <- calculaMatrizConfusaoRelativa(cm_test)
cm_relative_test

acc_bal_test_rf <- (cm_relative_test[1,1] + cm_relative_test[2,2] + cm_relative_test[3,3])/3
acc_bal_test_rf
