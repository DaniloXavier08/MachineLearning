# Nome dos integrantes do grupo:
# - Danilo Xavier Silva
# - Patrícia Cristina Fernandes


# Funcao de Apoio ao Trabalho 01 de Aprendizado Supervisionado I. 
# Esta fun??o escreve a formula dos modelos polinomiais. 
# Parametros:

# real_feature_names: Um vetor com os nomes dos atributos continuos que voce
#                     quer que seja elevado ao grau desejado.
#  
# categorical_feature_names: Um vetor com os nomes dos atributos categoricos
#                            que voce quer que seja adicionado a hipotese. 
#                            Eles n?o s?o elevados ao grau especificado ja que
#                            sao valores binarios (0 ou 1). Se voce quer uma
#                            hipotese que nao tenha nenhum valor categorico, mas
#                            apenas os reais, basta nao passar nenhum valor 
#                            para este parametro quando chamar a funcao.
#
#
# degree: Grau que voc? deseja que os atributos reais em "real_feature_names"
#         sejam elevados. Ao chamar a funcao, escreva explicitamente
#         o grau desejado. Por exemplo, para grau igual 2, escreva degree=2

# Vejam os exerc?cios 02 e 03 para ver o funcionamento 
# de uma funcao similar a essa.


getHypothesis <- function(real_feature_names, categorical_feature_names=F, degree=3){
    
    hypothesis_string <- "hypothesis <- formula(target ~ "
    for(d in 1:degree){
        for(i in 1:length(real_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       "I(", real_feature_names[i], "^", d, ") + ",
                                       sep = "")
        }
    }
    
    if(typeof(categorical_feature_names) != "logical"){
        for(i in 1:length(categorical_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       categorical_feature_names[i], " + ",
                                       sep = "")
        } 
    }
    
    
    hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
    hypothesis_string <- paste(hypothesis_string, ")")
    hypothesis <- eval(parse(text=hypothesis_string))
    return(hypothesis)
}

# Comandos que leem os conjuntos de treino e de validacao
setwd("~/MDC/0615 - Aprendizado de M?quina Supervisionado I/Trabalho 1")
train_set <- read.csv("training_set_air_quality.csv", stringsAsFactors=TRUE)
val_set <- read.csv("validation_set_air_quality.csv", stringsAsFactors=TRUE)

# Desenvolvam o trabalho a partir daqui, apos executarem os comandos a cima

# 1)

summary(train_set)
summary(val_set)
dim(train_set)
dim(val_set)
merge(train_set,val_set)

train_set$No <- NULL
val_set$No <- NULL

# One-Hot-Encoding
train_set$E <- as.numeric(train_set$wd == "E")
train_set$ENE <- as.numeric(train_set$wd == "ENE")
train_set$ESE <- as.numeric(train_set$wd == "ESE")
train_set$N <- as.numeric(train_set$wd == "N")
train_set$NE <- as.numeric(train_set$wd == "NE")
train_set$NNE <- as.numeric(train_set$wd == "NNE")
train_set$NNW <- as.numeric(train_set$wd == "NNW")
train_set$NW <- as.numeric(train_set$wd == "NW")
train_set$S <- as.numeric(train_set$wd == "S")
train_set$SE <- as.numeric(train_set$wd == "SE")
train_set$SSE <- as.numeric(train_set$wd == "SSE")
train_set$SSW <- as.numeric(train_set$wd == "SSW")
train_set$SW <- as.numeric(train_set$wd == "SW")
train_set$W <- as.numeric(train_set$wd == "W")
train_set$WNW <- as.numeric(train_set$wd == "WNW")
train_set$WSW <- as.numeric(train_set$wd == "WSW")
train_set$wd <- NULL

val_set$E <- as.numeric(val_set$wd == "E")
val_set$ENE <- as.numeric(val_set$wd == "ENE")
val_set$ESE <- as.numeric(val_set$wd == "ESE")
val_set$N <- as.numeric(val_set$wd == "N")
val_set$NE <- as.numeric(val_set$wd == "NE")
val_set$NNE <- as.numeric(val_set$wd == "NNE")
val_set$NNW <- as.numeric(val_set$wd == "NNW")
val_set$NW <- as.numeric(val_set$wd == "NW")
val_set$S <- as.numeric(val_set$wd == "S")
val_set$SE <- as.numeric(val_set$wd == "SE")
val_set$SSE <- as.numeric(val_set$wd == "SSE")
val_set$SSW <- as.numeric(val_set$wd == "SSW")
val_set$SW <- as.numeric(val_set$wd == "SW")
val_set$W <- as.numeric(val_set$wd == "W")
val_set$WNW <- as.numeric(val_set$wd == "WNW")
val_set$WSW <- as.numeric(val_set$wd == "WSW")
val_set$wd <- NULL


any(is.na(train_set))
any(is.na(val_set))
# N?o h? nenhuma feature sem anota??es

# 2)

# MinMax normalization
min_features <- apply(train_set[,1:14], 2, min)
min_features

max_features <- apply(train_set[,1:14], 2, max)
max_features

diff <- max_features - min_features
diff

train_set[,1:14] <- sweep(train_set[,1:14], 2, min_features, "-")
train_set[,1:14] <- sweep(train_set[,1:14], 2, diff, "/")
summary(train_set)

val_set[,1:14] <- sweep(val_set[,1:14], 2, min_features, "-")
val_set[,1:14] <- sweep(val_set[,1:14], 2, diff, "/")

# 3) 
feature_names <- colnames(train_set[,1:14])
feature_names

categorical_names <- colnames(train_set[,16:31])
categorical_names

hypothesis <- getHypothesis(feature_names, categorical_feature_names=categorical_names, degree=1)
hypothesis

baseline <- lm(formula=hypothesis, data=train_set)
summary(baseline)

train_pred <- predict(baseline, train_set)
val_pred <- predict(baseline, val_set)
#test_pred <- predict(baseline, test_set)


MAE <- function(preds, labels){
    mae_values <- sum(abs(preds-labels))/length(preds)
    return(mae_values)
}

MSE <- function(preds, labels){
    mse_values <- sum((preds-labels)**2)/length(preds)
    return(mse_values)
}

mae_train_baseline <- MAE(train_pred, train_set$target)
mae_train_baseline

mae_val_baseline <- MAE(val_pred, val_set$target)
mae_val_baseline

# 4)

## Combining features ### 
f01 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 
               + NO2 + O3 + TEMP + PRES + DEWP + RAIN + WSPM
               + (year + month + day + hour + PM2.5 + PM10 + SO2 
                  + NO2 + O3 + TEMP + PRES + DEWP + RAIN + WSPM)^2)

f02 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 
               + NO2 + O3 + TEMP + PRES + DEWP + RAIN + WSPM
               + (year + month + day + hour + PM2.5 + PM10 + SO2 
                  + NO2 + O3 + TEMP + PRES + DEWP + RAIN + WSPM)^3)

f03 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 
               + NO2 + O3 + TEMP + PRES + DEWP + RAIN + WSPM
               + (year + month + day + hour + PM2.5 + PM10 + SO2 
                  + NO2 + O3 + TEMP + PRES + DEWP + RAIN + WSPM)^4)

modelsNoCategorical <- c(f01, f02, f03)
total_mae_train_noCat <- c(length(modelsNoCategorical))
total_mae_val_noCat <- c(length(modelsNoCategorical))

i <- 1
for(f in modelsNoCategorical){
    print(i)
    model <- lm(formula=f, data=train_set)
    
    valPred <- predict(model, val_set)
    trainPred <- predict(model, train_set)
    
    mae_train <- MAE(trainPred, train_set$target)
    total_mae_train_noCat[i] <- mae_train
    # print(mae_train)
    
    mae_val <- MAE(valPred, val_set$target)
    total_mae_val_noCat[i] <- mae_val
    # print(mae_val)
    i <- i + 1

}

plot(total_mae_val_noCat, xlab="Complexity", ylab="Target", 
     ylim=c(260, 400), pch="+", col="blue",  xaxt="n")
axis(1, at=1:length(modelsNoCategorical), labels=seq(from = 1, to = 3, by = 1), las=1)
points(total_mae_train_noCat, pch="*", col="red")
points(rep(mae_val_baseline, length(total_mae_val_noCat)), pch="o", col="green")

lines(total_mae_train_noCat, col="red", lty=2)
lines(total_mae_val_noCat, col="blue", lty=2)
lines(rep(mae_val_baseline, length(total_mae_val_noCat)), col="green", lty=2)

# Pegando o menor valor de valida??o
min(total_mae_val_noCat) # 3

# 5)

# Polinomios

# Polinomios

f01 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 + NO2 
               + O3 + TEMP + PRES + DEWP + RAIN + WSPM, data=train_set)

f02 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 + NO2 
               + O3 + TEMP + PRES + DEWP + RAIN + WSPM
               + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) 
               + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2)+ I(TEMP^2) + I(PRES^2) 
               + I(DEWP^2) + I(RAIN^2) + I(WSPM^2), data=train_set)

f03 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 + NO2 
               + O3 + TEMP + PRES + DEWP + RAIN + WSPM
               + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) 
               + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2)+ I(TEMP^2) + I(PRES^2) 
               + I(DEWP^2) + I(RAIN^2) + I(WSPM^2)
               + I(year^3) + I(month^3) + I(day^3) + I(hour^3) + I(PM2.5^3) 
               + I(PM10^3) + I(SO2^3) + I(NO2^3) + I(O3^3)+ I(TEMP^3) + I(PRES^3) 
               + I(DEWP^3) + I(RAIN^3) + I(WSPM^3), data=train_set)

f04 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 + NO2 
               + O3 + TEMP + PRES + DEWP + RAIN + WSPM
               + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) 
               + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2)+ I(TEMP^2) + I(PRES^2) 
               + I(DEWP^2) + I(RAIN^2) + I(WSPM^2)
               + I(year^3) + I(month^3) + I(day^3) + I(hour^3) + I(PM2.5^3) 
               + I(PM10^3) + I(SO2^3) + I(NO2^3) + I(O3^3)+ I(TEMP^3) + I(PRES^3) 
               + I(DEWP^3) + I(RAIN^3) + I(WSPM^3)
               + I(year^4) + I(month^4) + I(day^4) + I(hour^4) + I(PM2.5^4) 
               + I(PM10^4) + I(SO2^4) + I(NO2^4) + I(O3^4)+ I(TEMP^4) + I(PRES^4) 
               + I(DEWP^4) + I(RAIN^4) + I(WSPM^4), data=train_set)

f05 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 + NO2 
               + O3 + TEMP + PRES + DEWP + RAIN + WSPM
               + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) 
               + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2)+ I(TEMP^2) + I(PRES^2) 
               + I(DEWP^2) + I(RAIN^2) + I(WSPM^2)
               + I(year^3) + I(month^3) + I(day^3) + I(hour^3) + I(PM2.5^3) 
               + I(PM10^3) + I(SO2^3) + I(NO2^3) + I(O3^3)+ I(TEMP^3) + I(PRES^3) 
               + I(DEWP^3) + I(RAIN^3) + I(WSPM^3)
               + I(year^4) + I(month^4) + I(day^4) + I(hour^4) + I(PM2.5^4) 
               + I(PM10^4) + I(SO2^4) + I(NO2^4) + I(O3^4)+ I(TEMP^4) + I(PRES^4) 
               + I(DEWP^4) + I(RAIN^4) + I(WSPM^4)
               + I(year^5) + I(month^5) + I(day^5) + I(hour^5) + I(PM2.5^5) 
               + I(PM10^5) + I(SO2^5) + I(NO2^5) + I(O3^5)+ I(TEMP^5) + I(PRES^5) 
               + I(DEWP^5) + I(RAIN^5) + I(WSPM^5), data=train_set)

f06 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 + NO2 
               + O3 + TEMP + PRES + DEWP + RAIN + WSPM
               + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) 
               + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2)+ I(TEMP^2) + I(PRES^2) 
               + I(DEWP^2) + I(RAIN^2) + I(WSPM^2)
               + I(year^3) + I(month^3) + I(day^3) + I(hour^3) + I(PM2.5^3) 
               + I(PM10^3) + I(SO2^3) + I(NO2^3) + I(O3^3)+ I(TEMP^3) + I(PRES^3) 
               + I(DEWP^3) + I(RAIN^3) + I(WSPM^3)
               + I(year^4) + I(month^4) + I(day^4) + I(hour^4) + I(PM2.5^4) 
               + I(PM10^4) + I(SO2^4) + I(NO2^4) + I(O3^4)+ I(TEMP^4) + I(PRES^4) 
               + I(DEWP^4) + I(RAIN^4) + I(WSPM^4)
               + I(year^5) + I(month^5) + I(day^5) + I(hour^5) + I(PM2.5^5) 
               + I(PM10^5) + I(SO2^5) + I(NO2^5) + I(O3^5)+ I(TEMP^5) + I(PRES^5) 
               + I(DEWP^5) + I(RAIN^5) + I(WSPM^5)
               + I(year^6) + I(month^6) + I(day^6) + I(hour^6) + I(PM2.5^6) 
               + I(PM10^6) + I(SO2^6) + I(NO2^6) + I(O3^6)+ I(TEMP^6) + I(PRES^6) 
               + I(DEWP^6) + I(RAIN^6) + I(WSPM^6), data=train_set)

f07 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 + NO2 
               + O3 + TEMP + PRES + DEWP + RAIN + WSPM
               + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) 
               + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2)+ I(TEMP^2) + I(PRES^2) 
               + I(DEWP^2) + I(RAIN^2) + I(WSPM^2)
               + I(year^3) + I(month^3) + I(day^3) + I(hour^3) + I(PM2.5^3) 
               + I(PM10^3) + I(SO2^3) + I(NO2^3) + I(O3^3)+ I(TEMP^3) + I(PRES^3) 
               + I(DEWP^3) + I(RAIN^3) + I(WSPM^3)
               + I(year^4) + I(month^4) + I(day^4) + I(hour^4) + I(PM2.5^4) 
               + I(PM10^4) + I(SO2^4) + I(NO2^4) + I(O3^4)+ I(TEMP^4) + I(PRES^4) 
               + I(DEWP^4) + I(RAIN^4) + I(WSPM^4)
               + I(year^5) + I(month^5) + I(day^5) + I(hour^5) + I(PM2.5^5) 
               + I(PM10^5) + I(SO2^5) + I(NO2^5) + I(O3^5)+ I(TEMP^5) + I(PRES^5) 
               + I(DEWP^5) + I(RAIN^5) + I(WSPM^5)
               + I(year^6) + I(month^6) + I(day^6) + I(hour^6) + I(PM2.5^6) 
               + I(PM10^6) + I(SO2^6) + I(NO2^6) + I(O3^6)+ I(TEMP^6) + I(PRES^6) 
               + I(DEWP^6) + I(RAIN^6) + I(WSPM^6)
               + I(year^7) + I(month^7) + I(day^7) + I(hour^7) + I(PM2.5^7) 
               + I(PM10^7) + I(SO2^7) + I(NO2^7) + I(O3^7)+ I(TEMP^7) + I(PRES^7) 
               + I(DEWP^7) + I(RAIN^7) + I(WSPM^7), data=train_set)

f08 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 + NO2 
               + O3 + TEMP + PRES + DEWP + RAIN + WSPM
               + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) 
               + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2)+ I(TEMP^2) + I(PRES^2) 
               + I(DEWP^2) + I(RAIN^2) + I(WSPM^2)
               + I(year^3) + I(month^3) + I(day^3) + I(hour^3) + I(PM2.5^3) 
               + I(PM10^3) + I(SO2^3) + I(NO2^3) + I(O3^3)+ I(TEMP^3) + I(PRES^3) 
               + I(DEWP^3) + I(RAIN^3) + I(WSPM^3)
               + I(year^4) + I(month^4) + I(day^4) + I(hour^4) + I(PM2.5^4) 
               + I(PM10^4) + I(SO2^4) + I(NO2^4) + I(O3^4)+ I(TEMP^4) + I(PRES^4) 
               + I(DEWP^4) + I(RAIN^4) + I(WSPM^4)
               + I(year^5) + I(month^5) + I(day^5) + I(hour^5) + I(PM2.5^5) 
               + I(PM10^5) + I(SO2^5) + I(NO2^5) + I(O3^5)+ I(TEMP^5) + I(PRES^5) 
               + I(DEWP^5) + I(RAIN^5) + I(WSPM^5)
               + I(year^6) + I(month^6) + I(day^6) + I(hour^6) + I(PM2.5^6) 
               + I(PM10^6) + I(SO2^6) + I(NO2^6) + I(O3^6)+ I(TEMP^6) + I(PRES^6) 
               + I(DEWP^6) + I(RAIN^6) + I(WSPM^6)
               + I(year^7) + I(month^7) + I(day^7) + I(hour^7) + I(PM2.5^7) 
               + I(PM10^7) + I(SO2^7) + I(NO2^7) + I(O3^7)+ I(TEMP^7) + I(PRES^7) 
               + I(DEWP^7) + I(RAIN^7) + I(WSPM^7)
               + I(year^8) + I(month^8) + I(day^8) + I(hour^8) + I(PM2.5^8) 
               + I(PM10^8) + I(SO2^8) + I(NO2^8) + I(O3^8)+ I(TEMP^8) + I(PRES^8) 
               + I(DEWP^8) + I(RAIN^8) + I(WSPM^8), data=train_set)

f09 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 + NO2 
               + O3 + TEMP + PRES + DEWP + RAIN + WSPM
               + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) 
               + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2)+ I(TEMP^2) + I(PRES^2) 
               + I(DEWP^2) + I(RAIN^2) + I(WSPM^2)
               + I(year^3) + I(month^3) + I(day^3) + I(hour^3) + I(PM2.5^3) 
               + I(PM10^3) + I(SO2^3) + I(NO2^3) + I(O3^3)+ I(TEMP^3) + I(PRES^3) 
               + I(DEWP^3) + I(RAIN^3) + I(WSPM^3)
               + I(year^4) + I(month^4) + I(day^4) + I(hour^4) + I(PM2.5^4) 
               + I(PM10^4) + I(SO2^4) + I(NO2^4) + I(O3^4)+ I(TEMP^4) + I(PRES^4) 
               + I(DEWP^4) + I(RAIN^4) + I(WSPM^4)
               + I(year^5) + I(month^5) + I(day^5) + I(hour^5) + I(PM2.5^5) 
               + I(PM10^5) + I(SO2^5) + I(NO2^5) + I(O3^5)+ I(TEMP^5) + I(PRES^5) 
               + I(DEWP^5) + I(RAIN^5) + I(WSPM^5)
               + I(year^6) + I(month^6) + I(day^6) + I(hour^6) + I(PM2.5^6) 
               + I(PM10^6) + I(SO2^6) + I(NO2^6) + I(O3^6)+ I(TEMP^6) + I(PRES^6) 
               + I(DEWP^6) + I(RAIN^6) + I(WSPM^6)
               + I(year^7) + I(month^7) + I(day^7) + I(hour^7) + I(PM2.5^7) 
               + I(PM10^7) + I(SO2^7) + I(NO2^7) + I(O3^7)+ I(TEMP^7) + I(PRES^7) 
               + I(DEWP^7) + I(RAIN^7) + I(WSPM^7)
               + I(year^8) + I(month^8) + I(day^8) + I(hour^8) + I(PM2.5^8) 
               + I(PM10^8) + I(SO2^8) + I(NO2^8) + I(O3^8)+ I(TEMP^8) + I(PRES^8) 
               + I(DEWP^8) + I(RAIN^8) + I(WSPM^8)
               + I(year^9) + I(month^9) + I(day^9) + I(hour^9) + I(PM2.5^9) 
               + I(PM10^9) + I(SO2^9) + I(NO2^9) + I(O3^9)+ I(TEMP^9) + I(PRES^9) 
               + I(DEWP^9) + I(RAIN^9) + I(WSPM^9), data=train_set)

f10 <- formula(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 + NO2 
               + O3 + TEMP + PRES + DEWP + RAIN + WSPM
               + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) 
               + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2)+ I(TEMP^2) + I(PRES^2) 
               + I(DEWP^2) + I(RAIN^2) + I(WSPM^2)
               + I(year^3) + I(month^3) + I(day^3) + I(hour^3) + I(PM2.5^3) 
               + I(PM10^3) + I(SO2^3) + I(NO2^3) + I(O3^3)+ I(TEMP^3) + I(PRES^3) 
               + I(DEWP^3) + I(RAIN^3) + I(WSPM^3)
               + I(year^4) + I(month^4) + I(day^4) + I(hour^4) + I(PM2.5^4) 
               + I(PM10^4) + I(SO2^4) + I(NO2^4) + I(O3^4)+ I(TEMP^4) + I(PRES^4) 
               + I(DEWP^4) + I(RAIN^4) + I(WSPM^4)
               + I(year^5) + I(month^5) + I(day^5) + I(hour^5) + I(PM2.5^5) 
               + I(PM10^5) + I(SO2^5) + I(NO2^5) + I(O3^5)+ I(TEMP^5) + I(PRES^5) 
               + I(DEWP^5) + I(RAIN^5) + I(WSPM^5)
               + I(year^6) + I(month^6) + I(day^6) + I(hour^6) + I(PM2.5^6) 
               + I(PM10^6) + I(SO2^6) + I(NO2^6) + I(O3^6)+ I(TEMP^6) + I(PRES^6) 
               + I(DEWP^6) + I(RAIN^6) + I(WSPM^6)
               + I(year^7) + I(month^7) + I(day^7) + I(hour^7) + I(PM2.5^7) 
               + I(PM10^7) + I(SO2^7) + I(NO2^7) + I(O3^7)+ I(TEMP^7) + I(PRES^7) 
               + I(DEWP^7) + I(RAIN^7) + I(WSPM^7)
               + I(year^8) + I(month^8) + I(day^8) + I(hour^8) + I(PM2.5^8) 
               + I(PM10^8) + I(SO2^8) + I(NO2^8) + I(O3^8)+ I(TEMP^8) + I(PRES^8) 
               + I(DEWP^8) + I(RAIN^8) + I(WSPM^8)
               + I(year^9) + I(month^9) + I(day^9) + I(hour^9) + I(PM2.5^9) 
               + I(PM10^9) + I(SO2^9) + I(NO2^9) + I(O3^9)+ I(TEMP^9) + I(PRES^9) 
               + I(DEWP^9) + I(RAIN^9) + I(WSPM^9)
               + I(year^10) + I(month^10) + I(day^10) + I(hour^10) + I(PM2.5^10) 
               + I(PM10^10) + I(SO2^10) + I(NO2^10) + I(O3^10)+ I(TEMP^10) + I(PRES^10) 
               + I(DEWP^10) + I(RAIN^10) + I(WSPM^10), data=train_set)


formulas <- list(f01, f02, f03, f04, f05, f06, f07, f08, f09, f10)
total_mae_train_poly <- c(length(formulas))
total_mae_val_poly <- c(length(formulas))
i <- 1
for(i in 1:10){
    model <- lm(formula=formulas[[i]], data=train_set)
    
    print("model")
    valPred <- predict(model, val_set)
    trainPred <- predict(model, train_set)
    print("predict")
    
    mae_train <- MAE(trainPred, train_set$target)
    total_mae_train_poly[i] <- mae_train
    print(mae_train)
    
    mae_val <- MAE(valPred, val_set$target)
    total_mae_val_poly[i] <- mae_val
    print(mae_val)
    i <- i + 1
    
}

summary(model)
plot(total_mae_val_poly, xlab="Complexity", ylab="Error", 
     ylim=c(330, max(total_mae_val_poly)+30), pch="+", col="blue")

points(total_mae_train_poly, pch="*", col="red")
points(rep(mae_val_baseline, length(total_mae_val_poly)), pch="o", col="green")

lines(total_mae_train_poly, col="red", lty=2)
lines(total_mae_val_poly, col="blue", lty=2)
lines(rep(mae_val_baseline, length(total_mae_val_poly)), col="green", lty=2)
legend(1, 420, legend=c("Train", "Validation", "Baseline"), 
       col=c("red","blue", "green"), lty=2, cex=0.7)

# Pegando o menor valor de valida??o
min(total_mae_val_poly)  # 5


# Descomente a linha abaixo apenas quando o conjunto de teste esiver dispon?vel
test_set <- read.csv("test_set_air_quality.csv", stringsAsFactors=TRUE)
any(is.na(test_set))

test_set$E <- as.numeric(test_set$wd == "E")
test_set$ENE <- as.numeric(test_set$wd == "ENE")
test_set$ESE <- as.numeric(test_set$wd == "ESE")
test_set$N <- as.numeric(test_set$wd == "N")
test_set$NE <- as.numeric(test_set$wd == "NE")
test_set$NNE <- as.numeric(test_set$wd == "NNE")
test_set$NNW <- as.numeric(test_set$wd == "NNW")
test_set$NW <- as.numeric(test_set$wd == "NW")
test_set$S <- as.numeric(test_set$wd == "S")
test_set$SE <- as.numeric(test_set$wd == "SE")
test_set$SSE <- as.numeric(test_set$wd == "SSE")
test_set$SSW <- as.numeric(test_set$wd == "SSW")
test_set$SW <- as.numeric(test_set$wd == "SW")
test_set$W <- as.numeric(test_set$wd == "W")
test_set$WNW <- as.numeric(test_set$wd == "WNW")
test_set$WSW <- as.numeric(test_set$wd == "WSW")

test_set$wd <- NULL
test_set$No <- NULL

test_set[,1:14] <- sweep(test_set[,1:14], 2, min_features, "-")
test_set[,1:14] <- sweep(test_set[,1:14], 2, diff, "/")


# Pegando o menor valor de valida??o
min(total_mae_val_noCat) # 3 - 269.5159
min(total_mae_val_poly)  # 5 - 343.4777


## Retrain the best model ##
# 4) Apenas melhor solu??o baseada no conjunto de valida??o 
best_model <- lm(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 
                 + NO2 + O3 + TEMP + PRES + DEWP + RAIN + WSPM
                 + (year + month + day + hour + PM2.5 + PM10 + SO2 
                 + NO2 + O3 + TEMP + PRES + DEWP + RAIN + WSPM)^4, data=train_set)

############################
test_pred <- predict(best_model, test_set)
mae_test <- MAE(test_pred, test_set$target)
mae_test


## Retrain the best model ##
# 5) Apenas o melhor modelo polinomial baseado no conjunto de valida??o
best_model <- lm(target ~ year + month + day + hour + PM2.5 + PM10 + SO2 + NO2 
                 + O3 + TEMP + PRES + DEWP + RAIN + WSPM
                 + I(year^2) + I(month^2) + I(day^2) + I(hour^2) + I(PM2.5^2) 
                 + I(PM10^2) + I(SO2^2) + I(NO2^2) + I(O3^2)+ I(TEMP^2) + I(PRES^2) 
                 + I(DEWP^2) + I(RAIN^2) + I(WSPM^2)
                 + I(year^3) + I(month^3) + I(day^3) + I(hour^3) + I(PM2.5^3) 
                 + I(PM10^3) + I(SO2^3) + I(NO2^3) + I(O3^3)+ I(TEMP^3) + I(PRES^3) 
                 + I(DEWP^3) + I(RAIN^3) + I(WSPM^3)
                 + I(year^4) + I(month^4) + I(day^4) + I(hour^4) + I(PM2.5^4) 
                 + I(PM10^4) + I(SO2^4) + I(NO2^4) + I(O3^4)+ I(TEMP^4) + I(PRES^4) 
                 + I(DEWP^4) + I(RAIN^4) + I(WSPM^4)
                 + I(year^5) + I(month^5) + I(day^5) + I(hour^5) + I(PM2.5^5) 
                 + I(PM10^5) + I(SO2^5) + I(NO2^5) + I(O3^5)+ I(TEMP^5) + I(PRES^5) 
                 + I(DEWP^5) + I(RAIN^5) + I(WSPM^5), data=train_set)

############################
test_pred <- predict(best_model, test_set)
mae_test <- MAE(test_pred, test_set$target)
mae_test







